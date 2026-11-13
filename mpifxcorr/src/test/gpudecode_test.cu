/*
 * gpudecode_test: the GPU's per-sample decoders against mark5access, sample by sample.
 *
 * For each mode (VDIF or CODIF, real or complex, 1/2/4/8/16 bits, 1..16 channels) it
 * builds a few frames with real headers (vdifio / codifio) and random payloads, decodes
 * them with mark5access exactly as mpifxcorr's CPU path does (Mk5Mode: an unpacker stream
 * with the format string Configuration::genMk5FormatName makes, and
 * mark5_unpack[_complex]_with_offset over the frames), and decodes the same bytes on the
 * GPU with decode_one_gpu / decode_one_complex_gpu, called as the fused decode+fringe
 * kernels call them (skipped = codif ? 0 : channel_skip(nchan), decimation 1).  Every
 * sample must agree exactly, except for 4- and 8-bit VDIF, where 1 ulp is allowed:
 * mark5access computes those levels in double precision and stores them as float
 * ((v-7.5)/2.95, (v-128)/3.3), the GPU computes them in single precision with a
 * reciprocal multiply.  The difference (~1e-7 relative) is immaterial; making the GPU
 * bit-identical would need lookup tables or IEEE division in the decode kernel.
 *
 * Modes that mark5access cannot decode are reported as SKIP.  Only power-of-2 channel
 * counts are tested: mark5access's CODIF decoders for other counts assume VDIF-style
 * padding, which CODIF does not have (fixed on the cjp_vdif_testing branch, not yet in dev).
 *
 * KNOWN_BAD lists modes where mark5access itself is wrong, so the CPU reference is: they
 * are reported as KNOWN and do not fail the test.  If one of them starts to agree (the
 * mark5access fix has arrived), it is reported as FIXED and the test fails, so that the
 * entry gets removed.
 *
 * Usage: gpudecode_test [-v]      exit status 0 if every tested mode agrees exactly
 */

#include "gpudecode.cu"   // the device decode functions under test (one translation unit)

#include <cuda_runtime.h>
#include <vector>
#include <string>
#include <random>
#include <cmath>
#include <cstdio>

namespace {

const int NFRAMES = 4;
const int PAYLOAD = 8000;          // bytes; 64000 bits, divisible by every sample size used
const int FRAMESPERSEC = 1000;

struct Mode { bool codif; bool cplx; int nbit; int nchan; };

// mark5access bugs (CPU reference wrong), fixed on cjp_vdif_testing by 410bd6e69
// ("mark5access: fix CODIF decoder bugs"), not yet in dev.
struct Known { Mode m; const char *why; };
const Known KNOWN_BAD[] = {
	{{true, true, 1, 1}, "mark5access complex 1-bit 1/2 ch advances the output one sample too far per byte (410bd6e69)"},
	{{true, true, 1, 2}, "mark5access complex 1-bit 1/2 ch advances the output one sample too far per byte (410bd6e69)"},
	{{true, false, 16, 2}, "mark5access real 16-bit 2/3 ch truncates values (integer /8) (410bd6e69)"},
};

const char *known_bad(const Mode &m) {
	for (const Known &k : KNOWN_BAD)
		if (k.m.codif == m.codif && k.m.cplx == m.cplx && k.m.nbit == m.nbit && k.m.nchan == m.nchan)
			return k.why;
	return nullptr;
}

#define CUDA_OK(x) do { cudaError_t e_ = (x); if (e_ != cudaSuccess) { \
	fprintf(stderr, "CUDA error %s at %s:%d\n", cudaGetErrorString(e_), __FILE__, __LINE__); exit(2); } } while (0)

__global__ void decode_all(const unsigned char *frames, int framebytes, int headerbytes, int nframes,
		int spf, int nbit, int nchan, int skipped, bool codif, bool cplx, float2 *out) {
	const long i = (long)blockIdx.x * blockDim.x + threadIdx.x;   // (frame, sample, band)
	const long n = (long)nframes * spf * nchan;
	if (i >= n) return;
	const int band = (int)(i % nchan);
	const long s = i / nchan;
	const int frame = (int)(s / spf);
	const int sample_in_frame = (int)(s % spf);
	const unsigned char *payload = frames + (size_t)frame * framebytes + headerbytes;
	if (cplx) {
		cuFloatComplex c = decode_one_complex_gpu(payload, nbit, nchan, 1, skipped, sample_in_frame, band, codif);
		out[band * (long)nframes * spf + s] = make_float2(c.x, c.y);
	} else {
		float v = decode_one_gpu(payload, nbit, nchan, 1, skipped, sample_in_frame, band, codif);
		out[band * (long)nframes * spf + s] = make_float2(v, 0.f);
	}
}

// Frames with valid headers and random payloads.  Returns the frame size, 0 on failure.
int make_frames(const Mode &m, std::vector<unsigned char> &buf, std::mt19937 &rng) {
	const int hdr = m.codif ? CODIF_HEADER_BYTES : VDIF_HEADER_BYTES;
	const int framebytes = hdr + PAYLOAD;
	const int cfact = m.cplx ? 2 : 1;
	const int spf = PAYLOAD * 8 / (m.nbit * cfact * m.nchan);
	const int mjd = 60000;
	buf.assign((size_t)framebytes * NFRAMES, 0);
	std::uniform_int_distribution<int> byte(0, 255);
	if (m.codif) {
		codif_header h;
		int completesample = m.nbit * cfact * m.nchan;          // bits, as in generateVDIF
		int sampleblock = 64;
		while (sampleblock % completesample) sampleblock += 64;
		sampleblock /= 64;
		char id[3] = {'T', 't', 0};
		if (createCODIFHeader(&h, PAYLOAD, 0, 0, m.nbit, m.nchan, sampleblock, 1,
				(uint64_t)spf * FRAMESPERSEC, m.cplx, id, 0) != CODIF_NOERROR)
			return 0;
		setCODIFEpochMJD(&h, mjd);
		setCODIFFrameMJDSec(&h, (uint64_t)mjd * 86400);
		setCODIFFrameNumber(&h, 0);
		for (int f = 0; f < NFRAMES; f++) {
			memcpy(&buf[(size_t)f * framebytes], &h, hdr);
			nextCODIFHeader(&h, FRAMESPERSEC);
		}
	} else {
		vdif_header h;
		if (createVDIFHeader(&h, PAYLOAD, 0, m.nbit, m.nchan, m.cplx, "Tt") != VDIF_NOERROR)
			return 0;
		setVDIFEpochMJD(&h, mjd);
		setVDIFFrameMJDSec(&h, (uint64_t)mjd * 86400);
		setVDIFFrameNumber(&h, 0);
		for (int f = 0; f < NFRAMES; f++) {
			memcpy(&buf[(size_t)f * framebytes], &h, hdr);
			nextVDIFHeader(&h, FRAMESPERSEC);
		}
	}
	for (int f = 0; f < NFRAMES; f++)
		for (int b = 0; b < PAYLOAD; b++)
			buf[(size_t)f * framebytes + hdr + b] = (unsigned char)byte(rng);
	return framebytes;
}

// 0 = agree, 1 = mismatch, 2 = skipped (mark5access cannot decode it)
int test_mode(const Mode &m, std::mt19937 &rng, bool verbose, std::string &note) {
	std::vector<unsigned char> buf;
	const int framebytes = make_frames(m, buf, rng);
	if (!framebytes) { note = "header creation failed"; return 1; }
	const int cfact = m.cplx ? 2 : 1;
	const int spf = PAYLOAD * 8 / (m.nbit * cfact * m.nchan);
	const long nsamp = (long)NFRAMES * spf;

	char fmt[64];
	snprintf(fmt, sizeof(fmt), "%s%s_%d-%dm%d-%d-%d", m.codif ? "CODIF" : "VDIF", m.cplx ? "C" : "",
			PAYLOAD, FRAMESPERSEC, 1, m.nchan, m.nbit);
	struct mark5_format_generic *f = new_mark5_format_generic_from_string(fmt);
	if (!f) { note = std::string("mark5access: no format for ") + fmt; return 2; }
	struct mark5_stream *ms = new_mark5_stream_absorb(new_mark5_stream_unpacker(0), f);
	if (!ms) { note = std::string("mark5access: cannot open ") + fmt; return 2; }

	// CPU reference
	std::vector<std::vector<float>> ref(m.nchan, std::vector<float>(nsamp * cfact));
	std::vector<float *> refp(m.nchan);
	for (int c = 0; c < m.nchan; c++) refp[c] = ref[c].data();
	int got = m.cplx ? mark5_unpack_complex_with_offset(ms, buf.data(), 0, (mark5_float_complex **)refp.data(), (int)nsamp)
	                 : mark5_unpack_with_offset(ms, buf.data(), 0, refp.data(), (int)nsamp);
	delete_mark5_stream(ms);
	if (got != nsamp) {
		char t[96]; snprintf(t, sizeof(t), "mark5access decoded %d of %ld samples", got, nsamp);
		note = t; return 1;
	}

	// GPU
	unsigned char *dframes; float2 *dout;
	CUDA_OK(cudaMalloc(&dframes, buf.size()));
	CUDA_OK(cudaMalloc(&dout, sizeof(float2) * nsamp * m.nchan));
	CUDA_OK(cudaMemcpy(dframes, buf.data(), buf.size(), cudaMemcpyHostToDevice));
	const long n = nsamp * m.nchan;
	const int skipped = m.codif ? 0 : channel_skip(m.nchan);
	decode_all<<<(unsigned)((n + 255) / 256), 256>>>(dframes, framebytes,
			m.codif ? CODIF_HEADER_BYTES : VDIF_HEADER_BYTES, NFRAMES, spf, m.nbit, m.nchan, skipped,
			m.codif, m.cplx, dout);
	CUDA_OK(cudaGetLastError());
	std::vector<float2> gpu(n);
	CUDA_OK(cudaMemcpy(gpu.data(), dout, sizeof(float2) * n, cudaMemcpyDeviceToHost));
	CUDA_OK(cudaFree(dframes)); CUDA_OK(cudaFree(dout));

	const bool ulp1 = !m.codif && (m.nbit == 4 || m.nbit == 8);   // see the header comment
	auto agree = [ulp1](float a, float b) {
		return a == b || (ulp1 && (a == nextafterf(b, INFINITY) || a == nextafterf(b, -INFINITY)));
	};
	long bad = 0, nulp = 0; double maxdiff = 0;
	for (int c = 0; c < m.nchan; c++)
		for (long s = 0; s < nsamp; s++) {
			const float2 g = gpu[c * nsamp + s];
			const float re = m.cplx ? ref[c][2 * s] : ref[c][s];
			const float im = m.cplx ? ref[c][2 * s + 1] : 0.f;
			if (g.x != re || g.y != im) {
				maxdiff = fmax(maxdiff, fmax(fabs(g.x - re), fabs(g.y - im)));
				if (agree(g.x, re) && agree(g.y, im)) { nulp++; continue; }
				if (verbose && bad < 5)
					printf("    band %d sample %ld: mark5access (%g, %g) GPU (%g, %g)\n", c, s, re, im, g.x, g.y);
				bad++;
			}
		}
	char t[160];
	if (ulp1)
		snprintf(t, sizeof(t), "%s: %ld samples x %d bands, %ld differ, %ld by 1 ulp (allowed; max |diff| %g)",
				fmt, nsamp, m.nchan, bad, nulp, maxdiff);
	else
		snprintf(t, sizeof(t), "%s: %ld samples x %d bands, %ld differ (max |diff| %g)", fmt, nsamp, m.nchan, bad, maxdiff);
	note = t;
	return bad ? 1 : 0;
}

}  // namespace

int main(int argc, char **argv) {
	const bool verbose = argc > 1 && std::string(argv[1]) == "-v";
	std::mt19937 rng(20261113);
	int npass = 0, nfail = 0, nskip = 0, nknown = 0, nfixed = 0;
	for (int codif = 0; codif <= 1; codif++)
		for (int cplx = 0; cplx <= 1; cplx++)
			for (int nbit : {1, 2, 4, 8, 16})
				for (int nchan : {1, 2, 4, 8, 16}) {
					Mode m{codif == 1, cplx == 1, nbit, nchan};
					std::string note;
					int r = test_mode(m, rng, verbose, note);
					const char *why = known_bad(m);
					const char *verdict = r == 0 ? "PASS" : r == 1 ? "FAIL" : "SKIP";
					if (why && r == 1) { verdict = "KNOWN"; note += std::string("; ") + why; nknown++; }
					else if (why && r == 0) { verdict = "FIXED"; note += "; listed in KNOWN_BAD but now agrees: remove it"; nfixed++; }
					else (r == 0 ? npass : r == 1 ? nfail : nskip)++;
					printf("%-5s %-5s %-7s %2d-bit %2d ch  %s\n", verdict, codif ? "CODIF" : "VDIF",
							cplx ? "complex" : "real", nbit, nchan, note.c_str());
					fflush(stdout);
				}
	printf("\n%d PASS, %d FAIL, %d KNOWN (mark5access bug), %d FIXED (remove from KNOWN_BAD), %d SKIP (mark5access has no decoder)\n",
			npass, nfail, nknown, nfixed, nskip);
	return (nfail || nfixed) ? 1 : 0;
}
