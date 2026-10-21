#include <mark5access.h>
#include <mark5access/mark5_stream.h>
#include <vdifio.h>
#include "gpudecode.cuh"
#include <iostream>
#include <bitset>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#define PCAL_DEBUG_FFTLOOP       0
#define PCAL_DEBUG_SUBLOOP       0
#define PCAL_DEBUG_BAND          0
#define PCAL_DEBUG_CH_FIRST      0
#define PCAL_DEBUG_CH_COUNT      64

/* Select one tiled channel block: blockIdx.y. */
#define PCAL_DEBUG_TILE_Y        0


#define MARK5_FILL_WORD64 0x1122334411223344ULL

// 2*pi to double precision - matches architecture.h's TWO_PI (IPP_2PI) that the
// non-fused fringe kernels used, so the fused rotator below is numerically
// equivalent. Defined locally because gpudecode.cu does not include the IPP/FFTW
// architecture.h layer.
#ifndef GPUDECODE_TWO_PI
#define GPUDECODE_TWO_PI 6.283185307179586476925286766559
#endif

/* the high mag value for 2-bit reconstruction */
static const float HiMag = OPTIMAL_2BIT_HIGH;
static const float FourBit1sigma = 2.95;

// Stack all quantization levels (nbit <= 2) float values in one array and use nbit to offset accordingly
__constant__ float lutall[6] = {-1.0, 1.0, -HiMag, -1.0, 1.0, HiMag};

__device__ __forceinline__ float bitsread_gpu(char byte, int pos, int nbit) {
	// std::cout << "Byte : " << std::bitset<8>(byte) << std::endl;
	// std::cout << "Pos :  " << pos << "\tnbit :  " << nbit << std::endl;
	// std::cout << "Index: " << ((byte >> pos) & ((2 << (nbit - 1)) - 1)) + (nbit << 1) - 2 << std::endl << std::endl;
	return lutall[((byte >> pos) & ((1 << nbit) - 1)) + (nbit << 1) - 2];		// Should see if this can be optimised
}

__device__ __forceinline__ cuFloatComplex complex_bitsread_gpu(char byte, int pos, int nbit) {
	return make_cuFloatComplex(lutall[((byte >> pos) & ((1 << nbit) - 1)) + (nbit << 1) - 2], lutall[((byte >> (pos+nbit)) & ((1 << nbit) - 1)) + (nbit << 1) - 2]);
}


__device__ __forceinline__ float multibitsread_gpu(int32_t word, int pos, int nbit) {
	// Larger numbers of bits have equidistant quantization spacing

	float quant_factor;
	// TODO: define constants in a header maybe?
	switch (nbit) {
	case 4:
		quant_factor = 1.0 / FourBit1sigma;
		break;
	case 8:
		quant_factor = 1.0 / 3.3;
		break;
	case 16:
		quant_factor = 1.0 / 8.0;
		break;
	case 32:
		quant_factor = 1.0 / 8.0;		// ERROR: 32 bit doesn't work for some reason?
		break;
	default:
		break;
	}

	return (((word >> pos) & ((1L << nbit) - 1)) - (1L << (nbit - 1))) * quant_factor;
}


__device__ __forceinline__ cuFloatComplex complex_multibitsread_gpu(int32_t word, int pos, int nbit) {
	// Larger numbers of bits have equidistant quantization spacing

	float quant_factor;
	// TODO: define constants in a header maybe?
	switch (nbit) {
	case 4:
		quant_factor = 1.0 / FourBit1sigma;
		break;
	case 8:
		quant_factor = 1.0 / 3.3;
		break;
	case 16:
		quant_factor = 1.0 / 8.0;
		break;
	default:
		break;
	}

	return make_cuFloatComplex(((((word >> pos) & ((1L << nbit) - 1)) - (1L << (nbit - 1))) * quant_factor),((((word >> (pos+nbit)) & ((1L << nbit) - 1)) - (1L << (nbit - 1))) * quant_factor));
}


__device__ int blanker_vdif_gpu(struct mark5_stream *ms)
{
	unsigned long long *data;
	int nword;

	if(!ms->payload)
	{
		ms->blankzoneendvalid[0] = 0;

		return 0;
	}

	data = (unsigned long long *)ms->payload;

	nword = ms->databytes/8;

	/* only 1 zone for VDIF data.  a packet is either good or bad.
	 *
	 * To be good, it cannot have fill pattern at beginning or end
	 */

	ms->blankzonestartvalid[0] = 0;

	/* Reject frames that fail the VDIF header sanity checks.  On the CPU
	 * path these live in mark5_format_vdif_validate() (called per frame
	 * from mark5_stream_next_frame(), which blanks the whole frame on
	 * failure); the GPU unpacker stubs validate out, so they belong here.
	 * As validate runs in the unpacker context (ms->mjd == 0, so its
	 * frame-time consistency test is inert), exactly two checks are live:
	 *  - Data Frame Length of zero ("overly unusual header"): catches
	 *    zero-padding, e.g. a send buffer tail past the end of the
	 *    recording that no real frame was ever written into.
	 *  - The invalid bit: set by vdifmux on frames it fabricates for
	 *    missing input data, and by recorders for known-bad frames.
	 * Without these the GPU decodes and correlates junk that the CPU
	 * correctly blanks.  This reads the vdifio vdif_header bitfields
	 * directly rather than calling getVDIFFrameInvalid()/
	 * getVDIFFrameBytes(), which are host-only static inlines.
	 *
	 * FIXME: hard-coding vdif_header is only safe because the GPU path is
	 * restricted to VDIF-family formats (enforced in Configuration::getMode).
	 * If/when the GPU branch grows CODIF (or other) support, the frame
	 * validity test must be made format-aware - a per-format validate hook
	 * like the CPU's mark5_format_*_validate(), selected at setup time,
	 * with formats that carry no per-frame validity flag skipping the test. */
	if(ms->frame)
	{
		const vdif_header *vh = (const vdif_header *)ms->frame;
		if(vh->invalid || vh->framelength8 == 0)
		{
			ms->blankzoneendvalid[0] = 0;
			return 0;
		}
	}

	/* Check for fill pattern */
	if(data[0] == MARK5_FILL_WORD64 || data[nword-1] == MARK5_FILL_WORD64)
	{
		ms->blankzoneendvalid[0] = 0;
		return 0;
	}
	else
	{
		//fprintf(m5stderr, "Frame is good\n");
		ms->blankzoneendvalid[0] = 1<<30;
		return 1;
	}
}

// The number of ignored channels when nchan is not a power of two (the packed
// bitstream carries the next power of two of channels; the unused ones are
// skipped). Depends only on nchan, so the launcher precomputes it once per
// subint on the host and passes it in - out of the per-thread inner loop that
// the old gpu_unpack recomputed it in.
__host__ __device__ __forceinline__ int channel_skip(int nchan) {
	int skipped = 0;
	int n = nchan;
	while (n != 0) { n >>= 1; skipped++; }
	return ((1 << skipped) - nchan) % nchan;
}

// Decode a SINGLE real sample for one band directly from the packed frame
// payload. This is the per-(band, sample) primitive shared by the fused
// decode+fringe kernel; the old gpu_unpack looped this over every channel and
// wrote the result to a global unpacked buffer that the fringe kernel then read
// straight back. framepayload = packed + frame*framebytes + payloadoffset.
__device__ __forceinline__ float decode_one_gpu(
		const unsigned char *framepayload, int nbit, int nchan, int decimation,
		int skipped, int sample_in_frame, int band) {
	const bool bitreadflag = (nbit == 1) || (nbit == 2);
	int bit_counter = sample_in_frame * nbit * (nchan * decimation + skipped) + band * nbit;
	if (bitreadflag)
		return bitsread_gpu(framepayload[bit_counter / 8], bit_counter % 8, nbit);
	else
		return multibitsread_gpu(((const u_int32_t *)framepayload)[bit_counter / 32], bit_counter % 32, nbit);
}

// Complex twin of decode_one_gpu (real+imag packed adjacently per sample, so
// twice the bits per band and per sample; mirrors the old
// mk5_decode_complex_sample_gpu bit layout).
__device__ __forceinline__ cuFloatComplex decode_one_complex_gpu(
		const unsigned char *framepayload, int nbit, int nchan, int decimation,
		int skipped, int sample_in_frame, int band) {
	const bool bitreadflag = (nbit == 1) || (nbit == 2);
	int bit_counter = sample_in_frame * 2 * nbit * (nchan * decimation + skipped) + band * 2 * nbit;
	if (bitreadflag)
		return complex_bitsread_gpu(framepayload[bit_counter / 8], bit_counter % 8, nbit);
	else
		return complex_multibitsread_gpu(((const u_int32_t *)framepayload)[bit_counter / 32], bit_counter % 32, nbit);
}

// Per-frame validity, one thread per frame. Replaces the validity side of the
// old gpu_unpack (which set goodframes[] as a by-product of decoding every
// sample): the fused decode+fringe kernel decodes samples itself, so all that
// is needed up front is the good/bad flag per frame that gpu_set_weights and
// the fused kernel read. blanker_vdif_gpu also sets ms.blankzoneendvalid on the
// thread-local copy, but only its int return (good=1/bad=0) is kept.
__global__ void gpu_blank_frames(struct mark5_stream ms, const void *packed, int nframes, bool *valid_frames) {
	int frame = blockIdx.x * blockDim.x + threadIdx.x;
	if (frame >= nframes) return;

	ms.frame        = (const unsigned char *)packed + (size_t)frame * ms.framebytes;
	ms.payload      = ms.frame + ms.payloadoffset;
	ms.framenum     = frame;

	valid_frames[frame] = blanker_vdif_gpu(&ms);
}

// ===========================================================================
// Fused decode + fringe-rotation kernel.
//
// Replaces the old two-pass gpu_unpack (write unpacked samples to a global
// buffer) + gpu_fringeRotation (read them straight back). Each thread owns one
// (FFT window, band, channel), decodes exactly the one sample it needs from the
// packed frame payload (keeping it in a register), applies the precomputed
// fringe rotator and writes the complex FFT input - no global unpacked buffer,
// no round-trip. Thread/grid mapping and the destIndex/rotator math are
// identical to the old gpu_fringeRotation, so the rotated output is unchanged.
//
// When DOPCAL, the SAME raw decoded sample (pre-rotation) is also folded into
// its phase-cal bin, subsuming the standalone gpu_pcalextraction kernel (whose
// only real work was this binning atomicAdd - the offset/phase assembly is done
// host-side later). The bin index reproduces gpu_pcalextraction exactly:
// (datasamples + sampleIndexes[window] + channel) % N_pcal_bins[band]. The
// no-pcal instantiation compiles all of this away.
//
// Blanking/tail parity with the old path: a sample whose frame is invalid
// (!valid_frames[frame]) or past the delivered data (frame >= framestounpack)
// decodes to 0 - exactly what gpu_unpack's per-frame blankzone zeroing and the
// explicit unpacked-tail memset produced.
template<bool DOPCAL>
__global__ void gpu_fused_fringe(
		cuFloatComplex *const dest,
		const int *const sampleIndexes,
		const bool *const validSamples,
		const double *const bigA,
		const double *const bigBred,
		int fftloop,
		int startblock,
		int numblocks,
		size_t fftchannels,
		struct mark5_stream ms,
		const void *packed,
		const bool *const valid_frames,
		int skipped,
		int framestounpack,
		float *pcal_output,
		const int *N_pcal_bins,
		int datasamples,
		int pcal_bin_stride_length) {
	const size_t subloopindex = blockIdx.x;
	if (!validSamples[subloopindex]) return;

	size_t index = fftloop * gridDim.x + subloopindex + startblock;
	if (index >= startblock + numblocks) return;

	const size_t bandindex = threadIdx.x;
	const size_t channelindex = (blockIdx.y * blockDim.y) + threadIdx.y;
	const size_t numrecordedbands = blockDim.x;
	if (channelindex >= fftchannels) return;

	// Decode this thread's one raw sample on the fly (was: read src[band][idx]
	// from the global unpacked buffer).
	const int sample_index = sampleIndexes[subloopindex];
	const long global_sample = (long)sample_index + (long)channelindex;
	float srcVal = 0.f;
	if (global_sample >= 0) {
		const int frame = (int)(global_sample / ms.framesamples);
		if (frame < framestounpack && valid_frames[frame]) {
			const int sample_in_frame = (int)(global_sample - (long)frame * ms.framesamples);
			const unsigned char *fp = (const unsigned char *)packed +
					(size_t)frame * ms.framebytes + ms.payloadoffset;
			srcVal = decode_one_gpu(fp, ms.nbit, ms.nchan, ms.decimation, skipped, sample_in_frame, (int)bandindex);
		}
	}

	if (DOPCAL) {
		const int n_bins = N_pcal_bins[bandindex];
		const long so = (long)datasamples + global_sample;
		const int bin = (int)(((so % n_bins) + n_bins) % n_bins);
        // DEBUGGING
    	const size_t pidx =
    		bandindex * (size_t)pcal_bin_stride_length + (size_t)bin;
    
    	const bool print_pcal =
    		(fftloop == PCAL_DEBUG_FFTLOOP) &&
    		(subloopindex == PCAL_DEBUG_SUBLOOP) &&
    		(bandindex == PCAL_DEBUG_BAND) &&
    		(channelindex >= PCAL_DEBUG_CH_FIRST) &&
    		(channelindex < PCAL_DEBUG_CH_FIRST + PCAL_DEBUG_CH_COUNT);
    
    	if (print_pcal) {
    		printf("PCAL-UNTILED: fftloop=%d subloop=%zu band=%zu chan=%zu "
    			   "sampleIndex=%d global_sample=%ld frame=%d "
    			   "srcVal=%+.7f n_bins=%d datasamples=%d so=%ld "
    			   "bin=%d stride=%d pidx=%zu\n",
    			   fftloop,
    			   subloopindex,
    			   bandindex,
    			   channelindex,
    			   sample_index,
    			   global_sample,
    			   (global_sample >= 0) ?
    				   (int)(global_sample / ms.framesamples) : -1,
    			   srcVal,
    			   n_bins,
    			   datasamples,
    			   so,
    			   bin,
    			   pcal_bin_stride_length,
    			   pidx);
    	} 
        // END DEBUGGING

		atomicAdd(&pcal_output[bandindex * pcal_bin_stride_length + bin], srcVal);


	}

	const size_t destIndex = (subloopindex * fftchannels * numrecordedbands) + (bandindex * fftchannels) + channelindex;

	/* complexrotator[j] = exp( 2 pi i * (A*j + B) ), where A/B (bigAval and
	   bigB_reduced) are precomputed per (window, band) by
	   gpu_precompute_fringe_rotator - so this per-sample kernel only forms the
	   phase and applies the rotator. */
	const double bigAval = bigA[subloopindex * numrecordedbands + bandindex];
	const double bigB_reduced = bigBred[subloopindex * numrecordedbands + bandindex];
	double exponent = (bigAval * (double)channelindex + bigB_reduced);
	exponent -= int(exponent);
	cuFloatComplex cr;
	__sincosf(-GPUDECODE_TWO_PI * exponent, &cr.y, &cr.x);
	cuFloatComplex c = make_cuFloatComplex(srcVal, 0.f);
	dest[destIndex] = cuCmulf(c, cr);
}

// Complex-sampled twin of gpu_fused_fringe.
template<bool DOPCAL>
__global__ void gpu_fused_fringe_complex(
		cuFloatComplex *const dest,
		const int *const sampleIndexes,
		const bool *const validSamples,
		const double *const bigA,
		const double *const bigBred,
		int fftloop,
		int startblock,
		int numblocks,
		size_t fftchannels,
		struct mark5_stream ms,
		const void *packed,
		const bool *const valid_frames,
		int skipped,
		int framestounpack,
		cuFloatComplex *pcal_output,
		const int *N_pcal_bins,
		int datasamples,
		int pcal_bin_stride_length) {
	const size_t subloopindex = blockIdx.x;
	if (!validSamples[subloopindex]) return;

	size_t index = fftloop * gridDim.x + subloopindex + startblock;
	if (index >= startblock + numblocks) return;

	const size_t bandindex = threadIdx.x;
	const size_t channelindex = (blockIdx.y * blockDim.y) + threadIdx.y;
	const size_t numrecordedbands = blockDim.x;
	if (channelindex >= fftchannels) return;

	const int sample_index = sampleIndexes[subloopindex];
	const long global_sample = (long)sample_index + (long)channelindex;
	cuFloatComplex srcVal = make_cuFloatComplex(0.f, 0.f);
	if (global_sample >= 0) {
		const int frame = (int)(global_sample / ms.framesamples);
		if (frame < framestounpack && valid_frames[frame]) {
			const int sample_in_frame = (int)(global_sample - (long)frame * ms.framesamples);
			const unsigned char *fp = (const unsigned char *)packed +
					(size_t)frame * ms.framebytes + ms.payloadoffset;
			srcVal = decode_one_complex_gpu(fp, ms.nbit, ms.nchan, ms.decimation, skipped, sample_in_frame, (int)bandindex);
		}
	}

	if (DOPCAL) {
		const int n_bins = N_pcal_bins[bandindex];
		const long so = (long)datasamples + global_sample;
		const int bin = (int)(((so % n_bins) + n_bins) % n_bins);
		cuFloatComplex *dst = &pcal_output[bandindex * pcal_bin_stride_length + bin];
		atomicAdd(&dst->x, srcVal.x);
		atomicAdd(&dst->y, srcVal.y);
	}

	const size_t destIndex = (subloopindex * fftchannels * numrecordedbands) + (bandindex * fftchannels) + channelindex;

	const double bigAval = bigA[subloopindex * numrecordedbands + bandindex];
	const double bigB_reduced = bigBred[subloopindex * numrecordedbands + bandindex];
	double exponent = (bigAval * (double)channelindex + bigB_reduced);
	exponent -= int(exponent);
	cuFloatComplex cr;
	__sincosf(-GPUDECODE_TWO_PI * exponent, &cr.y, &cr.x);
	dest[destIndex] = cuCmulf(srcVal, cr);
}

// ---------------------------------------------------------------------------
// Tiled twin of the fused decode+fringe kernels above: identical arithmetic,
// but the global write is transposed through shared memory so that it
// coalesces.
//
// The two halves of the kernel want opposite thread mappings. decode_one_gpu
// reads band-adjacent bits of one sample word, so band-on-lane costs a warp a
// single sector - that is why L1 hit rate is ~91% and DRAM throughput only
// ~21%. But `dest` is band-major (it is the cuFFT batched input layout, which
// is not ours to change), so band-on-lane scatters a warp's 8-byte stores over
// as many sectors as there are lanes: measured on a 2070, 29% excessive sectors
// and 58% of warp stall cycles waiting on L1TEX. This kernel keeps band on the
// lane for the decode+rotate, stages the result in shared memory, and puts
// channel on the lane for the store. Rationale, tile policy and measurements:
// docs/gpu-fringetile-design.md.
//
// One block owns BT bands x CT channels of one FFT window, BT*CT threads (256
// whenever the job is wide enough to fill them - see fringeTileCT), so every
// (bands, channels) shape stores the same 256-contiguous-byte run per warp. Grid is
// (numBufferedFFTs, ceil(fftchannels/CT), ceil(numrecordedbands/BT)); gridDim.x
// stays numBufferedFFTs because the kernel's `index` arithmetic uses it.
static const int FRINGE_TILE_THREADS = 256;

static __host__ __device__ constexpr int fringe_tile_log2(int v) {
	return (v <= 1) ? 0 : 1 + fringe_tile_log2(v >> 1);
}

// Every argument of the tiled kernel in one POD, so the 16 instantiations
// (4 tile shapes x pcal x real/complex) share one dispatch signature instead of
// repeating a 19-argument list. Passed by value, as `ms` already is.
struct FringeTileArgs {
	cuFloatComplex *dest;
	const int *sampleIndexes;
	const bool *validSamples;
	const double *bigA;
	const double *bigBred;
	int fftloop;
	int startblock;
	int numblocks;
	// Explicit rather than read back from gridDim.x: `index` below needs the
	// window count, and tying that to a grid dimension makes any later grid
	// reshuffle silently select the wrong FFT window.
	size_t numBufferedFFTs;
	size_t fftchannels;
	// Explicit, unlike the untiled kernels which read it from blockDim.x - a
	// tiled block's blockDim.x is the flat tile, not the band count.
	size_t numrecordedbands;
	struct mark5_stream ms;
	const void *packed;
	const bool *valid_frames;
	int skipped;
	int framestounpack;
	void *pcal_output;
	const int *N_pcal_bins;
	int datasamples;
	int pcal_bin_stride_length;
};

template<int BT, int CT, bool DOPCAL, bool CPLX>
__global__ void gpu_fused_fringe_tiled(const FringeTileArgs a) {
	constexpr int BT_LOG2 = fringe_tile_log2(BT);
	constexpr int CT_LOG2 = fringe_tile_log2(CT);
	// At BT=1 the two lane decompositions coincide, so the transpose is an
	// identity: store straight to global and skip the shared round-trip and the
	// barrier. Without this the narrowest shapes pay for staging they cannot use
	// (measured 0.98x at 1 band x 64 channels, where the kernel is ~12 us).
	constexpr bool TRANSPOSE = (BT > 1);
	// Row padding, chosen so phase 1's band-strided shared writes hit 32
	// distinct banks. An 8-byte shared access is serviced in half-warps of 16
	// lanes (16 x 8 B = the 32 banks exactly), and phase 1's lane decomposition
	// is `lane = band + BT*ch`, so the 16 lanes tile the banks iff the row
	// stride in elements is congruent to 16/BT mod 16. Padding by one element
	// (the obvious choice, and what this first was) gives stride 1 mod 16 and a
	// 2-way conflict on every store - ncu measured 640k conflicts across 320k
	// store requests before this was fixed.
	constexpr int PAD = TRANSPOSE ? (16 / BT) : 0;
	__shared__ cuFloatComplex tile[BT][TRANSPOSE ? (CT + PAD) : 1];

	const size_t subloopindex = blockIdx.x;
	// Both of these depend only on blockIdx.x, so they are block-uniform and
	// returning here cannot leave part of the block stranded at the barrier.
	if (!a.validSamples[subloopindex]) return;
	const size_t index = a.fftloop * a.numBufferedFFTs + subloopindex + a.startblock;
	if (index >= a.startblock + a.numblocks) return;

	const int tid = threadIdx.x;
	const size_t bandbase = (size_t)blockIdx.z * BT;
	const size_t chbase = (size_t)blockIdx.y * CT;
	const int sample_index = a.sampleIndexes[subloopindex];

	// Phase 1 - band on the lane: decode this thread's raw sample, fold pcal,
	// apply the rotator, and leave the result in the shared tile.
	{
		const int band_local = tid & (BT - 1);
		const int ch_local = tid >> BT_LOG2;
		const size_t bandindex = bandbase + band_local;
		const size_t channelindex = chbase + ch_local;
		// Predicated, never returned from: every thread must reach the barrier.
		if (bandindex < a.numrecordedbands && channelindex < a.fftchannels) {
			const long global_sample = (long)sample_index + (long)channelindex;
			cuFloatComplex c = make_cuFloatComplex(0.f, 0.f);
			if (global_sample >= 0) {
				const int frame = (int)(global_sample / a.ms.framesamples);
				if (frame < a.framestounpack && a.valid_frames[frame]) {
					const int sample_in_frame = (int)(global_sample - (long)frame * a.ms.framesamples);
					const unsigned char *fp = (const unsigned char *)a.packed +
							(size_t)frame * a.ms.framebytes + a.ms.payloadoffset;
					if (CPLX)
						c = decode_one_complex_gpu(fp, a.ms.nbit, a.ms.nchan, a.ms.decimation,
								a.skipped, sample_in_frame, (int)bandindex);
					else
						c = make_cuFloatComplex(decode_one_gpu(fp, a.ms.nbit, a.ms.nchan,
								a.ms.decimation, a.skipped, sample_in_frame, (int)bandindex), 0.f);
				}
			}

			// Phase cal folds the raw, unrotated sample - and, as in the
			// untiled kernels, folds a zero for a pre-start sample rather than
			// skipping the bin.
			if (DOPCAL) {
				const int n_bins = a.N_pcal_bins[bandindex];
				const long so = (long)a.datasamples + global_sample;
				const int bin = (int)(((so % n_bins) + n_bins) % n_bins);
				//const size_t pidx = bandindex * a.pcal_bin_stride_length + bin;
				const size_t pidx = bandindex * (size_t)a.pcal_bin_stride_length + (size_t)bin;
// DEBUG ********************************************************************************************

const size_t first_pcal_output_index_for_band =
	bandindex * (size_t)a.pcal_bin_stride_length;

const size_t one_past_last_pcal_output_index_for_band =
	first_pcal_output_index_for_band + (size_t)n_bins;

/*
 * For 1-bit data, valid decoded values are 0, -1, and +1.
 * Zero is valid here because invalid frames, tail samples, and pre-start
 * samples are intentionally folded as zero.
 *
 * For 2-bit data, use the values directly from the same constant lookup
 * table used by bitsread_gpu():
 *
 *     lutall[2] = -HiMag
 *     lutall[3] = -1.0
 *     lutall[4] = +1.0
 *     lutall[5] = +HiMag
 */
bool decoded_raw_sample_is_unexpected = false;

if (a.ms.nbit == 1) {
	decoded_raw_sample_is_unexpected =
		(c.x != 0.0f) &&
		(c.x != -1.0f) &&
		(c.x != 1.0f);
} else if (a.ms.nbit == 2) {
	decoded_raw_sample_is_unexpected =
		(c.x != 0.0f) &&
		(c.x != lutall[2]) &&
		(c.x != lutall[3]) &&
		(c.x != lutall[4]) &&
		(c.x != lutall[5]);
}

/*
 * These are PCAL mapping failures detectable after bin and pidx have
 * already been calculated.
 *
 * Note: n_bins must be positive before the modulo operation above.
 * That is an existing precondition of the accumulation code, so it cannot
 * be diagnosed here after the fact.
 */
const bool pcal_mapping_is_invalid =
	(a.pcal_bin_stride_length <= 0) ||
	(n_bins > a.pcal_bin_stride_length) ||
	(bin < 0) ||
	(bin >= n_bins) ||
	(pidx < first_pcal_output_index_for_band) ||
	(pidx >= one_past_last_pcal_output_index_for_band);

/*
 * Print an anomaly from any FFT loop, subloop, tile, band, or channel.
 * This condition affects printf only. PCAL accumulation remains unchanged.
 */
const bool print_pcal =
	pcal_mapping_is_invalid ||
	decoded_raw_sample_is_unexpected;

if (print_pcal) {
	printf(
		"PCAL-TILED-ANOMALY: "
		"fftloop=%d "
		"subloopindex=%u "
		"blockIndexX=%u blockIndexY=%u blockIndexZ=%u "
		"threadIndex=%d "
		"bandTileSize=%d channelTileSize=%d "
		"bandLocalIndex=%d channelLocalIndex=%d "
		"bandindex=%u channelindex=%u "
		"sample_index=%d "
		"global_sample_low_32_bits=0x%08x "
		"decoded_raw_sample=%+.7f "
		"number_of_pcal_bins=%d "
		"pcal_bin_stride_length=%d "
		"pcal_bin=%d "
		"first_pcal_output_index_for_band=%u "
		"pcal_output_index=%u "
		"one_past_last_pcal_output_index_for_band=%u "
		"pcal_mapping_is_invalid=%d "
		"decoded_raw_sample_is_unexpected=%d\n",

		a.fftloop,
		(unsigned int)subloopindex,

		(unsigned int)blockIdx.x,
		(unsigned int)blockIdx.y,
		(unsigned int)blockIdx.z,

		tid,

		BT,
		CT,

		band_local,
		ch_local,

		(unsigned int)bandindex,
		(unsigned int)channelindex,

		sample_index,

		(unsigned int)((unsigned long long)global_sample & 0xffffffffULL),

		c.x,

		n_bins,
		a.pcal_bin_stride_length,
		bin,

		(unsigned int)first_pcal_output_index_for_band,
		(unsigned int)pidx,
		(unsigned int)one_past_last_pcal_output_index_for_band,

		(int)pcal_mapping_is_invalid,
		(int)decoded_raw_sample_is_unexpected);
}

// END DEBUG ********************************************************************************************
				


				if (CPLX) {
					cuFloatComplex *dst = &((cuFloatComplex *)a.pcal_output)[pidx];
					atomicAdd(&dst->x, c.x);
					atomicAdd(&dst->y, c.y);
				} else {
					atomicAdd(&((float *)a.pcal_output)[pidx], c.x);
				}
			}

			const double bigAval = a.bigA[subloopindex * a.numrecordedbands + bandindex];
			const double bigB_reduced = a.bigBred[subloopindex * a.numrecordedbands + bandindex];
			double exponent = (bigAval * (double)channelindex + bigB_reduced);
			exponent -= int(exponent);
			cuFloatComplex cr;
			__sincosf(-GPUDECODE_TWO_PI * exponent, &cr.y, &cr.x);
			const cuFloatComplex val = cuCmulf(c, cr);
			if (TRANSPOSE) {
				tile[band_local][ch_local] = val;
			} else {
				const size_t destIndex = (subloopindex * a.fftchannels * a.numrecordedbands) +
						(bandindex * a.fftchannels) + channelindex;
				a.dest[destIndex] = val;
			}
		}
	}

	if (TRANSPOSE) {
		__syncthreads();

		// Phase 2 - channel on the lane: each warp stores one run of 32
		// consecutive channels, i.e. 256 contiguous bytes of fully-used sectors.
		const int ch_local = tid & (CT - 1);
		const int band_local = tid >> CT_LOG2;
		const size_t bandindex = bandbase + band_local;
		const size_t channelindex = chbase + ch_local;
		if (bandindex < a.numrecordedbands && channelindex < a.fftchannels) {
			const size_t destIndex = (subloopindex * a.fftchannels * a.numrecordedbands) +
					(bandindex * a.fftchannels) + channelindex;
			a.dest[destIndex] = tile[band_local][ch_local];
		}
	}
}

// The tiled path is on by default; DIFX_GPU_FRINGE_TILE=0 selects the untiled
// kernels above unchanged, so the two can be A/B'd (and the tiled path backed
// out on an architecture where it does not pay) without a rebuild.
static bool fringeTileEnabled() {
	// Function-local static: initialisation is thread-safe by the standard
	// (C++11 6.7/4), which the read-modify-written `static int cached = -1`
	// idiom used elsewhere in this tree is not - launch_fused_fringe is called
	// from every Core processing thread, one Mode per thread.
	static const bool enabled = []() {
		// Default on. Only an explicitly off-ish value disables it: an
		// `atoi(e) == 0` test would have made DIFX_GPU_FRINGE_TILE=true, =yes
		// and =<empty> all silently select the OLD path, i.e. the opposite of
		// what someone typing them means.
		const char *e = getenv("DIFX_GPU_FRINGE_TILE");
		if (e == NULL) return true;
		const bool off = (strcmp(e, "0") == 0) || (strcasecmp(e, "false") == 0) ||
				(strcasecmp(e, "no") == 0) || (strcasecmp(e, "off") == 0);
		return !off;
	}();
	return enabled;
}

// BT = the largest power of two <= 8 that DIVIDES numrecordedbands. Powers of
// two so the two index decompositions are shifts and masks; capped at 8 so CT
// never drops below 32 and a warp's store run stays 256 B; and a divisor so no
// block is left with idle band slots. The divisor condition costs nothing at the
// band counts VLBI actually uses (1, 2, 4, 8, 16, 32, 64) and stops odd counts
// paying for a partly-empty tile - measured: 3 and 6 bands lost 0-3% under a
// plain pow2_floor rule, which rounded 3 up to 4 and 6 up to 8 band slots.
static int fringeTileBT(size_t numrecordedbands) {
	if (numrecordedbands % 8 == 0) return 8;
	if (numrecordedbands % 4 == 0) return 4;
	if (numrecordedbands % 2 == 0) return 2;
	return 1;
}

template<int BT, int CT, bool DOPCAL, bool CPLX>
static void launch_fused_fringe_tiled(cudaStream_t stream, size_t numBufferedFFTs,
		const FringeTileArgs &a) {
	const dim3 grid((unsigned int)numBufferedFFTs,
			(unsigned int)((a.fftchannels + CT - 1) / CT),
			(unsigned int)((a.numrecordedbands + BT - 1) / BT));
	gpu_fused_fringe_tiled<BT, CT, DOPCAL, CPLX><<<grid, BT * CT, 0, stream>>>(a);
}

// CT = 256/BT, but never more channels than the job actually has, and never
// below 32 (a warp must still store one contiguous run). Without the clamp a
// narrow job wastes most of each block: at 1 band x 64 channels, CT=256 left 64
// of 256 threads working and the tiled path measured **0.46x** - the untiled
// geometry sizes its block to the channel count and wins outright there.
static int fringeTileCT(int bt, size_t fftchannels) {
	int ct = FRINGE_TILE_THREADS / bt;
	while (ct > 32 && (size_t)ct > fftchannels) ct >>= 1;
	return ct;
}

template<bool DOPCAL, bool CPLX>
static void dispatch_fused_fringe_tiled(cudaStream_t stream, size_t numBufferedFFTs,
		const FringeTileArgs &a) {
	const int bt = fringeTileBT(a.numrecordedbands);
	const int ct = fringeTileCT(bt, a.fftchannels);
	switch (bt) {
		case 8:
			launch_fused_fringe_tiled<8, 32, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			break;
		case 4:
			if (ct >= 64) launch_fused_fringe_tiled<4, 64, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			else          launch_fused_fringe_tiled<4, 32, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			break;
		case 2:
			if (ct >= 128)     launch_fused_fringe_tiled<2, 128, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			else if (ct >= 64) launch_fused_fringe_tiled<2, 64, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			else               launch_fused_fringe_tiled<2, 32, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			break;
		default:
			if (ct >= 256)      launch_fused_fringe_tiled<1, 256, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			else if (ct >= 128) launch_fused_fringe_tiled<1, 128, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			else if (ct >= 64)  launch_fused_fringe_tiled<1, 64, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			else                launch_fused_fringe_tiled<1, 32, DOPCAL, CPLX>(stream, numBufferedFFTs, a);
			break;
	}
}

// Host launcher for gpu_blank_frames (keeps the <<<>>> launch and the
// mark5_stream template plumbing in this translation unit, so the caller in
// mk5mode_gpu.cu just calls a plain function).
void launch_blank_frames(dim3 grid, dim3 block, cudaStream_t stream,
		struct mark5_stream ms, const void *packed, int nframes, bool *valid_frames) {
	gpu_blank_frames<<<grid, block, 0, stream>>>(ms, packed, nframes, valid_frames);
}

// Host launcher for the fused decode+fringe kernel. Dispatches on the
// real/complex and pcal/no-pcal axes to the four template instantiations, so
// the common (no-pcal) path carries none of the pcal code.
void launch_fused_fringe(cudaStream_t stream,
		size_t numBufferedFFTs, size_t numrecordedbands, int maxThreadsPerBlock,
		bool usecomplex, bool dopcal,
		cuFloatComplex *dest, const int *sampleIndexes, const bool *validSamples,
		const double *bigA, const double *bigBred,
		int fftloop, int startblock, int numblocks, size_t fftchannels,
		struct mark5_stream ms, const void *packed, const bool *valid_frames,
		int framestounpack,
		void *pcal_output, const int *N_pcal_bins, int datasamples, int pcal_bin_stride_length) {
	const int skipped = channel_skip(ms.nchan);

	if (fringeTileEnabled()) {
		FringeTileArgs a;
		a.dest = dest;
		a.sampleIndexes = sampleIndexes;
		a.validSamples = validSamples;
		a.bigA = bigA;
		a.bigBred = bigBred;
		a.fftloop = fftloop;
		a.startblock = startblock;
		a.numblocks = numblocks;
		a.numBufferedFFTs = numBufferedFFTs;
		a.fftchannels = fftchannels;
		a.numrecordedbands = numrecordedbands;
		a.ms = ms;
		a.packed = packed;
		a.valid_frames = valid_frames;
		a.skipped = skipped;
		a.framestounpack = framestounpack;
		a.pcal_output = dopcal ? pcal_output : nullptr;
		a.N_pcal_bins = dopcal ? N_pcal_bins : nullptr;
		a.datasamples = datasamples;
		a.pcal_bin_stride_length = pcal_bin_stride_length;
		if (dopcal) {
			if (usecomplex) dispatch_fused_fringe_tiled<true, true>(stream, numBufferedFFTs, a);
			else            dispatch_fused_fringe_tiled<true, false>(stream, numBufferedFFTs, a);
		} else {
			if (usecomplex) dispatch_fused_fringe_tiled<false, true>(stream, numBufferedFFTs, a);
			else            dispatch_fused_fringe_tiled<false, false>(stream, numBufferedFFTs, a);
		}
		return;
	}

	// Untiled geometry (DIFX_GPU_FRINGE_TILE=0): one block per (window, channel
	// chunk) with band on threadIdx.x and as many channels as the remaining
	// thread budget allows. Kept verbatim from before the tiled path so the two
	// are directly comparable.
	size_t fftchannels_block = fftchannels;
	size_t fftchannels_grid = 1;
	size_t divisor = maxThreadsPerBlock / numrecordedbands;
	if (fftchannels > divisor) {
		fftchannels_block = divisor;
		fftchannels_grid = (fftchannels / divisor);
		if (fftchannels % divisor != 0) {
			fftchannels_grid++;
		}
	}
	const dim3 grid(numBufferedFFTs, fftchannels_grid);
	const dim3 block(numrecordedbands, fftchannels_block);

	if (usecomplex) {
		if (dopcal)
			gpu_fused_fringe_complex<true><<<grid, block, 0, stream>>>(
					dest, sampleIndexes, validSamples, bigA, bigBred,
					fftloop, startblock, numblocks, fftchannels,
					ms, packed, valid_frames, skipped, framestounpack,
					(cuFloatComplex *)pcal_output, N_pcal_bins, datasamples, pcal_bin_stride_length);
		else
			gpu_fused_fringe_complex<false><<<grid, block, 0, stream>>>(
					dest, sampleIndexes, validSamples, bigA, bigBred,
					fftloop, startblock, numblocks, fftchannels,
					ms, packed, valid_frames, skipped, framestounpack,
					nullptr, nullptr, datasamples, pcal_bin_stride_length);
	} else {
		if (dopcal)
			gpu_fused_fringe<true><<<grid, block, 0, stream>>>(
					dest, sampleIndexes, validSamples, bigA, bigBred,
					fftloop, startblock, numblocks, fftchannels,
					ms, packed, valid_frames, skipped, framestounpack,
					(float *)pcal_output, N_pcal_bins, datasamples, pcal_bin_stride_length);
		else
			gpu_fused_fringe<false><<<grid, block, 0, stream>>>(
					dest, sampleIndexes, validSamples, bigA, bigBred,
					fftloop, startblock, numblocks, fftchannels,
					ms, packed, valid_frames, skipped, framestounpack,
					nullptr, nullptr, datasamples, pcal_bin_stride_length);
	}
}
