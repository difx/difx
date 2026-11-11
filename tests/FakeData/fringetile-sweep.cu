// fringetile-sweep.cu - shape sweep for the fused decode+fringe kernel.
//
// Answers the question a DiFX job cannot answer cheaply: does the tiled
// (shared-memory-transposed) kernel win at EVERY reasonable (bands, channels)
// shape, or only at the one the benchmark happens to use? Building a real job
// per shape would mean vex surgery per shape; this drives the real launcher
// over a parameter grid instead.
//
// It `#include`s gpudecode.cu, so it exercises the shipped kernels and the
// shipped dispatch - there is no second copy of the code to drift.
//
// Two things are reported per shape:
//   time  - best of NREPS launches, cudaEvent-timed
//   hash  - FNV-1a over the whole destination buffer
//
// The tiled and untiled paths do identical arithmetic in identical order (no
// atomics touch `dest`), so their hashes must match BIT-EXACTLY. Run the binary
// twice, once with DIFX_GPU_FRINGE_TILE=1 and once with 0, and diff the tables:
// fringetile-sweep.sh does that and joins them. Equal hashes are the
// all-shapes correctness check (partial tiles, non-power-of-2 band counts);
// the times are the performance answer.
//
// Usage: fringetile-sweep [nBufferedFFTs] [nreps]
// Env:   SWEEP_COMPLEX=1  exercise the complex-sampled twin (2x the bits per
//                         sample, so half the samples per frame - a different
//                         read pattern for the same tiling)
//        SWEEP_PCAL=1     exercise the DOPCAL path and dump the phase-cal bins
//                         per shape to pcal-*.f32. pcal is accumulated with
//                         atomicAdd, whose order the tiling changes, so those
//                         are compared with a tolerance rather than by hash -
//                         fringetile-sweep.sh does the comparison. Without this
//                         the DOPCAL template instantiations are never run.

#include "gpudecode.cu"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include <algorithm>

#define CHECK(x) do { cudaError_t e = (x); if (e != cudaSuccess) { \
    fprintf(stderr, "%s:%d %s -> %s\n", __FILE__, __LINE__, #x, cudaGetErrorString(e)); \
    exit(1); } } while (0)

// The shapes to cover. Channel counts are fftchannels (= 2 x nChan for real
// sampling), so 128 here is the smallest real setup (nChan=64).
struct Shape { int bands; int fftchannels; const char *why; };
static const Shape SHAPES[] = {
    {  1,  4096, "single band, many channels: already coalesced today - must not regress" },
    {  1, 16384, "single band, max channels" },
    {  2,  2048, "BT=2" },
    {  3,  1024, "odd band count: BT falls back to 1" },
    {  4,   512, "BT=4" },
    {  6,   512, "non-power-of-2 bands: BT=2 divides 6" },
    {  8,   256, "BT=8 exactly" },
    { 16,   256, "the profiled benchmark shape" },
    { 16,   128, "smallest sensible channel count" },
    { 32,   256, "wide band count" },
    { 64,   128, "many bands, few channels: legacy read mapping's best case" },
    { 128,  128, "extreme band count" },
    { 16,  4096, "many bands AND many channels" },
    {  1,    64, "fftchannels < CT: only 64 of 256 threads do work (BT=1)" },
    { 16,    64, "fftchannels < CT with BT=8, so CT=32 still fits" },
};

static unsigned long long fnv1a(const void *p, size_t n) {
    const unsigned char *b = (const unsigned char *)p;
    unsigned long long h = 1469598103934665603ULL;
    for (size_t i = 0; i < n; i++) { h ^= b[i]; h *= 1099511628211ULL; }
    return h;
}

int main(int argc, char **argv) {
    const int numBufferedFFTs = (argc > 1) ? atoi(argv[1]) : 10;
    const int nreps = (argc > 2) ? atoi(argv[2]) : 20;
    const char *tileenv = getenv("DIFX_GPU_FRINGE_TILE");
    const bool tiled = !(tileenv != NULL && atoi(tileenv) == 0);
    const char *cplxenv = getenv("SWEEP_COMPLEX");
    const bool usecomplex = (cplxenv != NULL && atoi(cplxenv) != 0);
    const char *pcalenv = getenv("SWEEP_PCAL");
    const bool dopcal = (pcalenv != NULL && atoi(pcalenv) != 0);
    // Bins per band, and the stride between bands. Real pcal periods give
    // thousands of bins; 64 would funnel every sample of every window into 64
    // floats and make the kernel a pure atomic-contention benchmark, which
    // tells us nothing about the tiling.
    const int PCAL_BINS = 1024;

    cudaStream_t stream;
    CHECK(cudaStreamCreate(&stream));
    cudaEvent_t t0, t1;
    CHECK(cudaEventCreate(&t0));
    CHECK(cudaEventCreate(&t1));
    int maxThreadsPerBlock = 1024;
    cudaDeviceProp prop;
    CHECK(cudaGetDeviceProperties(&prop, 0));
    maxThreadsPerBlock = prop.maxThreadsPerBlock;

    printf("# device %s, sm_%d%d, path=%s, sampling=%s, numBufferedFFTs=%d, best of %d\n",
           prop.name, prop.major, prop.minor, tiled ? "tiled" : "untiled",
           usecomplex ? "complex" : "real", numBufferedFFTs, nreps);
    if (dopcal) printf("# pcal ON: %d bins/band, bins dumped to pcal-*.f32\n", PCAL_BINS);
    printf("# %6s %8s %11s %20s  %s\n", "bands", "fftchan", "us/launch", "hash", "shape rationale");

    for (size_t si = 0; si < sizeof(SHAPES) / sizeof(SHAPES[0]); si++) {
        const int nbands = SHAPES[si].bands;
        const int fftchannels = SHAPES[si].fftchannels;

        // A plausible VDIF stream: 2-bit real samples, one band per channel.
        struct mark5_stream ms;
        memset(&ms, 0, sizeof(ms));
        ms.nbit = 2;
        ms.nchan = nbands;
        ms.decimation = 1;
        ms.framebytes = 8032;
        ms.payloadoffset = 32;
        const int bitspersample = (usecomplex ? 2 : 1) * ms.nbit *
                (ms.nchan * ms.decimation + channel_skip(ms.nchan));
        ms.framesamples = ((ms.framebytes - ms.payloadoffset) * 8) / bitspersample;
        if (ms.framesamples <= 0) { printf("# skip %d x %d: no samples per frame\n", nbands, fftchannels); continue; }

        // Windows are laid end to end, as the correlator does within a subint.
        const long maxsample = (long)numBufferedFFTs * fftchannels;
        const int framestounpack = (int)(maxsample / ms.framesamples) + 2;

        const size_t destelems = (size_t)numBufferedFFTs * nbands * fftchannels;
        const size_t packedbytes = (size_t)framestounpack * ms.framebytes;

        cuFloatComplex *dest = NULL;
        void *pcal_output = NULL;
        int *N_pcal_bins = NULL;
        void *packed = NULL;
        int *sampleIndexes = NULL;
        bool *validSamples = NULL, *valid_frames = NULL;
        double *bigA = NULL, *bigBred = NULL;
        CHECK(cudaMalloc(&dest, destelems * sizeof(cuFloatComplex)));
        CHECK(cudaMalloc(&packed, packedbytes));
        CHECK(cudaMalloc(&sampleIndexes, numBufferedFFTs * sizeof(int)));
        CHECK(cudaMalloc(&validSamples, numBufferedFFTs * sizeof(bool)));
        CHECK(cudaMalloc(&valid_frames, framestounpack * sizeof(bool)));
        CHECK(cudaMalloc(&bigA, (size_t)numBufferedFFTs * nbands * sizeof(double)));
        CHECK(cudaMalloc(&bigBred, (size_t)numBufferedFFTs * nbands * sizeof(double)));
        const size_t pcalelems = (size_t)nbands * PCAL_BINS;
        const size_t pcalbytes = pcalelems * (usecomplex ? sizeof(cuFloatComplex) : sizeof(float));
        if (dopcal) {
            CHECK(cudaMalloc(&pcal_output, pcalbytes));
            CHECK(cudaMalloc(&N_pcal_bins, nbands * sizeof(int)));
            std::vector<int> hbins(nbands, PCAL_BINS);
            CHECK(cudaMemcpy(N_pcal_bins, hbins.data(), nbands * sizeof(int), cudaMemcpyHostToDevice));
        }

        // Deterministic pseudo-random payload: the decoded values must depend on
        // the data, or a mis-indexed read would still hash equal.
        std::vector<unsigned char> hpacked(packedbytes);
        unsigned int r = 12345u;
        for (size_t i = 0; i < packedbytes; i++) { r = r * 1103515245u + 12345u; hpacked[i] = (unsigned char)(r >> 16); }
        std::vector<int> hidx(numBufferedFFTs);
        for (int i = 0; i < numBufferedFFTs; i++) hidx[i] = i * fftchannels;
        std::vector<char> hvalid(numBufferedFFTs, 1), hvframes(framestounpack, 1);
        std::vector<double> hA((size_t)numBufferedFFTs * nbands), hB((size_t)numBufferedFFTs * nbands);
        for (size_t i = 0; i < hA.size(); i++) {
            hA[i] = 1e-4 * (double)(i % 37) + 3e-5;   // phase slope per (window, band)
            hB[i] = 0.01 * (double)(i % 11);
        }
        CHECK(cudaMemcpy(packed, hpacked.data(), packedbytes, cudaMemcpyHostToDevice));
        CHECK(cudaMemcpy(sampleIndexes, hidx.data(), numBufferedFFTs * sizeof(int), cudaMemcpyHostToDevice));
        CHECK(cudaMemcpy(validSamples, hvalid.data(), numBufferedFFTs * sizeof(bool), cudaMemcpyHostToDevice));
        CHECK(cudaMemcpy(valid_frames, hvframes.data(), framestounpack * sizeof(bool), cudaMemcpyHostToDevice));
        CHECK(cudaMemcpy(bigA, hA.data(), hA.size() * sizeof(double), cudaMemcpyHostToDevice));
        CHECK(cudaMemcpy(bigBred, hB.data(), hB.size() * sizeof(double), cudaMemcpyHostToDevice));
        CHECK(cudaMemset(dest, 0, destelems * sizeof(cuFloatComplex)));

        float best = 1e30f;
        for (int rep = 0; rep < nreps; rep++) {
            CHECK(cudaEventRecord(t0, stream));
            launch_fused_fringe(stream, numBufferedFFTs, nbands, maxThreadsPerBlock,
                    usecomplex, dopcal,
                    dest, sampleIndexes, validSamples, bigA, bigBred,
                    /*fftloop*/0, /*startblock*/0, /*numblocks*/numBufferedFFTs, fftchannels,
                    ms, packed, valid_frames, framestounpack,
                    pcal_output, N_pcal_bins, /*datasamples*/12345,
                    dopcal ? PCAL_BINS : 0);
            CHECK(cudaEventRecord(t1, stream));
            CHECK(cudaEventSynchronize(t1));
            float msec = 0.f;
            CHECK(cudaEventElapsedTime(&msec, t0, t1));
            best = std::min(best, msec);
        }
        CHECK(cudaGetLastError());

        // One clean accumulation for the pcal comparison: the timed loop above
        // launched nreps times, so its pcal bins hold nreps subints' worth.
        if (dopcal) {
            CHECK(cudaMemset(pcal_output, 0, pcalbytes));
            launch_fused_fringe(stream, numBufferedFFTs, nbands, maxThreadsPerBlock,
                    usecomplex, dopcal,
                    dest, sampleIndexes, validSamples, bigA, bigBred,
                    0, 0, numBufferedFFTs, fftchannels,
                    ms, packed, valid_frames, framestounpack,
                    pcal_output, N_pcal_bins, 12345, PCAL_BINS);
            CHECK(cudaStreamSynchronize(stream));
            std::vector<float> hpcal(pcalbytes / sizeof(float));
            CHECK(cudaMemcpy(hpcal.data(), pcal_output, pcalbytes, cudaMemcpyDeviceToHost));
            char fn[256];
            snprintf(fn, sizeof(fn), "pcal-%s-%s-%dx%d.f32", usecomplex ? "complex" : "real",
                     tiled ? "tiled" : "untiled", nbands, fftchannels);
            FILE *f = fopen(fn, "wb");
            if (f) { fwrite(hpcal.data(), sizeof(float), hpcal.size(), f); fclose(f); }
        }

        std::vector<cuFloatComplex> hdest(destelems);
        CHECK(cudaMemcpy(hdest.data(), dest, destelems * sizeof(cuFloatComplex), cudaMemcpyDeviceToHost));
        printf("  %6d %8d %11.1f %20llu  %s\n", nbands, fftchannels, best * 1000.f,
               fnv1a(hdest.data(), destelems * sizeof(cuFloatComplex)), SHAPES[si].why);
        fflush(stdout);

        CHECK(cudaFree(dest));
        CHECK(cudaFree(packed));
        CHECK(cudaFree(sampleIndexes));
        CHECK(cudaFree(validSamples));
        CHECK(cudaFree(valid_frames));
        CHECK(cudaFree(bigA));
        CHECK(cudaFree(bigBred));
        if (dopcal) { CHECK(cudaFree(pcal_output)); CHECK(cudaFree(N_pcal_bins)); }
    }
    return 0;
}
