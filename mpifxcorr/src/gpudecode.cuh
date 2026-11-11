#ifndef GPUDECODE_H
#define GPUDECODE_H

#include <cuComplex.h>
#include <cuda_runtime.h>

struct mark5_stream;

// Per-frame validity: one thread per frame runs the VDIF blanker and writes
// valid_frames[frame]. Produces the flags gpu_set_weights and the fused
// decode+fringe kernel consume (was a by-product of the old gpu_unpack).
void launch_blank_frames(dim3 grid, dim3 block, cudaStream_t stream,
		struct mark5_stream ms, const void *packed, int nframes, bool *valid_frames);

// Fused decode + fringe rotation (+ optional phase-cal folding). Decodes each
// sample straight from the packed frame payload into a register, rotates it and
// writes the complex FFT input - no global unpacked buffer round-trip. Replaces
// the old gpu_unpack + gpu_fringeRotation (+ gpu_pcalextraction when dopcal).
// Dispatches to the real/complex x pcal/no-pcal template instantiations, and
// owns the launch geometry: by default the shared-memory-transposed tiled
// kernel (docs/gpu-fringetile-design.md), or the untiled one when
// DIFX_GPU_FRINGE_TILE=0.
void launch_fused_fringe(cudaStream_t stream,
		size_t numBufferedFFTs, size_t numrecordedbands, int maxThreadsPerBlock,
		bool usecomplex, bool dopcal,
		cuFloatComplex *dest, const int *sampleIndexes, const bool *validSamples,
		const double *bigA, const double *bigBred,
		int fftloop, int startblock, int numblocks, size_t fftchannels,
		struct mark5_stream ms, const void *packed, const bool *valid_frames,
		int framestounpack,
		void *pcal_output, const int *N_pcal_bins, int datasamples, int pcal_bin_stride_length);

#endif
