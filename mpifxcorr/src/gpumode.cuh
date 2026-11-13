#ifndef GPUMODE_H
#define GPUMODE_H

#include <cuda_runtime.h>
#include <cuComplex.h>
#include <cufft.h>
#include <cufftXt.h>
#include "mode.h"
#include "gpumode_kernels.cuh"
#include "gpudecode.cuh"
#include <mutex>
#include <chrono>

class Configuration;

class GPUMode : public Mode {
public:
    GPUMode(Configuration *conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend,
            int gsamples, int nrecordedfreqs, double recordedbw, double *recordedfreqclkoffs,
            double *recordedfreqclkoffsdelta, double *recordedfreqphaseoffs, double *recordedfreqlooffs,
            int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling,
            Configuration::complextype tcomplex, int unpacksamp, bool fbank, bool linear2circular, int fringerotorder,
            int arraystridelen, bool cacorrs, double bclock);

    ~GPUMode() override;

    /**
     * Install the compute stream that all subsequently constructed GPUModes
     * will enqueue their work on. GPUCore calls this with its own stream
     * before any Mode is constructed, so station processing, the XMAC and
     * the transfers form a single in-order queue - the explicit ordering
     * that the procslots pipelining work builds on. If no stream has been
     * installed a GPUMode creates (and owns) a private one.
     */
    static void setSharedComputeStream(cudaStream_t stream) { sharedComputeStream = stream; }

    /**
     * True (default) when per-window weights/validity/sample indexes are
     * computed on the device by the gpu_set_weights kernel, eliminating the
     * valid_frames D2H round-trip and the per-datastream stream drains.
     * DIFX_GPU_WEIGHTS_HOST=1 restores the host set_weights path (which also
     * carries the full-fidelity WDEBUG output).
     */
    /** True when the XMAC computes autocorrelations as ordinary baselines
     * (DIFX_GPU_XMAC_AUTOCORR), so Mode accumulates none: no atomics and no
     * cross-pol pass in the rotation kernel, no device buffer, no D2H, no host
     * mirror. See docs/gpu-autocorr-design.md. */
    static bool deviceAutocorrs();
    static bool useGpuWeights();
    /// Test path (DIFX_GPU_SIDEBAND_CPUCOMPAT=1): reproduce the CPU's sideband
    /// handling exactly, for a parity test against the CPU at machine precision.
    static bool sidebandCpuCompat();

    /**
     * Land the device-computed per-window weights on the host and run the
     * (interim, see docs/gpu-deserialization-design.md) host accumulations
     * that consume them: Mode::dataweight[] for the baseline-weight loops
     * and the per-band autocorrelation weights. Must be called after the
     * end-of-subint compute-stream drain (GPUCore does, before
     * host_accumulate). No-op when this subint's weights were computed on
     * the host (fallback path or invalid subint).
     */
    /**
     * Fold this subint's device-computed weights and autocorrelations from the
     * pinned staging halves into the host mirrors (weights[][], autocorrelations
     * [][]). validsubint is subint-N's validity captured at issue time (see
     * GPUCore per-slot validsubint) - it must be passed rather than read from
     * the mutable Mode state, which the pipelined next-subint issue has already
     * overwritten by the time this runs. No-op on the host-weights fallback
     * (keyed off the static useGpuWeights()) or an invalid subint.
     */
    void finishWeights(bool validsubint);

    /**
     * Was the subint most recently set up (setData/setOffsets) valid? Captured
     * by GPUCore per procslot right after process_gpu_tofft, before the next
     * subint's issue overwrites datalengthbytes/offsetseconds. Validity is
     * datalengthbytes>1 && offsetseconds!=INVALID_SUBINT (matches the invalid
     * early-return in process_gpu_tofft and the old set_weights subintValid).
     */
    [[nodiscard]] bool isSubintValid() const {
        return (datalengthbytes > 1) && (offsetseconds != INVALID_SUBINT);
    }

    /**
     * Declare whether the Core receive buffers (procslots[].databuffer[]) that
     * setData() hands to process_gpu are page-locked (cudaHostRegister'd).
     * GPUCore calls this once, before any Mode is constructed, after
     * registering (or failing to register) those buffers. When true,
     * process_gpu issues its input H2D copy directly from the delivered
     * buffer; when false it falls back to staging through packeddata_gpu's
     * pinned host half.
     */
    static void setInputBuffersPinned(bool pinned) { inputBuffersPinned = pinned; }

    /**
     * Estimate the total device memory (bytes) a GPUMode for this
     * config/datastream will allocate, using only Configuration lookups -
     * callable before any Mode exists. Mirrors the constructor's device
     * allocations plus the cuFFT plan work area; used by GPUCore's start-up
     * VRAM budget check so an oversized job fails immediately with a clear
     * message instead of partway through buffer allocation.
     */
    static size_t estimateDeviceBytes(Configuration *config, int configindex, int dsindex);

    /**
     * First half of a subint's station processing: input H2D, unpack,
     * gpu_set_weights (sample indices/validity + per-window dataweights),
     * fringe rotation, FFT. Writes NONE of the buffers the deferred host tail
     * reads (autocorr/gTotalWeight/pcal host mirrors), so the next subint's
     * process_gpu_tofft can run on the compute stream while the current
     * subint's outputs drain and its host tail runs. Returns numBufferedFFTs.
     * On an invalid subint (datalengthbytes<=1) it zeros fftd/gDataWeights
     * /validity and returns early (no sync); process_gpu_afterfft then no-ops.
     */
    int process_gpu_tofft(int fftloop, int numBufferedFFTs, int startblock,
                          int numblocks);

    /**
     * Tell this Mode which procslot (0..RECEIVE_RING_LENGTH-1) the next
     * process_gpu_tofft belongs to. Selects the RING-deep host-staging slot so
     * that filling subint N+1's host buffers cannot clobber subint N's still-
     * in-flight async H2D uploads (the tail-overlap pipeline runs the host ~1
     * subint ahead of the GPU). Called by GPUCore::issue_tofft.
     */
    void setProcSlot(int slot) { procSlot = slot; }
    /**
     * Second half: gTotalWeight D2H (the sum itself is now fused into
     * gpu_set_weights in tofft), pcal extraction +
     * copyToHost, fractionalRotation (autocorrelations) + autocorr copyToHost.
     * Produces every tail-consumed output. Enqueued on the compute stream after
     * tofft; must run before the NEXT subint's tofft (single-stream order keeps
     * the fftd/gDataWeights it reads valid). No-op for an invalid subint.
     */
    int process_gpu_afterfft(int fftloop, int numBufferedFFTs, int startblock,
                             int numblocks);

    void set_weights(int subloopindex, int nframes, int *counts, int numBufferedFFTs);
    /// Compute per-frame validity (valid_frames) for this subint. Format-specific
    /// (needs the mark5_stream), so overridden in Mk5_GPUMode; base is a no-op.
    /// Replaces the validity side of the old unpack_all.
    virtual void blankFrames(int framestounpack) {}
    /// Launch the fused decode+fringe-rotation kernel (see launch_fused_fringe).
    /// Format-specific (needs the mark5_stream + packed data), so overridden in
    /// Mk5_GPUMode; base is a no-op. Called by GPUMode::fringeRotation after the
    /// (format-agnostic) per-window coefficient precompute.
    virtual void launchFusedRotate(int numBufferedFFTs, int fftloop,
                                   int startblock, int numblocks, int framestounpack) {}
    void runFFT();
    void fringeRotation(int fftloop, int numBufferedFFTs, int startblock, int numblocks, int framestounpack);
    void calculatePre_cpu(int fftloop, int numBufferedFFTs, int startblock, int numblocks);
    void fractionalRotation(int fftloop, int numBufferedFFTs, int startblock, int numblocks, bool calccrosspolautocorrs, int *counts);

    /// Device-side per-FFT validity flags. Invalid FFT windows have stale
    /// (never zeroed) spectra in fftd_gpu - the mode kernels
    /// skip them - so consumers (the XMAC kernel) must exclude them; the CPU
    /// path instead zeroes such spectra so they contribute nothing.
    [[nodiscard]] const bool* getGpuValidSamples() const { return gValidSamples->gpuPtr(); }

    /// Device-side per-FFT-window data weights, computed by gpu_set_weights.
    /// Consumed by GPUCore's baseline-weight reduction (gpu_baseline_weights),
    /// which sums dw1[w]*dw2[w] over the subint's windows on the device instead
    /// of on the host. Valid only on the device-weights path (the
    /// DIFX_GPU_WEIGHTS_HOST fallback fills the host dataweight[] array, not
    /// this buffer).
    [[nodiscard]] const float* getGpuDataWeights() const { return gDataWeights->gpuPtr(); }

    /// Rotated spectra: the FFT output with the fractional-sample rotation
    /// applied in place. Consumers that need the conjugate (the XMAC, the
    /// cross-pol autocorrelations) conjugate in the multiply via cuCmulConjf -
    /// there is deliberately no second, pre-conjugated copy of this array.
    GpuMemHelper<cuFloatComplex> *fftd_gpu;

protected:
    int cudaMaxThreadsPerBlock;
    int cfg_numBufferedFFTs;
    // The unpacked-sample buffers (unpackeddata_gpu / complex_unpackeddata_gpu
    // and their pointer arrays) were removed when unpack was fused into the
    // fringe-rotation kernel: samples are now decoded straight from the packed
    // frame payload into registers (see gpu_fused_fringe), so there is no
    // global unpacked buffer to round-trip through.
    GpuMemHelper<cuFloatComplex> *complex_fringe_rotated_gpu;
    GpuMemHelper<cuFloatComplex> *temp_autocorrelations_gpu;
    GpuMemHelper<char> *packeddata_gpu;
    GpuMemHelper<bool> *valid_frames;

    size_t estimatedbytes_gpu;

    // Remember how long the 'unpackedarrays' are -- norally this would be
    // 'unpacksamples' but e.g. the Mk5Mode implementation overwrites that
    size_t unpackedarrays_elem_count;

    GpuMemHelper<int> *gSampleIndexes;
    GpuMemHelper<bool> *gValidSamples;
    /// This subint's validity bit-words (host-born, uploaded per subint for
    /// the gpu_set_weights kernel; FLAGS_PER_INT bits per word).
    GpuMemHelper<unsigned int> *gValidFlags;
    /// Per-window data weights, computed on the device by gpu_set_weights.
    /// The full array is only D2H'd under the WDEBUG gate (Increment 2b);
    /// routine consumers use the reduced scalar gTotalWeight instead.
    GpuMemHelper<float> *gDataWeights;
    /// Single-scalar device+host reduction of gDataWeights over the subint's
    /// windows (Increment 2b): totalW = sum_w dataweight[w]. The AC per-band
    /// weight accumulation is totalW times a config-static band multiplicity,
    /// so only this scalar needs to come back to the host each subint -
    /// replacing the per-subint full gDataWeights D2H.
    GpuMemHelper<float> *gTotalWeight;
    /// Per-freq matching-band count for the indices band map - pure
    /// configuration, built once at construction (see also indices).
    int *countsStatic;
    /// True while the current subint's weights live only on the device
    /// (i.e. gpu_set_weights ran and finishWeights() has not yet run).
    bool weightsOnDevice;
    GpuMemHelper<double> *gInterpolator;
    GpuMemHelper<float> *gFracSampleError;
    GpuMemHelper<double> *gLoFreqs;
    /// Per-recorded-band decode-time transforms (GPUDECODE_BAND_* flags), set
    /// once in the constructor: every band is turned into upper sideband while
    /// it is decoded, so nothing after the decode needs to know the sideband.
    GpuMemHelper<unsigned char> *gBandFlags;
    /// True if any band has a decode-time transform (LSB, DSB or VGOS flip).
    bool anyBandTransform;
    /// Per-band offset (MHz) added to the channel frequencies of the
    /// fractional-sample correction: 0, except on the CPU-compatible test path.
    GpuMemHelper<double> *gFracFreqOffset;
    /// Sample encoding / frame header of this datastream (VDIF family or CODIF).
    GpuDecodeFormat decodeFormat;
    /// Per-(FFT window, band) fringe-rotation phase slope (bigAval) and
    /// reduced intercept (bigB_reduced), precomputed once per subint by
    /// gpu_precompute_fringe_rotator so the per-sample rotation kernels do not
    /// recompute the FP64 interpolator/bigA/bigB math in every thread. Layout
    /// [window * numrecordedbands + band]; device-only. See gpu-plan.md.
    GpuMemHelper<double> *gBigA;
    GpuMemHelper<double> *gBigBred;
    GpuMemHelper<unsigned int> *indices;
    GpuMemHelper<double>* grecordedfreqclockoffsets;
    GpuMemHelper<double>* grecordedfreqclockoffsetsdelta;
    GpuMemHelper<double>* grecordedfreqlooffsets;
    GpuMemHelper<int>* pcal_offsets_hz;
    GpuMemHelper<float> *pcal_output_real = nullptr;  // temporary unassembled output for the pcaloffsethz==0.0f case
    GpuMemHelper<cuFloatComplex> *pcal_output_complex = nullptr;  // temporary unassembled output for the pcaloffsethz!=0.0f case
    GpuMemHelper<int>* N_pcal_bins;
    GpuMemHelper<int>* counts_gpu;
    cudaStream_t cuStream;
    /// The stream installed by setSharedComputeStream (GPUCore's), or nullptr.
    static cudaStream_t sharedComputeStream;
    /// True when GPUCore has page-locked the procslots receive buffers, so
    /// process_gpu can DMA directly from them (see setInputBuffersPinned).
    static bool inputBuffersPinned;
    /// True when cuStream is a private stream this mode must destroy.
    bool ownsStream;

    // precalc
    GpuMemHelper<int> *nearestSamples;

    // toggle for reseting pcal_output_real and pcal_output_complex to zero
    bool pcalResetInitialized = false;


    //GpuMemHelper<int> *counts_gpu;
private:
    cufftHandle fft_plan;
    /// Current procslot (0..RECEIVE_RING_LENGTH-1), set by setProcSlot before
    /// each process_gpu_tofft; indexes the RING-deep host-staging buffers.
    int procSlot = 0;
    int pcalResetDataSec = INVALID_SUBINT;
    int pcalResetDataNs = 0;

    bool is_dataweight_valid(int subloopindex);
    bool is_data_valid(int index, int subloopindex);

    /// Per-freq matching-band counts, filled in process_gpu_tofft (device path:
    /// copied from countsStatic; host-weights path: filled by set_weights) and
    /// consumed by fractionalRotation in process_gpu_afterfft. A member (not a
    /// process_gpu local) because the two halves are now separate calls.
    /// Allocated to numrecordedfreqs in the constructor.
    int *savedProcessCounts = nullptr;
    /// True between process_gpu_tofft's invalid early-return and afterfft, so
    /// afterfft no-ops for an invalid subint (mirrors the old single-function
    /// early return). Only meaningful between the paired tofft/afterfft calls.
    bool tofftInvalidSubint = false;

    std::chrono::time_point<std::chrono::system_clock, std::chrono::system_clock::duration> constructor_time;
    // Per-instance CUDA timing events
    cudaEvent_t ev_start = nullptr, ev_copy1 = nullptr, ev_unpack = nullptr;
    cudaEvent_t ev_copy2 = nullptr, ev_pcal  = nullptr, ev_rotate = nullptr;
    cudaEvent_t ev_fft   = nullptr, ev_frac  = nullptr;

    // Per-instance timing accumulators (microseconds)
    long long t_copyto      = 0;
    long long t_unpack      = 0;
    long long t_pcal        = 0;
    long long t_rotate      = 0;
    long long t_fft         = 0;
    long long t_fracrotate  = 0;
    long long t_postprocess = 0;
    long long t_total       = 0;
    int calls               = 0;


};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
