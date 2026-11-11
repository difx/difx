#ifndef GPUCORE_H
#define GPUCORE_H

#include <cuda_runtime.h>
#include <cuComplex.h>
#include <vector>
#include "core.h"

class Mode;

/**
 * @class GPUCore
 * @brief GPU-accelerated subclass of Core for cross-multiplication and averaging.
 *
 * In the legacy CPU implementation, data is heavily partitioned into `xmacstridelen` 
 * chunks to keep working data inside the CPU's L1/L2 caches. The CPU calculates 
 * intermediate cross-multiplies, stores them in `scratchspace->threadcrosscorrs`, 
 * and later un-jumbles them via frequency averaging into `procslots[index].results`.
 * * GPUCore completely bypasses this cache-thrashing architecture. It fuses the XMAC 
 * (cross-multiply accumulate) and frequency averaging steps into a single GPU kernel, 
 * writing contiguous, perfectly formatted visibilities directly to a device-side 
 * results buffer, mirroring the final `procslots` layout natively.
 */
class GPUCore : public Core {
public:
    GPUCore(const int id, Configuration *const conf, int *const dids, MPI_Comm rcomm);
    ~GPUCore();

    virtual void loopprocess(int threadid);

    /**
     * First pipeline stage for subint `index`: per-datastream host prep
     * (setData/setValidFlags/setOffsets - NOT zeroAutocorrelations/resetpcal,
     * which move to the tail) followed by GPUMode::process_gpu_tofft (input H2D,
     * unpack, weights, fringe rotation, FFT) on the compute stream. Captures
     * each datastream's validity into validsubint[index][ds]. Writes no
     * tail-consumed output, so it can run on the compute stream while the
     * PREVIOUS subint's outputs drain and its host tail runs.
     */
    void issue_tofft(int index, int threadid, int startblock, int numblocks, Mode **modes, Polyco *currentpolyco,
                     threadscratchspace *scratchspace);

    /**
     * Second pipeline stage for subint `index`: per-datastream
     * GPUMode::process_gpu_afterfft (weight reduction, pcal, fractional
     * rotation/autocorr) then the fused XMAC into results_gpu[index] and the
     * baseline-weight reduction, all on the compute stream; records
     * evComputeDone[index]; then the asynchronous output device->host copies
     * (visibilities onto d2hStream, gated on evComputeDone) recording
     * d2hDone[index] and h2dInputDone[index]. Does NOT wait for them or run the
     * host tail - that is completegpudata(index).
     */
    void issue_afterfft_xmac_drain(int index, int threadid, int startblock, int numblocks, Mode **modes,
                                   Polyco *currentpolyco, threadscratchspace *scratchspace);

    /**
     * Complete subint `index`: wait for its device->host copies (d2hDone[index],
     * h2dInputDone[index]) to land, then run the host tail - fold the
     * device-computed weights and autocorrelations (finishWeights), accumulate
     * autocorrelations/baseline-weights into procslots[index].results, copy
     * pcal, and memcpy the staged visibilities across. After this the slot's
     * results are complete and it may be released to the manager. Reads
     * validsubint[index][ds] (captured at issue time) for the invalid-subint
     * skip, since the Mode scalars now reflect the next subint.
     */
    void completegpudata(int index, int threadid, int startblock, int numblocks, Mode **modes,
                         Polyco *currentpolyco, threadscratchspace *scratchspace);

protected:
    virtual Mode *getMode(const int configindex, const int datastreamindex) {
        return config->getMode(configindex, datastreamindex, true);
    }

private:
    // -------------------------------------------------------------------------
    // GPU Memory Pointers & Streams
    // -------------------------------------------------------------------------
    /// Compute stream: all station processing (GPUMode) and the XMAC enqueue here,
    /// in order, so the XMAC naturally follows the FFT output it consumes.
    cudaStream_t cuStream;
    /// Device-to-host stream: the visibility transfer back to the host runs here so
    /// it can overlap the NEXT subintegration's compute on cuStream.
    cudaStream_t d2hStream;

    /// Per-procslot GPU-side state, one entry per RECEIVE_RING_LENGTH ring slot,
    /// indexed by the procslots ring index. Complements Core::procslots (the
    /// CPU-side ring): consolidates the buffers and events the tail-overlap
    /// pipeline keeps per slot (previously six parallel std::vectors).
    struct gpuprocslot {
        /// Device-side visibility buffer, mapped exactly to the CPU's
        /// procslots[index].results layout (replaces scratchspace->threadcrosscorrs).
        /// One per slot so subint N's deferred, overlapped D2H reads a stable
        /// buffer while subint N+1's XMAC writes its own. Size:
        /// maxcoreresultlength * sizeof(cuFloatComplex).
        cuFloatComplex* results_gpu = nullptr;

        /// PINNED host staging for the visibility transfer. The D2H must land in
        /// page-locked memory to be truly async (cudaMemcpyAsync to pageable is
        /// effectively synchronous and would defeat the overlap); procslots[].results
        /// is pageable, so we stage here and completegpudata() copies the landed
        /// prefix across on the host.
        cuFloatComplex* results_host = nullptr;

        /// Event marking completion of this slot's visibility D2H on d2hStream.
        /// completegpudata() waits on it before the slot is released for the send.
        cudaEvent_t d2hDone = nullptr;

        /// Event marking completion of this slot's input H2D copies on cuStream
        /// (recorded in issue_afterfft_xmac_drain). completegpudata() waits on it
        /// before release so the manager cannot refill procslots[].databuffer[]
        /// while an async copy from it (pinned-input path) is still in flight.
        cudaEvent_t h2dInputDone = nullptr;

        /// Event recorded on cuStream after this subint's afterfft + XMAC +
        /// baseline-weight reduction (and their output D2Hs). d2hStream waits on it
        /// before the visibility D2H, and it lets issue_tofft(N+1) be enqueued
        /// right after - replacing the old end-of-subint cudaStreamSynchronize.
        cudaEvent_t evComputeDone = nullptr;

        /// validsubint[ds]: each datastream's subint validity captured
        /// (GPUMode::isSubintValid()) right after issue_tofft, before the pipelined
        /// next-subint issue overwrites the Mode's datalengthbytes/offsetseconds.
        /// completegpudata reads it to skip the weight/autocorr fold for datastreams
        /// whose subint had no valid data.
        std::vector<char> validsubint;
    };
    std::vector<gpuprocslot> gpuprocslots;

    /// When true (default; disable with DIFX_GPU_PIPELINE=0), subint N's visibility
    /// transfer is left in flight while subint N+1 is issued, and only awaited just
    /// before slot N is released - overlapping the transfer with N+1's compute. When
    /// false, each subint is completed immediately after it is issued (no overlap),
    /// reproducing the pre-pipelining behaviour for A/B comparison.
    bool pipeline;

    /**
     * @brief Device pointers to the FFT output buffers for Datastream 1 (one per baseline).
     * In the CPU case, this is accessed dynamically via `modes[ds1index]->getFreqs()`.
     * Here, we copy an array of device pointers so the GPU kernel can read the
     * raw VRAM buffers directly without querying the host. These pointers are
     * frequency-independent (they depend only on the baseline's datastream) and,
     * because a GPUMode's fftd buffers are allocated once and never move, they are
     * invariant for a given configuration - so they are populated once by
     * buildXmacPlans() rather than every subintegration.
     *
     * NOTE: when the per-slot GPU buffer pool ([DEPTH] double/triple buffering)
     * lands, these cached pointers will need to be rebuilt per slot.
     */

    /**
     * @brief Device pointers to the rotated FFT output buffers for Datastream 2 (one per baseline).
     *
     * These are the SAME (un-conjugated) buffers as d_m1_ptrs draws from - the
     * XMAC kernel conjugates this operand in the multiply (cuCmulConjf). The
     * CPU path's equivalent, `modes[ds2index]->getConjugatedFreqs()`, does hand
     * back a materialised conjugate; the GPU path deliberately does not keep
     * one. See d_m1_ptrs.
     */

    /**
     * @brief Device pointers to the per-FFT validity flags of each baseline's
     * two datastreams (one entry per baseline, like d_m1_ptrs/d_m2_ptrs).
     * Invalid FFT windows hold stale spectra in the modes' FFT buffers, so the
     * fused XMAC kernel skips any FFT flagged invalid on either datastream
     * (the CPU path zeroes such spectra, making their contribution zero).
     */

    /**
     * @brief Per-configuration baseline-weight reduction plan (Increment 2).
     *
     * Replaces the per-window host loop that used to sum dataweight1[w] *
     * dataweight2[w] over the subint's FFT windows for every (freq, baseline,
     * polproduct) in host_accumulate. The accumulators are enumerated in the
     * EXACT order the host finalize fold consumes them (used freq -> baseline
     * with localfreqindex>=0 -> polproduct), so the reduced values D2H'd into
     * h_bweightResults map one-to-one onto the fold's walk. d_bwDw1/d_bwDw2 hold,
     * per accumulator, the two datastreams' device per-window dataweight arrays
     * (GPUMode::getGpuDataWeights). Built once per configuration in
     * buildXmacPlans, freed in freeXmacPlans. Only used on the device-weights
     * path; the DIFX_GPU_WEIGHTS_HOST fallback keeps the host loop.
     *
     * bwDestOffset carries each accumulator's destination: the float index into
     * procslots[].floatresults where its reduced weight is added. Recording it
     * at plan-build time makes the flat fold self-describing - it does not
     * re-walk (and cannot silently diverge from) the config enumeration.
     */
    const float** d_bwDw1 = nullptr;    ///< [bweightNumAccum] ds1 dataweight ptrs
    const float** d_bwDw2 = nullptr;    ///< [bweightNumAccum] ds2 dataweight ptrs
    float* d_bweightResults = nullptr;  ///< [bweightNumAccum] per-subint device sums
    float* h_bweightResults = nullptr;  ///< [bweightNumAccum] pinned host mirror
    std::vector<int> bwDestOffset;      ///< [bweightNumAccum] floatresults index per accumulator
    int bweightNumAccum = 0;

    /**
     * @brief Cached, per-frequency launch metadata for the fused XMAC kernel.
     *
     * All of the DiFX Configuration lookups (band indexes, baseline result
     * offsets, channel counts, pol-product counts) that feed a fused-kernel
     * launch are invariant for a given configindex. Rather than re-walk the
     * config tree and re-upload these small arrays on every subintegration, we
     * compute them once per configuration in buildXmacPlans() and reuse them.
     * The device arrays below are persistent (allocated in buildXmacPlans,
     * freed in freeXmacPlans / on config change).
     */
    struct XmacFreqPlan {
        int numPolarisationProducts;
        int num_averaged_channels;
        int channelstoaverage;
        /** Baselines in THIS frequency's launch: the cross baselines, followed by
         * one autocorrelation baseline per datastream that records this
         * frequency. An autocorrelation is a baseline whose two antennas are the
         * same, so it carries the frequency's polarisation products exactly as a
         * cross baseline does - 45 baselines becomes 55, not 365. Because the
         * self-baselines differ per frequency, each plan owns its own pointer
         * and index arrays rather than sharing one set. */
        int numBaselines;
        const cuFloatComplex** d_m1_ptrs;  ///< [numBaselines]
        const cuFloatComplex** d_m2_ptrs;  ///< [numBaselines]
        const bool** d_v1_ptrs;            ///< [numBaselines]
        const bool** d_v2_ptrs;            ///< [numBaselines]
        int* d_stream1BandIndexes;         ///< [numBaselines * numPolarisationProducts]
        int* d_stream2BandIndexes;         ///< [numBaselines * numPolarisationProducts]
        /** [numBaselines * numPolarisationProducts]: one results-buffer offset
         * per polarisation product, -1 where the product does not exist. Per
         * product rather than per baseline because an autocorrelation baseline's
         * parallel and cross-pol products sit in different sub-blocks of the
         * autocorrelation region; cross baselines still get
         * base + pol * num_averaged_channels, unchanged. */
        int* d_coreResultOffsets;
        /* Per-baseline, per-stream FFT buffer strides. A GPUMode's fftd buffer
         * is laid out [window][band][fftchannels], where fftchannels =
         * freqchannels * (1 for complex sampling, 2 for real) and the band
         * count are properties of THAT datastream - so on a mixed baseline
         * (e.g. real x complex) the two streams' strides differ and each side
         * must be indexed with its own. */
        int* d_stream1BandStride;          ///< [numBaselines] = that stream's fftchannels
        int* d_stream1WindowStride;        ///< [numBaselines] = fftchannels * numrecordedbands
        int* d_stream2BandStride;          ///< [numBaselines]
        int* d_stream2WindowStride;        ///< [numBaselines]
    };
    std::vector<XmacFreqPlan> xmacPlans;

    /** One autocorrelation output run, expressed as a synthetic baseline for the
     * XMAC. An autocorrelation is just a baseline whose two streams are the same
     * datastream, so the existing kernel computes it unchanged - see
     * docs/gpu-autocorr-design.md.
     *
     * There is one of these per output run rather than one per (datastream,
     * frequency), and each uses only pol slot 0. That is deliberate: a
     * baseline's pol products land at `base + pol * num_averaged_channels`, a
     * fixed stride, whereas the results buffer orders autocorrelations by band
     * index. Packing two polarisations into one synthetic baseline would
     * therefore only work if a frequency's two bands were adjacent, which is
     * usual but NOT guaranteed (F0R, F1R, F0L, F1L is legal). One run per
     * baseline places each output by its own arbitrary base offset and so
     * assumes nothing about band ordering. */
    struct SelfBaseline {
        int datastream;      ///< the antenna on both sides of this "baseline"
        int nproducts;       ///< live polarisation products (1, 2 or 4)
        int band1[4];        ///< per product: the un-conjugated side
        int band2[4];        ///< per product: the conjugated side (== band1 for a
                             ///< parallel hand)
        int resultOffset[4]; ///< per product: absolute complex offset in results
    };

    /** Device autocorrelations (DIFX_GPU_XMAC_AUTOCORR, default off while this
     * is being built): compute autocorrelations in the XMAC, straight into the
     * results buffer, instead of accumulating them in Mode and folding them in
     * on the host. Read once. */
    static bool xmacAutocorrEnabled();

    /** Length, in complex values, to stage back from the device: the
     * cross-correlations, or - with device autocorrelations - everything up to
     * the end of the autocorrelation region.
     *
     * NOTE the results buffer is NOT [xcorrs][autocorrs]: populateResultLengths
     * lays it out [visibilities][baseline weights][shift decorr][autocorrs]
     * [ac weights][pcals], and `coreresultxcorrslength` covers the visibilities
     * only. The middle regions are folded in by the host, so this one transfer
     * carries them across as filler and `copyStagedResults` must skip them -
     * copying straight through zeroed the baseline weights and made Visibility
     * drop every cross-correlation record. */
    int stagedResultsLength(int configindex) const;

    /// Copy the staged device results into a procslot, skipping the host-owned
    /// regions in the middle. See stagedResultsLength.
    void copyStagedResults(int index);

    /// The configindex the cached xmacPlans were built for (-1 = none yet).
    int xmacPlanConfigIndex = -1;

    /// (Re)build the cached per-frequency XMAC launch metadata for a configuration.
    void buildXmacPlans(int configindex, Mode **modes);
    /// Free the device arrays held by the cached XMAC plans.
    void freeXmacPlans();

    /**
     * Start-up VRAM budget check. Estimates the peak device memory this Core
     * will need (all datastreams' GPUModes for the most demanding config, plus
     * this Core's own results/pointer buffers) via GPUMode::estimateDeviceBytes
     * and compares it against what the device reports free, applying a safety
     * margin. cfatal + MPI_Abort if it will not fit, so an oversized job fails
     * in the first second with a clear required-vs-available message instead of
     * a mid-run allocation failure. Must run before any GPUMode is constructed.
     */
    void checkDeviceMemory();

    int cudaMaxThreadsPerBlock;
    /// SM count of the device, used to size the fused-XMAC launch grid.
    int cudaMultiProcessorCount;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
