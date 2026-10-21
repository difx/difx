#include "gpumode.cuh"
#include "core.h"   // Core::RECEIVE_RING_LENGTH for the host-staging ring depth
#include "alert.h"
#include <cuda_runtime.h>
#include <string>
#include <unistd.h>
#include <cufftXt.h>
#include <pcal.h>
#include <algorithm>
#include <cmath>

#include <chrono>
#include <omp.h>
#include <thread>
#include <mutex>
#include <cstdlib>
#include <cstdio>
#include "mathutil.h"
#include <unistd.h>
#include <mpi.h>


using namespace std::chrono;

const int MAX_INDICIES = 10;

cudaStream_t GPUMode::sharedComputeStream = nullptr;
bool GPUMode::inputBuffersPinned = false;

static int weightDebugFrom();   // defined above set_weights

bool GPUMode::deviceAutocorrs() {
    // The XMAC computes autocorrelations straight into the results buffer
    // (GPUCore builds them as real baselines), so Mode does not accumulate them
    // at all: no atomics in the rotation kernel, no cross-pol phase, no device
    // buffer, no D2H, no host mirror. DIFX_GPU_XMAC_AUTOCORR=0 restores the lot,
    // which is also what keeps STA dumps available. See
    // docs/gpu-autocorr-design.md.
    static const bool enabled = []() {
        const char *e = getenv("DIFX_GPU_XMAC_AUTOCORR");
        return (e != NULL) && (strcmp(e, "0") != 0) && (strcasecmp(e, "false") != 0) &&
               (strcasecmp(e, "no") != 0) && (strcasecmp(e, "off") != 0);
    }();
    return enabled;
}

bool GPUMode::useGpuWeights() {
    // Function-local static, so initialisation is thread-safe by the standard
    // (C++11 6.7/4): this is called per subint from every Core processing
    // thread, and the previous `static int cached = -1` read-modify-write was a
    // formal data race (benign only because every writer stores the same value).
    static const bool usegpu = []() {
        const char *e = getenv("DIFX_GPU_WEIGHTS_HOST");
        return !(e != NULL && atoi(e) != 0);
    }();
    return usegpu;
}

void GPUMode::finishWeights(bool validsubint) {
    // Host-weights fallback (static, whole-run) fills the host arrays directly
    // in process_gpu_afterfft, and an invalid subint has no device outputs to
    // fold - either way there is nothing to do here. validsubint is passed by
    // GPUCore (captured per-slot at issue time) rather than read from the
    // mutable Mode state, which the pipelined next-subint issue has already
    // overwritten by the time this deferred tail runs.
    if (!useGpuWeights() || !validsubint)
        return;

    // GPUCore has drained the compute stream, so the async D2H of the device
    // reductions (total weight, autocorr accumulators) have landed in the
    // pinned halves. The per-window dataweight[] array is only refreshed for
    // the WDEBUG parity output below - Increment 2b replaced the routine full
    // D2H with the single total-weight scalar (gTotalWeight).
    const bool wdebug = weightDebugFrom() >= 0 && datasec >= weightDebugFrom();
    if (wdebug)
        memcpy(dataweight, gDataWeights->ptr(), cfg_numBufferedFFTs * sizeof(f32));

    // Host mirror of the device autocorrelation accumulators (was section 8
    // of process_gpu on the host-weights path). Not needed when the XMAC owns
    // the autocorrelations - nothing reads Mode::autocorrelations then.
    if (!deviceAutocorrs()) {
        for (int i = 0; i < autocorrwidth; i++) {
            for (int j = 0; j < numrecordedbands; j++) {
                vectorCopy_cf32(
                        reinterpret_cast<const cf32 *>(&temp_autocorrelations_gpu->ptr()[(i * numrecordedbands * recordedbandchannels) + (j * recordedbandchannels)]),
                        autocorrelations[i][j],
                        recordedbandchannels
                );
            }
        }
    }

    // Per-band autocorrelation weight accumulation (Increment 2b). The band
    // map (indices/countsStatic) is window-independent, so the host path's
    // per-window sum weights[c][band] += dataweight[w] equals the device-reduced
    // total weight (gTotalWeight = sum_w dataweight[w]) times each band's static
    // multiplicity. Zero-weight windows contributed nothing to the old sum, so
    // this matches to FP level.
    // NOTE: assumes perbandweights is not in use on the GPU path (true today;
    // the host tail has a perbandweights branch this does not replicate - see
    // gpu-plan.md work item on GPU perbandweights support).
    if (perbandweights) {
        cfatal << startl << "GPUMode::finishWeights: perbandweights is set but not supported on the device-weights path" << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    const f32 totalW = gTotalWeight->ptr()[0];
    for (int i = 0; i < numrecordedfreqs; i++) {
        const int count = countsStatic[i];
        for (int k = 0; k < count; k++)
            weights[0][indices->ptr()[(i * MAX_INDICIES) + k]] += totalW;
        if (count > 1) {
            weights[1][indices->ptr()[(i * MAX_INDICIES)]] += totalW;
            weights[1][indices->ptr()[(i * MAX_INDICIES) + 1]] += totalW;
        }
    }

    // WDEBUG parity output for the device path: reconstruct the host path's
    // per-window lines (validity inputs are all host-known; dataweight[] was
    // refreshed above under the same gate).
    if (wdebug) {
        const bool subintValid = (datalengthbytes > 1) && (offsetseconds != INVALID_SUBINT);
        for (int w = 0; w < cfg_numBufferedFFTs; w++) {
            const bool flagged_ok =
                ((static_cast<unsigned int>(validflags[w / FLAGS_PER_INT]) >> (w % FLAGS_PER_INT)) & 0x01) != 0;
            const int ns = nearestSamples->ptr()[w];
            if (!subintValid || !flagged_ok) {
                fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=0.000000000 valid=0 reason=rejected\n",
                        datastreamindex, datasec, datans, w, ns);
            } else {
                fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=%.9f valid=%d reason=ok\n",
                        datastreamindex, datasec, datans, w, ns,
                        dataweight[w], dataweight[w] > 0.0f ? 1 : 0);
            }
        }
    }
}

// Env-gated per-window spectral tracing (DIFX_SPEC_DEBUG=<datasec>), the GPU
// twin of the SPECDEBUG lines in CPUMode::process (cpumode.cpp) - identical
// format, so sorted grepped logs diff directly and any CPU-vs-GPU divergence
// localizes to a stage (unpack / fringe rotation+FFT / frac correction).
static int specDebugFrom()
{
    static int from = -2;
    if (from == -2) {
        const char* e = getenv("DIFX_SPEC_DEBUG");
        from = (e != NULL) ? atoi(e) : -1;
    }
    return from;
}


// Device twin of the host set_weights() window loop: one thread per FFT
// window computes the window's data weight from the frame validity the
// unpack/blanker kernels just produced - entirely on-device, so there is
// no valid_frames D2H, no host loop, and no re-upload of the results.
// Deliberate simplifications vs the host path (agreed in
// docs/gpu-deserialization-design.md): unpackstartsamples is always 0 so
// sampleIndexes[w] is nearestSamples[w] directly; nearestSamples == -1
// (the calculatePre sentinel) marks the window invalid instead of
// aborting; and a window spanning more than two frames gets weight 0.
// Each thread also accumulates its weight into the single per-subint total
// (Increment 2b, fused 2026-07-23): the old separate <<<1,1>>> gpu_sum_weights
// reduction kernel is gone - the total is a free by-product of the per-window
// work. `totalWeight` must be zeroed on the stream before this launch. The
// atomicAdd reorders the sum vs the old window-order loop (FP-level, not
// bit-identical for multi-occurrence bands - within the acceptance bar; the
// final visibilities are not bit-reproducible anyway).
__global__ void gpu_set_weights(const int *nearest, const bool *validFrames,
                                const unsigned int *validFlagWords,
                                float *dataweight, bool *validSamples,
                                int *sampleIndexes,
                                int numWindows, int nframes,
                                int framesamples, int fftchannels,
                                bool subintValid, float *totalWeight) {
    const int w = blockIdx.x * blockDim.x + threadIdx.x;
    if (w >= numWindows)
        return;

    const int ns = nearest[w];
    sampleIndexes[w] = ns;

    const bool flagged_ok =
        ((validFlagWords[w / FLAGS_PER_INT] >> (w % FLAGS_PER_INT)) & 1u) != 0u;
    float weight = 0.0f;
    if (subintValid && flagged_ok && ns >= 0) {
        // Frames at or beyond nframes were not delivered this subintegration
        // (their buffers hold stale contents), so they count as invalid -
        // identical to the host path's frame_ok().
        const int start_frame = ns / framesamples;
        const int end_frame = (w + 1 == numWindows)
            ? (ns + fftchannels - 1) / framesamples
            : (nearest[w + 1] - 1) / framesamples;

        const float ok_start =
            (start_frame >= 0 && start_frame < nframes && validFrames[start_frame]) ? 1.0f : 0.0f;
        if (start_frame == end_frame) {
            weight = ok_start;
        } else if (start_frame + 1 == end_frame) {
            const float ok_end =
                (end_frame < nframes && validFrames[end_frame]) ? 1.0f : 0.0f;
            const float frac_first =
                (float)(end_frame * framesamples - ns) / (float)fftchannels;
            weight = frac_first * ok_start + (1.0f - frac_first) * ok_end;
        }
        // (a window spanning >2 frames keeps weight 0)
    }

    dataweight[w] = weight;
    validSamples[w] = weight > 0.0f;
    // Fused total-weight reduction (replaces gpu_sum_weights): sum_w dataweight[w].
    atomicAdd(totalWeight, weight);
}




GPUMode::GPUMode(Configuration *conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend,
                 int gsamples, int nrecordedfreqs, double recordedbw, double *recordedfreqclkoffs,
                 double *recordedfreqclkoffsdelta, double *recordedfreqphaseoffs, double *recordedfreqlooffs,
                 int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling,
                 Configuration::complextype tcomplex, int unpacksamp, bool fbank, bool linear2circular,
                 int fringerotorder, int arraystridelen, bool cacorrs, double bclock) :
        Mode(conf, confindex, dsindex, recordedbandchan, chanstoavg, bpersend, gsamples, nrecordedfreqs, recordedbw,
             recordedfreqclkoffs, recordedfreqclkoffsdelta, recordedfreqphaseoffs, recordedfreqlooffs, nrecordedbands,
             nzoombands, nbits, sampling, tcomplex, unpacksamp, fbank, linear2circular, fringerotorder, arraystridelen,
             cacorrs, bclock), estimatedbytes_gpu(0) {
    //std::cout << "Constructing a new GPUMode" << std::endl;
    auto start = high_resolution_clock::now();


    

    size_t buffer_payload_bytes = (config->getMaxDataBytes() / config->getMultiplexedFrameBytes(confindex, dsindex)) * config->getMultiplexedFramePayloadBytes(confindex, dsindex);
    std::cout << "buffer_payload_bytes: " << buffer_payload_bytes << std::endl;

    size_t unpacked_size = buffer_payload_bytes * 8 / (config->getDNumBits(confindex, dsindex) * config->getDNumRecordedBands(confindex, dsindex));
    if (usecomplex) {
        unpacked_size /= 2;
    }
    std::cout << "unpacked_size: " << unpacked_size << std::endl;

    // in gpumode.cu, right after unpacked_size is computed:
    size_t maxframes = config->getMaxDataBytes() /
                       config->getMultiplexedFrameBytes(confindex, dsindex);
    size_t payloadsamples = config->getMultiplexedFramePayloadBytes(confindex, dsindex) * 8 /
                            (config->getDNumBits(confindex, dsindex) *
                             config->getDNumRecordedBands(confindex, dsindex));
    if (usecomplex) payloadsamples /= 2;   // mirror Mk5's complex framesamples
    


    // Per-band capacity of the unpacked sample buffers; also the stride
    // between bands within unpackeddata_gpu / complex_unpackeddata_gpu.
    unpackedarrays_elem_count = unpacked_size;

    // What's the largest number of FFTs we can fit?
    cfg_numBufferedFFTs = (unpacked_size + fftchannels - 1) / fftchannels;
    //std::cout << "Working on " << cfg_numBufferedFFTs << " FFTs" << std::endl;
    // cfg_numBufferedFFTs = config->getNumBufferedFFTs(confindex);
    vectorFree(dataweight);
    dataweight = vectorAlloc_f32(cfg_numBufferedFFTs);
    for (size_t i = 0; i < cfg_numBufferedFFTs; i++)
        dataweight[i] = 0.0;


    cudaDeviceProp prop;
    checkCuda(cudaGetDeviceProperties( &prop, 0));

    // Use the compute stream GPUCore installed so all modes' station
    // processing and the XMAC share one in-order queue; fall back to a
    // private stream only if none was installed (standalone use).
    if (sharedComputeStream != nullptr) {
        cuStream = sharedComputeStream;
        ownsStream = false;
    } else {
        checkCuda(cudaStreamCreate(&cuStream));
        ownsStream = true;
    }



    cudaMaxThreadsPerBlock = prop.maxThreadsPerBlock;
 
    // Pre-allocate packed data buffer at max possible size. When this mode
    // will use the direct pinned-input path (pinned buffers + shared stream,
    // the same condition process_gpu tests), the host staging half is never
    // touched - skip it (gpuOnly) rather than page-locking getMaxDataBytes
    // per mode that would sit idle.
    packeddata_gpu = new GpuMemHelper<char>(config->getMaxDataBytes(), cuStream,
                                            inputBuffersPinned && !ownsStream);
    checkCuda(cudaStreamSynchronize(cuStream));

    complex_fringe_rotated_gpu = new GpuMemHelper<cuFloatComplex>(fftchannels * cfg_numBufferedFFTs * numrecordedbands, cuStream, true);
    estimatedbytes_gpu += complex_fringe_rotated_gpu->size();

    fftd_gpu = new GpuMemHelper<cuFloatComplex>(fftchannels * cfg_numBufferedFFTs * numrecordedbands, cuStream, false);
    estimatedbytes_gpu += fftd_gpu->size();

    temp_autocorrelations_gpu = new GpuMemHelper<cuFloatComplex>(autocorrwidth * numrecordedbands * recordedbandchannels, cuStream);
    estimatedbytes_gpu += temp_autocorrelations_gpu->size();

    // (The unpacked-sample buffers that used to live here are gone: unpack was
    // fused into the fringe rotation, which now decodes samples straight from
    // packeddata_gpu into registers. unpacked_size still sizes cfg_numBufferedFFTs
    // above.)
    gSampleIndexes = new GpuMemHelper<int>(cfg_numBufferedFFTs, cuStream);
    gValidSamples = new GpuMemHelper<bool>(cfg_numBufferedFFTs, cuStream);


    // gInterpolator was previously constructed wrapping the host interpolator[]
    // member (register-an-existing-pointer ctor). It is now a managed helper so
    // it can carry a RING-deep host staging buffer (see enableHostRing below);
    // process_gpu_tofft copies interpolator[] into its active host slot each
    // subint before uploading.
    gInterpolator = new GpuMemHelper<double>(3, cuStream);
    gFracSampleError = new GpuMemHelper<float>(cfg_numBufferedFFTs, cuStream);
    gLoFreqs = new GpuMemHelper<double>(numrecordedbands, cuStream);
    counts_gpu = new GpuMemHelper<int>(numrecordedfreqs, cuStream); 

    int max_framestounpack = config->getMaxDataBytes() / config->getMultiplexedFrameBytes(confindex, dsindex);
    valid_frames = new GpuMemHelper<bool>(max_framestounpack, cuStream);
    
    indices = new GpuMemHelper<unsigned int>((MAX_INDICIES * numrecordedfreqs), cuStream);
    for (auto i = 0; i < (MAX_INDICIES * numrecordedfreqs); i++) {
        indices->ptr()[i] = 0xffffffff;
    }
    // The per-freq matching-band map (and its counts) are pure configuration:
    // build and upload them once here instead of rebuilding them for every
    // FFT window in the host set_weights loop (which still refreshes its own
    // copy when the DIFX_GPU_WEIGHTS_HOST fallback is active).
    countsStatic = new int[numrecordedfreqs]();
    savedProcessCounts = new int[numrecordedfreqs]();
    for (int i = 0; i < numrecordedfreqs; i++) {
        int count = 0;
        for (int j = 0; j < numrecordedbands; j++) {
            if (config->matchingRecordedBand(configindex, datastreamindex, i, j))
                indices->ptr()[(i * MAX_INDICIES) + count++] = j;
        }
        countsStatic[i] = count;
    }
    indices->copyToDevice();
    gValidFlags = new GpuMemHelper<unsigned int>(cfg_numBufferedFFTs / FLAGS_PER_INT + 1, cuStream);
    gDataWeights = new GpuMemHelper<float>(cfg_numBufferedFFTs, cuStream);
    gTotalWeight = new GpuMemHelper<float>(1, cuStream);
    weightsOnDevice = false;
    grecordedfreqclockoffsets = new GpuMemHelper<double>(numrecordedbands, cuStream);
    grecordedfreqclockoffsetsdelta = new GpuMemHelper<double>(numrecordedbands, cuStream);
    grecordedfreqlooffsets = new GpuMemHelper<double>(numrecordedbands, cuStream);
    // Per-(window, band) precomputed fringe-rotation coefficients (device-only;
    // filled each subint by gpu_precompute_fringe_rotator).
    gBigA = new GpuMemHelper<double>((size_t)cfg_numBufferedFFTs * numrecordedbands, cuStream, true);
    gBigBred = new GpuMemHelper<double>((size_t)cfg_numBufferedFFTs * numrecordedbands, cuStream, true);
    // Copy the lofreq and freq clock offset values to the GPU
    for (auto i = 0; i < numrecordedbands; i++) {
        int localfreqindex = config->getDLocalRecordedFreqIndex(configindex, datastreamindex, i);
        gLoFreqs->ptr()[i] = config->getDRecordedFreq(configindex, datastreamindex, localfreqindex);
        grecordedfreqclockoffsets->ptr()[i] = recordedfreqclockoffsets[localfreqindex];
        grecordedfreqclockoffsetsdelta->ptr()[i] = recordedfreqclockoffsetsdelta[localfreqindex];
        grecordedfreqlooffsets->ptr()[i] = recordedfreqlooffsets[localfreqindex];
    }

    gLoFreqs->copyToDevice();
    grecordedfreqclockoffsets->copyToDevice();
    grecordedfreqclockoffsetsdelta->copyToDevice();
    grecordedfreqlooffsets->copyToDevice();


    // The below has be moved from pcal extraction to here 
    if(!(config->getDPhaseCalIntervalMHz(configindex, datastreamindex) == 0)) { 
        pcal_offsets_hz = new GpuMemHelper<int>(numrecordedbands, cuStream);
        N_pcal_bins = new GpuMemHelper<int>(numrecordedbands, cuStream);
        double bandwidth_hz = 1e6*recordedbandwidth;
        double fs_hz = 2 * bandwidth_hz;
        double pcal_spacing_hz = 1e6*config->getDPhaseCalIntervalMHz(configindex, datastreamindex);
        int N_pcal_bins_max=0;    
     
        for (int ii=0; ii<numrecordedbands; ii++) { 
            int localfreqindex = config->getDLocalRecordedFreqIndex(configindex, datastreamindex, ii);
            pcal_offsets_hz->ptr()[ii] = config->getDRecordedFreqPCalOffsetsHz(configindex, datastreamindex, localfreqindex);
            N_pcal_bins->ptr()[ii] = (int)(fs_hz/gcd(fs_hz,pcal_offsets_hz->ptr()[ii]));
            if (N_pcal_bins->ptr()[ii] > N_pcal_bins_max) {
                N_pcal_bins_max = N_pcal_bins->ptr()[ii]; 
            }  
        }
        pcal_bin_stride_length = N_pcal_bins_max*2;
        pcal_offsets_hz->copyToDevice();
        N_pcal_bins->copyToDevice();
        if (usecomplex) {
            pcal_output_complex = new GpuMemHelper<cuFloatComplex>(numrecordedbands*pcal_bin_stride_length, cuStream);
            for (size_t ii=0; ii<numrecordedbands*pcal_bin_stride_length; ii++)
                pcal_output_complex->ptr()[ii] = make_cuFloatComplex(0.0f, 0.0f);
            pcal_output_complex->copyToDevice();
        } else {
            pcal_output_real = new GpuMemHelper<float>(numrecordedbands*pcal_bin_stride_length, cuStream);
            for (size_t ii=0; ii<numrecordedbands*pcal_bin_stride_length; ii++)
                pcal_output_real->ptr()[ii] = 0.0;
            pcal_output_real->copyToDevice();
        }
    }




    int n[] = {fftchannels};
    int istride = 1;
    int ostride = 1;
    int idist = fftchannels;
    int odist = fftchannels;

    int inembed[] = {0};
    int onembed[] = {0};

    checkCufft(
            cufftPlanMany(
                    &fft_plan,
                    1,
                    (int *) &n,
                    (int *) &inembed,
                    istride,
                    idist,
                    (int *) &onembed,
                    ostride,
                    odist,
                    CUFFT_C2C,
                    numrecordedbands * cfg_numBufferedFFTs
            )
    );
    checkCufft(cufftSetStream(fft_plan, cuStream));


    // precalc
    nearestSamples = new GpuMemHelper<int>(cfg_numBufferedFFTs, cuStream);

    // RING-deep the HOST side of every per-subint host-staging buffer that the
    // device path uploads. The tail-overlap pipeline (DIFX_GPU_PIPELINE=1) runs
    // the host ~1 subint ahead, so subint N+1 fills these host buffers while
    // subint N's tiny async H2D from them may still be queued behind the GPU's
    // compute backlog. Without per-slot host buffers, N+1's fill corrupts N's
    // upload (was masked by the whole-stream drain in Mk5_GPUMode::unpack_all,
    // now removed on the device path). Device buffers stay single: device reads
    // are stream-ordered. RECEIVE_RING_LENGTH-deep matches the procslot ring.
    nearestSamples->enableHostRing(Core::RECEIVE_RING_LENGTH);
    gFracSampleError->enableHostRing(Core::RECEIVE_RING_LENGTH);
    gInterpolator->enableHostRing(Core::RECEIVE_RING_LENGTH);
    gValidFlags->enableHostRing(Core::RECEIVE_RING_LENGTH);
    gValidSamples->enableHostRing(Core::RECEIVE_RING_LENGTH);

    checkCuda(cudaStreamSynchronize(cuStream));

    // Cross-check the start-up VRAM estimator against what was actually
    // tallied (the tally covers only the large buffers, so it must never
    // exceed the estimate - if it does, estimateDeviceBytes is stale).
    size_t estimate = estimateDeviceBytes(config, confindex, dsindex);
    if (estimatedbytes_gpu > estimate)
        cwarn << startl << "GPUMode::estimateDeviceBytes (" << estimate
              << " bytes) is smaller than the allocated-buffer tally (" << estimatedbytes_gpu
              << " bytes) for datastream " << dsindex << " - the estimator is out of date" << endl;
    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - start);
    //cout << "GPUMode(): " << duration.count() << endl;



    constructor_time = high_resolution_clock::now();
}


GPUMode::~GPUMode() {
    auto start = high_resolution_clock::now();
    std::cout << "Starting destructor" << std::endl;
    delete packeddata_gpu;
    delete complex_fringe_rotated_gpu;
    delete fftd_gpu;
    delete temp_autocorrelations_gpu;

    delete gSampleIndexes;
    delete gValidSamples;
    delete gValidFlags;
    delete gDataWeights;
    delete gTotalWeight;
    delete[] countsStatic;
    delete[] savedProcessCounts;
    delete gInterpolator;
    delete gFracSampleError;
    delete gBigA;
    delete gBigBred;

    delete nearestSamples;
    delete counts_gpu;
    delete valid_frames;


    if(!(config->getDPhaseCalIntervalMHz(configindex, datastreamindex) == 0)) { 
        delete pcal_offsets_hz;
        delete N_pcal_bins;
         if (usecomplex) {
            delete pcal_output_complex;
        } else {
            delete pcal_output_real;
        }       
    }
    printf("pcal_output_real_gpu_mode \n");
    if (pcal_output_real_gpu_mode != nullptr) {
        pcal_output_real_gpu_mode = nullptr;
    }
    printf("freed pcal_output_real_gpu_mode \n");
    if (pcal_output_complex_gpu_mode != nullptr) {
        pcal_output_complex_gpu_mode = nullptr;
    }

    printf("done \n");

    if (ev_start) {
        cudaEventDestroy(ev_start);  cudaEventDestroy(ev_copy1);
        cudaEventDestroy(ev_unpack); cudaEventDestroy(ev_copy2);
        cudaEventDestroy(ev_pcal);   cudaEventDestroy(ev_rotate);
        cudaEventDestroy(ev_fft);    cudaEventDestroy(ev_frac);
    }


    checkCufft(cufftDestroy(fft_plan));
    // Never destroy the shared compute stream - GPUCore owns it, and modes
    // are destroyed/recreated mid-run on configuration changes.
    if (ownsStream)
        checkCuda(cudaStreamDestroy(cuStream));

    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - start);

    if (calls > 0) {
        cout << "GPUMode pid=" << getpid()
        // << " tid=" << std::this_thread::get_id()
         << " DS=" << datastreamindex
         << " (" << calls << " calls):" << endl;
        cout << "  copyto:      " << t_copyto      / calls << " us" << endl;
        cout << "  unpack:      " << t_unpack      / calls << " us" << endl;
        cout << "  rotate:      " << t_rotate      / calls << " us" << endl;
        cout << "  fft:         " << t_fft         / calls << " us" << endl;
        cout << "  fracrotate:  " << t_fracrotate  / calls << " us" << endl;
        cout << "  pcal:        " << t_pcal        / calls << " us" << endl;
        cout << "  postprocess: " << t_postprocess / calls << " us" << endl;
        cout << "  total:       " << (double)t_total / 1e6 << " s" << endl;
    }


}

size_t GPUMode::estimateDeviceBytes(Configuration *config, int configindex, int dsindex)
{
    // Mirrors the constructor's device allocations (GpuMemHelper device
    // buffers plus the cuFFT plan work area) using only Configuration
    // lookups. If the constructor gains/loses allocations this must be
    // updated to match - the constructor cross-checks its actual tally
    // against this estimate and warns on divergence.
    const int nbands = config->getDNumRecordedBands(configindex, dsindex);
    const int nfreqs = config->getDNumRecordedFreqs(configindex, dsindex);
    const int nbits = config->getDNumBits(configindex, dsindex);
    const bool complexsampled = (config->getDSampling(configindex, dsindex) == Configuration::COMPLEX);
    const int freqindex = config->getDRecordedFreqIndex(configindex, dsindex, 0);
    const int recordedbandchan = config->getFNumChannels(freqindex);
    int fftchannels = recordedbandchan * 2;
    if (complexsampled)
        fftchannels /= 2;

    const size_t maxdatabytes = config->getMaxDataBytes();
    const int framebytes = config->getMultiplexedFrameBytes(configindex, dsindex);
    if (framebytes <= 0 || nbands <= 0 || fftchannels <= 0)
        return 0; // not a format the GPU path can run; getMode's gate reports it
    const size_t maxframes = maxdatabytes / framebytes;
    size_t unpacked_size = maxframes * config->getMultiplexedFramePayloadBytes(configindex, dsindex) * 8 /
                           (nbits * nbands);
    if (complexsampled)
        unpacked_size /= 2;
    const size_t nFFTs = (unpacked_size + fftchannels - 1) / fftchannels; // = cfg_numBufferedFFTs

    size_t bytes = 0;
    bytes += maxdatabytes;                                                          // packeddata_gpu
    bytes += 2 * (size_t)fftchannels * nFFTs * nbands * sizeof(cuFloatComplex);     // fringe rotated + fftd
    const int acwidth = config->writeAutoCorrs(configindex) ? 2 : 1;
    bytes += (size_t)acwidth * nbands * recordedbandchan * sizeof(cuFloatComplex);  // temp_autocorrelations_gpu
    // (unpacked-sample buffer removed: unpack is fused into fringe rotation)
    bytes += nFFTs * (2 * sizeof(int) + sizeof(bool) + sizeof(float));              // gSampleIndexes+nearestSamples+gValidSamples+gFracSampleError
    bytes += 3 * sizeof(double);                                                    // gInterpolator
    bytes += (size_t)nbands * 4 * sizeof(double);                                   // gLoFreqs + 3 clock/lo offset arrays
    bytes += 2 * (size_t)nFFTs * nbands * sizeof(double);                           // gBigA + gBigBred (precomputed fringe coeffs)
    bytes += (size_t)nfreqs * sizeof(int);                                          // counts_gpu
    bytes += maxframes * sizeof(bool);                                              // valid_frames
    bytes += (size_t)MAX_INDICIES * nfreqs * sizeof(unsigned int);                  // indices

    if (config->getDPhaseCalIntervalMHz(configindex, dsindex) > 0) {
        // mirror the constructor's N_pcal_bins_max / pcal_bin_stride_length
        double bandwidth_hz = 1e6 * config->getFreqTableBandwidth(freqindex);
        double fs_hz = 2 * bandwidth_hz;
        int N_pcal_bins_max = 0;
        for (int b = 0; b < nbands; b++) {
            int localfreqindex = config->getDLocalRecordedFreqIndex(configindex, dsindex, b);
            int offset_hz = config->getDRecordedFreqPCalOffsetsHz(configindex, dsindex, localfreqindex);
            int nbins = (int)(fs_hz / gcd(fs_hz, (double)offset_hz));
            if (nbins > N_pcal_bins_max)
                N_pcal_bins_max = nbins;
        }
        const size_t stride = (size_t)N_pcal_bins_max * 2;
        bytes += 2 * (size_t)nbands * sizeof(int);                                  // pcal_offsets_hz + N_pcal_bins
        bytes += (size_t)nbands * stride *
                 (complexsampled ? sizeof(cuFloatComplex) : sizeof(float));         // pcal_output
    }

    // cuFFT C2C plan work area, worst case (same shape as the constructor's
    // cufftPlanMany: rank-1 length-fftchannels batched over bands*windows)
    size_t fftwork = 0;
    if (cufftEstimate1d(fftchannels, CUFFT_C2C, nbands * (int)nFFTs, &fftwork) == CUFFT_SUCCESS)
        bytes += fftwork;

    return bytes;
}






int GPUMode::process_gpu_tofft(int fftloop, int numBufferedFFTs, int startblock,
                               int numblocks)  //frac sample error is in microseconds
{


    auto begin_time = high_resolution_clock::now();
    calls += 1;
    
    // Sanity checks
    //std::cout << "numBufferedFFTs: " << numBufferedFFTs << std::endl;
    //std::cout << "cfg_numBufferedFFTs: " << cfg_numBufferedFFTs << std::endl;

    assert(numblocks == numBufferedFFTs); // All FFTs are always done in one go
    assert(numBufferedFFTs <= cfg_numBufferedFFTs); // The value calculated in the constructor should be the same as we are told now

//    if (config->getDPhaseCalIntervalMHz(configindex, datastreamindex) != 0) {
//        NOT_SUPPORTED("DPhaseCal");
//    }

    if (fringerotationorder != 1) { // linear only
        NOT_SUPPORTED("fringerotationorder = " + to_string(fringerotationorder));
    }

    if (usedouble) {
        NOT_SUPPORTED("usedouble branch");
    }

    for (auto i = 0; i < numrecordedfreqs; i++) {
        if (recordedfreqlooffsets[i] > 0.0 || recordedfreqlooffsets[i] < 0.0) {
            NOT_SUPPORTED("lo offsets");
        }
    }

    if (usecomplex && usedouble) {
        NOT_SUPPORTED("complex double-sideband data");
    } //else if (usecomplex) {
     //   NOT_SUPPORTED("complex data");
    //}

    if (deltapoloffsets) {
        NOT_SUPPORTED("deltapoloffsets");
    }

    if (config->getDRecordedLowerSideband(configindex, datastreamindex, 0)) {
        NOT_SUPPORTED("lower sideband");
    }

    if (dumpkurtosis) {
        NOT_SUPPORTED("dump_kurtosis branch");
    }

    if (linear2circular) {
        NOT_SUPPORTED("linear to circular polarisation conversion");
    } else if (phasepoloffset) {
        NOT_SUPPORTED("phase polarisation offset");
    }


    if (ev_start == nullptr) {
        checkCuda(cudaEventCreate(&ev_start));
        checkCuda(cudaEventCreate(&ev_copy1));
        checkCuda(cudaEventCreate(&ev_unpack));
        checkCuda(cudaEventCreate(&ev_copy2));
        checkCuda(cudaEventCreate(&ev_pcal));
        checkCuda(cudaEventCreate(&ev_rotate));
        checkCuda(cudaEventCreate(&ev_fft));
        checkCuda(cudaEventCreate(&ev_frac));
    }

    // Select this subint's RING-deep host-staging slot before any host write to
    // / upload from these buffers (invalid-subint path, calculatePre_cpu, the
    // device set_weights block). Keeps subint N+1's host fills off subint N's
    // in-flight async H2D source buffers under tail-overlap. See enableHostRing.
    nearestSamples->setHostSlot(procSlot);
    gFracSampleError->setHostSlot(procSlot);
    gInterpolator->setHostSlot(procSlot);
    gValidFlags->setHostSlot(procSlot);
    gValidSamples->setHostSlot(procSlot);



    // Copy packed data to device, needed to refactor this since we moved packed data allocation to the constructor.
    cudaEventRecord(ev_start, cuStream);

    // Input H2D. When GPUCore has page-locked the procslots receive buffers
    // (see DIFX_GPU_PIN_INPUT), DMA directly from the delivered buffer -
    // no host staging copy. GPUCore's h2dInputDone event guarantees the
    // buffer is not recycled while this async copy is in flight. Otherwise
    // fall back to staging through packeddata_gpu's pinned host half
    // (pageable -> pinned -> device); there the host memcpy dominates this
    // range, the H2D itself is small.
    DIFX_NVTX_PUSH("h2d_stage");
    // The !ownsStream condition guards the direct path's reuse fence:
    // GPUCore's h2dInputDone event is recorded on the SHARED stream, so a
    // mode running on a private stream (standalone use) must stage instead.
    if (inputBuffersPinned && !ownsStream) {
        checkCuda(cudaMemcpyAsync(packeddata_gpu->gpuPtr(), data,
                                  datalengthbytes, cudaMemcpyHostToDevice, cuStream));
    } else {
        memcpy(packeddata_gpu->ptr(), data, datalengthbytes);
        checkCuda(cudaMemcpyAsync(packeddata_gpu->gpuPtr(), packeddata_gpu->ptr(),
                                  datalengthbytes, cudaMemcpyHostToDevice, cuStream));
    }
    DIFX_NVTX_POP();

    // Figure out how many frames in the packed data
    int framestounpack = datalengthbytes / config->getMultiplexedFrameBytes(configindex, datastreamindex);
    //std::cout << "framestounpack: " << framestounpack << std::endl;
    if (datalengthbytes > 1) {  // datalengthbytes <= 1 means an invalid sub int which should be handled....
        //std::cout << "datalengthbytes: " << datalengthbytes << " getMultiplexedFrameBytes: " << config->getMultiplexedFrameBytes(configindex, datastreamindex) << std::endl;
        assert(datalengthbytes % config->getMultiplexedFrameBytes(configindex, datastreamindex) == 0);     // Buffer contains fraction of a frame :(. This shouldn't happen!
        
    } else {

      // set everything to zero and return
        checkCuda(cudaMemsetAsync(fftd_gpu->gpuPtr(), 0.0, fftchannels * cfg_numBufferedFFTs * numrecordedbands * sizeof(cuFloatComplex), cuStream));

        // We return before set_weights() runs, so explicitly invalidate every
        // FFT window and zero its data weight - otherwise stale values from
        // the previous subintegration survive, and GPUCore's baseline-weight
        // accumulation (and the XMAC kernel's validity flags) would count a
        // subint that contains no data. The CPU path sets dataweight = 0 for
        // every FFT of such a subint (Mode::process validity branch).
        for (int i = 0; i < cfg_numBufferedFFTs; i++) {
            dataweight[i] = 0.0;
            if (perbandweights) {
                for (int b = 0; b < numrecordedbands; b++)
                    perbandweights[i][b] = 0.0;
            }
            gValidSamples->ptr()[i] = false;
        }
        gValidSamples->copyToDevice();

        // Zero the DEVICE per-window weights too: GPUCore's baseline-weight
        // reduction (gpu_baseline_weights) reads gDataWeights directly on the
        // device-weights path, so leaving it stale would let an invalid
        // datastream contribute its previous subint's weights to the baseline
        // weight. The host dataweight[] zeroed above only serves the host
        // fallback/WDEBUG; the device buffer needs its own zero.
        checkCuda(cudaMemsetAsync(gDataWeights->gpuPtr(), 0,
                                  cfg_numBufferedFFTs * sizeof(float), cuStream));

        // Host arrays are authoritative for this subint (all zero) - make
        // sure finishWeights() does not overwrite them from the device.
        weightsOnDevice = false;


        // DEBUG ********************************************
        if (datastreamindex == 2) {
            std::cerr
                << "PCAL-GPU-TOFFT-INVALID: "
                << "mode=" << this
                << " ds=" << datastreamindex
                << " procSlot=" << procSlot
                << " datasec=" << datasec
                << " datans=" << datans
                << " datalengthbytes=" << datalengthbytes
                << " offsetseconds=" << offsetseconds
                << std::endl;
        }
        // END DEBUG *****************************************

        // Invalid subint: process_gpu_afterfft() will no-op on this flag. No
        // stream drain here - the memsets are enqueued on cuStream and ordered
        // ahead of everything the pipeline issues next; GPUCore's per-slot
        // validsubint (captured from isSubintValid()) drives the tail skip.
        tofftInvalidSubint = true;
	    return numBufferedFFTs;
    }
    tofftInvalidSubint = false;

    //valid_frames = new GpuMemHelper<bool>(framestounpack, cuStream, false); 

    // Reset pcal accumulation only once at the start of a subintegration.
    // Bug fix MSD 
    const bool pcalEnabled =
        (config->getDPhaseCalIntervalMHz(
            configindex,
            datastreamindex) != 0);
    
    if (pcalEnabled &&
        (!pcalResetInitialized ||
         datasec != pcalResetDataSec ||
         datans != pcalResetDataNs)) {
    
        if (usecomplex) {
            checkCuda(cudaMemsetAsync(
                pcal_output_complex->gpuPtr(),
                0,
                sizeof(cuFloatComplex) *
                    numrecordedbands *
                    pcal_bin_stride_length,
                cuStream));
        } else {
            checkCuda(cudaMemsetAsync(
                pcal_output_real->gpuPtr(),
                0,
                sizeof(float) *
                    numrecordedbands *
                    pcal_bin_stride_length,
                cuStream));
        }
    
        pcalResetDataSec = datasec;
        pcalResetDataNs = datans;
        pcalResetInitialized = true;
    }
 
 
 

    // (The temp_autocorrelations device reset moves to process_gpu_afterfft,
    // immediately before fractionalRotation accumulates into it.)

    // Update the interpolator: gInterpolator is now a managed helper with a
    // RING-deep host buffer, so stage the current interpolator[] into its active
    // host slot before uploading (previously it wrapped interpolator[] directly).
    memcpy(gInterpolator->ptr(), interpolator, 3 * sizeof(double));
    gInterpolator->copyToDevice();
    cudaEventRecord(ev_copy1, cuStream);




    // ==========================================
    // 2. UNPACK
    // ==========================================
    // Host-side per-FFT-window delay-polynomial evaluation (nearestSamples etc.).
    DIFX_NVTX_PUSH("calculatePre_cpu");
    calculatePre_cpu(fftloop, numBufferedFFTs, startblock, numblocks);
    DIFX_NVTX_POP();

    // HDRDEBUG (gated by DIFX_WEIGHT_DEBUG, same as the CPU twin in
    // cpumode.cpp): dump the delivered buffer's VDIF frame-class transitions
    // (real / all-zero / fill-pattern / invalid-bit) once per subint. The
    // delivered tail past the true end of data is unspecified buffer content
    // and can differ from run to run, so each run's log needs its own dump.
    if (weightDebugFrom() >= 0 && datasec >= weightDebugFrom()) {
        static const char* dbg_names[4] = {"zero", "invalidbit", "fill", "real"};
        int dbg_framebytes = config->getMultiplexedFrameBytes(configindex, datastreamindex);
        int prevclass = -1;
        for (int f = 0; f < framestounpack; f++) {
            // Read from the delivered buffer, not packeddata_gpu's host half -
            // the latter is no longer filled when the input buffers are pinned.
            const unsigned int *hdr = (const unsigned int *)((const unsigned char *)data + (size_t)f * dbg_framebytes);
            const unsigned int *pay = hdr + 8;
            int cls;
            if (hdr[0] == 0 && hdr[1] == 0 && hdr[2] == 0 && hdr[3] == 0)
                cls = 0;
            else if ((hdr[0] >> 31) & 0x1)
                cls = 1;
            else if (pay[0] == 0x11223344 || (hdr[2] & 0xFFFFFF) == 0)
                cls = 2;
            else
                cls = 3;
            if (cls != prevclass || f == framestounpack - 1)
                fprintf(stderr, "HDRDEBUG ds=%d datasec=%d datans=%d frame=%d/%d hdr=%08x %08x %08x %08x pay0=%08x class=%s\n",
                        datastreamindex, datasec, datans, f, framestounpack,
                        hdr[0], hdr[1], hdr[2], hdr[3], pay[0], dbg_names[cls]);
            prevclass = cls;
        }
    }

    // Historic drain with no data dependency (the blank-frames kernel is
    // stream-ordered after the input H2D); kept only on the host-weights
    // fallback path so DIFX_GPU_WEIGHTS_HOST=1 reproduces old behaviour.
    if (!useGpuWeights())
        packeddata_gpu->sync();
    // Per-frame validity only (valid_frames), consumed by gpu_set_weights and
    // the fused decode+fringe kernel. Actual sample decoding is fused into the
    // fringe rotation below, so there is no unpacked buffer to fill or to
    // tail-zero any more: the fused kernel decodes a straddling window's
    // out-of-data samples to 0 directly (frame >= framestounpack), exactly what
    // the old unpacked-tail memset produced.
    blankFrames(framestounpack);

    cudaEventRecord(ev_unpack, cuStream);
   
    // ==========================================
    // 3. COPY TO (Weights & Indices)
    // ==========================================
    // counts is filled here (tofft) and consumed by fractionalRotation in
    // process_gpu_afterfft, so it lives in the member savedProcessCounts rather
    // than a process_gpu local. The gpu_set_weights kernel that produces the
    // per-window dataweights/sample-indices/validity stays here (fringe
    // rotation below needs the indices) and also accumulates the single total
    // weight (fused reduction). Only the gTotalWeight D2H moves to
    // process_gpu_afterfft, so its pinned host mirror is written at drain time,
    // not while the next subint's tail may still be reading it.
    int *counts = savedProcessCounts;
    for (int i = 0; i < numrecordedfreqs; i++) counts[i] = 0;

    if (useGpuWeights()) {
        // Device-side weights (default): upload this subint's validity
        // bit-words (host-born, tiny) and compute weight/validity/sample
        // index per window in place on the device - no drain, no
        // valid_frames round trip, no result re-upload. Only the reduced
        // total weight (a single scalar) is brought back each subint for the
        // host AC-weight accumulation in finishWeights(); the full per-window
        // dataweight[] array is D2H'd only under the WDEBUG gate.
        DIFX_NVTX_PUSH("set_weights");
        const int nflagwords = cfg_numBufferedFFTs / FLAGS_PER_INT + 1;
        const int srcwords = (flaglength < nflagwords) ? flaglength : nflagwords;
        memcpy(gValidFlags->ptr(), validflags, srcwords * sizeof(unsigned int));
        if (srcwords < nflagwords)
            memset(gValidFlags->ptr() + srcwords, 0,
                   (nflagwords - srcwords) * sizeof(unsigned int));
        gValidFlags->copyToDevice();

        int framesamples = config->getMultiplexedFramePayloadBytes(configindex, datastreamindex)*8 /
            (config->getDNumBits(configindex, datastreamindex)*config->getDNumRecordedBands(configindex, datastreamindex)*config->getDDecimationFactor(configindex, datastreamindex));
        if (usecomplex)
            framesamples /= 2;
        const bool subintValid =
            (datalengthbytes > 1) && (offsetseconds != INVALID_SUBINT);

        // Zero the per-subint total weight before gpu_set_weights accumulates
        // into it (the fused reduction that replaced gpu_sum_weights). Stream-
        // ordered, so all set_weights threads see 0. The gTotalWeight D2H stays
        // in process_gpu_afterfft (drain-time write of the pinned mirror).
        checkCuda(cudaMemsetAsync(gTotalWeight->gpuPtr(), 0, sizeof(float), cuStream));
        const int tpb = 128;
        gpu_set_weights<<<(numBufferedFFTs + tpb - 1) / tpb, tpb, 0, cuStream>>>(
            nearestSamples->gpuPtr(), valid_frames->gpuPtr(),
            gValidFlags->gpuPtr(),
            gDataWeights->gpuPtr(), gValidSamples->gpuPtr(),
            gSampleIndexes->gpuPtr(),
            numBufferedFFTs, framestounpack, framesamples, fftchannels,
            subintValid, gTotalWeight->gpuPtr());
        // (gTotalWeight D2H moved to process_gpu_afterfft.)
        weightsOnDevice = true;
        memcpy(counts, countsStatic, numrecordedfreqs * sizeof(int));
        DIFX_NVTX_POP();
    } else {
        // Host fallback (DIFX_GPU_WEIGHTS_HOST=1): the original round-trip
        // path - drain so valid_frames is host-visible, loop over windows
        // on the host, upload the results.
        // CRITICAL: nearestSamples->copyToDevice() in calculatePre_cpu() is async.
        // Sync before reading nearestSamples->ptr() to avoid reading stale data.
        nearestSamples->sync();

        DIFX_NVTX_PUSH("set_weights");
        for (int fftwin = 0; fftwin < numBufferedFFTs; fftwin++) {
            set_weights(fftwin, framestounpack, counts, numBufferedFFTs);
        }
        DIFX_NVTX_POP();

        // Indices are now calculated, so we can copy them to the gpu
        indices->copyToDevice();

        // We need to copy the sample indexes to the gpu
        gSampleIndexes->copyToDevice();
        gValidSamples->copyToDevice();
        weightsOnDevice = false;
    }

    cudaEventRecord(ev_copy2, cuStream);

    // (PCAL extraction moves to process_gpu_afterfft - it reads the unpacked
    // samples, still valid there by stream order, and writes a tail-consumed
    // host mirror, so it belongs with the other outputs after the FFT.)

    // ==========================================
    // 5. FRINGE ROTATION
    // ==========================================
    fringeRotation(fftloop, numBufferedFFTs, startblock, numblocks, framestounpack);
    cudaEventRecord(ev_rotate, cuStream);

    // ==========================================
    // 6. FFT
    // ==========================================

    runFFT();
    cudaEventRecord(ev_fft, cuStream);

    // End of process_gpu_tofft: fftd_gpu now holds this subint's
    // spectra. No tail-consumed output buffer has been written, so the next
    // subint's process_gpu_tofft may run on the compute stream while this
    // subint's afterfft outputs drain and its host tail runs.
    t_total += duration_cast<microseconds>(high_resolution_clock::now() - begin_time).count();
    return numBufferedFFTs;
}

int GPUMode::process_gpu_afterfft(int fftloop, int numBufferedFFTs, int startblock,
                                  int numblocks)
{
    int pcal_debug_mpi_rank = -1;
    MPI_Comm_rank(
        MPI_COMM_WORLD,
        &pcal_debug_mpi_rank);

    if (datastreamindex == 2) {
        std::cerr
            << "PCAL-GPU-AFTERFFT-ENTRY: "
            << "mpi_rank=" << pcal_debug_mpi_rank
            << " mode=" << this
            << " ds=" << datastreamindex
            << " procSlot=" << procSlot
            << " tofftInvalidSubint="
            << static_cast<int>(tofftInvalidSubint)
            << " datasec=" << datasec
            << " datans=" << datans
            << " datalengthbytes=" << datalengthbytes
            << " offsetseconds=" << offsetseconds
            << std::endl;
    }

    // No-op for an invalid subint.
    if (tofftInvalidSubint) {
        if (datastreamindex == 2) {
            std::cerr
                << "PCAL-GPU-AFTERFFT-SKIP-INVALID: "
                << "mpi_rank=" << pcal_debug_mpi_rank
                << " mode=" << this
                << " ds=" << datastreamindex
                << " procSlot=" << procSlot
                << " datasec=" << datasec
                << " datans=" << datans
                << " datalengthbytes=" << datalengthbytes
                << " offsetseconds=" << offsetseconds
                << std::endl;
        }

        return numBufferedFFTs;
    }


    auto begin_time = high_resolution_clock::now();
    int *counts = savedProcessCounts;

    // Reset the autocorrelations (device) immediately before fractionalRotation
    // accumulates into them (moved here from process_gpu_tofft). Skipped when
    // the XMAC owns them: nothing accumulates into this buffer then.
    if (!deviceAutocorrs())
        checkCuda(cudaMemsetAsync(temp_autocorrelations_gpu->gpuPtr(), 0,
                                  sizeof(cf32) * numrecordedbands * recordedbandchannels * autocorrwidth, cuStream));

    if (useGpuWeights()) {
        // Bring back the single total-weight scalar (Increment 2b): the sum was
        // accumulated on the device by gpu_set_weights in tofft (fused reduction,
        // replacing gpu_sum_weights); finishWeights' AC per-band accumulation
        // needs only this total. The full per-window array is D2H'd only under
        // the WDEBUG gate. The D2H stays HERE (not tofft) so gTotalWeight's
        // pinned mirror is written at drain time, after which GPUCore's
        // completegpudata reads it in the tail.
        checkCuda(cudaMemcpyAsync(gTotalWeight->ptr(), gTotalWeight->gpuPtr(),
                                  sizeof(float), cudaMemcpyDeviceToHost, cuStream));
        if (weightDebugFrom() >= 0 && datasec >= weightDebugFrom())
            checkCuda(cudaMemcpyAsync(gDataWeights->ptr(), gDataWeights->gpuPtr(),
                                      cfg_numBufferedFFTs * sizeof(float),
                                      cudaMemcpyDeviceToHost, cuStream));
    }

    // ==========================================
    // 4. PCAL EXTRACTION
    // ==========================================
    // The phase-cal bin folding is now fused into the decode+fringe kernel in
    // process_gpu_tofft (accumulated into pcal_output there, after the
    // per-integration reset); here we only bring the completed bins back to the
    // host for the PCal assembly. (Reset -> accumulate -> copy ordering is
    // unchanged: reset is early in tofft, the fused kernel accumulates in tofft,
    // this D2H runs in afterfft after both.)
    if(!(config->getDPhaseCalIntervalMHz(configindex, datastreamindex) == 0)) {
        // point pcal_output_real_gpu_mode
        if (usecomplex) {
            pcal_output_complex->copyToHost();
            pcal_output_complex_gpu_mode = reinterpret_cast<cf32 *>(pcal_output_complex->ptr());
            //if (datastreamindex == 2) {
            //std::cerr
            //    << "PCAL-GPU-HOST-READY: "
            //    << "mode=" << thisgrep -n "validsubint" ./mpifxcorr/src/gpucore.cuh
            //    << " complex_gpu_buffer="
            //    << pcal_output_complex_gpu_mode
            //    << std::endl;
            //}
        } else {
            pcal_output_real->copyToHost();
            pcal_output_real_gpu_mode = pcal_output_real->ptr();

        //    if (datastreamindex == 2) {
        //        std::cerr
        //            << "PCAL-GPU-HOST-READY: "
        //            << "mpi_rank=" << pcal_debug_mpi_rank
        //            << " mode=" << this
        //            << " ds=" << datastreamindex
        //            << " procSlot=" << procSlot
        //            << " datasec=" << datasec
        //            << " datans=" << datans
        //            << " real_gpu_buffer="
        //            << pcal_output_real_gpu_mode
        //            << std::endl;
        //    }            

        }
    }
    cudaEventRecord(ev_pcal, cuStream);



    // ==========================================
    // 7. FRACTIONAL ROTATION
    // ==========================================
    // do the frac sample correct (+ phase shifting if applicable, + fringe rotate if its post-f)
    fractionalRotation(fftloop, numBufferedFFTs, startblock, numblocks, calccrosspolautocorrs, counts);
    cudaEventRecord(ev_frac, cuStream);
    // The per-datastream end drain exists for the host-weights path (and
    // for the event-timer readbacks below). On the device-weights path the
    // stream runs free - GPUCore's end-of-subint drain is the only barrier.
    if (!useGpuWeights()) {
        cudaEventSynchronize(ev_frac);
    } else if (specDebugFrom() >= 0 && datasec >= specDebugFrom()) {
        // SPECDEBUG (debug-only) reads device buffers synchronously and the
        // host halves of the validity/index arrays - afford it a drain and
        // the extra copies.
        gValidSamples->copyToHost();
        gSampleIndexes->copyToHost();
        checkCuda(cudaStreamSynchronize(cuStream));
    }

    // GPU twin of the CPU path's SPECDEBUG tracing (see specDebugFrom above).
    // All device work for this batch is complete at this point, so small
    // synchronous copies of the traced windows are safe.
    if (specDebugFrom() >= 0 && datasec >= specDebugFrom()) {
        for (int sub = 0; sub < numBufferedFFTs; sub++) {
            int idx = fftloop * numBufferedFFTs + startblock + sub;
            if (idx % 128 != 5 || idx >= startblock + numblocks)
                continue;
            if (!gValidSamples->ptr()[sub])
                continue;
            int off = gSampleIndexes->ptr()[sub];
            if (off < 0 || (size_t)off + 516 > unpackedarrays_elem_count)
                continue;
            for (int b = 0; b < numrecordedbands; b++) {
                // The mid-window fields (unp2/rot, offsets 510-515) bracket the
                // single-corrupted-sample-at-offset-512 signature seen in the
                // complex-complex test: spec differences of the form d*(-1)^k.
                // The raw unpacked (unp/unp2) samples are no longer persisted -
                // unpack is fused into the fringe rotation, decoding straight to
                // registers - so those fields are reported as 0; the rotated
                // (rot) and spectral (spec) fields still trace the pipeline.
                float ur[4] = {0}, ui[4] = {0}, u2r[6] = {0}, u2i[6] = {0};
                cuFloatComplex rot[6], spec[4];
                checkCuda(cudaMemcpy(rot, complex_fringe_rotated_gpu->gpuPtr() + (size_t)sub*fftchannels*numrecordedbands + (size_t)b*fftchannels + 510,
                                     6*sizeof(cuFloatComplex), cudaMemcpyDeviceToHost));
                checkCuda(cudaMemcpy(spec, fftd_gpu->gpuPtr() + (size_t)sub*fftchannels*numrecordedbands + (size_t)b*fftchannels,
                                     4*sizeof(cuFloatComplex), cudaMemcpyDeviceToHost));
                fprintf(stderr, "SPECDEBUG ds=%d datasec=%d datans=%d index=%d band=%d nearest=%d unp=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f unp2=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f rot=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f spec=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f\n",
                        datastreamindex, datasec, datans, idx, b, nearestSamples->ptr()[sub],
                        ur[0], ui[0], ur[1], ui[1], ur[2], ui[2], ur[3], ui[3],
                        u2r[0], u2i[0], u2r[1], u2i[1], u2r[2], u2i[2], u2r[3], u2i[3], u2r[4], u2i[4], u2r[5], u2i[5],
                        rot[0].x, rot[0].y, rot[1].x, rot[1].y, rot[2].x, rot[2].y,
                        rot[3].x, rot[3].y, rot[4].x, rot[4].y, rot[5].x, rot[5].y,
                        spec[0].x, spec[0].y, spec[1].x, spec[1].y, spec[2].x, spec[2].y, spec[3].x, spec[3].y);
            }
        }
    }
    
    if (!useGpuWeights()) {
        // Event-timer readbacks require the events to have completed, which
        // only the host-weights path's end drain guarantees; on the device
        // path NVTX/nsys is the profiling tool.
        float ms_copy1 = 0, ms_unpack = 0, ms_copy2 = 0, ms_pcal = 0, ms_rotate = 0, ms_fft = 0, ms_frac = 0;

        cudaEventElapsedTime(&ms_copy1, ev_start, ev_copy1);
        cudaEventElapsedTime(&ms_unpack, ev_copy1, ev_unpack);
        cudaEventElapsedTime(&ms_copy2, ev_unpack, ev_copy2);
        cudaEventElapsedTime(&ms_pcal, ev_copy2, ev_pcal);
        cudaEventElapsedTime(&ms_rotate, ev_pcal, ev_rotate);
        cudaEventElapsedTime(&ms_fft, ev_rotate, ev_fft);
        cudaEventElapsedTime(&ms_frac, ev_fft, ev_frac);

        // Filter out garbage/negative thread-clash timings BEFORE casting to long long
        t_copyto += (long long)((std::max(0.0f, ms_copy1) + std::max(0.0f, ms_copy2)) * 1000.0f);
        t_unpack += (long long)(std::max(0.0f, ms_unpack) * 1000.0f);
        t_pcal   += (long long)(std::max(0.0f, ms_pcal) * 1000.0f);
        t_rotate += (long long)(std::max(0.0f, ms_rotate) * 1000.0f);
        t_fft    += (long long)(std::max(0.0f, ms_fft) * 1000.0f);
        t_fracrotate += (long long)(std::max(0.0f, ms_frac) * 1000.0f);

        // ==========================================
        // 8. POST-PROCESSING (Host Autocorrelations)
        // ==========================================
        // This synchronise is really needed, as we need the GPU processing/memcpys
        // to finish before we read the result data in to the autocorrelation
        // vectors. On the device-weights path both the drain and the copy move
        // to finishWeights(), after GPUCore's single end-of-subint drain.
        auto post_start = high_resolution_clock::now();
        // Nothing to drain or copy when the XMAC owns the autocorrelations.
        if (!deviceAutocorrs()) {
            temp_autocorrelations_gpu->sync();

            // Copy over the autocorrs
            for (int i = 0; i < autocorrwidth; i++) {
                for (int j = 0; j < numrecordedbands; j++) {
                    vectorCopy_cf32(
                            reinterpret_cast<const cf32 *>(&temp_autocorrelations_gpu->ptr()[(i * numrecordedbands * recordedbandchannels) + (j * recordedbandchannels)]),
                            autocorrelations[i][j],
                            recordedbandchannels
                    );
                }
            }
        }

        auto post_stop = high_resolution_clock::now();
        t_postprocess += duration_cast<microseconds>(post_stop - post_start).count();
    }

    // 9. TOTAL PROCESSING TIME
    auto end_time = high_resolution_clock::now();
    t_total += duration_cast<microseconds>(end_time - begin_time).count();

    // TODO: the return value might need to change? Not sure how its used
    //return numfftsprocessed;
    return numBufferedFFTs;
}




bool GPUMode::is_dataweight_valid(int subloopindex) {
    int status;

    if (dataweight[subloopindex] <= 0.0) {
        return false;
    }

    return true;
}

bool GPUMode::is_data_valid(int index, int subloopindex) {
    int status;
    const int validflagwordindex = index / FLAGS_PER_INT;
    const int validflagbitindex = index % FLAGS_PER_INT;
    const unsigned int validflagword = static_cast<unsigned int>(validflags[validflagwordindex]);
    const int validflagbit = ((validflagword >> validflagbitindex) & 0x01);
    const int reason_datalen = (datalengthbytes <= 1);
    const int reason_subint = (offsetseconds == INVALID_SUBINT);
    const int reason_validflag = (validflagbit == 0);

    // Check the data is valid for this index
    if (reason_datalen || reason_subint || reason_validflag) {
   
        return false; //don't process crap data
    }

    // Check that the nearest sample is valid
    if (nearestSamples->ptr()[subloopindex] < -1 ||
        (((nearestSamples->ptr()[subloopindex] + fftchannels) / samplesperblock) * bytesperblocknumerator) / bytesperblockdenominator >
        datalengthbytes) {
 
        return false;
    }

    return true;
}

// Env-gated per-FFT-window weight tracing for CPU-vs-GPU debugging: set
// DIFX_WEIGHT_DEBUG=<datasec> to emit one WDEBUG line per FFT window for all
// subints with datasec >= <datasec>. The format is identical to the one in
// CPUMode::process (cpumode.cpp) so the grepped logs diff directly.
static int weightDebugFrom()
{
    static int from = -2;
    if (from == -2) {
        const char* e = getenv("DIFX_WEIGHT_DEBUG");
        from = (e != NULL) ? atoi(e) : -1;
    }
    return from;
}

void GPUMode::set_weights(int subloopindex, int nframes, int *counts, int numBufferedFFTs) {

    int framesamples = config->getMultiplexedFramePayloadBytes(configindex, datastreamindex)*8/(config->getDNumBits(configindex, datastreamindex)*config->getDNumRecordedBands(configindex, datastreamindex)*config->getDDecimationFactor(configindex, datastreamindex));
    if (usecomplex)
      framesamples /= 2;

    // Not sure if this is still needed. Set to zero for now.
     unpackstartsamples = 0;
    // Clear the perbandweights for this subloopindex

    if(perbandweights)
    {
        for(int b = 0; b < numrecordedbands; ++b)
        {
            perbandweights[subloopindex][b] = 0.0;
        }
    }

    if (!is_data_valid(subloopindex, subloopindex)) {

        // since these data weights can be retreived after this processing ends, reset them to a default of zero in case they don't get updated
        dataweight[subloopindex] = 0.0;

        gValidSamples->ptr()[subloopindex] = false;
        if (weightDebugFrom() >= 0 && datasec >= weightDebugFrom())
            fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=0.000000000 valid=0 reason=rejected\n",
                    datastreamindex, datasec, datans, subloopindex, nearestSamples->ptr()[subloopindex]);
        return;
    }
    //std::cout << "Data is valid for subloopindex " << subloopindex << std::endl;
    gValidSamples->ptr()[subloopindex] = true;

    // Frames at or beyond nframes were not delivered in this subintegration:
    // valid_frames (and the unpacked buffers) hold stale values from the
    // previous subint there, so such frames must count as invalid. This gives
    // an FFT window straddling the end of the delivered data the same
    // fractional weight the CPU path's unpacker returns (its missing samples
    // are zeroed after unpacking, also matching the CPU).
    auto frame_ok = [&](int frame) -> float {
        return (frame >= 0 && frame < nframes && valid_frames->ptr()[frame]) ? 1.0f : 0.0f;
    };

    if (nearestSamples->ptr()[subloopindex] == -1) {
        nearestSamples->ptr()[subloopindex] = 0;
        dataweight[subloopindex] = 1.0;
        cerr << "Why is this happening?" << std::endl;      // I'm not sure what case this branch is for
        abort();
    } else if (nearestSamples->ptr()[subloopindex] < unpackstartsamples ||
               nearestSamples->ptr()[subloopindex] > unpackstartsamples + unpacksamples - fftchannels ||
               subloopindex + 1 == numBufferedFFTs) {
        //std::cout << "Entered standard path subloopindex = " << subloopindex << ", nearestSamples = " << nearestSamples->ptr()[subloopindex] << ", unpackstartsamples = " << unpackstartsamples << ", unpacksamples = " << unpacksamples << ", fftchannels = " << fftchannels << std::endl;
        // Standard path. TODO: above condition can be simplified I think
        int start_frame = nearestSamples->ptr()[subloopindex] / framesamples;
        // The last window of the batch has no successor to consult, so use
        // its own extent to locate its final frame.
        int end_frame;
        if (subloopindex + 1 == numBufferedFFTs)
            end_frame = (nearestSamples->ptr()[subloopindex] + fftchannels - 1) / framesamples;
        else
            end_frame = (nearestSamples->ptr()[subloopindex + 1] - 1) / framesamples;
        if (start_frame == end_frame) {
            // This FFT window does not cross a frame boundary
            dataweight[subloopindex] = frame_ok(start_frame);
        } else if (start_frame + 1 == end_frame) {
            // Crosses frame boundary: set weight proportional to occupancy in each frame
            float frac_first_frame = (float)(end_frame * framesamples - nearestSamples->ptr()[subloopindex]) / (float)fftchannels;
            dataweight[subloopindex] = (frac_first_frame) * frame_ok(start_frame) + (1 - frac_first_frame) * frame_ok(end_frame);
        } else {
            cerr << "FFT window somehow spans more than two frames. This is suspicious to me but maybe allowed?" << std::endl;
            abort();
        };
    }
    // Need to access samplegranularity which was out of scope
    
    
    gSampleIndexes->ptr()[subloopindex] = nearestSamples->ptr()[subloopindex] - unpackstartsamples;

    if (!is_dataweight_valid(subloopindex)) {
        //std::cout << "Data weight is not valid for subloopindex " << subloopindex << std::endl;
        gValidSamples->ptr()[subloopindex] = false;
    } else {
        // Todo: This can definitely be cleaned up and improved
        for (int i = 0; i < numrecordedfreqs; i++) {
            // PWCR numrecordedbands = 2 for the test; but e.g. 8 is very realistical
            // Loop over all recorded bands looking for the matching frequency we should be dealing with
            int count = 0;
            for (int j = 0; j < numrecordedbands; j++) {
                // For upper sideband bands, normally just need to copy the fftd channels.
                // However for complex double upper sideband, the two halves of the frequency space are swapped, so they need to be swapped back

                if (config->matchingRecordedBand(configindex, datastreamindex, i, j)) {
                    indices->ptr()[(i * MAX_INDICIES) + count++] = j;
		            counts[i] = count;
                    // At this point in the code the array fftd_gpu[j] contains complex-valued voltage spectra with the following properties:
                    //
                    // 1. The zero element corresponds to the lowest sky frequency.  That is:
                    //    fftd_gpu[j][0] = Local Oscillator Frequency              (for Upper Sideband)
                    //    fftd_gpu[j][0] = Local Oscillator Frequency - bandwidth  (for Lower Sideband)
                    //    fftd_gpu[j][0] = Local Oscillator Frequency - bandwidth  (for Complex Lower Sideband)
                    //    fftd_gpu[j][0] = Local Oscillator Frequency - bandwidth/2(for Complex Double Upper Sideband)
                    //    fftd_gpu[j][0] = Local Oscillator Frequency - bandwidth/2(for Complex Double Lower Sideband)
                    //
                    // 2. The frequency increases monotonically with index
                    //
                    // 3. The last element of the array corresponds to the highest sky frequency minus the spectral resolution.
                    //    (i.e., the first element beyond the array bound corresponds to the highest sky frequency)

                    if (perbandweights) {
                        weights[0][j] += perbandweights[subloopindex][j];
                    } else {
                        weights[0][j] += dataweight[subloopindex];
                    }
                }
            }

            if (count > 1) {
                //store the weights
                if (perbandweights) {
                    weights[1][indices->ptr()[(i * MAX_INDICIES)]] += perbandweights[subloopindex][indices->ptr()[(i * MAX_INDICIES)]] *
                                                     perbandweights[subloopindex][indices->ptr()[(i * MAX_INDICIES) + 1]];
                    weights[1][indices->ptr()[(i * MAX_INDICIES) + 1]] += perbandweights[subloopindex][indices->ptr()[(i * MAX_INDICIES)]] *
                                                     perbandweights[subloopindex][indices->ptr()[(i * MAX_INDICIES) + 1]];
                } else {
                    weights[1][indices->ptr()[(i * MAX_INDICIES)]] += dataweight[subloopindex];
                    weights[1][indices->ptr()[(i * MAX_INDICIES) + 1]] += dataweight[subloopindex];
                }
            }
        }
    }

    if (weightDebugFrom() >= 0 && datasec >= weightDebugFrom())
        fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=%.9f valid=%d reason=ok\n",
                datastreamindex, datasec, datans, subloopindex, nearestSamples->ptr()[subloopindex],
                dataweight[subloopindex], gValidSamples->ptr()[subloopindex] ? 1 : 0);
}

void GPUMode::calculatePre_cpu(int fftloop, int numBufferedFFTs, int startblock, int numblocks) {
    int startIndex = fftloop * numBufferedFFTs + startblock;
    int endIndex = startblock + numblocks;

    // Always initialize the full batch to avoid carrying stale values
    // from a previous process_gpu() call when this pass is invalid/short.
    for (int subloopindex = 0; subloopindex < numBufferedFFTs; subloopindex++) {
        nearestSamples->ptr()[subloopindex] = -1;
        gFracSampleError->ptr()[subloopindex] = 0.0f;
    }

    // Invalid subints are filtered later by is_data_valid(); keep nearestSamples
    // in a sentinel state so debug output doesn't show misleading large negatives.
    if (offsetseconds == INVALID_SUBINT) {
        gFracSampleError->copyToDevice();
        nearestSamples->copyToDevice();
        return;
    }

    for (int subloopindex = 0; subloopindex < numBufferedFFTs; subloopindex++) {
        int index = startIndex + subloopindex;
        if (index >= endIndex)
            break; // may not have to fully complete last fftloop

        double fftcentre = index + 0.5;
        double averagedelay = interpolator[0] * fftcentre * fftcentre + interpolator[1] * fftcentre + interpolator[2];
        double fftstartmicrosec = index * fftchannels * sampletime; //CHRIS CHECK
        double starttime = (offsetseconds - datasec) * 1000000.0 +
                (double) (static_cast<long long>(offsetns) - static_cast<long long>(datans)) / 1000.0 + fftstartmicrosec -
                           averagedelay;
        nearestSamples->ptr()[subloopindex] = int(starttime / sampletime + 0.5);

        double nearestsampletime = nearestSamples->ptr()[subloopindex] * sampletime;
        gFracSampleError->ptr()[subloopindex] = float(starttime - nearestsampletime);
    }

    // Start copying the fracSampleErrors and nearestSamples to the gpu
    gFracSampleError->copyToDevice();
    nearestSamples->copyToDevice();
}

// Fringe-rotation interpolator hoisting: precompute, once per subint, the
// per-(FFT window, band) phase slope (bigAval) and reduced intercept
// (bigB_reduced) that the rotation kernels used to recompute in every
// (window, band, channel) thread. One thread per (window, band). This is a
// pure hoist of the same FP64 arithmetic (same expressions, same order) out of
// the per-sample inner loop, so the rotation is numerically equivalent
// (identical modulo FMA-contraction between the two kernels; the GPU's final
// visibilities are not bit-reproducible run-to-run anyway, due to downstream
// XMAC atomics). fringeRotation is ~66% of GPU time on GeForce where FP64 runs
// at 1/32 rate, so removing the per-sample recompute is a large win there.
// bigA/bigBred layout: [window * numrecordedbands + band].
__global__ void gpu_precompute_fringe_rotator(
        const double* const interpolator,
        const double* const lofreqs,
        const double* const recordedfreqlooffsets,
        double sampletime,
        int fftloop,
        int startblock,
        size_t fftchannels,
        int numrecordedbands,
        double* const bigA,
        double* const bigBred
    ) {
    const size_t subloopindex = blockIdx.x;   // FFT window within the subint
    const size_t bandindex = threadIdx.x;     // launched with blockDim.x == numrecordedbands
    const size_t index = fftloop * gridDim.x + subloopindex + startblock;

    const double d0 = interpolator[0] * (double) index * (double) index + interpolator[1] * (double) index + interpolator[2];
    const double d1 = interpolator[0] * ((double) index + 0.5) * ((double) index + 0.5) + interpolator[1] * ((double) index + 0.5) + interpolator[2];
    const double d2 = interpolator[0] * ((double) index + 1) * ((double) index + 1) + interpolator[1] * ((double) index + 1) + interpolator[2];
    const double a = d2 - d0;
    const double b = d0 + (d1 - (a * 0.5 + d0)) / 3.0;

    const double bigAval = a * lofreqs[bandindex] / (double) fftchannels - sampletime * 1.e-6 * recordedfreqlooffsets[bandindex];
    const double bigBval = b * lofreqs[bandindex];
    const double bigB_reduced = bigBval - int(bigBval);

    bigA[subloopindex * numrecordedbands + bandindex] = bigAval;
    bigBred[subloopindex * numrecordedbands + bandindex] = bigB_reduced;
}


// Adapted from https://forums.developer.nvidia.com/t/atomic-add-for-complex-numbers/39757
__device__ void atomicAddFloatComplex(cuFloatComplex* a, cuFloatComplex b){
    // transform the addresses of real and imag. parts to double pointers
    auto *x = (float*) a;
    auto *y = x + 1;
    //use atomicAdd for float variables
    atomicAdd(x, cuCrealf(b));
    atomicAdd(y, cuCimagf(b));
}


void GPUMode::fringeRotation(int fftloop, int numBufferedFFTs, int startblock, int numblocks, int framestounpack) {

    // At this point we have
    // * valid_frames on GPU (from blankFrames)
    // * packed data on GPU (fused kernel decodes samples from it on the fly)
    // * Output buffer on GPU ready to go
    // * Sample indexes into the sample stream
    // * BigA and BigB (computed just below)
    // * Which samples are valid - ie that we need to operate on

    //  For LSB data, gLoFreqs needs to have already been corrected for the fact that we will convert to
    // USB in unpacking. This means than loFreq = loFreq - bandwidth.

    // Precompute the per-(window, band) fringe-rotation coefficients once for
    // this subint (one thread per window/band), so the per-sample rotation
    // kernel below no longer recomputes the FP64 interpolator/bigA/bigB math in
    // every (window, band, channel) thread. Enqueued on cuStream ahead of the
    // rotation kernel, so it is complete before the rotation reads gBigA/gBigBred.
    // Format-agnostic (no mark5_stream), so it stays here in the base class.
    gpu_precompute_fringe_rotator<<<numBufferedFFTs, numrecordedbands, 0, cuStream>>>(
            gInterpolator->gpuPtr(),
            gLoFreqs->gpuPtr(),
            grecordedfreqlooffsets->gpuPtr(),
            sampletime,
            fftloop,
            startblock,
            fftchannels,
            numrecordedbands,
            gBigA->gpuPtr(),
            gBigBred->gpuPtr()
    );

    // The fused decode+fringe-rotation kernel needs the mark5_stream/packed data
    // to decode samples on the fly, so its launch lives in Mk5_GPUMode.
    // Launch geometry now lives with the kernel in launch_fused_fringe, which
    // picks the tiled or untiled shape (docs/gpu-fringetile-design.md).
    launchFusedRotate(numBufferedFFTs, fftloop, startblock, numblocks, framestounpack);
}



/** The fractional-sample rotation. With DOAUTOCORR it also accumulates the
 * per-band autocorrelations (an atomicAdd per element) and the cross-pol
 * autocorrelations (a second pass over the bands, re-reading the rotated
 * spectra); without it, it is a pure elementwise rotate and the XMAC computes
 * the autocorrelations as ordinary baselines instead - see
 * docs/gpu-autocorr-design.md. On the A100 those two phases were measured at
 * 24% and 25% of this kernel. */
template<bool DOAUTOCORR>
__global__ void gpu_resultsrotatorMultiply(
        cuFloatComplex* const gpufftd_gpu,
        cuFloatComplex* const autocorrelations,
        const float* const fracSampleError,
        const bool* const validSamples,
        const unsigned int* const indices,
        const double* const recordedfreqclockoffsets,
        const double* const recordedfreqclockoffsetsdelta,
        const double recordedbandwidth,
        int fftloop,
        int startblock,
        int numblocks,
        size_t fftchannels,
        size_t recordedbandchannels,
        size_t numrecordedbands,
        size_t numrecordedfreqs,
	bool calccrosspolautocorrs,
	int* counts_gpu
    ) {

    //for (int ii=0; ii<numrecordedfreqs; ii++){
    //    printf("counts_gpu[%d] = %d\n",ii,counts_gpu[ii]);
    //}

    // numBufferedFFTs(blockIdx.x) * fftchannels(threadIdx.x)

    // blockIdx.x in this case is the subloopindex index [0 .. numBufferedFFTs]
    // blockIdx.y in this case is the fftchannels_grid. The actual fftchannels value is calculated by fftchannels_grid idx * fftchannels_block size + fftchannels idx (blockIdx.y * blockDim.y) + threadIdx.y
    // threadIdx.x in this case is the fftchannels_block index [0 .. fftchannels_block]
    // blockDim.x in this case is the fftchannels_block size
    // gridDim.x in this case is the numBufferedFFTs size
    // gridDim.y in this case is the fftchannels_grid size

    // Check if this subloopindex is valid
    const size_t subloopindex = blockIdx.x;
    if (!validSamples[subloopindex]) {
        // Not valid, so don't do anything
        return;
    }

    // Check if we should bother processing this sample
    size_t index = fftloop * gridDim.x + subloopindex + startblock;
    if (index >= startblock + numblocks) {
        // May not have to fully complete last fftloop, drop out
        return;
    }

    const size_t channelindex = (blockIdx.y * blockDim.x) + threadIdx.x;

    if (channelindex >= recordedbandchannels) {
        return;
    }

    for (size_t bandindex = 0; bandindex < numrecordedbands; bandindex++) {
        /* Creating a fractional sample rotation array
         *  The actual calculation being performed is as follows:
         *  Assume we know the frequency of every FFT output channel, and it is stored in an array of length fftchannels, called channelfreq
         *  then for every frequency subband f (in the range 0 … recordedbandchannels), calculate the slope as:
         *  A = fracsampleerror - recordedfreqclockoffsets[f] + recordedfreqclockoffsetsdelta[f]/2
         *  (for the second polarisation, a is identical except subtracting recordedfreqclockoffsetsdelta[f]/2)
         * then calculate complexrotator[j]  (for j = 0 to fftchannels-1) as:
         * complexrotator[j] = exp( 2 pi i * (A*fftchannels[j]) )
         *
         * So how is fftchannels calculated? For “regular data” it is as follows (for j = 0 to fftchannels-1)
         * fftchannels[j] = recordedbandwidth * j / fftchannels
         * For lower sideband data it is:
         * fftchannels[j] = -recordedbandwidth * j / fftchannels
         * For double sideband data it is:
         * fftchannels[j] = recordedbandwidth * j / fftchannels - recordedbandwidth/2.0
        */

        // todo: Move recorded freq out of the kernel as a dim?
        const size_t dataIndex = (subloopindex * fftchannels * numrecordedbands) + (bandindex * fftchannels) + channelindex;

        // Calculate fracsampleerror - recordedfreqclockoffsets[f] + recordedfreqclockoffsetsdelta[f]/2
        double bigAval = fracSampleError[subloopindex] - recordedfreqclockoffsets[bandindex] + recordedfreqclockoffsetsdelta[bandindex] / 2;

        // Calculate fftchannels[j] = recordedbandwidth * j / fftchannels
        double subFreq = recordedbandwidth * (double) channelindex / (double) recordedbandchannels;

        // Calculate
        double exponent = bigAval * subFreq;
        exponent -= int(exponent);
        cuFloatComplex cr;
        __sincosf(TWO_PI * exponent, &cr.y, &cr.x);
        const cuFloatComplex v = cuCmulf(gpufftd_gpu[dataIndex], cr);
        gpufftd_gpu[dataIndex] = v;

        // Autocorrelation, straight from the register: v * conj(v) is |v|^2,
        // which is real by construction, so only the real component is
        // accumulated (the imaginary one is provably zero and used to cost a
        // second atomic on every element).
        if (DOAUTOCORR) {
            const size_t autocorrIndex = (bandindex * recordedbandchannels) + channelindex;
            atomicAdd(&((float*)&autocorrelations[autocorrIndex])[0],
                      v.x * v.x + v.y * v.y);
        }
    }

    if (DOAUTOCORR) {
        // Cross-polarisation autocorrelations. Both operands now come from the
        // rotated spectra and the second is conjugated in the multiply
        // (cuCmulConjf below) - there is no materialised conjugate array any more.
        // Each value read here was written by THIS thread earlier in the band loop
        // (same window, same channel, a different band), so there is no race.
        for (size_t recordedfreq = 0; recordedfreq < numrecordedfreqs; recordedfreq++) {
            if (calccrosspolautocorrs && counts_gpu[recordedfreq] > 1) {
                const size_t bandA = indices[(recordedfreq * MAX_INDICIES) + 0];
                const size_t bandB = indices[(recordedfreq * MAX_INDICIES) + 1];
                const size_t windowBase = (subloopindex * fftchannels * numrecordedbands) + channelindex;
                const size_t idxA = windowBase + (bandA * fftchannels);
                const size_t idxB = windowBase + (bandB * fftchannels);
                const size_t crossBase = (numrecordedbands * recordedbandchannels) + channelindex;

                atomicAddFloatComplex(&autocorrelations[crossBase + (bandA * recordedbandchannels)],
                                      cuCmulConjf(gpufftd_gpu[idxA], gpufftd_gpu[idxB]));
                atomicAddFloatComplex(&autocorrelations[crossBase + (bandB * recordedbandchannels)],
                                      cuCmulConjf(gpufftd_gpu[idxB], gpufftd_gpu[idxA]));
            }
        }
    }
}

void GPUMode::fractionalRotation(int fftloop, int numBufferedFFTs, int startblock, int numblocks, bool calccrosspolautocorrs, int* counts) {
    // At this point we have
    // * FFT results on GPU
    // * subchannelfreqs
    // * Which samples are valid - ie that we need to operate on

    // numBufferedFFTs(blockIdx.x) * fftchannels(threadIdx.x)
    size_t fftchannels_block = recordedbandchannels;
    size_t fftchannels_grid = 1;

    size_t divisor = cudaMaxThreadsPerBlock;
    if (recordedbandchannels > divisor) {
        fftchannels_block = divisor;
        fftchannels_grid = recordedbandchannels / divisor;

        if (recordedbandchannels % divisor != 0) {
            fftchannels_grid++;
        }
    }
    // The band-pair map and its per-frequency counts only exist for the
    // cross-pol autocorrelation phase, which is gone when the XMAC owns the
    // autocorrelations - so is this upload.
    if (!deviceAutocorrs()) {
        for (int ii=0; ii < numrecordedfreqs; ii++) {
            counts_gpu->ptr()[ii] = counts[ii];
        }
        counts_gpu->copyToDevice();
    }

    const dim3 rotgrid(numBufferedFFTs, fftchannels_grid);
    const dim3 rotblock(fftchannels_block);
    if (deviceAutocorrs()) {
        // Pure elementwise rotation: no atomics, no cross-pol pass.
        gpu_resultsrotatorMultiply<false><<<rotgrid, rotblock, 0, cuStream>>>(
                fftd_gpu->gpuPtr(), nullptr,
                gFracSampleError->gpuPtr(), gValidSamples->gpuPtr(), nullptr,
                grecordedfreqclockoffsets->gpuPtr(),
                grecordedfreqclockoffsetsdelta->gpuPtr(),
                recordedbandwidth, fftloop, startblock, numblocks,
                fftchannels, recordedbandchannels, numrecordedbands,
                numrecordedfreqs, false, nullptr);
    } else {
        gpu_resultsrotatorMultiply<true><<<rotgrid, rotblock, 0, cuStream>>>(
                fftd_gpu->gpuPtr(), temp_autocorrelations_gpu->gpuPtr(),
                gFracSampleError->gpuPtr(), gValidSamples->gpuPtr(),
                indices->gpuPtr(),
                grecordedfreqclockoffsets->gpuPtr(),
                grecordedfreqclockoffsetsdelta->gpuPtr(),
                recordedbandwidth, fftloop, startblock, numblocks,
                fftchannels, recordedbandchannels, numrecordedbands,
                numrecordedfreqs, calccrosspolautocorrs,
                counts_gpu->gpuPtr());
    }

    // We should zero the first channel (lowest frequency) of any LSB bands of fftd_gpu.
    // In future, would be more efficient to do this at visibility.cpp, just prior to writing to disk (if either band in the baseline is LSB)

    // Start copying the autocorrelations back to the host
    temp_autocorrelations_gpu->copyToHost();
    // This delete prevents a memory but it introduces a huge performance overhead
    //delete counts_gpu;
}

void GPUMode::runFFT() {
    checkCufft(cufftExecC2C(fft_plan, complex_fringe_rotated_gpu->gpuPtr(), fftd_gpu->gpuPtr(), CUFFT_FORWARD));
}
