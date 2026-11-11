#ifndef GPUMODE_KERNELS_H
#define GPUMODE_KERNELS_H

#include <iostream>
#include <cuComplex.h>
#include <cufft.h>

/**
 * a * conj(b), for the cross-multiplies that make up the XMAC and the
 * cross-polarisation autocorrelations.
 *
 * WHY THIS EXISTS: the GPU pipeline used to materialise a whole second array of
 * conjugated spectra (conj_fftd_gpu) purely so consumers could read a
 * pre-conjugated operand. That cost an 8-byte global write per spectral point -
 * ~40% of the fractional-rotation kernel's DRAM traffic - plus a full duplicate
 * of the spectra in VRAM, to save nothing: conjugating in the multiply has the
 * SAME instruction count as a plain complex multiply.
 *
 *     (a+bi)(c+di) = (ac - bd) + (ad + bc)i
 *     (a+bi)(c-di) = (ac + bd) + (bc - ad)i
 *
 * i.e. four multiplies and two adds either way - only the signs differ, and
 * they fold into the FMA sign bits. Writing it out rather than composing
 * cuCmulf(a, cuConjf(b)) keeps that explicit.
 */
__host__ __device__ static __inline__ cuFloatComplex cuCmulConjf(cuFloatComplex a, cuFloatComplex b)
{
    return make_cuFloatComplex(a.x * b.x + a.y * b.y,
                               a.y * b.x - a.x * b.y);
}

// ---------------------------------------------------------------------------
// Lightweight NVTX instrumentation.
//
// Marks host-side phases (staging copies, the per-FFT-window CPU loops, XMAC
// launch, host result accumulation, transfer waits) so they show up as named
// ranges on an Nsight Systems timeline captured with --trace=nvtx.  NVTX v3 is
// header-only, so this adds no link dependency.  Calls are near-zero cost when
// no profiler is attached; define DIFX_DISABLE_NVTX to compile them out
// entirely.
//
//   DIFX_NVTX_RANGE("name")  - scoped range: active until end of the enclosing block
//   DIFX_NVTX_PUSH("name") / DIFX_NVTX_POP()  - explicit range around a statement span
// ---------------------------------------------------------------------------
#ifndef DIFX_DISABLE_NVTX
#include <nvtx3/nvToolsExt.h>
struct DifxNvtxRange {
    DifxNvtxRange(const char *name) { nvtxRangePushA(name); }
    ~DifxNvtxRange() { nvtxRangePop(); }
};
#define DIFX_NVTX_CONCAT2(a, b) a##b
#define DIFX_NVTX_CONCAT(a, b) DIFX_NVTX_CONCAT2(a, b)
#define DIFX_NVTX_RANGE(name) DifxNvtxRange DIFX_NVTX_CONCAT(_difx_nvtx_range_, __COUNTER__)(name)
#define DIFX_NVTX_PUSH(name) nvtxRangePushA(name)
#define DIFX_NVTX_POP() nvtxRangePop()
#else
#define DIFX_NVTX_RANGE(name) do {} while (0)
#define DIFX_NVTX_PUSH(name) do {} while (0)
#define DIFX_NVTX_POP() do {} while (0)
#endif

#define NOT_SUPPORTED(x) { std::cerr << "Whoops, we don't support this on the GPU: " << x << std::endl; exit(1); }

#define checkCuda(err) __checkCuda(err, (char *)__FILE__, __LINE__)
inline cudaError_t __checkCuda(cudaError_t err, char *file, int line) {
  if (err != cudaSuccess) {
    std::cerr << "Error in calling CUDA operation in " << file << " at line " << line << std::endl;
    std::cerr << "Error was " << cudaGetErrorString(err) << std::endl;
    exit(1);
  }
  return err;
}

#define checkCufft(err) __checkCufft(err, (char *)__FILE__, __LINE__)
inline cufftResult_t __checkCufft(const cufftResult_t err, const char *const file, const int line) {
  if (err != CUFFT_SUCCESS) {
    std::cerr << "Error calling a cuFFT operation in " << file << " at line " << line << std::endl;
    // TODO: should we convert err to a string? (it is an enum documented in
    // the cuFFT documentation - there doesn't seem to be an official errorcode
    // -> string conversion routine, but there is _cudaGetErrorEnum...)
    std::cerr << "Error was " << (int)err << std::endl;
    exit(1);
  }
  return err;
}

template <class T>
class GpuMemHelper {
public:
    //GpuMemHelper(size_t nElems, cudaStream_t stream) : managed(true), nBytes(sizeof(T) * nElems), cuStream(stream) {
    //    cpuData = new T[nElems];
    //    checkCuda(cudaHostRegister(cpuData, nBytes, cudaHostRegisterPortable));
    //    checkCuda(cudaMallocAsync(&gpuData, nBytes, cuStream));
    //}
    GpuMemHelper(size_t nElems, cudaStream_t stream) : managed(true), nBytes(sizeof(T) * nElems), cuStream(stream) {
        checkCuda(cudaMallocHost(&cpuData, nBytes));
        checkCuda(cudaMallocAsync(&gpuData, nBytes, cuStream));
    }


    GpuMemHelper(T* hostPtr, size_t nElems, cudaStream_t stream) : managed(false), cpuData(hostPtr), nBytes(sizeof(T) * nElems), cuStream(stream) {
        checkCuda(cudaHostRegister(cpuData, nBytes, cudaHostRegisterPortable));
        checkCuda(cudaMallocAsync(&gpuData, nBytes, cuStream));
    }

//    GpuMemHelper(size_t nElems, cudaStream_t stream, bool gpuOnly) : managed(false), cpuData(nullptr), nBytes(sizeof(T) * nElems), cuStream(stream) {
//        checkCuda(cudaMallocAsync(&gpuData, nBytes, cuStream));

//        if (!gpuOnly) {
//            cpuData = new T[nElems];
//            checkCuda(cudaHostRegister(cpuData, nBytes, cudaHostRegisterPortable));
//        }
    GpuMemHelper(size_t nElems, cudaStream_t stream, bool gpuOnly) : managed(true), cpuData(nullptr), nBytes(sizeof(T) * nElems), cuStream(stream) {
        checkCuda(cudaMallocAsync(&gpuData, nBytes, cuStream));
        
        if (!gpuOnly) {
            checkCuda(cudaMallocHost(&cpuData, nBytes));
        }
    }


    

    ~GpuMemHelper() {
//      if (cpuData) {
//          checkCuda(cudaHostUnregister(cpuData));
//
//          if (managed) {
//              delete[] cpuData;
//              cpuData = nullptr;
//          }
//      }
        if (cpuRing) {
            // Host ring owns all its (pinned, managed) buffers; cpuData just
            // points at the active one, so free the ring, not cpuData separately.
            for (int i = 0; i < nHostSlots; i++)
                if (cpuRing[i]) checkCuda(cudaFreeHost(cpuRing[i]));
            delete[] cpuRing;
            cpuRing = nullptr;
            cpuData = nullptr;
        } else if (cpuData) {
            if (managed) {
                checkCuda(cudaFreeHost(cpuData));
                cpuData = nullptr;
            } else {
                checkCuda(cudaHostUnregister(cpuData));
            }
        }

        if (gpuData) {
            checkCuda(cudaFreeAsync(gpuData, cuStream));
            gpuData = nullptr;
        }
    }

    // Enable RING-deep HOST staging: allocate nSlots pinned host buffers so the
    // host can fill slot (subint+1) while an async H2D from slot (subint) is
    // still queued behind the GPU's compute backlog (the tail-overlap pipeline
    // runs the host ~1 subint ahead). The DEVICE buffer stays single - device
    // reads are stream-ordered, so only the host source needs duplicating. Call
    // once after construction on a MANAGED helper (cpuData from cudaMallocHost).
    void enableHostRing(int nSlots) {
        if (nSlots <= 1 || cpuRing)
            return;
        nHostSlots = nSlots;
        cpuRing = new T*[nHostSlots];
        cpuRing[0] = cpuData;   // reuse the ctor's pinned buffer as slot 0
        for (int i = 1; i < nHostSlots; i++)
            checkCuda(cudaMallocHost(&cpuRing[i], nBytes));
    }

    // Select which host-ring slot ptr()/copyToDevice()/copyToHost() act on.
    // No-op when the ring is not enabled.
    inline void setHostSlot(int i) {
        if (cpuRing)
            cpuData = cpuRing[i % nHostSlots];
    }

    inline GpuMemHelper* copyToDevice() {
        checkCpuData();

        checkCuda(cudaMemcpyAsync(gpuData, cpuData, nBytes, cudaMemcpyHostToDevice, cuStream));

        return this;
    }

    inline GpuMemHelper* copyToHost() {
        checkCpuData();

        checkCuda(cudaMemcpyAsync(cpuData, gpuData, nBytes, cudaMemcpyDeviceToHost, cuStream));

        return this;
    }

    inline GpuMemHelper* sync() {
        checkCuda(cudaStreamSynchronize(cuStream));

        return this;
    }

    inline T* ptr() { return cpuData; }
    inline T* gpuPtr() { return gpuData; }

    inline size_t size() { return nBytes; }

private:
    T* cpuData;
    T* gpuData;
    cudaStream_t cuStream;
    bool managed;
    size_t nBytes;
    T** cpuRing = nullptr;   // RING-deep host staging (see enableHostRing); null = single-buffered
    int nHostSlots = 1;

    void checkCpuData() {
        if (!cpuData) {
            cout << "Attempt to use null cpuData in GpuMemHelper" << endl;
            exit(1);
        }
    }
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
