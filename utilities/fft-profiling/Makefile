# fftbench — standalone cuFFT plan/work-area microbenchmark.
# Reproduces mpifxcorr's GPU FFT usage (10 plans on one shared stream,
# default work-area allocation) and compares it against per-plan explicit
# work areas and a single batched plan, to locate the per-station
# cufftExecC2C stream sync found in the 2026-07-21 A100 profile.
# See ../../docs/gpu-profiling.md and ../../docs/gpu-plan.md (work item 0).
#
#   make            # build
#   make run        # build + run (400 subints)
#   make run N=1000 # override subint count
# Override CUDA_HOME / GPU_ARCH for a different install or GPU:
#   make GPU_ARCH=sm_80 CUDA_HOME=/opt/cuda

CUDA_HOME ?= /usr/local/cuda
NVCC      ?= $(CUDA_HOME)/bin/nvcc
GPU_ARCH      ?= sm_75          # RTX 2070 (desktop); A100 = sm_80
N         ?= 400

NVCCFLAGS := -O2 -arch=$(GPU_ARCH) -I$(CUDA_HOME)/include
LDFLAGS   := -L$(CUDA_HOME)/lib64 -lcufft

fftbench: fftbench.cu
	$(NVCC) $(NVCCFLAGS) $< -o $@ $(LDFLAGS)

run: fftbench
	LD_LIBRARY_PATH=$(CUDA_HOME)/lib64:$$LD_LIBRARY_PATH ./fftbench $(N)

clean:
	rm -f fftbench

.PHONY: run clean
