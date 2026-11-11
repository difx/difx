// fftbench.cu — reproduce the mpifxcorr GPU FFT usage pattern and compare
// three plan/work-area strategies for the per-exec cuFFT stream sync found
// in the 2026-07-21 A100 profile.
//
// Faithful config (benchprof, per docs/gpu-profiling.md):
//   fftchannels n = 128 (nChan 64, REAL sampling -> 2x)
//   batch/station = numrecordedbands(16) * numBufferedFFTs(10) = 160
//   10 "stations" = 10 cuFFT plans, all on ONE shared stream (as GPUCore does)
//   ~400 "subints" = iterations
//   CUFFT_C2C forward, distinct in/out buffers per station (like the Modes)
//
// Approaches:
//   A current : 10 plans, default work-area allocation, shared stream
//   B per-plan: 10 plans, explicit per-plan work area (SetAutoAllocation off)
//   C single  : 1 plan batching all 10 stations (batch = 1600), 1 exec/subint
//
// Key metric: host time spent ISSUING the execs with no intra-loop sync.
//   async exec  -> host races ahead, issue time << GPU time
//   per-exec sync inside cufftExecC2C -> host blocks, issue time ~= GPU time
// The gap between A and B/C is the whole point of the experiment.

#include <cstdio>
#include <cstdlib>
#include <chrono>
#include <cuda_runtime.h>
#include <cufft.h>
#include <nvtx3/nvToolsExt.h>

// Optional per-station "filler": a spin kernel standing in for the real
// per-station non-FFT GPU work (unpack + fringe rotation) queued between FFT
// execs. Without it the stream is nearly idle at each exec, so cuFFT's
// internal sync (if any) returns instantly and hides the stall; with a
// realistic backlog it blocks - exactly the production situation. Run twice:
// no filler, then ~1 ms/station filler (production ~1.4 ms GPU/station).
__global__ void filler(long long cycles, int* sink){
  long long t0=clock64(); int x=0;
  while(clock64()-t0 < cycles) x++;
  if(x==-1) *sink=x;   // never true; defeats dead-code elimination
}

#define CK(x)  do{ cudaError_t e=(x); if(e!=cudaSuccess){ \
  fprintf(stderr,"CUDA %s:%d %s\n",__FILE__,__LINE__,cudaGetErrorString(e)); exit(1);} }while(0)
#define CF(x)  do{ cufftResult r=(x); if(r!=CUFFT_SUCCESS){ \
  fprintf(stderr,"CUFFT %s:%d err %d\n",__FILE__,__LINE__,(int)r); exit(1);} }while(0)

static const int N        = 128;   // fftchannels
static const int BANDS    = 16;    // numrecordedbands
static const int NBUF     = 10;    // numBufferedFFTs
static const int BATCH    = BANDS*NBUF;   // 160 transforms per station-exec
static const int NSTATION = 10;

using clk = std::chrono::steady_clock;
static double ms(clk::time_point a, clk::time_point b){
  return std::chrono::duration<double,std::milli>(b-a).count();
}

struct Result { double host_issue, host_total, gpu; long nexec; };

// run one approach: exec_fn issues one subint's worth of FFTs; nexec_per_subint
// counts the execs issued (for the per-exec host cost).
template<class F>
static Result run(const char* name, cudaStream_t s, int nsubint,
                  int nexec_per_subint, F exec_subint){
  cudaEvent_t e0,e1; CK(cudaEventCreate(&e0)); CK(cudaEventCreate(&e1));
  // warmup
  exec_subint(); CK(cudaStreamSynchronize(s));

  nvtxRangePushA(name);
  CK(cudaEventRecord(e0,s));
  auto h0=clk::now();
  for(int i=0;i<nsubint;i++) exec_subint();
  auto h1=clk::now();                 // all execs issued, no sync yet
  CK(cudaEventRecord(e1,s));
  CK(cudaStreamSynchronize(s));
  auto h2=clk::now();
  nvtxRangePop();

  float g=0; CK(cudaEventElapsedTime(&g,e0,e1));
  CK(cudaEventDestroy(e0)); CK(cudaEventDestroy(e1));
  Result r{ ms(h0,h1), ms(h0,h2), (double)g, (long)nsubint*nexec_per_subint };
  return r;
}

static void report(const char* tag, const Result& r){
  double per_exec_us = 1000.0*r.host_issue/r.nexec;
  double ratio = r.host_issue / r.gpu;
  // host-issue tracking gpu under a backlog is launch back-pressure (GPU
  // saturated) - the WANTED state, not a sync. The authoritative sync test
  // is the single-op probe printed above; here just report the numbers.
  printf("  %-18s host-issue %8.1f ms | gpu %8.1f ms | host/gpu %5.2f | "
         "%6ld execs @ %6.1f us host each\n",
         tag, r.host_issue, r.gpu, ratio, r.nexec, per_exec_us);
}

int main(int argc,char**argv){
  int nsubint = (argc>1)? atoi(argv[1]) : 400;
  printf("fftbench: n=%d batch/station=%d stations=%d subints=%d  (C2C fwd)\n",
         N,BATCH,NSTATION,nsubint);

  cudaStream_t s; CK(cudaStreamCreate(&s));
  int n[1]={N};

  // ---- buffers: distinct in/out per station (like each GPUMode) ----------
  cufftComplex *in[NSTATION], *out[NSTATION];
  for(int k=0;k<NSTATION;k++){
    CK(cudaMalloc(&in[k],  sizeof(cufftComplex)*(size_t)BATCH*N));
    CK(cudaMalloc(&out[k], sizeof(cufftComplex)*(size_t)BATCH*N));
    CK(cudaMemsetAsync(in[k],0,sizeof(cufftComplex)*(size_t)BATCH*N,s));
  }
  // one big buffer pair for approach C
  cufftComplex *inC,*outC;
  CK(cudaMalloc(&inC,  sizeof(cufftComplex)*(size_t)NSTATION*BATCH*N));
  CK(cudaMalloc(&outC, sizeof(cufftComplex)*(size_t)NSTATION*BATCH*N));
  CK(cudaMemsetAsync(inC,0,sizeof(cufftComplex)*(size_t)NSTATION*BATCH*N,s));
  int* sink; CK(cudaMalloc(&sink,sizeof(int)));
  CK(cudaStreamSynchronize(s));

  // Calibrate the filler to ~1 ms (production ~1.4 ms GPU/station).
  auto timefill=[&](long long c){
    cudaEvent_t a,b; CK(cudaEventCreate(&a)); CK(cudaEventCreate(&b));
    CK(cudaEventRecord(a,s)); filler<<<80,128,0,s>>>(c,sink); CK(cudaEventRecord(b,s));
    CK(cudaStreamSynchronize(s)); float g; CK(cudaEventElapsedTime(&g,a,b));
    CK(cudaEventDestroy(a)); CK(cudaEventDestroy(b)); return (double)g;
  };
  double probe_ms=timefill(1LL<<20);
  long long FC=(long long)((1LL<<20)*(1.0/probe_ms));   // cycles for ~1 ms
  printf("filler: %.3f ms per station-kernel (calibrated)\n", timefill(FC));

  // ---- decisive probe: does cufftExecC2C block on prior stream work? -----
  // Queue ONE long (~50 ms) kernel, then time the host return of a single
  // cufftExecC2C vs a single plain kernel launch. One op each => no launch-
  // queue back-pressure, so a large host-return time can only be an internal
  // sync inside cufftExecC2C. (This is the clean test the per-approach passes
  // below cannot give, because 4000 x ~1 ms fillers also overflow the async
  // launch queue and block the launches themselves.)
  {
    long long big=FC*50;                       // ~50 ms of GPU work
    cufftHandle pp; CF(cufftPlanMany(&pp,1,n,NULL,1,N,NULL,1,N,CUFFT_C2C,BATCH));
    CF(cufftSetStream(pp,s));
    filler<<<80,128,0,s>>>(big,sink);
    auto t0=clk::now(); CF(cufftExecC2C(pp,in[0],out[0],CUFFT_FORWARD)); auto t1=clk::now();
    CK(cudaStreamSynchronize(s));
    filler<<<80,128,0,s>>>(big,sink);
    auto k0=clk::now(); filler<<<1,1,0,s>>>(1,sink); auto k1=clk::now();
    CK(cudaStreamSynchronize(s));
    printf("probe (after ~50 ms backlog): cufftExecC2C host-return %.2f ms | "
           "plain launch %.2f ms  => cuFFT %s\n",
           ms(t0,t1), ms(k0,k1), ms(t0,t1)>5.0 ? "SYNCS internally":"is async");
    CF(cufftDestroy(pp));
  }

  // Two passes per approach: no backlog, then a realistic per-station backlog.
  const long long fcs[2]={0, FC};
  const char* ftag[2]={"", " +filler"};

  // ===== A: 10 plans, default work area, shared stream ====================
  {
    cufftHandle p[NSTATION];
    for(int k=0;k<NSTATION;k++){
      CF(cufftPlanMany(&p[k],1,n,NULL,1,N,NULL,1,N,CUFFT_C2C,BATCH));
      CF(cufftSetStream(p[k],s));
    }
    for(int f=0;f<2;f++){ long long fc=fcs[f];
      Result r=run("A",s,nsubint,NSTATION,[&]{
        for(int k=0;k<NSTATION;k++){
          if(fc) filler<<<80,128,0,s>>>(fc,sink);
          CF(cufftExecC2C(p[k],in[k],out[k],CUFFT_FORWARD));
        }
      });
      char t[32]; snprintf(t,sizeof t,"A default%s",ftag[f]); report(t,r);
    }
    for(int k=0;k<NSTATION;k++) CF(cufftDestroy(p[k]));
  }

  // ===== B: 10 plans, explicit per-plan work area =========================
  {
    cufftHandle p[NSTATION]; void* wa[NSTATION];
    for(int k=0;k<NSTATION;k++){
      CF(cufftCreate(&p[k]));
      CF(cufftSetAutoAllocation(p[k],0));
      size_t ws=0;
      CF(cufftMakePlanMany(p[k],1,n,NULL,1,N,NULL,1,N,CUFFT_C2C,BATCH,&ws));
      CK(cudaMalloc(&wa[k],ws?ws:1));
      CF(cufftSetWorkArea(p[k],wa[k]));
      CF(cufftSetStream(p[k],s));
      if(k==0) printf("  (per-plan work area = %zu bytes)\n",ws);
    }
    for(int f=0;f<2;f++){ long long fc=fcs[f];
      Result r=run("B",s,nsubint,NSTATION,[&]{
        for(int k=0;k<NSTATION;k++){
          if(fc) filler<<<80,128,0,s>>>(fc,sink);
          CF(cufftExecC2C(p[k],in[k],out[k],CUFFT_FORWARD));
        }
      });
      char t[32]; snprintf(t,sizeof t,"B per-plan%s",ftag[f]); report(t,r);
    }
    for(int k=0;k<NSTATION;k++){ CF(cufftDestroy(p[k])); CK(cudaFree(wa[k])); }
  }

  // ===== C: single plan batching all stations =============================
  {
    cufftHandle p;
    CF(cufftPlanMany(&p,1,n,NULL,1,N,NULL,1,N,CUFFT_C2C,NSTATION*BATCH));
    CF(cufftSetStream(p,s));
    for(int f=0;f<2;f++){ long long fc=fcs[f];
      Result r=run("C",s,nsubint,1,[&]{
        if(fc) for(int k=0;k<NSTATION;k++) filler<<<80,128,0,s>>>(fc,sink);
        CF(cufftExecC2C(p,inC,outC,CUFFT_FORWARD));
      });
      char t[32]; snprintf(t,sizeof t,"C single%s",ftag[f]); report(t,r);
    }
    CF(cufftDestroy(p));
  }

  printf("\nInterpretation:\n"
    "* The PROBE is the authoritative sync test (one op, no queue back-pressure).\n"
    "  'is async' => cufftExecC2C does NOT sync the stream on this cuFFT version;\n"
    "  the tail-overlap pipeline is not defeated by the FFT here.\n"
    "* No-filler rows isolate FFT efficiency + launch overhead: compare gpu ms\n"
    "  (fewer, larger batched transforms in C are far cheaper than many tiny ones)\n"
    "  and per-exec host us (launch overhead).\n"
    "* +filler rows add a realistic per-station backlog; host-issue ~= gpu there is\n"
    "  launch-queue back-pressure (GPU saturated) - expected, not a sync.\n"
    "* If the A100 cluster cuFFT differs, its probe may say SYNCS - run there too\n"
    "  (make GPU_ARCH=sm_80) to settle whether production's per-exec sync is cuFFT.\n");
  return 0;
}
