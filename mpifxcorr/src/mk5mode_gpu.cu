#include <mpi.h>
#include "mk5mode_gpu.cuh"
#include "gpumode_kernels.cuh"
#include "gpudecode.cuh"
//#include "mk5.h"
#include "alert.h"
#include <iostream>
#include <bitset>
#include <unistd.h>
#include <chrono>

using namespace std::chrono;

#define NOT_SUPPORTED(x) { std::cerr << "Whoops, we don't support this on the GPU: " << x << std::endl; exit(1); }

Mk5_GPUMode::Mk5_GPUMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling, Configuration::complextype tcomplex, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, int framebytes, int framesamples, Configuration::dataformat format)
  : GPUMode(conf, confindex, dsindex, recordedbandchan, chanstoavg, bpersend, gsamples, nrecordedfreqs, recordedbw, recordedfreqclkoffs, recordedfreqclkoffsdelta, recordedfreqphaseoffs, recordedfreqlooffs, nrecordedbands, nzoombands, nbits, sampling, tcomplex, recordedbandchan*2+4, fbank, linear2circular, fringerotorder, arraystridelen, cacorrs, recordedbw*2)
{

  //printf("In Mk5_GPUMode constructor\n");
  char formatname[64];
  //cout << "Mk5 format parameters: " << nrecordedbands << " bands, " << recordedbw << " MHz bandwidth, " << nbits << " bits/sample, sampling type " << sampling << ", framebytes = " << framebytes << endl;
  fanout = config->genMk5FormatName(format, nrecordedbands, recordedbw, nbits, sampling, framebytes, conf->getDDecimationFactor(confindex, dsindex), config->getDAlignmentSeconds(confindex, dsindex), conf->getDNumMuxThreads(confindex, dsindex), formatname);
  //cout << "Mk5 format: " << formatname << " (fanout = " << fanout << ")" << endl;
  //exit(0);
  invalid = 0;

  if(fanout < 0)
    initok = false;
  else
  {
    // since we allocated the max amount of space needed above, we need to change
    // this to the number actually needed.
    this->framesamples = framesamples;
    if (usecomplex) {
      unpacksamples = recordedbandchan;
      samplestounpack = recordedbandchan;
    } else {
      unpacksamples = recordedbandchan*2;
      samplestounpack = recordedbandchan*2;
    }
    //create the mark5_stream used for unpacking
    mark5stream = new_mark5_stream( new_mark5_stream_unpacker(0), new_mark5_format_generic_from_string(formatname) );
    if(mark5stream == 0)
    {
      cfatal << startl << "Mk5_GPUMode::Mk5_GPUMode : mark5stream is null" << endl;
      initok = false;
    }
    else
    {
      if(conf->isNetwork(dsindex))
        mark5stream->blanker = blanker_none;
      if(mark5stream->samplegranularity > 1)
        samplestounpack += mark5stream->samplegranularity;
      string orig_streamname(mark5stream->streamname);
      sprintf(mark5stream->streamname, "DS%d <%s>", dsindex, orig_streamname.c_str());
      if(framesamples != mark5stream->framesamples)
      {
        cfatal << startl << "Mk5_GPUMode::Mk5_GPUMode : framesamples inconsistent (told " << framesamples << "/ stream says " << mark5stream->framesamples << ") - for stream index " << dsindex << endl;
        initok = false;
      }
      else
      {
        this->framesamples = mark5stream->framesamples;
      }
      /*
      * Currently not using perbandweights - to be added
      */
      /*
      if(format == Configuration::INTERLACEDVDIF)
      {
        invalid = new int[nrecordedbands];
        std::cout << "mk5_gpu ctor: allocating perbandweights with cfg_numBufferedFFTs and nrecordedbands = " << cfg_numBufferedFFTs << " " << nrecordedbands << std::endl;
        perbandweights = new f32*[cfg_numBufferedFFTs];
        for(int i=0;i<cfg_numBufferedFFTs;++i)
        {
          perbandweights[i] = new f32[nrecordedbands];
          for(int b = 0; b < nrecordedbands; ++b)
          {
            perbandweights[i][b] = 0.0;
          }
        }
      }
      */
    }
  }
}

Mk5_GPUMode::~Mk5_GPUMode()
{
  delete_mark5_stream(mark5stream);
  if(invalid)
  {
    delete [] invalid;
  }
}

// Compute per-frame validity (valid_frames) for this subint - one GPU thread
// per frame runs the VDIF blanker. This is all that survives of the old
// unpack_all: the actual sample decode is fused into the fringe rotation
// (launchFusedRotate below), which decodes straight from the packed payload.
void Mk5_GPUMode::blankFrames(int framestounpack)
{
  const int tpb = 256;
  launch_blank_frames(
      dim3((framestounpack + tpb - 1) / tpb), dim3(tpb), cuStream,
      *mark5stream, packeddata_gpu->gpuPtr(), framestounpack, valid_frames->gpuPtr());

  // Host-weights fallback (DIFX_GPU_WEIGHTS_HOST=1): set_weights() reads
  // valid_frames on the host, so land it and drain - the historic behaviour the
  // old unpack_all provided. The device-weights path reads valid_frames->gpuPtr()
  // directly (stream-ordered after this kernel), so no copy/drain is needed there.
  if (!GPUMode::useGpuWeights()) {
    valid_frames->copyToHost();
    valid_frames->sync();
  }
}

// Launch the fused decode + fringe-rotation kernel. Lives here (not in
// GPUMode::fringeRotation) because it needs the mark5_stream and packed data to
// decode samples on the fly. When phase cal is active the same kernel folds the
// raw samples into pcal_output (the DOPCAL template path); otherwise the pcal
// arguments are unused.
void Mk5_GPUMode::launchFusedRotate(int numBufferedFFTs, int fftloop,
                                    int startblock, int numblocks, int framestounpack)
{
  const bool dopcal = (config->getDPhaseCalIntervalMHz(configindex, datastreamindex) != 0);
  void *pcalout = nullptr;
  const int *nbins = nullptr;
  if (dopcal) {
    pcalout = usecomplex ? (void *)pcal_output_complex->gpuPtr()
                         : (void *)pcal_output_real->gpuPtr();
    nbins = N_pcal_bins->gpuPtr();
  }

  launch_fused_fringe(
      cuStream, numBufferedFFTs, numrecordedbands, cudaMaxThreadsPerBlock,
      usecomplex, dopcal,
      complex_fringe_rotated_gpu->gpuPtr(), gSampleIndexes->gpuPtr(), gValidSamples->gpuPtr(),
      gBigA->gpuPtr(), gBigBred->gpuPtr(),
      fftloop, startblock, numblocks, fftchannels,
      *mark5stream, packeddata_gpu->gpuPtr(), valid_frames->gpuPtr(),
      framestounpack,
      pcalout, nbins, datasamples, pcal_bin_stride_length);
}
// vim: shiftwidth=2:softtabstop=2:expandtab
