#include <mutex>
#include <cstdlib>
#include <cstdio>
#include "cpumode.h"
#include "alert.h"

static int debug_dataweight_cpu_prints = 0;
static int debug_dataweight_cpu_prints_invalid = 0;
static int debug_dataweight_cpu_calls = 0;

// Env-gated per-FFT-window weight tracing for CPU-vs-GPU debugging: set
// DIFX_WEIGHT_DEBUG=<datasec> to emit one WDEBUG line per FFT window for all
// subints with datasec >= <datasec>. The format is identical to the one in
// GPUMode::set_weights (gpumode.cu) so the grepped logs diff directly.
static int weightDebugFrom()
{
  static int from = -2;
  if(from == -2)
  {
    const char* e = getenv("DIFX_WEIGHT_DEBUG");
    from = (e != NULL) ? atoi(e) : -1;
  }
  return from;
}

// Env-gated per-window spectral tracing for CPU-vs-GPU debugging: set
// DIFX_SPEC_DEBUG=<datasec> to emit, for every 128th FFT window of subints
// with datasec >= <datasec>, one SPECDEBUG line per band holding the first 4
// unpacked samples and the first 4 fully-processed (post fringe rotation,
// FFT and fractional-sample correction) spectral channels. The format is
// identical to the one in GPUMode::process_gpu (gpumode.cu), so sorted
// grepped logs diff directly and any divergence localizes to a stage.
static int specDebugFrom()
{
  static int from = -2;
  if(from == -2)
  {
    const char* e = getenv("DIFX_SPEC_DEBUG");
    from = (e != NULL) ? atoi(e) : -1;
  }
  return from;
}

// HDRDEBUG (gated by DIFX_WEIGHT_DEBUG, same as the GPU twin in gpumode.cu):
// dump the delivered buffer's VDIF frame-class transitions (real / all-zero /
// fill-pattern / invalid-bit) once per subint. The delivered tail past the
// true end of data is unspecified buffer content and can differ from run to
// run, so each run's log needs its own dump. Only meaningful for VDIF-family
// formats (assumes a 32-byte non-legacy header).
static void dumpFrameClasses(const unsigned char* buf, int databytes, int framebytes,
                             int dsindex, int datasec, int datans)
{
  static const char* names[4] = {"zero", "invalidbit", "fill", "real"};
  int prevclass = -1;

  if(databytes <= 1 || framebytes <= 0)
    return;
  int nframes = databytes / framebytes;
  for(int f = 0; f < nframes; f++)
  {
    const unsigned int* hdr = (const unsigned int*)(buf + (size_t)f*framebytes);
    const unsigned int* pay = hdr + 8;
    int cls;
    if(hdr[0] == 0 && hdr[1] == 0 && hdr[2] == 0 && hdr[3] == 0)
      cls = 0;
    else if((hdr[0] >> 31) & 0x1)
      cls = 1;
    else if(pay[0] == 0x11223344 || (hdr[2] & 0xFFFFFF) == 0)
      cls = 2;
    else
      cls = 3;
    if(cls != prevclass || f == nframes-1)
      fprintf(stderr, "HDRDEBUG ds=%d datasec=%d datans=%d frame=%d/%d hdr=%08x %08x %08x %08x pay0=%08x class=%s\n",
              dsindex, datasec, datans, f, nframes, hdr[0], hdr[1], hdr[2], hdr[3], pay[0], names[cls]);
    prevclass = cls;
  }
}

CPUMode::CPUMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling, Configuration::complextype tcomplex, int unpacksamp, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, double bclock):
    Mode(conf, confindex, dsindex, recordedbandchan, chanstoavg, bpersend, gsamples, nrecordedfreqs, recordedbw, recordedfreqclkoffs, recordedfreqclkoffsdelta, recordedfreqphaseoffs, recordedfreqlooffs, nrecordedbands, nzoombands, nbits, sampling, tcomplex, unpacksamp, fbank, linear2circular, fringerotorder, arraystridelen, cacorrs, bclock)
{
  int status;
  switch(fringerotationorder) {
    case 2:
      /* fall through */
    case 1:

      if (isfft) {
        status = vectorInitFFTC_cf32(&pFFTSpecC, order, flag, hint, &fftbuffersize, &fftbuffer);
        if (status != vecNoErr)
          csevere << startl << "Error in FFT initialisation!!!" << status << endl;
      } else {
        status = vectorInitDFTC_cf32(&pDFTSpecC, fftchannels, flag, hint, &fftbuffersize, &fftbuffer);
        if(status != vecNoErr)
          csevere << startl << "Error in DFT initialisation!!!" << status << endl;
      }
      break;
  }
}

CPUMode::~CPUMode() {
  for(int i=0;i<numrecordedbands;i++)
    vectorFree(unpackedarrays[i]);
  delete [] unpackedarrays;


  switch(fringerotationorder) {
    case 2:
      /* fall through */
    case 1:
      if(isfft) {
        vectorFreeFFTC_cf32(pFFTSpecC);
      } else {
        vectorFreeDFTC_cf32(pDFTSpecC);
      }
      break;
  }
}

void CPUMode::process(int index, int subloopindex)  //frac sample error is in microseconds 
{
  //static int nth_call = 0;
  //++nth_call;
  //std::cout << "call " << nth_call << "to Mode::process" << std::endl;
  ++debug_dataweight_cpu_calls;
  double phaserotation, averagedelay, nearestsampletime, starttime, lofreq, walltimesecs, fracwalltime, fftcentre, d0, d1, d2, fraclooffset;
  f32 phaserotationfloat, fracsampleerror;
  int status, count, nearestsample, integerdelay, RcpIndex, LcpIndex, intwalltime;
  cf32* fftptr;
  f32* currentstepchannelfreqs;
  f32* currentsubchannelfreqs;
  int indices[10];
  bool looff, isfraclooffset;
  //cout << "For Mode of datastream " << datastreamindex << ", index " << index << ", validflags is " << validflags[index/FLAGS_PER_INT] << ", after shift you get " << ((validflags[index/FLAGS_PER_INT] >> (index%FLAGS_PER_INT)) & 0x01) << endl;

  if(weightDebugFrom() >= 0 && datasec >= weightDebugFrom() && index == 0)
    dumpFrameClasses((const unsigned char*)data, datalengthbytes,
                     config->getMultiplexedFrameBytes(configindex, datastreamindex),
                     datastreamindex, datasec, datans);

  //since these data weights can be retreived after this processing ends, reset them to a default of zero in case they don't get updated
  dataweight[subloopindex] = 0.0;
  if(perbandweights)
  {
    for(int b = 0; b < numrecordedbands; ++b)
    {
      perbandweights[subloopindex][b] = 0.0;
    }
  }
  
  const int validflagwordindex = index / FLAGS_PER_INT;
  const int validflagbitindex = index % FLAGS_PER_INT;
  const unsigned int validflagword = static_cast<unsigned int>(validflags[validflagwordindex]);
  const int validflag = ((validflagword >> validflagbitindex) & 0x01);

  const int reason_datalen = (datalengthbytes <= 1);
  const int reason_subint = (offsetseconds == INVALID_SUBINT);
  const int reason_validflag = (validflag == 0);

  if(reason_datalen || reason_subint || reason_validflag)
  {
    //if (debug_dataweight_cpu_prints_invalid < 256 && datastreamindex == 4) {
    //  if (datans == 36500000) {
    //      printf("DEBUG_DATAWEIGHT_CPU_INVALID call=%d ds=%d idx=%d sub=%d datalen=%d offsetsec=%d invalid_subint=%d datasec=%d datans=%d validflag=%d flagword_idx=%d flagbit_idx=%d flagword=0x%08x reason_len=%d reason_subint=%d reason_flag=%d\n",
    //         debug_dataweight_cpu_calls,
    //         datastreamindex,
    //         index,
    //         subloopindex,
    //         datalengthbytes,
    //         offsetseconds,
    //         (int)(offsetseconds == INVALID_SUBINT),
    //         datasec,
    //         datans,
    //         validflag,
    //         validflagwordindex,
    //         validflagbitindex,
    //         validflagword,
    //         reason_datalen,
    //         reason_subint,
    //         reason_validflag
    //        );
    //    debug_dataweight_cpu_prints_invalid++;
    //  }
    for(int i=0;i<numrecordedbands;i++)
    {
      status = vectorZero_cf32(fftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
      status = vectorZero_cf32(conjfftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
    }
    //cout << "Mode for DS " << datastreamindex << " is bailing out of index " << index << "/" << subloopindex << " which is scan " << currentscan << ", sec " << offsetseconds << ", ns " << offsetns << " because datalengthbytes is " << datalengthbytes << " and validflag was " << ((validflags[index/FLAGS_PER_INT] >> (index%FLAGS_PER_INT)) & 0x01) << endl;
    if(weightDebugFrom() >= 0 && datasec >= weightDebugFrom())
      fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=0.000000000 valid=0 reason=rejected\n",
              datastreamindex, datasec, datans, index, -999999);
    return; //don't process crap data
  }

  fftcentre = index+0.5;
  averagedelay = interpolator[0]*fftcentre*fftcentre + interpolator[1]*fftcentre + interpolator[2];
  fftstartmicrosec = index*fftchannels*sampletime; //CHRIS CHECK
  starttime = (offsetseconds-datasec)*1000000.0 + (static_cast<long long>(offsetns) - static_cast<long long>(datans))/1000.0 + fftstartmicrosec - averagedelay;
  nearestsample = int(starttime/sampletime + 0.5);
  walltimesecs = model->getScanStartSec(currentscan, config->getStartMJD(), config->getStartSeconds()) + offsetseconds + offsetns/1.0e9 + fftstartmicrosec/1.0e6;
  intwalltime = static_cast<int>(walltimesecs);
  fracwalltime = walltimesecs - intwalltime;
  //cinfo << startl << "ATD: fftstartmicrosec " << fftstartmicrosec << ", sampletime " << sampletime << ", fftchannels " << fftchannels << ", bytesperblocknumerator " << bytesperblocknumerator << ", nearestsample " << nearestsample << endl;

  //if we need to, unpack some more data - first check to make sure the pos is valid at all
  //cout << "Datalengthbytes for " << datastreamindex << " is " << datalengthbytes << endl;
  //cout << "Fftchannels for " << datastreamindex << " is " << fftchannels << endl;
  //cout << "samplesperblock for " << datastreamindex << " is " << samplesperblock << endl;
  //cout << "nearestsample for " << datastreamindex << " is " << nearestsample << endl;
  //cout << "bytesperblocknumerator for " << datastreamindex << " is " << bytesperblocknumerator << endl;
  //cout << "bytesperblockdenominator for " << datastreamindex << " is " << bytesperblockdenominator << endl;
  if(nearestsample < -1 || (((nearestsample + fftchannels)/samplesperblock)*bytesperblocknumerator)/bytesperblockdenominator > datalengthbytes)
  {
    cerror << startl << "MODE error for datastream " << datastreamindex << " - trying to process data outside range - aborting!!! nearest sample was " << nearestsample << ", the max bytes should be " << datalengthbytes << " and hence last sample should be " << (datalengthbytes*bytesperblockdenominator)/(bytesperblocknumerator*samplesperblock)  << " (fftchannels is " << fftchannels << "), offsetseconds was " << offsetseconds << ", offsetns was " << offsetns << ", index was " << index << ", average delay was " << averagedelay << ", datasec was " << datasec << ", datans was " << datans << ", fftstartmicrosec was " << fftstartmicrosec << endl;
    for(int i=0;i<numrecordedbands;i++)
    {
      status = vectorZero_cf32(fftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
      status = vectorZero_cf32(conjfftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
    }
    if(weightDebugFrom() >= 0 && datasec >= weightDebugFrom())
      fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=0.000000000 valid=0 reason=rejected\n",
              datastreamindex, datasec, datans, index, nearestsample);
    return;
  }
  if(nearestsample == -1)
  {
    nearestsample = 0;
    dataweight[subloopindex] = unpack(nearestsample, subloopindex);
  }
  else if(nearestsample < unpackstartsamples || nearestsample > unpackstartsamples + unpacksamples - fftchannels)
    //need to unpack more data
    dataweight[subloopindex] = unpack(nearestsample, subloopindex);
  if(weightDebugFrom() >= 0 && datasec >= weightDebugFrom())
    fprintf(stderr, "WDEBUG ds=%d datasec=%d datans=%d index=%d nearest=%d weight=%.9f valid=%d reason=ok\n",
            datastreamindex, datasec, datans, index, nearestsample, dataweight[subloopindex],
            (dataweight[subloopindex] > 0.0) ? 1 : 0);

 /*
  * After DiFX-2.4, it is proposed to change the handling of lower sideband and dual sideband data, such
  * that the data are manipulated here (directly after unpacking) to ensure that it appears like single 
  * sideband USB data.  In order to do that, we will loop over all recorded bands, performing the 
  * following checks and if necessary manipulations:
  * 1) if band sense is LSB, cast the unpacked data as a complex f32 and conjugate it.  If it was a complex
  *    sampled band, this flips the sideband.  If it was a real sampled band, then every second sample
  *    will be multiplied by -1, which is exactly was is required to flip the sideband also.
  *    *** NOTE: For real data, will need to use fracwalltimesecs plus the sampling rate to determine
  *              whether it is necessary to offset the start of the vector by one sample.
  * 2) Now the frequencies definitely run from most negative to most positive, but we also want the lowest
  *    frequency channel to be "DC", and this is not the case for complex double sideband data.  So for
  *    complex double sideband data, rotate the unpacked data by e^{-i 2 pi BW t} to shift the most negative
  *    frequency component up to 0 Hz.  Need to use wallclocksecs for time here too.
  * Now nothing in mode.cpp or core.cpp needs to know about whether the data was originally lower sideband
  * or not.  That will mean taking out some of the current logic, pretty much all to do with fractional sample
  * correction.
  * 
  * Some other specific implementation notes:
  * - Need to do this straight after an unpack, for the whole unpacksamples, so the two calls to unpack()
  *   above will need to be combined.
  * - It may be profitable to move the LO offset correction up to here also, and possibly also to refactor 
  *   it to change the steptval array rather than doing a separate addition. (although a separate addition
  *   for fraclooffset if required would still be needed).  Be careful of zero-order fringe rotation.
  * - lsbfracsample arrays will need to be removed, as will the checks that select them.
  * - Elsewhere, it will probably be preferable to maintain information slightly differently (for each
  *   subband, maintain lower edge frequency, bandwidth, SSLO, sampling type [real/complex], matching band).
  *   This would be in configuration.cpp/.h, maybe also vex2difx?
  * - mark5access has option to unpack real data as complex - could consider using this to save time.  
  *   Would need to make a similar option for LBA data.
  */

  if(!(dataweight[subloopindex] > 0.0)) {
  //  if (datans == 36500000) {
  //    printf("DEBUG_DATAWEIGHT_CPU call=%d ds=%d idx=%d sub=%d nearest=%d sampleIndex=%d weight=%.9f valid=%d offsetsec=%d datasec=%d datans=%d\n",
  //           debug_dataweight_cpu_calls,
  //           datastreamindex,
  //           index,
  //           subloopindex,
  //           nearestsample,
  //           nearestsample - unpackstartsamples,
  //           dataweight[subloopindex],
  //           0,
  //           offsetseconds,
  //           datasec,
  //           datans);
  //    debug_dataweight_cpu_prints++;
  //  }

    for(int i=0;i<numrecordedbands;i++)
    {
      status = vectorZero_cf32(fftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
      status = vectorZero_cf32(conjfftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
    }
    return;
  }

  //if (debug_dataweight_cpu_prints < 256 && datastreamindex == 4) {
  //  if (datans == 36500000) {
  //       printf("DEBUG_DATAWEIGHT_CPU call=%d ds=%d idx=%d sub=%d nearest=%d sampleIndex=%d weight=%.9f valid=%d offsetsec=%d datasec=%d datans=%d\n",
  //         debug_dataweight_cpu_calls,
  //         datastreamindex,
  //         index,
  //         subloopindex,
  //         nearestsample,
  //         nearestsample - unpackstartsamples,
  //         dataweight[subloopindex],
  //         1,
  //         offsetseconds,
  //         datasec,
  //         datans);
  //  debug_dataweight_cpu_prints++;
  //}

  nearestsampletime = nearestsample*sampletime;
  fracsampleerror = float(starttime - nearestsampletime);

  if(!(config->getDPhaseCalIntervalHz(configindex, datastreamindex) == 0))
  {
      size_t samplenrsincestart = nearestsample + static_cast<size_t>(double(datasec*1e9+datans)/(sampletime*1e3) + 0.5);
      for(int i=0;i<numrecordedbands;i++)
      {
        extractor[i]->adjustSampleOffset(samplenrsincestart);
        if (!usecomplex) {
	        status = extractor[i]->extractAndIntegrate (&(unpackedarrays[i][nearestsample
	                 - unpackstartsamples]), fftchannels);
        } else
	        status = extractor[i]->extractAndIntegrate ((f32 *) (&(unpackedcomplexarrays[i][nearestsample
	                 - unpackstartsamples])), fftchannels);
        if(status != true)
          csevere << startl << "Error in phase cal extractAndIntegrate" << endl;
      }
  }

  integerdelay = 0;
  switch(fringerotationorder) {
    case 0: //post-F
      integerdelay = static_cast<int>(averagedelay);
      break;
    case 1: //linear
      //std::cout << "linear" << std::endl; 
      d0 = interpolator[0]*index*index + interpolator[1]*index + interpolator[2];
      d1 = interpolator[0]*(index+0.5)*(index+0.5) + interpolator[1]*(index+0.5) + interpolator[2];
      d2 = interpolator[0]*(index+1)*(index+1) + interpolator[1]*(index+1) + interpolator[2];
      a = d2-d0;
      b = d0 + (d1 - (a*0.5 + d0))/3.0;
      integerdelay = static_cast<int>(b);
      b -= integerdelay;

      status = vectorMulC_f64(subxoff, a, subxval, arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error in linearinterpolate, subval multiplication" << endl;
      status = vectorMulC_f64(stepxoff, a, stepxval, numfrstrides);
      if(status != vecNoErr)
        csevere << startl << "Error in linearinterpolate, stepval multiplication" << endl;
      status = vectorAddC_f64_I(b, subxval, arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error in linearinterpolate, subval addition!!!" << endl;
//      for(int kk=0;kk<numfrstrides;kk++) {
//          std::cout << "stepxval["<< kk << "] = " << stepxval[kk] << std::endl;
//          std::cout << "subxval[" << kk << "] = " << stepxval[kk] << std::endl;
//      }	  
      break;
    case 2: //quadratic
      a = interpolator[0];
      b = interpolator[1] + index*interpolator[0]*2.0;
      c = interpolator[2] + index*interpolator[1] + index*index*interpolator[0];
      integerdelay = int(c);
      c -= integerdelay;

      status = vectorMulC_f64(subxoff, b + a*stepxoff[1], subxval, arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error in quadinterpolate, subval multiplication" << endl;
      status = vectorMulC_f64(subxoff, 2*a*stepxoff[1], subquadxval, arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error in quadinterpolate, subquadval multiplication" << endl;
      status = vectorMulC_f64(stepxoff, b, stepxval, numfrstrides);
      if(status != vecNoErr)
        csevere << startl << "Error in quadinterpolate, stepval multiplication" << endl;
      status = vectorMulC_f64(stepxoffsquared, a, tempstepxval, numfrstrides);
      if(status != vecNoErr)
        csevere << startl << "Error in quadinterpolate, tempstepval multiplication" << endl;
      status = vectorAdd_f64_I(tempstepxval, stepxval, numfrstrides);
      if(status != vecNoErr)
        csevere << startl << "Error in quadinterpolate, stepval addition!!!" << endl;
      status = vectorAddC_f64_I(c, subxval, arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error in quadinterpolate, subval addition!!!" << endl;
      break;
  }

  // Do the main work here
  // Loop over each frequency and to the fringe rotation and FFT of the data

  for(int i=0;i<numrecordedfreqs;i++)
  {
    count = 0;
    //updated so that Nyquist channel is not accumulated for either USB or LSB data
    //and is excised entirely, so both USB and LSB data start at the same place (no sidebandoffset)
    currentstepchannelfreqs = stepchannelfreqs;
    currentsubchannelfreqs = subchannelfreqs;
    if(usedouble)
    {
      currentstepchannelfreqs = dsbstepchannelfreqs;
      /*if(config->getDRecordedLowerSideband(configindex, datastreamindex, i))
      {
        currentstepchannelfreqs = ldsbstepchannelfreqs;
        currentsubchannelfreqs = ldsbsubchannelfreqs;
      }
      else
      {
        currentstepchannelfreqs = dsbstepchannelfreqs;
      }*/
    }
    else
    {
      if(config->getDRecordedLowerSideband(configindex, datastreamindex, i))
      {
        currentstepchannelfreqs = lsbstepchannelfreqs;
      }
    }

    looff = false;
    isfraclooffset = false;
    if(recordedfreqlooffsets[i] > 0.0 || recordedfreqlooffsets[i] < 0.0) {
      looff = true;
      fraclooffset = fabs(recordedfreqlooffsets[i]) - int(fabs(recordedfreqlooffsets[i]));
      if (fraclooffset > Mode::TINY)
        isfraclooffset = true;
      if (recordedfreqlooffsets[i] < 0)
        fraclooffset = -fraclooffset;
    }

    //get ready to apply fringe rotation, if it is pre-F.  
    //By default, the local oscillator frequency (which is used for fringe rotation) is the band edge, as specified inthe input file
    lofreq = config->getDRecordedFreq(configindex, datastreamindex, i);

    // For double-sideband data, the LO frequency is at the centre of the band, not the band edge
    if (usecomplex) {
      if (config->getDRecordedLowerSideband(configindex, datastreamindex, i))
        lofreq = -lofreq;  // LSB case
      if (usedouble)
        lofreq += config->getDRecordedBandwidth(configindex, datastreamindex, i)/2.0;  // Double sideband move LO to middle of band
    }
    //std::cout << "lofreq = " << lofreq << std::endl;


    switch(fringerotationorder) {
      case 1: // linear

/* The actual calculation that is going on for the linear case is as follows:

   Calculate complexrotator[j]  (for j = 0 to fftchanels-1) as:

   complexrotator[j] = exp( 2 pi i * (A*j + B) )

   where:

   A = a*lofreq/fftchannels - sampletime*1.0e-6*recordedfreqlooffsets[i]
   B = b*lofreq/fftchannels + fraclofreq*integerdelay - recordedfreqlooffsets[i]*fracwalltime - fraclooffset*intwalltime

   And a, b are computed outside the recordedfreq loop (variable i)
*/

        status = vectorMulC_f64(subxval, lofreq, subphase, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error in linearinterpolate lofreq sub multiplication!!!" << status << endl;
        status = vectorMulC_f64(stepxval, lofreq, stepphase, numfrstrides);
        if(status != vecNoErr)
          csevere << startl << "Error in linearinterpolate lofreq step multiplication!!!" << status << endl;
        if(fractionalLoFreq) {
          status = vectorAddC_f64_I((lofreq-int(lofreq))*double(integerdelay), subphase, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in linearinterpolate lofreq non-integer freq addition!!!" << status << endl;
        }
        if(looff) {
          status = vectorMulC_f64(subtoff, -recordedfreqlooffsets[i], subtval, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset calculation (time domain, sub vector)" << status << endl;
          status = vectorMulC_f64(steptoff, -recordedfreqlooffsets[i], steptval, numfrstrides);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset calculation (time domain, step vector)" << status << endl;
          status = vectorAdd_f64_I(subtval, subphase, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset addition (time domain, sub vector)" << status << endl;
          status = vectorAdd_f64_I(steptval, stepphase, numfrstrides);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset addition (time domain, step vector)" << status << endl;
          status = vectorAddC_f64_I(-recordedfreqlooffsets[i]*fracwalltime, subphase, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset addition (time domain, wallclock offset)" << status << endl;
          if(isfraclooffset) {
            status = vectorAddC_f64_I(-fraclooffset*intwalltime, subphase, arraystridelength);
            if(status != vecNoErr)
              csevere << startl << "Error in LO offset addition (time domain, frac LO offset wallclock offset)" << status << endl;
          }
        }
        for(int j=0;j<arraystridelength;j++) {
          subarg[j] = -TWO_PI*(subphase[j] - int(subphase[j]));
        }
        for(int j=0;j<numfrstrides;j++) {
          steparg[j] = -TWO_PI*(stepphase[j] - int(stepphase[j]));
        }
        status = vectorSinCos_f32(subarg, subsin, subcos, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error in sin/cos of sub rotate argument!!!" << endl;
        status = vectorSinCos_f32(steparg, stepsin, stepcos, numfrstrides);
        if(status != vecNoErr)
          csevere << startl << "Error in sin/cos of step rotate argument!!!" << endl;
        status = vectorRealToComplex_f32(subcos, subsin, complexrotator, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error assembling sub into complex!!!" << endl;
        status = vectorRealToComplex_f32(stepcos, stepsin, stepcplx, numfrstrides);
        if(status != vecNoErr)
          csevere << startl << "Error assembling step into complex!!!" << endl;
        for(int j=1;j<numfrstrides;j++) {
          status = vectorMulC_cf32(complexrotator, stepcplx[j], &complexrotator[j*arraystridelength], arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error doing the time-saving complex multiplication!!!" << endl;
        }
        break;
      case 2: // Quadratic
        status = vectorMulC_f64(subxval, lofreq, subphase, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error in quadinterpolate lofreq sub multiplication!!!" << status << endl;
        if(fractionalLoFreq) {
          status = vectorAddC_f64_I((lofreq-int(lofreq))*double(integerdelay), subphase, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in linearinterpolate lofreq non-integer freq addition!!!" << status << endl;
        }
        status = vectorMulC_f64(subquadxval, lofreq, subquadphase, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error in quadinterpolate lofreq subquad multiplication!!!" << status << endl;
        status = vectorMulC_f64(stepxval, lofreq, stepphase, numfrstrides);
        if(status != vecNoErr)
          csevere << startl << "Error in quadinterpolate lofreq step multiplication!!!" << status << endl;
        if(looff) {
          status = vectorMulC_f64(subtoff, -recordedfreqlooffsets[i], subtval, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset calculation (time domain, sub vector)" << status << endl;
          status = vectorMulC_f64(steptoff, -recordedfreqlooffsets[i], steptval, numfrstrides);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset calculation (time domain, step vector)" << status << endl;
          status = vectorAdd_f64_I(subtval, subphase, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset addition (time domain, sub vector)" << status << endl;
          status = vectorAdd_f64_I(steptval, stepphase, numfrstrides);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset addition (time domain, step vector)" << status << endl;
          status = vectorAddC_f64_I(-recordedfreqlooffsets[i]*fracwalltime, subphase, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error in LO offset addition (time domain, wallclock offset)" << status << endl;
          if(isfraclooffset) {
            status = vectorAddC_f64_I(-fraclooffset*intwalltime, subphase, arraystridelength);
            if(status != vecNoErr)
              csevere << startl << "Error in LO offset addition (time domain, frac LO offset wallclock offset)" << status << endl;
          }
        }
        for(int j=0;j<arraystridelength;j++) {
          subarg[j] = -TWO_PI*(subphase[j] - int(subphase[j]));
          subquadarg[j] = -TWO_PI*(subquadphase[j] - int(subquadphase[j]));
        }
        for(int j=0;j<numfrstrides;j++) {
          steparg[j] = -TWO_PI*(stepphase[j] - int(stepphase[j]));
        }
        status = vectorSinCos_f32(subarg, subsin, subcos, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error in sin/cos of sub rotate argument!!!" << endl;
        status = vectorSinCos_f32(subquadarg, subquadsin, subquadcos, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error in sin/cos of subquad rotate argument!!!" << endl;
        status = vectorSinCos_f32(steparg, stepsin, stepcos, numfrstrides);
        if(status != vecNoErr)
          csevere << startl << "Error in sin/cos of step rotate argument!!!" << endl;
        status = vectorRealToComplex_f32(subcos, subsin, piecewiserotator, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error assembling sub into complex" << endl;
        status = vectorRealToComplex_f32(subquadcos, subquadsin, quadpiecerotator, arraystridelength);
        if(status != vecNoErr)
          csevere << startl << "Error assembling sub into complex" << endl;
        status = vectorRealToComplex_f32(stepcos, stepsin, stepcplx, numfrstrides);
        if(status != vecNoErr)
          csevere << startl << "Error assembling step into complex" << endl;
        for(int j=0;j<numfrstrides;j++) {
          status = vectorMulC_cf32(piecewiserotator, stepcplx[j], &complexrotator[j*arraystridelength], arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error doing the time-saving complex mult (striding)" << endl;
          status = vectorMul_cf32_I(quadpiecerotator, piecewiserotator, arraystridelength);
          if(status != vecNoErr)
            csevere << startl << "Error doing the time-saving complex mult (adjusting linear gradient)" << endl;
        }
        break;
    }

    // Note recordedfreqclockoffsetsdata will usually be zero, but avoiding if statement
    status = vectorMulC_f32(currentsubchannelfreqs, fracsampleerror - recordedfreqclockoffsets[i] + recordedfreqclockoffsetsdelta[i]/2, subfracsamparg, arraystridelength);
    if(status != vecNoErr) {
      csevere << startl << "Error in frac sample correction, arg generation (sub)!!!" << status << endl;
      exit(1);
    }
    status = vectorMulC_f32(currentstepchannelfreqs, fracsampleerror - recordedfreqclockoffsets[i] + recordedfreqclockoffsetsdelta[i]/2, stepfracsamparg, numfracstrides/2);
    if(status != vecNoErr)
      csevere << startl << "Error in frac sample correction, arg generation (step)!!!" << status << endl;

    // For zero-th order (post-F) fringe rotation, calculate the fringe rotation (+ LO offset if necessary)
    if(fringerotationorder == 0) { // do both LO offset and fringe rotation  (post-F)
      phaserotation = (averagedelay-integerdelay)*lofreq;
      if(fractionalLoFreq)
        phaserotation += integerdelay*(lofreq-int(lofreq));
      phaserotation -= walltimesecs*recordedfreqlooffsets[i];
      phaserotationfloat = (f32)(-TWO_PI*(phaserotation-int(phaserotation)));
      status = vectorAddC_f32_I(phaserotationfloat, subfracsamparg, arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error in post-f phase rotation addition (+ maybe LO offset correction), sub!!!" << status << endl;
    }

    //create the fractional sample correction array
    status = vectorSinCos_f32(subfracsamparg, subfracsampsin, subfracsampcos, arraystridelength);
    if(status != vecNoErr)
      csevere << startl << "Error in frac sample correction, sin/cos (sub)!!!" << status << endl;
    status = vectorSinCos_f32(stepfracsamparg, stepfracsampsin, stepfracsampcos, numfracstrides/2);
    if(status != vecNoErr)
      csevere << startl << "Error in frac sample correction, sin/cos (sub)!!!" << status << endl;
    status = vectorRealToComplex_f32(subfracsampcos, subfracsampsin, fracsamprotatorA, arraystridelength);
    if(status != vecNoErr)
      csevere << startl << "Error in frac sample correction, real to complex (sub)!!!" << status << endl;
    status = vectorRealToComplex_f32(stepfracsampcos, stepfracsampsin, stepfracsampcplx, numfracstrides/2);
    if(status != vecNoErr)
      csevere << startl << "Error in frac sample correction, real to complex (step)!!!" << status << endl;
    for(int j=1;j<numfracstrides/2;j++) {
      status = vectorMulC_cf32(fracsamprotatorA, stepfracsampcplx[j], &(fracsamprotatorA[j*arraystridelength]), arraystridelength);
      if(status != vecNoErr)
        csevere << startl << "Error doing the time-saving complex multiplication in frac sample correction!!!" << endl;
    }

    // now do the first arraystridelength elements (which are different from fracsampptr1 for LSB case)
    status = vectorMulC_cf32_I(stepfracsampcplx[0], fracsamprotatorA, arraystridelength);
    if(status != vecNoErr)
    csevere << startl << "Error doing the first bit of the time-saving complex multiplication in frac sample correction!!!" << endl;

    // Repeat the post F correction steps if each pol is different
    if (deltapoloffsets) {
      status = vectorMulC_f32(currentsubchannelfreqs, fracsampleerror - recordedfreqclockoffsets[i] - recordedfreqclockoffsetsdelta[i]/2, subfracsamparg, arraystridelength); 
      if(status != vecNoErr) {
	csevere << startl << "Error in frac sample correction, arg generation (sub)!!!" << status << endl;
	exit(1);
      }
      status = vectorMulC_f32(currentstepchannelfreqs, fracsampleerror - recordedfreqclockoffsets[i] - recordedfreqclockoffsetsdelta[i]/2, stepfracsamparg, numfracstrides/2);  //L2C add delay
      if(status != vecNoErr)
	csevere << startl << "Error in frac sample correction, arg generation (step)!!!" << status << endl;

      // For zero-th order (post-F) fringe rotation, calculate the fringe rotation (+ LO offset if necessary)
      if(fringerotationorder == 0) { // do both LO offset and fringe rotation  (post-F)
	phaserotation = (averagedelay-integerdelay)*lofreq;
	if(fractionalLoFreq)
	  phaserotation += integerdelay*(lofreq-int(lofreq));
	phaserotation -= walltimesecs*recordedfreqlooffsets[i];
	phaserotationfloat = (f32)(-TWO_PI*(phaserotation-int(phaserotation)));
	status = vectorAddC_f32_I(phaserotationfloat, subfracsamparg, arraystridelength);
	if(status != vecNoErr)
	  csevere << startl << "Error in post-f phase rotation addition (+ maybe LO offset correction), sub!!!" << status << endl;
      }
      
      //create the fractional sample correction array
      status = vectorSinCos_f32(subfracsamparg, subfracsampsin, subfracsampcos, arraystridelength);
      if(status != vecNoErr)
	csevere << startl << "Error in frac sample correction, sin/cos (sub)!!!" << status << endl;
      status = vectorSinCos_f32(stepfracsamparg, stepfracsampsin, stepfracsampcos, numfracstrides/2);
      if(status != vecNoErr)
	csevere << startl << "Error in frac sample correction, sin/cos (sub)!!!" << status << endl;
      status = vectorRealToComplex_f32(subfracsampcos, subfracsampsin, fracsamprotatorB, arraystridelength); // L2C change pointers
      if(status != vecNoErr)
	csevere << startl << "Error in frac sample correction, real to complex (sub)!!!" << status << endl;
      status = vectorRealToComplex_f32(stepfracsampcos, stepfracsampsin, stepfracsampcplx, numfracstrides/2);
      if(status != vecNoErr)
	csevere << startl << "Error in frac sample correction, real to complex (step)!!!" << status << endl;
      for(int j=1;j<numfracstrides/2;j++) {
	status = vectorMulC_cf32(fracsamprotatorB, stepfracsampcplx[j], &(fracsamprotatorB[j*arraystridelength]), arraystridelength); // L2C change pointers
	if(status != vecNoErr)
	  csevere << startl << "Error doing the time-saving complex multiplication in frac sample correction!!!" << endl;
      }

      // now do the first arraystridelength elements (which are different from fracsampptr1 for LSB case)
      status = vectorMulC_cf32_I(stepfracsampcplx[0], fracsamprotatorB, arraystridelength); // L2C change pointers
      if(status != vecNoErr)
	csevere << startl << "Error doing the first bit of the time-saving complex multiplication in frac sample correction!!!" << endl;

    }

    for(int j=0;j<numrecordedbands;j++)  // Loop over all recorded bands looking for the matching frequency we should be dealing with
    {
      if(config->matchingRecordedBand(configindex, datastreamindex, i, j))
      {
        indices[count++] = j;
        switch(fringerotationorder) {
          case 0: //post-F
            if (usecomplex) {
              cfatal << startl << "Post-F (0th order) fringe rotation not currently supported for complex sampled data!" << endl;
              exit(1);
            }

            fftptr = (config->getDRecordedLowerSideband(configindex, datastreamindex, i))?conjfftoutputs[j][subloopindex]:fftoutputs[j][subloopindex];

            //do the fft
            // Chris add C2C fft for complex data
            if(isfft) {
              status = vectorFFT_RtoC_f32(&(unpackedarrays[j][nearestsample - unpackstartsamples]), (f32*) fftptr, pFFTSpecR, fftbuffer);
              if (status != vecNoErr)
                csevere << startl << "Error in FFT!!!" << status << endl;
            //fix the lower sideband if required
            }
            else{
              status = vectorDFT_RtoC_f32(&(unpackedarrays[j][nearestsample - unpackstartsamples]), (f32*) fftptr, pDFTSpecR, fftbuffer);
              if (status != vecNoErr)
                csevere << startl << "Error in DFT!!!" << status << endl;
            }
            if(config->getDRecordedLowerSideband(configindex, datastreamindex, i))
            {
              status = vectorConjFlip_cf32(&(fftptr[1]), fftoutputs[j][subloopindex], recordedbandchannels);
              if(status != vecNoErr)
                csevere << startl << "Error in conjugate!!!" << status << endl;
            }
            break;
          case 1: // Linear
             //std::cout << "linear" << std::endl;
            //std::cout << "unpackedarrays[" << j << "][" << nearestsample - unpackstartsamples << "] = " << unpackedarrays[j][nearestsample - unpackstartsamples] << std::endl;
          case 2: // Quadratic
              //std::cout << "quadratic" << std::endl;
            if (usecomplex) {
              status = vectorMul_cf32(complexrotator, &unpackedcomplexarrays[j][nearestsample - unpackstartsamples], complexunpacked, fftchannels);
              // The following can be uncommented (and the above commented) if wanting to 'turn off' fringe rotation for testing in the complex case
              //status = vectorCopy_cf32(&unpackedcomplexarrays[j][nearestsample - unpackstartsamples], complexunpacked, fftchannels);
              if (status != vecNoErr)
                csevere << startl << "Error in complex fringe rotation" << endl;
            } else {
              status = vectorRealToComplex_f32(&(unpackedarrays[j][nearestsample - unpackstartsamples]), NULL, complexunpacked, fftchannels);
              if (status != vecNoErr)
                csevere << startl << "Error in real->complex conversion" << endl;
              status = vectorMul_cf32_I(complexrotator, complexunpacked, fftchannels);
              if(status != vecNoErr)
              	csevere << startl << "Error in fringe rotation!!!" << status << endl;
            }
            if(isfft) {
              status = vectorFFT_CtoC_cf32(complexunpacked, fftd, pFFTSpecC, fftbuffer);
              if(status != vecNoErr)
                csevere << startl << "Error doing the FFT!!!" << endl;
            }
            else {
              status = vectorDFT_CtoC_cf32(complexunpacked, fftd, pDFTSpecC, fftbuffer);
              if(status != vecNoErr)
                csevere << startl << "Error doing the DFT!!!" << endl;
            }

            if(config->getDRecordedLowerSideband(configindex, datastreamindex, i)) {
              // All lower sideband bands need to be conjugated (achieved by taking the second half of the band for real-valued inputs)
              // Additionally for the complex-valued inputs, the order of the frequency channels is reversed so they need to be flipped
              // (for the double sideband case, in two halves, for the regular case, the whole thing)
              if (usecomplex) {
                if (usedouble) {
                  status = vectorConjFlip_cf32(fftd, fftoutputs[j][subloopindex], recordedbandchannels/2+1);
                  status = vectorConjFlip_cf32(&fftd[recordedbandchannels/2]+1, &fftoutputs[j][subloopindex][recordedbandchannels/2]+1, recordedbandchannels/2-1);
                } else {
                  //status = vectorConjFlip_cf32(fftd, fftoutputs[j][subloopindex], recordedbandchannels);
                  // note: using vectorConjFlip_cf32() -lofreq breaks Complex LSB (non-DSB!) fringes for VGOS *assuming* VGOS RDBE-G indeed LSB like memos claim
                  // fix?: LSB fringes are restored at least for a synthetic fully correlated data set of Complex USB and Complex LSB data.
                  //       The reversal has to be changed as below to retain DC in bin 0, producing not [ch1 ch2 ch3 ... DC] but instead [DC ch1 ch2 ch3 ...]
                  // todo: validate fix on real world definitely-known-LSB data (evidenced by pcal tone positions etc), then uncomment the next lines:
                  status = vectorConjFlip_cf32(fftd+1, fftoutputs[j][subloopindex]+1, recordedbandchannels-1);
                  fftoutputs[j][subloopindex][0] = fftd[0];
                }
              }
              else {
		// By grabbing the "upper half" of the FFT output, channels are naturally in frequency assending order, even for LSB data
                status = vectorCopy_cf32(&(fftd[recordedbandchannels]), fftoutputs[j][subloopindex], recordedbandchannels);
              }
            }
            else {
              // For upper sideband bands, normally just need to copy the fftd channels.
              // However for complex double upper sideband, the two halves of the frequency space are swapped, so they need to be swapped back
              if (usecomplex && usedouble) {
                status = vectorCopy_cf32(fftd, &fftoutputs[j][subloopindex][recordedbandchannels/2], recordedbandchannels/2);
                status = vectorCopy_cf32(&fftd[recordedbandchannels/2], fftoutputs[j][subloopindex], recordedbandchannels/2);
              } else {
                status = vectorCopy_cf32(fftd, fftoutputs[j][subloopindex], recordedbandchannels);
              }
            }
            if(status != vecNoErr)
              csevere << startl << "Error copying FFT results!!!" << endl;
            break;
        }

	// At this point in the code the array fftoutputs[j] contains complex-valued voltage spectra with the following properties:
	//
	// 1. The zero element corresponds to the lowest sky frequency.  That is:
	//    fftoutputs[j][0] = Local Oscillator Frequency              (for Upper Sideband)
	//    fftoutputs[j][0] = Local Oscillator Frequency - bandwidth  (for Lower Sideband)
	//    fftoutputs[j][0] = Local Oscillator Frequency - bandwidth  (for Complex Lower Sideband)
	//    fftoutputs[j][0] = Local Oscillator Frequency - bandwidth/2(for Complex Double Upper Sideband)
	//    fftoutputs[j][0] = Local Oscillator Frequency - bandwidth/2(for Complex Double Lower Sideband)
	//
	// 2. The frequency increases monotonically with index
	//
	// 3. The last element of the array corresponds to the highest sky frequency minus the spectral resolution.
	//    (i.e., the first element beyond the array bound corresponds to the highest sky frequency)

        if(dumpkurtosis) //do the necessary accumulation
        {
          status = vectorMagnitude_cf32(fftoutputs[j][subloopindex], kscratch, recordedbandchannels);
          if(status != vecNoErr)
            csevere << startl << "Error taking kurtosis magnitude!" << endl;
          status = vectorSquare_f32_I(kscratch, recordedbandchannels);
          if(status != vecNoErr)
            csevere << startl << "Error in first kurtosis square!" << endl;
          status = vectorAdd_f32_I(kscratch, s1[j], recordedbandchannels);
          if(status != vecNoErr)
            csevere << startl << "Error in kurtosis s1 accumulation!" << endl;
          status = vectorSquare_f32_I(kscratch, recordedbandchannels);
          if(status != vecNoErr)
            csevere << startl << "Error in second kurtosis square!" << endl;
          status = vectorAdd_f32_I(kscratch, s2[j], recordedbandchannels);
          if(status != vecNoErr)
            csevere << startl << "Error in kurtosis s2 accumulation!" << endl;
        }

        //do the frac sample correct (+ phase shifting if applicable, + fringe rotate if its post-f)
	if (deltapoloffsets==false || config->getDRecordedBandPol(configindex, datastreamindex, j)=='R') {
	  status = vectorMul_cf32_I(fracsamprotatorA, fftoutputs[j][subloopindex], recordedbandchannels);
	} else {
	  status = vectorMul_cf32_I(fracsamprotatorB, fftoutputs[j][subloopindex], recordedbandchannels);
	}
	if(status != vecNoErr)
	  csevere << startl << "Error in application of frac sample correction!!!" << status << endl;

        //do the conjugation
        status = vectorConj_cf32(fftoutputs[j][subloopindex], conjfftoutputs[j][subloopindex], recordedbandchannels);
        if(status != vecNoErr)
          csevere << startl << "Error in conjugate!!!" << status << endl;

        if(specDebugFrom() >= 0 && datasec >= specDebugFrom() && (index % 128) == 5)
        {
          // The mid-window fields (unp2/rot, offsets 510-515) bracket the
          // single-corrupted-sample-at-offset-512 signature seen in the
          // complex-complex test: spec differences of the form d*(-1)^k.
          float ur[4], ui[4], u2r[6], u2i[6];
          for(int k = 0; k < 4; k++)
          {
            if(usecomplex)
            {
              ur[k] = unpackedcomplexarrays[j][nearestsample - unpackstartsamples + k].re;
              ui[k] = unpackedcomplexarrays[j][nearestsample - unpackstartsamples + k].im;
            }
            else
            {
              ur[k] = unpackedarrays[j][nearestsample - unpackstartsamples + k];
              ui[k] = 0.0f;
            }
          }
          for(int k = 0; k < 6; k++)
          {
            if(usecomplex)
            {
              u2r[k] = unpackedcomplexarrays[j][nearestsample - unpackstartsamples + 510 + k].re;
              u2i[k] = unpackedcomplexarrays[j][nearestsample - unpackstartsamples + 510 + k].im;
            }
            else
            {
              u2r[k] = unpackedarrays[j][nearestsample - unpackstartsamples + 510 + k];
              u2i[k] = 0.0f;
            }
          }
          const cf32* rp = complexunpacked;   // fringe-rotated FFT input, still intact (out-of-place FFT)
          const cf32* sp = fftoutputs[j][subloopindex];
          fprintf(stderr, "SPECDEBUG ds=%d datasec=%d datans=%d index=%d band=%d nearest=%d unp=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f unp2=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f rot=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f spec=%.6f,%.6f;%.6f,%.6f;%.6f,%.6f;%.6f,%.6f\n",
                  datastreamindex, datasec, datans, index, j, nearestsample,
                  ur[0], ui[0], ur[1], ui[1], ur[2], ui[2], ur[3], ui[3],
                  u2r[0], u2i[0], u2r[1], u2i[1], u2r[2], u2i[2], u2r[3], u2i[3], u2r[4], u2i[4], u2r[5], u2i[5],
                  rp[510].re, rp[510].im, rp[511].re, rp[511].im, rp[512].re, rp[512].im,
                  rp[513].re, rp[513].im, rp[514].re, rp[514].im, rp[515].re, rp[515].im,
                  sp[0].re, sp[0].im, sp[1].re, sp[1].im, sp[2].re, sp[2].im, sp[3].re, sp[3].im);
        }

	if (!linear2circular) {
	  //do the autocorrelation (skipping Nyquist channel)
      // j is 0..numrecordedbands
	  status = vectorAddProduct_cf32(fftoutputs[j][subloopindex], conjfftoutputs[j][subloopindex], autocorrelations[0][j], recordedbandchannels);
	  if(status != vecNoErr)
	    csevere << startl << "Error in autocorrelation!!!" << status << endl;
	  //store the weight for the autocorrelations
          if(perbandweights)
          {
	    weights[0][j] += perbandweights[subloopindex][j];
          }
          else
          {
	    weights[0][j] += dataweight[subloopindex];
          }
	}
      }
    }


    if (count>1) {
      // Do linear to circular conversion if required
      if (linear2circular) {

	// FIXME: Apply gain correction

	if (config->getDRecordedBandPol(configindex, datastreamindex, indices[0])=='R') {
	    RcpIndex = indices[0];
	    LcpIndex = indices[1];
	  } else {
	    RcpIndex = indices[1];
	    LcpIndex = indices[0];
	  }
	  
	  // Rotate Lcp by 90deg
	  vectorMulC_cf32_I(phasecorrA[i], fftoutputs[LcpIndex][subloopindex], recordedbandchannels);
	  vectorMulC_cf32_I(phasecorrconjA[i], conjfftoutputs[LcpIndex][subloopindex], recordedbandchannels);

	  // Add and subtract
	  vectorSub_cf32(fftoutputs[LcpIndex][subloopindex], fftoutputs[RcpIndex][subloopindex], tmpvec, recordedbandchannels);
	  vectorAdd_cf32_I(fftoutputs[LcpIndex][subloopindex], fftoutputs[RcpIndex][subloopindex], recordedbandchannels);
	  vectorCopy_cf32(tmpvec, fftoutputs[LcpIndex][subloopindex], recordedbandchannels);

	  vectorSub_cf32(conjfftoutputs[LcpIndex][subloopindex], conjfftoutputs[RcpIndex][subloopindex], tmpvec, recordedbandchannels);
	  vectorAdd_cf32_I(conjfftoutputs[LcpIndex][subloopindex], conjfftoutputs[RcpIndex][subloopindex], recordedbandchannels);
	  vectorCopy_cf32(tmpvec, conjfftoutputs[LcpIndex][subloopindex], recordedbandchannels);

	  break; 
      } else if (phasepoloffset) {
	// Add phase offset to Lcp

	if (config->getDRecordedBandPol(configindex, datastreamindex, indices[0])=='R') {
	    LcpIndex = indices[1];
	  } else {
	    LcpIndex = indices[0];
	  }
	  
	  // Rotate Lcp by phase offset deg
	  vectorMulC_cf32_I(phasecorrA[i], fftoutputs[LcpIndex][subloopindex], recordedbandchannels);
	  vectorMulC_cf32_I(phasecorrconjA[i], conjfftoutputs[LcpIndex][subloopindex], recordedbandchannels);
      }

      //if we need to, do the cross-polar autocorrelations

      if(calccrosspolautocorrs) {	
	status = vectorAddProduct_cf32(fftoutputs[indices[0]][subloopindex], conjfftoutputs[indices[1]][subloopindex], autocorrelations[1][indices[0]], recordedbandchannels);
	if(status != vecNoErr)
	  csevere << startl << "Error in cross-polar autocorrelation!!!" << status << endl;
	status = vectorAddProduct_cf32(fftoutputs[indices[1]][subloopindex], conjfftoutputs[indices[0]][subloopindex], autocorrelations[1][indices[1]], recordedbandchannels);
	if(status != vecNoErr)
	  csevere << startl << "Error in cross-polar autocorrelation!!!" << status << endl;

	//store the weights
        if(perbandweights)
        {
	  weights[1][indices[0]] += perbandweights[subloopindex][indices[0]]*perbandweights[subloopindex][indices[1]];
	  weights[1][indices[1]] += perbandweights[subloopindex][indices[0]]*perbandweights[subloopindex][indices[1]];
        }
        else
        {
	  weights[1][indices[0]] += dataweight[subloopindex];
	  weights[1][indices[1]] += dataweight[subloopindex];
        }
      }
    }
    
    if (linear2circular) {// Delay this as it is possible for linear2circular to be active, but just one pol present
      for (int k=0; k<count; k++) {
	//do the autocorrelation (skipping Nyquist channel)
	status = vectorAddProduct_cf32(fftoutputs[indices[k]][subloopindex], conjfftoutputs[indices[k]][subloopindex], autocorrelations[0][indices[k]], recordedbandchannels);
	if(status != vecNoErr)
	  csevere << startl << "Error in autocorrelation!!!" << status << endl;

	//store the weight
        if(perbandweights)
        {
	  weights[0][indices[k]] += perbandweights[subloopindex][indices[k]];
        }
        else
        {
	  weights[0][indices[k]] += dataweight[subloopindex];
        }
      }
    }
  }
}

LBA_CPUMode::LBA_CPUMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, const s16* unpackvalues)
  : CPUMode(conf,confindex,dsindex,recordedbandchan,chanstoavg,bpersend,gsamples,nrecordedfreqs,recordedbw,recordedfreqclkoffs,recordedfreqclkoffsdelta,recordedfreqphaseoffs,recordedfreqlooffs,nrecordedbands,nzoombands,nbits,Configuration::REAL,Configuration::DOUBLE, recordedbandchan*2,fbank,linear2circular,fringerotorder,arraystridelen,cacorrs,(recordedbw<16.0)?recordedbw*2.0:32.0)
{
  int shift, outputshift;
  int count = 0;
  int numtimeshifts = (sizeof(u16)*bytesperblockdenominator)/bytesperblocknumerator;

  // CJP 18-Feb
  // The following line might be needed in some corner cases. I found it in some
  // uncommited test code. I cannot remember why I added it. I have not activated it as
  // it obviously is not normally needed. Leaving it here to maybe help future debugging

  //if (numtimeshifts==0) numtimeshifts = 1;

  //build the lookup table - NOTE ASSUMPTION THAT THE BYTE ORDER IS **LITTLE-ENDIAN**!!!
  for(u16 i=0;i<MAX_U16;i++)
  {
    shift = 0;
    for(int j=0;j<numtimeshifts;j++)
    {
      for(int k=0;k<numrecordedbands;k++)
      {
        for(int l=0;l<samplesperblock;l++)
        {
          if(samplesperblock > 1 && numrecordedbands > 1) //32 MHz or 64 MHz dual pol
            if(samplesperblock == 4) //64 MHz
              outputshift = 3*(2-l) - 3*k;
            else
              outputshift = -k*samplesperblock + k + l;
          else if (samplesperblock == 4) //64 MHz single pol
	    outputshift = -2*l + 3;
	  else
            outputshift = 0;

          //if(samplesperblock > 1 && numinputbands > 1) //32 MHz or 64 MHz dual pol
          //  outputshift = (2 - (k + l))*(samplesperblock-1);
          //else
          //  outputshift = 0;

          //littleendian means that the shift, starting from 0, will go through the MS byte, then the LS byte, just as we'd like
          lookup[count + outputshift] = unpackvalues[(i >> shift) & 0x03];
          shift += 2;
          count++;
        }
      }
    }
  }

  // Get the last values, i = 1111111111111111
  for (int i=0;i<samplesperlookup;i++)
  {
    lookup[count + i] = unpackvalues[3]; //every sample is 11 = 3
  }
}

LBA8Bit_CPUMode::LBA8Bit_CPUMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs)
: CPUMode(conf,confindex,dsindex,recordedbandchan,chanstoavg,bpersend,gsamples,nrecordedfreqs,recordedbw,recordedfreqclkoffs,recordedfreqclkoffsdelta,recordedfreqphaseoffs,recordedfreqlooffs,nrecordedbands,nzoombands,nbits,Configuration::REAL,Configuration::SINGLE,recordedbandchan*2,fbank,linear2circular,fringerotorder,arraystridelen,cacorrs,recordedbw*2.0)
{}

float LBA8Bit_CPUMode::unpack(int sampleoffset)
{
  unsigned char * packed = (unsigned char *)(&(data[((unpackstartsamples/samplesperblock)*bytesperblocknumerator)/bytesperblockdenominator]));

  for(int i=0;i<unpacksamples;i++)
  {
    for(int j=0;j<numrecordedbands;j++)
    {
      unpackedarrays[j][i] = (float)(*packed) - 128.0;
      packed++;
    }
  }
  return 1.0;
}

LBA16Bit_CPUMode::LBA16Bit_CPUMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs)
  : CPUMode(conf,confindex,dsindex,recordedbandchan,chanstoavg,bpersend,gsamples,nrecordedfreqs,recordedbw,recordedfreqclkoffs,recordedfreqclkoffsdelta,recordedfreqphaseoffs,recordedfreqlooffs,nrecordedbands,nzoombands,nbits,Configuration::REAL,Configuration::SINGLE,recordedbandchan*2,fbank,linear2circular,fringerotorder,arraystridelen,cacorrs,recordedbw*2.0)
{}

float LBA16Bit_CPUMode::unpack(int sampleoffset)
{
  unsigned short * packed = (unsigned short *)(&(data[((unpackstartsamples/samplesperblock)*bytesperblocknumerator)/bytesperblockdenominator]));

  for(int i=0;i<unpacksamples;i++)
  {
    for(int j=0;j<numrecordedbands;j++)
    {
      unpackedarrays[j][i] = (float)(*packed) - 32768.0;
      packed++;
    }
  }
  return 1.0;
}

const s16 LBA_CPUMode::stdunpackvalues[] = {MAX_S16/4, -MAX_S16/4 - 1, 3*MAX_S16/4, -3*MAX_S16/4 - 1};
const s16 LBA_CPUMode::vsopunpackvalues[] = {-3*MAX_S16/4 - 1, MAX_S16/4, -MAX_S16/4 - 1, 3*MAX_S16/4};
// vim: shiftwidth=2:softtabstop=2:expandtab
