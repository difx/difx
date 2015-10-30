/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <mpi.h>
#include <iomanip>
#include "mode.h"
#include "math.h"
#include "architecture.h"
#include "alert.h"
#include "pcal.h"

//using namespace std;
const float Mode::TINY = 0.000000001;

Mode::Mode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, Configuration::datasampling sampling, Configuration::complextype tcomplex, int unpacksamp, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, double bclock)
  : config(conf), configindex(confindex), datastreamindex(dsindex), recordedbandchannels(recordedbandchan), channelstoaverage(chanstoavg), blockspersend(bpersend), guardsamples(gsamples), fftchannels(recordedbandchan*2), numrecordedfreqs(nrecordedfreqs), numrecordedbands(nrecordedbands), numzoombands(nzoombands), numbits(nbits), unpacksamples(unpacksamp), fringerotationorder(fringerotorder), arraystridelength(arraystridelen), recordedbandwidth(recordedbw), blockclock(bclock), filterbank(fbank), linear2circular(linear2circular), calccrosspolautocorrs(cacorrs), recordedfreqclockoffsets(recordedfreqclkoffs), recordedfreqclockoffsetsdelta(recordedfreqclkoffsdelta), recordedfreqphaseoffset(recordedfreqphaseoffs), recordedfreqlooffsets(recordedfreqlooffs)
{
  int status, localfreqindex, parentfreqindex;
  int decimationfactor = config->getDDecimationFactor(configindex, datastreamindex);
  estimatedbytes = 0;
  double looffsetcorrectioninterval, looffsetphasechange, worstlooffsetphasechange;

  if (sampling==Configuration::COMPLEX) {
    usecomplex=1;
    if (tcomplex==Configuration::DOUBLE) 
      usedouble=1;
    else
      usedouble=0;
  }  else {
    usecomplex=0;
    usedouble=0;
  }


  //Uses bitwise test to check if numchannels is power of 2
  if(!(fftchannels & (fftchannels - 1)))
  {
    isfft = true;
  }
  else
  {
    isfft = false;
  }

  perbandweights = 0;
  model = config->getModel();
  initok = true;
  intclockseconds = int(floor(config->getDClockCoeff(configindex, dsindex, 0)/1000000.0 + 0.5));
  if (usecomplex) fftchannels /=2;

  numfracstrides = numfrstrides = fftchannels/arraystridelength;
  if (usecomplex) numfracstrides *= 2;
  sampletime = 1.0/(2.0*recordedbandwidth); //microseconds
  if (usecomplex) sampletime *= 2.0;
  fftdurationmicrosec = fftchannels*sampletime;  // This is never used??
  flaglength = blockspersend/FLAGS_PER_INT;
  if(blockspersend%FLAGS_PER_INT > 0)
    flaglength++;
  validflags = vectorAlloc_s32(flaglength);
  estimatedbytes += sizeof(s32)*flaglength;
  fractionalLoFreq = false;
  for(int i=0;i<numrecordedfreqs;i++)
  {
    if(config->getDRecordedFreq(configindex, datastreamindex, i) - int(config->getDRecordedFreq(configindex, datastreamindex, i)) > TINY)
      fractionalLoFreq = true;
  }

  //check whether LO offset correction will decorrelate too badly, if used
  looffsetcorrectioninterval = sampletime/1e6;
  if(fringerotorder == 0)
    looffsetcorrectioninterval *= fftchannels;
  worstlooffsetphasechange = 0.0;
  for(int i=0;i<numrecordedfreqs;i++)
  {
    if(fabs(recordedfreqlooffsets[i]) > TINY) {
      looffsetphasechange = fabs(recordedfreqlooffsets[i])*looffsetcorrectioninterval*TWO_PI;
      if(looffsetphasechange > worstlooffsetphasechange)
        worstlooffsetphasechange = looffsetphasechange;
  }
  if(worstlooffsetphasechange > 0.1)
    csevere << startl << "LO offset will lead to significant decorrelation - phase change of " << worstlooffsetphasechange*360/TWO_PI << " degrees between updates!" << endl;
  else if (worstlooffsetphasechange > 0.01)
    cwarn << startl << "LO offset will lead to some decorrelation - phase change of " << worstlooffsetphasechange*360/TWO_PI << " degrees between updates!" << endl;
  }

  //now do the rest of the initialising
  samplesperblock = int(recordedbandwidth*2/blockclock);
  if(samplesperblock == 0)
  {
    cfatal << startl << "Error!!! Samplesperblock is less than 1, current implementation cannot handle this situation.  Aborting!" << endl;
    initok = false;
  }
  else
  {
    int numsamplebits = numbits;
    if (usecomplex) numsamplebits *=2;
    bytesperblocknumerator = (numrecordedbands*samplesperblock*numsamplebits*decimationfactor)/8;
    if(bytesperblocknumerator == 0)
    {
      bytesperblocknumerator = 1;
      bytesperblockdenominator = 8/(numrecordedbands*samplesperblock*numsamplebits*decimationfactor);
      unpacksamples += bytesperblockdenominator*sizeof(u16)*samplesperblock;
    }
    else
    {
      bytesperblockdenominator = 1;
    }
    samplesperlookup = (numrecordedbands*sizeof(u16)*samplesperblock*bytesperblockdenominator)/bytesperblocknumerator;
    numlookups = (unpacksamples*bytesperblocknumerator)/(bytesperblockdenominator*sizeof(u16)*samplesperblock);
    if(samplesperblock > 1)
      numlookups++;

    unpackedarrays = new f32*[numrecordedbands];
    if (usecomplex) unpackedcomplexarrays = new cf32*[numrecordedbands];
    for(int i=0;i<numrecordedbands;i++) {
      unpackedarrays[i] = vectorAlloc_f32(unpacksamples);
      estimatedbytes += sizeof(f32)*unpacksamples;
      if (usecomplex) unpackedcomplexarrays[i] = (cf32*) unpackedarrays[i];
    }

    interpolator = new f64[3];

    fftoutputs = new cf32**[numrecordedbands + numzoombands];
    conjfftoutputs = new cf32**[numrecordedbands + numzoombands];
    estimatedbytes += 4*(numrecordedbands + numzoombands);
    for(int j=0;j<numrecordedbands+numzoombands;j++)
    {
      fftoutputs[j] = new cf32*[config->getNumBufferedFFTs(confindex)];
      conjfftoutputs[j] = new cf32*[config->getNumBufferedFFTs(confindex)];
      for(int k=0;k<config->getNumBufferedFFTs(confindex);k++)
      {
        if(j<numrecordedbands)
        {
	  if(fringerotationorder == 0) // post-F
	  {
	    fftoutputs[j][k] = vectorAlloc_cf32(recordedbandchannels+1);
            conjfftoutputs[j][k] = vectorAlloc_cf32(recordedbandchannels+1);
	  }
	  else
	  {
            fftoutputs[j][k] = vectorAlloc_cf32(recordedbandchannels);
            conjfftoutputs[j][k] = vectorAlloc_cf32(recordedbandchannels);
	  }
          estimatedbytes += 2*sizeof(cf32)*recordedbandchannels;
        }
        else
        {
          localfreqindex = config->getDLocalZoomFreqIndex(confindex, dsindex, j-numrecordedbands);
          parentfreqindex = config->getDZoomFreqParentFreqIndex(confindex, dsindex, localfreqindex);
          fftoutputs[j][k] = 0;
          conjfftoutputs[j][k] = 0;
          for(int l=0;l<numrecordedbands;l++) {
            if(config->getDLocalRecordedFreqIndex(confindex, dsindex, l) == parentfreqindex && config->getDRecordedBandPol(confindex, dsindex, l) == config->getDZoomBandPol(confindex, dsindex, j-numrecordedbands)) {
              fftoutputs[j][k] = &(fftoutputs[l][k][config->getDZoomFreqChannelOffset(confindex, dsindex, localfreqindex)]);
	      conjfftoutputs[j][k] = &(conjfftoutputs[l][k][config->getDZoomFreqChannelOffset(confindex, dsindex, localfreqindex)]);
            }
          }
          if(fftoutputs[j][k] == 0)
            csevere << startl << "Couldn't find the parent band for zoom band " << j-numrecordedbands << endl;
        }
      }
    }
    dataweight = 0.0;

    lookup = vectorAlloc_s16((MAX_U16+1)*samplesperlookup);
    linearunpacked = vectorAlloc_s16(numlookups*samplesperlookup);
    estimatedbytes += 2*(numlookups*samplesperlookup + (MAX_U16+1)*samplesperlookup);

    //initialise the fft info
    order = 0;
    while((fftchannels) >> order != 1)
      order++;
    flag = vecFFT_NoReNorm;
    hint = vecAlgHintFast;

    switch(fringerotationorder) {
      case 2: // Quadratic
        piecewiserotator = vectorAlloc_cf32(arraystridelength);
        quadpiecerotator = vectorAlloc_cf32(arraystridelength);
        estimatedbytes += 2*8*arraystridelength;

        subquadxval  = vectorAlloc_f64(arraystridelength);
        subquadphase = vectorAlloc_f64(arraystridelength);
        subquadarg   = vectorAlloc_f32(arraystridelength);
        subquadsin   = vectorAlloc_f32(arraystridelength);
        subquadcos   = vectorAlloc_f32(arraystridelength);
        estimatedbytes += (8+8+4+4+4)*arraystridelength;

        stepxoffsquared = vectorAlloc_f64(numfrstrides);
        tempstepxval = vectorAlloc_f64(numfrstrides);
        estimatedbytes += 16*numfrstrides;
      case 1:
        subtoff  = vectorAlloc_f64(arraystridelength);
        subtval  = vectorAlloc_f64(arraystridelength);
        subxoff  = vectorAlloc_f64(arraystridelength);
        subxval  = vectorAlloc_f64(arraystridelength);
        subphase = vectorAlloc_f64(arraystridelength);
        subarg   = vectorAlloc_f32(arraystridelength);
        subsin   = vectorAlloc_f32(arraystridelength);
        subcos   = vectorAlloc_f32(arraystridelength);
        estimatedbytes += (3*8+3*4)*arraystridelength;

        steptoff  = vectorAlloc_f64(numfrstrides);
        steptval  = vectorAlloc_f64(numfrstrides);
        stepxoff  = vectorAlloc_f64(numfrstrides);
        stepxval  = vectorAlloc_f64(numfrstrides);
        stepphase = vectorAlloc_f64(numfrstrides);
        steparg   = vectorAlloc_f32(numfrstrides);
        stepsin   = vectorAlloc_f32(numfrstrides);
        stepcos   = vectorAlloc_f32(numfrstrides);
        stepcplx  = vectorAlloc_cf32(numfrstrides);
        estimatedbytes += (3*8+3*4+8)*numfrstrides;

        complexunpacked = vectorAlloc_cf32(fftchannels);
        complexrotator = vectorAlloc_cf32(fftchannels);
        fftd = vectorAlloc_cf32(fftchannels);
        estimatedbytes += 3*sizeof(cf32)*fftchannels;

        for(int i=0;i<arraystridelength;i++) {
          subxoff[i] = (double(i)/double(fftchannels));
          subtoff[i] = i*sampletime/1e6;
        }
        for(int i=0;i<numfrstrides;i++) {
          stepxoff[i] = double(i*arraystridelength)/double(fftchannels);
          steptoff[i] = i*arraystridelength*sampletime/1e6;
        }
        if(fringerotationorder == 2) { // Quadratic
          for(int i=0;i<numfrstrides;i++)
            stepxoffsquared[i] = stepxoff[i]*stepxoff[i];
        }

        if (isfft) {
          status = vectorInitFFTC_cf32(&pFFTSpecC, order, flag, hint);
          if (status != vecNoErr)
            csevere << startl << "Error in FFT initialisation!!!" << status << endl;
          status = vectorGetFFTBufSizeC_cf32(pFFTSpecC, &fftbuffersize);
          if (status != vecNoErr)
            csevere << startl << "Error in FFT buffer size calculation!!!" << status << endl;
        }
        else {
          status = vectorInitDFTC_cf32(&pDFTSpecC, fftchannels, flag, hint);
          if(status != vecNoErr)
            csevere << startl << "Error in DFT initialisation!!!" << status << endl;
          status = vectorGetDFTBufSizeC_cf32(pDFTSpecC, &fftbuffersize);
          if (status != vecNoErr)
            csevere << startl << "Error in DFT buffer size calculation!!!" << status << endl;
        }
        break;
      case 0: //zeroth order interpolation, can do "post-F"
        if (isfft) {
          status = vectorInitFFTR_f32(&pFFTSpecR, order, flag, hint);
          if (status != vecNoErr)
            csevere << startl << "Error in FFT initialisation!!!" << status << endl;
          status = vectorGetFFTBufSizeR_f32(pFFTSpecR, &fftbuffersize);
          if (status != vecNoErr)
            csevere << startl << "Error in FFT buffer size calculation!!!" << status << endl;
        }
        else {
          status = vectorInitDFTR_f32(&pDFTSpecR, fftchannels, flag, hint);
          if (status != vecNoErr)
            csevere << startl << "Error in DFT initialisation!!!" << status << endl;
          status = vectorGetDFTBufSizeR_f32(pDFTSpecR, &fftbuffersize);
          if (status != vecNoErr)
            csevere << startl << "Error in DFT buffer size calculation!!!" << status << endl;
        }
        break;
    }

    fftbuffer = vectorAlloc_u8(fftbuffersize);
    estimatedbytes += fftbuffersize;

    subfracsamparg = vectorAlloc_f32(arraystridelength);
    subfracsampsin = vectorAlloc_f32(arraystridelength);
    subfracsampcos = vectorAlloc_f32(arraystridelength);
    subchannelfreqs = vectorAlloc_f32(arraystridelength);
    estimatedbytes += 5*4*arraystridelength;
    /*cout << "subfracsamparg is " << subfracsamparg << endl;
    cout << "subfracsampsin is " << subfracsampsin << endl;
    cout << "subfracsampcos is " << subfracsampcos << endl;
    cout << "subchannelfreqs is " << subchannelfreqs << endl; */
    for(int i=0;i<arraystridelength;i++) {
      subchannelfreqs[i] = (float)((TWO_PI*(i)*recordedbandwidth)/recordedbandchannels);
    }

    stepfracsamparg = vectorAlloc_f32(numfracstrides/2);
    stepfracsampsin = vectorAlloc_f32(numfracstrides/2);
    stepfracsampcos = vectorAlloc_f32(numfracstrides/2);
    stepfracsampcplx = vectorAlloc_cf32(numfracstrides/2);
    stepchannelfreqs = vectorAlloc_f32(numfracstrides/2);
    lsbstepchannelfreqs = vectorAlloc_f32(numfracstrides/2);
    estimatedbytes += (5*2+4)*numfracstrides;

    for(int i=0;i<numfracstrides/2;i++) {
      stepchannelfreqs[i] = (float)((TWO_PI*i*arraystridelength*recordedbandwidth)/recordedbandchannels);
      lsbstepchannelfreqs[i] = (float)((-TWO_PI*((numfracstrides/2-i)*arraystridelength)*recordedbandwidth)/recordedbandchannels);
    }

    deltapoloffsets = false;
    phasepoloffset = false;
    for (int i=0; i<numrecordedfreqs; i++) {
      if (recordedfreqclockoffsetsdelta[i]!=0.0) {
	deltapoloffsets = true;
      }
      if (recordedfreqphaseoffset[i]!=0.0) {
	phasepoloffset = true;
      }
    }

    fracsamprotatorA = vectorAlloc_cf32(recordedbandchannels);
    if (deltapoloffsets) {
      fracsamprotatorB = vectorAlloc_cf32(recordedbandchannels);
    } else {
      fracsamprotatorB = fracsamprotatorA;
    }

    estimatedbytes += 8*recordedbandchannels;
    /*cout << "Numstrides is " << numstrides << ", recordedbandchannels is " << recordedbandchannels << ", arraystridelength is " << arraystridelength << endl;
    cout << "fracsamprotator is " << fracsamprotator << endl;
    cout << "stepchannelfreqs[5] is " << stepchannelfreqs[5] << endl;
    cout << "subchannelfreqs[5] is " << subchannelfreqs[5] << endl;
    cout << "stepchannelfreqs(last) is " << stepchannelfreqs[numstrides/2-1] << endl;
    fracmult = vectorAlloc_f32(recordedbandchannels + 1);
    fracmultcos = vectorAlloc_f32(recordedbandchannels + 1);
    fracmultsin = vectorAlloc_f32(recordedbandchannels + 1);
    complexfracmult = vectorAlloc_cf32(recordedbandchannels + 1);

    channelfreqs = vectorAlloc_f32(recordedbandchannels + 1);
    for(int i=0;i<recordedbandchannels + 1;i++)
      channelfreqs[i] = (float)((TWO_PI*i*recordedbandwidth)/recordedbandchannels);
    lsbchannelfreqs = vectorAlloc_f32(recordedbandchannels + 1);
    for(int i=0;i<recordedbandchannels + 1;i++)
      lsbchannelfreqs[i] = (float)((-TWO_PI*(recordedbandchannels-i)*recordedbandwidth)/recordedbandchannels);
    */

    //space for the autocorrelations
    if(calccrosspolautocorrs)
      autocorrwidth = 2;
    else
      autocorrwidth = 1;
    autocorrelations = new cf32**[autocorrwidth];
    weights = new f32*[autocorrwidth];
    for(int i=0;i<autocorrwidth;i++)
    {
      weights[i] = new f32[numrecordedbands];
      estimatedbytes += sizeof(f32)*numrecordedbands;
      autocorrelations[i] = new cf32*[numrecordedbands+numzoombands];
      for(int j=0;j<numrecordedbands;j++) {
        autocorrelations[i][j] = vectorAlloc_cf32(recordedbandchannels);
        estimatedbytes += 8*recordedbandchannels;
      }
      for(int j=0;j<numzoombands;j++)
      {
        localfreqindex = config->getDLocalZoomFreqIndex(confindex, dsindex, j);
        parentfreqindex = config->getDZoomFreqParentFreqIndex(confindex, dsindex, localfreqindex);
        autocorrelations[i][j+numrecordedbands] = 0;
        for(int l=0;l<numrecordedbands;l++) {
          if(config->getDLocalRecordedFreqIndex(confindex, dsindex, l) == parentfreqindex && config->getDRecordedBandPol(confindex, dsindex, l) == config->getDZoomBandPol(confindex, dsindex, j)) {
            autocorrelations[i][j+numrecordedbands] = &(autocorrelations[i][l][config->getDZoomFreqChannelOffset(confindex, dsindex, localfreqindex)/channelstoaverage]);
          }
        }
        if(autocorrelations[i][j+numrecordedbands] == 0)
          csevere << startl << "Couldn't find the parent band for autocorr of zoom band " << j << endl;
      }
    }

    //kurtosis-specific stuff
    dumpkurtosis = false; //off by default
    s1 = 0;
    s2 = 0;
    sk = 0;
    kscratch = 0;
  }
  // Phase cal stuff
  if(config->getDPhaseCalIntervalMHz(configindex, datastreamindex))
  {
    pcalresults = new cf32*[numrecordedbands];
    extractor = new PCal*[numrecordedbands];
    pcalnbins = new int[numrecordedbands];
    for(int i=0;i<numrecordedbands;i++)
    {
      int pcalOffset,
          lsb;
      
      localfreqindex = conf->getDLocalRecordedFreqIndex(confindex, dsindex, i);

      pcalresults[i] = new cf32[conf->getDRecordedFreqNumPCalTones(configindex, dsindex, localfreqindex)];

      pcalOffset = config->getDRecordedFreqPCalOffsetsHz(configindex, dsindex, localfreqindex);

      lsb = config->getDRecordedLowerSideband(configindex, datastreamindex, localfreqindex);

      extractor[i] = PCal::getNew(1e6*recordedbandwidth, 
                                  1e6*config->getDPhaseCalIntervalMHz(configindex, datastreamindex),
                                      pcalOffset, 0, usecomplex, lsb);

      estimatedbytes += extractor[i]->getEstimatedBytes();

      if (extractor[i]->getLength() != conf->getDRecordedFreqNumPCalTones(configindex, dsindex, localfreqindex))
        csevere << startl << "Developer Error: configuration.cpp and pcal.cpp do not agree on the number of tones: " << extractor[i]->getLength() << " != " << conf->getDRecordedFreqNumPCalTones(configindex, dsindex, localfreqindex) << " ." << endl;
      pcalnbins[i] = extractor[i]->getNBins();
    }
  }

  if (linear2circular || phasepoloffset ) {
    
    tmpvec = new cf32[recordedbandchannels];

    phasecorrA = new cf32[numrecordedfreqs];
    phasecorrconjA = new cf32[numrecordedfreqs];
    phasecorrB = new cf32[numrecordedfreqs];
    phasecorrconjB = new cf32[numrecordedfreqs];
    double degphase;
    for (int i=0; i<numrecordedfreqs; i++) {
      degphase = -recordedfreqphaseoffset[i]/2;
      phasecorrA[i].re = cos(degphase*M_PI/180.0);
      phasecorrA[i].im = -sin(degphase*M_PI/180.0);
      phasecorrconjA[i].re = phasecorrA[i].re;
      phasecorrconjA[i].im = -phasecorrA[i].im;

      if (linear2circular) {
	degphase = 90+recordedfreqphaseoffset[i]/2;
      } else {
	degphase = recordedfreqphaseoffset[i]/2;
      }
      phasecorrB[i].re = cos(degphase*M_PI/180.0);
      phasecorrB[i].im = -sin(degphase*M_PI/180.0);
      phasecorrconjB[i].re = phasecorrA[i].re;
      phasecorrconjB[i].im = -phasecorrA[i].im;
    }
  }
}

Mode::~Mode()
{
  int status;

  if(perbandweights)
  {
    delete [] perbandweights;
  }
  vectorFree(validflags);
  for(int j=0;j<numrecordedbands+numzoombands;j++)
  {
    for(int k=0;k<config->getNumBufferedFFTs(configindex);k++)
    {
      if(j<numrecordedbands) {
        vectorFree(fftoutputs[j][k]);
        vectorFree(conjfftoutputs[j][k]);
      }
    }
    delete [] fftoutputs[j];
    delete [] conjfftoutputs[j];
  }
  delete [] fftoutputs;
  delete [] conjfftoutputs;
  delete [] interpolator;

  for(int i=0;i<numrecordedbands;i++)
    vectorFree(unpackedarrays[i]);
  delete [] unpackedarrays;

  switch(fringerotationorder) {
    case 2: // Quadratic
      vectorFree(piecewiserotator);
      vectorFree(quadpiecerotator);

      vectorFree(subquadxval);
      vectorFree(subquadphase);
      vectorFree(subquadarg);
      vectorFree(subquadsin);
      vectorFree(subquadcos);

      vectorFree(stepxoffsquared);
      vectorFree(tempstepxval);
    case 1:
      vectorFree(subtoff);
      vectorFree(subtval);
      vectorFree(subxoff);
      vectorFree(subxval);
      vectorFree(subphase);
      vectorFree(subarg);
      vectorFree(subsin);
      vectorFree(subcos);

      vectorFree(steptoff);
      vectorFree(steptval);
      vectorFree(stepxoff);
      vectorFree(stepxval);
      vectorFree(stepphase);
      vectorFree(steparg);
      vectorFree(stepsin);
      vectorFree(stepcos);
      vectorFree(stepcplx);

      vectorFree(complexunpacked);
      vectorFree(complexrotator);
      vectorFree(fftd);

      if(isfft) {
        status = vectorFreeFFTC_cf32(pFFTSpecC);
        if (status != vecNoErr)
          csevere << startl << "Error in freeing FFT spec!!!" << status << endl;
      }
      else{
        status = vectorFreeDFTC_cf32(pDFTSpecC);
        if (status != vecNoErr)
          csevere << startl << "Error in freeing DFT spec!!!" << status << endl;
      }
      break;
    case 0: //zeroth order interpolation, "post-F"
      if(isfft) {
        status = vectorFreeFFTR_f32(pFFTSpecR);
        if (status != vecNoErr)
          csevere << startl << "Error in freeing FFT spec!!!" << status << endl;
      }
      else{
        status = vectorFreeDFTR_f32(pDFTSpecR);
        if (status != vecNoErr)
          csevere << startl << "Error in freeing DFT spec!!!" << status << endl;
      }
      break;
  }

  vectorFree(lookup);
  vectorFree(linearunpacked);
  vectorFree(fftbuffer);

  vectorFree(subfracsamparg);
  vectorFree(subfracsampsin);
  vectorFree(subfracsampcos);
  vectorFree(subchannelfreqs);

  vectorFree(stepfracsamparg);
  vectorFree(stepfracsampsin);
  vectorFree(stepfracsampcos);
  vectorFree(stepfracsampcplx);
  vectorFree(stepchannelfreqs);
  vectorFree(lsbstepchannelfreqs);

  vectorFree(fracsamprotatorA);
  if (deltapoloffsets) vectorFree(fracsamprotatorB);
  //vectorFree(fracmult);
  //vectorFree(fracmultcos);
  //vectorFree(fracmultsin);
  //vectorFree(complexfracmult);
  //vectorFree(channelfreqs);

  for(int i=0;i<autocorrwidth;i++)
  {
    for(int j=0;j<numrecordedbands;j++)
      vectorFree(autocorrelations[i][j]);
    delete [] autocorrelations[i];
    delete [] weights[i];
  }
  delete [] weights;
  delete [] autocorrelations;

  if(config->getDPhaseCalIntervalMHz(configindex, datastreamindex))
  {
    for(int i=0;i<numrecordedbands;i++) {
       delete extractor[i];
       delete[] pcalresults[i];
    }
    delete[] pcalresults;
    delete[] extractor;
    delete[] pcalnbins;
  }

  if(sk != 0) //also need to delete kurtosis stuff
  {
    for(int i=0;i<numrecordedbands;i++)
    {
      vectorFree(s1[i]);
      vectorFree(s2[i]);
      vectorFree(sk[i]);
    }
    delete [] s1;
    delete [] s2;
    delete [] sk;
    vectorFree(kscratch);
  }

  if (linear2circular) {
    delete [] tmpvec;
  }
}

float Mode::unpack(int sampleoffset)
{
  int status, leftoversamples, stepin = 0;

  if(bytesperblockdenominator/bytesperblocknumerator == 0)
    leftoversamples = 0;
  else
    leftoversamples = sampleoffset%(bytesperblockdenominator/bytesperblocknumerator);
  unpackstartsamples = sampleoffset - leftoversamples;
  if(samplesperblock > 1)
    stepin = unpackstartsamples%(samplesperblock*bytesperblockdenominator);
  u16 * packed = (u16 *)(&(data[((unpackstartsamples/samplesperblock)*bytesperblocknumerator)/bytesperblockdenominator]));

  //copy from the lookup table to the linear unpacked array
  for(int i=0;i<numlookups;i++)
  {
    status = vectorCopy_s16(&lookup[packed[i]*samplesperlookup], &linearunpacked[i*samplesperlookup], samplesperlookup);
    if(status != vecNoErr) {
      csevere << startl << "Error in lookup for unpacking!!!" << status << endl;
      return 0;
    }
  }

  //split the linear unpacked array into the separate subbands
  status = vectorSplitScaled_s16f32(&(linearunpacked[stepin*numrecordedbands]), unpackedarrays, numrecordedbands, unpacksamples);
  if(status != vecNoErr) {
    csevere << startl << "Error in splitting linearunpacked!!!" << status << endl;
    return 0;
  }

  return 1.0;
}

float Mode::process(int index, int subloopindex)  //frac sample error is in microseconds 
{
  double phaserotation, averagedelay, nearestsampletime, starttime, lofreq, walltimesecs, fracwalltime, fftcentre, d0, d1, d2, fraclooffset;
  f32 phaserotationfloat, fracsampleerror;
  int status, count, nearestsample, integerdelay, RcpIndex, LcpIndex, intwalltime;
  cf32* fftptr;
  f32* currentstepchannelfreqs;
  int indices[10];
  bool looff, isfraclooffset;
  //cout << "For Mode of datastream " << datastreamindex << ", index " << index << ", validflags is " << validflags[index/FLAGS_PER_INT] << ", after shift you get " << ((validflags[index/FLAGS_PER_INT] >> (index%FLAGS_PER_INT)) & 0x01) << endl;
  
  if((datalengthbytes <= 1) || (offsetseconds == INVALID_SUBINT) || (((validflags[index/FLAGS_PER_INT] >> (index%FLAGS_PER_INT)) & 0x01) == 0))
  {
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
    return 0.0; //don't process crap data
  }

  fftcentre = index+0.5;
  averagedelay = interpolator[0]*fftcentre*fftcentre + interpolator[1]*fftcentre + interpolator[2];
  fftstartmicrosec = index*fftchannels*sampletime; //CHRIS CHECK
  starttime = (offsetseconds-datasec)*1000000.0 + (static_cast<long long>(offsetns) - static_cast<long long>(datans))/1000.0 + fftstartmicrosec - averagedelay;
  nearestsample = int(starttime/sampletime + 0.5);
  walltimesecs = model->getScanStartSec(currentscan, config->getStartMJD(), config->getStartSeconds()) + offsetseconds + offsetns/1.0e9 + fftstartmicrosec/1.0e6;
  intwalltime = static_cast<int>(walltimesecs);
  fracwalltime = walltimesecs - intwalltime;

  //if we need to, unpack some more data - first check to make sure the pos is valid at all
  //cout << "Datalengthbytes for " << datastreamindex << " is " << datalengthbytes << endl;
  //cout << "Fftchannels for " << datastreamindex << " is " << fftchannels << endl;
  //cout << "samplesperblock for " << datastreamindex << " is " << samplesperblock << endl;
  //cout << "nearestsample for " << datastreamindex << " is " << nearestsample << endl;
  //cout << "bytesperblocknumerator for " << datastreamindex << " is " << bytesperblocknumerator << endl;
  //cout << "bytesperblockdenominator for " << datastreamindex << " is " << bytesperblockdenominator << endl;
  if(nearestsample < -1 || (((nearestsample + fftchannels)/samplesperblock)*bytesperblocknumerator)/bytesperblockdenominator > datalengthbytes)
  {
    cerror << startl << "MODE error for datastream " << datastreamindex << " - trying to process data outside range - aborting!!! nearest sample was " << nearestsample << ", the max bytes should be " << datalengthbytes << ".  offsetseconds was " << offsetseconds << ", offsetns was " << offsetns << ", index was " << index << ", average delay was " << averagedelay << ", datasec was " << datasec << ", datans was " << datans << ", fftstartmicrosec was " << fftstartmicrosec << endl;
    for(int i=0;i<numrecordedbands;i++)
    {
      status = vectorZero_cf32(fftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
      status = vectorZero_cf32(conjfftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
    }
    return 0.0;
  }
  if(nearestsample == -1)
  {
    nearestsample = 0;
    dataweight = unpack(nearestsample);
  }
  else if(nearestsample < unpackstartsamples || nearestsample > unpackstartsamples + unpacksamples - fftchannels)
    //need to unpack more data
    dataweight = unpack(nearestsample);

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

  if(!(dataweight > 0.0)) {
    for(int i=0;i<numrecordedbands;i++)
    {
      status = vectorZero_cf32(fftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
      status = vectorZero_cf32(conjfftoutputs[i][subloopindex], recordedbandchannels);
      if(status != vecNoErr)
        csevere << startl << "Error trying to zero fftoutputs when data is bad!" << endl;
    }
    return 0.0;
  }

  nearestsampletime = nearestsample*sampletime;
  fracsampleerror = float(starttime - nearestsampletime);

  if(!(config->getDPhaseCalIntervalMHz(configindex, datastreamindex) == 0)) 
  {
      for(int i=0;i<numrecordedbands;i++)
      {
        extractor[i]->adjustSampleOffset(datasamples+nearestsample);
        if (!usecomplex)
	        status = extractor[i]->extractAndIntegrate (&(unpackedarrays[i][nearestsample
	                 - unpackstartsamples]), fftchannels);
        else
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
    if(config->getDRecordedLowerSideband(configindex, datastreamindex, i))
    {
      currentstepchannelfreqs = lsbstepchannelfreqs;
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

    //get ready to apply fringe rotation, if it is pre-F
    lofreq = config->getDRecordedFreq(configindex, datastreamindex, i);
    if (usecomplex && usedouble) {
      if (config->getDRecordedLowerSideband(configindex, datastreamindex, i)) {
	lofreq -= config->getDRecordedBandwidth(configindex, datastreamindex, i);
      } else {
	lofreq += config->getDRecordedBandwidth(configindex, datastreamindex, i);
      }
    }

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
    status = vectorMulC_f32(subchannelfreqs, fracsampleerror - recordedfreqclockoffsets[i] + recordedfreqclockoffsetsdelta[i]/2, subfracsamparg, arraystridelength);
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
      status = vectorMulC_f32(subchannelfreqs, fracsampleerror - recordedfreqclockoffsets[i] - recordedfreqclockoffsetsdelta[i]/2, subfracsamparg, arraystridelength); 
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
          case 2: // Quadratic
            if (usecomplex) {
              status = vectorMul_cf32(complexrotator, &unpackedcomplexarrays[j][nearestsample - unpackstartsamples], complexunpacked, fftchannels);
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
            else{
              status = vectorDFT_CtoC_cf32(complexunpacked, fftd, pDFTSpecC, fftbuffer);
              if(status != vecNoErr)
                csevere << startl << "Error doing the DFT!!!" << endl;
            }

            if(config->getDRecordedLowerSideband(configindex, datastreamindex, i)) {
	      if (usecomplex) {
		if (usedouble) {
		  status = vectorFlip_cf32(fftd, fftoutputs[j][subloopindex], recordedbandchannels/2+1);
		  status = vectorFlip_cf32(&fftd[recordedbandchannels/2]+1, &fftoutputs[j][subloopindex][recordedbandchannels/2]+1, recordedbandchannels/2-1);
		} else {
              status = vectorCopy_cf32(fftd, fftoutputs[j][subloopindex], recordedbandchannels);
              }
	      } else {
		status = vectorCopy_cf32(&(fftd[recordedbandchannels]), fftoutputs[j][subloopindex], recordedbandchannels);
	      }
            }
            else {
	      if (usecomplex && usedouble) {
		status = vectorCopy_cf32(fftd, &fftoutputs[j][subloopindex][recordedbandchannels/2], recordedbandchannels/2);
		status = vectorCopy_cf32(&fftd[recordedbandchannels/2], fftoutputs[j][subloopindex], recordedbandchannels/2);
	      } else 
		status = vectorCopy_cf32(fftd, fftoutputs[j][subloopindex], recordedbandchannels);
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

	if (!linear2circular) {
	  //do the autocorrelation (skipping Nyquist channel)
	  status = vectorAddProduct_cf32(fftoutputs[j][subloopindex], conjfftoutputs[j][subloopindex], autocorrelations[0][j], recordedbandchannels);
	  if(status != vecNoErr)
	    csevere << startl << "Error in autocorrelation!!!" << status << endl;

	  //store the weight
          if(perbandweights)
          {
	    weights[0][j] += perbandweights[j];
          }
          else
          {
	    weights[0][j] += dataweight;
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
	  weights[1][indices[0]] += perbandweights[indices[0]]*perbandweights[indices[1]];
	  weights[1][indices[1]] += perbandweights[indices[0]]*perbandweights[indices[1]];
        }
        else
        {
	  weights[1][indices[0]] += dataweight;
	  weights[1][indices[1]] += dataweight;
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
	  weights[0][indices[k]] += perbandweights[indices[k]];
        }
        else
        {
	  weights[0][indices[k]] += dataweight;
        }
      }
    }
  }
  return dataweight;
}

void Mode::averageFrequency()
{
  cf32 tempsum;
  int status, outputchans;

  if(channelstoaverage == 1)
    return; //no need to do anything;

  outputchans = recordedbandchannels/channelstoaverage;
  for(int i=0;i<autocorrwidth;i++)
  {
    for(int j=0;j<numrecordedbands;j++)
    {
      status = vectorMean_cf32(autocorrelations[i][j], channelstoaverage, &tempsum, vecAlgHintFast);
      if(status != vecNoErr)
        cerror << startl << "Error trying to average in frequency!" << endl;
      autocorrelations[i][j][0] = tempsum;
      for(int k=1;k<outputchans;k++)
      {
        status = vectorMean_cf32(&(autocorrelations[i][j][k*channelstoaverage]), channelstoaverage, &(autocorrelations[i][j][k]), vecAlgHintFast);
        if(status != vecNoErr)
          cerror << startl << "Error trying to average in frequency!" << endl;
      }
    }
  }
}

bool Mode::calculateAndAverageKurtosis(int numblocks, int maxchannels)
{
  int status, kchanavg;
  bool nonzero = false;

  for(int i=0;i<numrecordedbands;i++)
  {
    nonzero = false;
    for(int j=0;j<recordedbandchannels;j++)
    {
      if(s1[i][j] > 0.0)
      {
        nonzero = true;
        break;
      }
    }
    if(!nonzero)
      continue;
    status = vectorSquare_f32_I(s1[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Problem calculating kurtosis (squaring s1)" << endl;
    status = vectorMulC_f32_I((f32)numblocks, s2[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Problem calculating kurtosis (scaling s2)" << endl;
    status = vectorDivide_f32(s1[i], s2[i], sk[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Problem calculating kurtosis (s2/s1^2)" << endl;
    status = vectorAddC_f32_I(-1.0, sk[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Problem calculating kurtosis (sk - 1)" << endl;
    status = vectorMulC_f32_I((f32)numblocks/(numblocks - 1.0), sk[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Problem calculating kurtosis (sk * numblocks/(numblocks-1))" << endl;
    status = vectorAddC_f32_I(-1.0, sk[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Problem calculating kurtosis (sk - 1)" << endl;
    if(maxchannels < recordedbandchannels)
    {
      kchanavg = recordedbandchannels/maxchannels;
      status = vectorMulC_f32_I(1.0/((f32)kchanavg), sk[i], recordedbandchannels);
      if(status != vecNoErr)
        cerror << startl << "Problem calculating kurtosis (sk average)" << endl;
      for(int j=0;j<maxchannels;j++)
      {
        sk[i][j] = sk[i][j*kchanavg];
        for(int k=1;k<kchanavg;k++)
          sk[i][j] += sk[i][j*kchanavg + k];
      }
    }
  }

  return nonzero;
}

void Mode::zeroAutocorrelations()
{
  int status;

  for(int i=0;i<autocorrwidth;i++)
  {
    for(int j=0;j<numrecordedbands;j++)
    {
      status = vectorZero_cf32(autocorrelations[i][j], recordedbandchannels);
      if(status != vecNoErr)
        cerror << startl << "Error trying to zero autocorrelations!" << endl;
      weights[i][j] = 0.0;
    }
  }
}

void Mode::zeroKurtosis()
{
  int status;

  if(sk == 0)
  {
    s1 = new f32*[numrecordedbands];
    s2 = new f32*[numrecordedbands];
    sk = new f32*[numrecordedbands];
    kscratch = vectorAlloc_f32(recordedbandchannels);
    for(int i=0;i<numrecordedbands;i++)
    {
      s1[i] = vectorAlloc_f32(recordedbandchannels);
      s2[i] = vectorAlloc_f32(recordedbandchannels);
      sk[i] = vectorAlloc_f32(recordedbandchannels);
    }
  }

  for(int i=0;i<numrecordedbands;i++)
  {
    status = vectorZero_f32(s1[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Error trying to zero kurtosis!" << endl;
    status = vectorZero_f32(s2[i], recordedbandchannels);
    if(status != vecNoErr)
      cerror << startl << "Error trying to zero kurtosis!" << endl;
  }
}

void Mode::setOffsets(int scan, int seconds, int ns)
{
  bool foundok;
  int srcindex;
  currentscan = scan;
  offsetseconds = seconds;
  offsetns = ns;
  if(datasec <= INVALID_SUBINT)
    return; //there is no valid data - this whole subint will be ignored
  if(currentscan != datascan) {
    cerror << startl << "Received a request to process scan " << currentscan << " (" << seconds << "/" << ns << " but received data from scan " << datascan << " (" << datasec << "/" << datans << ") - I'm confused and will ignore this data!" << endl;
    datalengthbytes = 0; //torch the whole subint - can't work with different scans!
    return;
  }

  //fill in the quadratic interpolator values from model
  if(model->getNumPhaseCentres(currentscan) == 1)
    srcindex = 1;
  else
    srcindex = 0;

  f64 timespan = blockspersend*2*recordedbandchannels*sampletime/1e6;
  if (usecomplex) timespan/=2;
  foundok = model->calculateDelayInterpolator(currentscan, (double)offsetseconds + ((double)offsetns)/1000000000.0, timespan, blockspersend, config->getDModelFileIndex(configindex, datastreamindex), srcindex, 2, interpolator);
  interpolator[2] -= 1000000*intclockseconds;

  if(!foundok) {
    cerror << startl << "Could not find a Model interpolator for scan " << scan << " offsetseconds " << seconds << " offsetns " << ns << " - will torch this subint!" << endl;
    offsetseconds = INVALID_SUBINT;
  }
}

void Mode::setValidFlags(s32 * v)
{
  int status = vectorCopy_s32(v, validflags, flaglength);
  if(status != vecNoErr)
    csevere << startl << "Error trying to copy valid data flags!!!" << endl;
}

void Mode::setData(u8 * d, int dbytes, int dscan, int dsec, int dns)
{
  data = d;
  datalengthbytes = dbytes;
  datascan = dscan;
  datasec = dsec;
  datans = dns;
  unpackstartsamples = -999999999;
  datasamples = static_cast<int>(datans/(sampletime*1e3) + 0.5);
}

void Mode::resetpcal()
{
  for(int i=0;i<numrecordedbands;i++)
  {
    extractor[i]->clear();
  }
}

void Mode::finalisepcal()
{
  for(int i=0;i<numrecordedbands;i++)
  {
    uint64_t samples = extractor[i]->getFinalPCal(pcalresults[i]);
    if ((samples == 0) && (datasec != INVALID_SUBINT) && (datalengthbytes > 1)) {
        //cdebug << startl << "finalisepcal band " << i << " samples==0 over valid subint " << datasec << "s+" << datans << "ns" << endl;
    }
  }
}

const float Mode::decorrelationpercentage[] = {0.63662, 0.88, 0.94, 0.96, 0.98, 0.99, 0.996, 0.998}; //note these are just approximate!!!



LBAMode::LBAMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs, const s16* unpackvalues)
  : Mode(conf,confindex,dsindex,recordedbandchan,chanstoavg,bpersend,gsamples,nrecordedfreqs,recordedbw,recordedfreqclkoffs,recordedfreqclkoffsdelta,recordedfreqphaseoffs,recordedfreqlooffs,nrecordedbands,nzoombands,nbits,Configuration::REAL,Configuration::DOUBLE, recordedbandchan*2,fbank,linear2circular,fringerotorder,arraystridelen,cacorrs,(recordedbw<16.0)?recordedbw*2.0:32.0)
{
  int shift, outputshift;
  int count = 0;
  int numtimeshifts = (sizeof(u16)*bytesperblockdenominator)/bytesperblocknumerator;

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

  //get the last values, i = 1111111111111111
  for (int i=0;i<samplesperlookup;i++)
  {
    lookup[count + i] = unpackvalues[3]; //every sample is 11 = 3
  }
}

LBA8BitMode::LBA8BitMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs)
: Mode(conf,confindex,dsindex,recordedbandchan,chanstoavg,bpersend,gsamples,nrecordedfreqs,recordedbw,recordedfreqclkoffs,recordedfreqclkoffsdelta,recordedfreqphaseoffs,recordedfreqlooffs,nrecordedbands,nzoombands,nbits,Configuration::REAL,Configuration::SINGLE,recordedbandchan*2,fbank,linear2circular,fringerotorder,arraystridelen,cacorrs,recordedbw*2.0)
{}

float LBA8BitMode::unpack(int sampleoffset)
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

LBA16BitMode::LBA16BitMode(Configuration * conf, int confindex, int dsindex, int recordedbandchan, int chanstoavg, int bpersend, int gsamples, int nrecordedfreqs, double recordedbw, double * recordedfreqclkoffs, double * recordedfreqclkoffsdelta, double * recordedfreqphaseoffs, double * recordedfreqlooffs, int nrecordedbands, int nzoombands, int nbits, bool fbank, bool linear2circular, int fringerotorder, int arraystridelen, bool cacorrs)
  : Mode(conf,confindex,dsindex,recordedbandchan,chanstoavg,bpersend,gsamples,nrecordedfreqs,recordedbw,recordedfreqclkoffs,recordedfreqclkoffsdelta,recordedfreqphaseoffs,recordedfreqlooffs,nrecordedbands,nzoombands,nbits,Configuration::REAL,Configuration::SINGLE,recordedbandchan*2,fbank,linear2circular,fringerotorder,arraystridelen,cacorrs,recordedbw*2.0)
{}

float LBA16BitMode::unpack(int sampleoffset)
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

const s16 LBAMode::stdunpackvalues[] = {MAX_S16/4, -MAX_S16/4 - 1, 3*MAX_S16/4, -3*MAX_S16/4 - 1};
const s16 LBAMode::vsopunpackvalues[] = {-3*MAX_S16/4 - 1, MAX_S16/4, -MAX_S16/4 - 1, 3*MAX_S16/4};
// vim: shiftwidth=2:softtabstop=2:expandtab
