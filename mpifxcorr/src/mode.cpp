/***************************************************************************
 *   Copyright (C) 2006-2017 by Adam Deller                                *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
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

#include "config.h"
#include <mpi.h>
#include <iomanip>
#include "mode.h"
#include "math.h"
#include "architecture.h"
#include "alert.h"
#include "pcal.h"

#include <cmath>
#include <iostream>

#define PCAL_FINAL_DEBUG_BAND 2
#define PCAL_FINAL_DEBUG_TONE_INDEX 4
#define PCAL_FINAL_DEBUG_RAW 1
#define PCAL_FINAL_DEBUG_BAND        2
#define PCAL_FINAL_DEBUG_TONE_INDEX  4
#define PCAL_FINAL_DEBUG_MAX_BAD_BINS 8

//using namespace std;
const float Mode::TINY = 0.000000001;

#if (ARCH == GENERIC)
pthread_mutex_t FFTinitMutex = PTHREAD_MUTEX_INITIALIZER;
#endif

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


  dataweight = vectorAlloc_f32(config->getNumBufferedFFTs(confindex));
  for(int i=0;i<config->getNumBufferedFFTs(confindex);++i)
  {
    dataweight[i] = 0.0;
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
    double loval = config->getDRecordedFreq(configindex, datastreamindex, i);
    if(usedouble)
    {
      if (config->getDRecordedLowerSideband(configindex, datastreamindex, i)) 
      {
        loval -= config->getDRecordedBandwidth(configindex, datastreamindex, i)/2.0;
      } 
      else 
      {
        loval += config->getDRecordedBandwidth(configindex, datastreamindex, i)/2.0;
      }
    }
    if(fabs(loval - int(loval + 0.5)) > TINY) {
      fractionalLoFreq = true;
    }
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

    interpolator = new f64[3];
    fprintf(stderr, "getNumBufferedFFTs(confindex) = %d\n",
        config->getNumBufferedFFTs(configindex));
    std::cout << "get_use_gpu(): " << config->get_use_gpu() << std::endl;
    
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

        break;
      case 0: //zeroth order interpolation, can do "post-F"
        if (isfft) {
          status = vectorInitFFTR_f32(&pFFTSpecR, order, flag, hint, &fftbuffersize, &fftbuffer);
          if (status != vecNoErr)
            csevere << startl << "Error in FFT initialisation!!!" << status << endl;
        }
        else {
          status = vectorInitDFTR_f32(&pDFTSpecR, fftchannels, flag, hint, &fftbuffersize, &fftbuffer);
          if (status != vecNoErr)
            csevere << startl << "Error in DFT initialisation!!!" << status << endl;
        }
        break;
    }
    estimatedbytes += fftbuffersize;

    unpackedarrays = new f32*[numrecordedbands];
    if (usecomplex) unpackedcomplexarrays = new cf32*[numrecordedbands];
    unpackedarrays[0] = vectorAlloc_f32(numrecordedbands*unpacksamples);
    for(int i=0;i<numrecordedbands;i++) {
      unpackedarrays[i] = unpackedarrays[0] + (i * unpacksamples);//vectorAlloc_f32(unpacksamples);
      estimatedbytes += sizeof(f32)*unpacksamples;
      if (usecomplex) unpackedcomplexarrays[i] = (cf32*) unpackedarrays[i];
    }

    subfracsamparg = vectorAlloc_f32(arraystridelength);
    subfracsampsin = vectorAlloc_f32(arraystridelength);
    subfracsampcos = vectorAlloc_f32(arraystridelength);
    subchannelfreqs = vectorAlloc_f32(arraystridelength);
    ldsbsubchannelfreqs = vectorAlloc_f32(arraystridelength);
    estimatedbytes += 6*4*arraystridelength;
    /*cout << "subfracsamparg is " << subfracsamparg << endl;
    cout << "subfracsampsin is " << subfracsampsin << endl;
    cout << "subfracsampcos is " << subfracsampcos << endl;
    cout << "subchannelfreqs is " << subchannelfreqs << endl; */
    for(int i=0;i<arraystridelength;i++) {
      subchannelfreqs[i] = (float)((TWO_PI*(i)*recordedbandwidth)/recordedbandchannels);
      ldsbsubchannelfreqs[i] = (float)((-TWO_PI*(i)*recordedbandwidth)/recordedbandchannels);
    }

    stepfracsamparg = vectorAlloc_f32(numfracstrides/2);
    stepfracsampsin = vectorAlloc_f32(numfracstrides/2);
    stepfracsampcos = vectorAlloc_f32(numfracstrides/2);
    stepfracsampcplx = vectorAlloc_cf32(numfracstrides/2);
    stepchannelfreqs = vectorAlloc_f32(numfracstrides/2);
    lsbstepchannelfreqs = vectorAlloc_f32(numfracstrides/2);
    dsbstepchannelfreqs = vectorAlloc_f32(numfracstrides/2);
    ldsbstepchannelfreqs = vectorAlloc_f32(numfracstrides/2);
    estimatedbytes += (7*2+4)*numfracstrides;

    for(int i=0;i<numfracstrides/2;i++) {
      stepchannelfreqs[i]     = (float)((TWO_PI*i*arraystridelength*recordedbandwidth)/recordedbandchannels);
      dsbstepchannelfreqs[i]  = (float)((TWO_PI*i*arraystridelength*recordedbandwidth)/recordedbandchannels - TWO_PI*recordedbandwidth/2.0);
      lsbstepchannelfreqs[i]  = (float)((-TWO_PI*((numfracstrides/2-i)*arraystridelength)*recordedbandwidth)/recordedbandchannels);
      //ldsbstepchannelfreqs[i] = (float)((-TWO_PI*((numfracstrides/2-i)*arraystridelength)*recordedbandwidth)/recordedbandchannels + TWO_PI*recordedbandwidth/2.0);
      ldsbstepchannelfreqs[i] = -dsbstepchannelfreqs[i];
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
  PCal::setMinFrequencyResolution(1e6);
  if(config->getDPhaseCalIntervalMHz(configindex, datastreamindex))
  {
    pcalresults = new cf32*[numrecordedbands];
    extractor = new PCal*[numrecordedbands];
    pcalnbins = new int[numrecordedbands];
    for(int i=0;i<numrecordedbands;i++)
    {
      localfreqindex = conf->getDLocalRecordedFreqIndex(confindex, dsindex, i);
      pcalresults[i] = new cf32[conf->getDRecordedFreqNumPCalTones(configindex, dsindex, localfreqindex)];
      extractor[i] = PCal::getNew(1e6*recordedbandwidth,
                                  1e6*config->getDPhaseCalIntervalMHz(configindex, datastreamindex),
                                  config->getDRecordedFreqPCalOffsetsHz(configindex, dsindex, localfreqindex), 0,
                                  sampling, tcomplex);
      if (extractor[i]->getLength() != conf->getDRecordedFreqNumPCalTones(configindex, dsindex, localfreqindex))
        csevere << startl << "Developer Error: configuration.cpp and pcal.cpp do not agree on the number of tones: " << extractor[i]->getLength() << " != " << conf->getDRecordedFreqNumPCalTones(configindex, dsindex, localfreqindex) << " ." << endl;
      estimatedbytes += extractor[i]->getEstimatedBytes();
      pcalnbins[i] = extractor[i]->getNBins();
//      if (pcalOffset>=0)
//        cverbose << startl << "PCal extractor internally uses " << pcalnbins[i] << " spectral channels (" << (long)(1e3*recordedbandwidth/pcalnbins[i]) << " kHz/channel)" << endl;
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
  //std::cout << "In Mode destructor" << std::endl;	  
  

  if(perbandweights)
  {
    //std::cout << "loop 1" << std::endl; 
    for(int i=0;i<config->getNumBufferedFFTs(configindex);++i)
    {
      delete [] perbandweights[i];
    }
    delete [] perbandweights;
  }
  vectorFree(dataweight);
  vectorFree(validflags);
  //std::cout << "loop2" << std::endl;
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
  //std::cout << "post loop 2" << std::endl;
  delete [] fftoutputs;
  delete [] conjfftoutputs;
  delete [] interpolator;
  
  //std::cout << "fringe rotation block" << std::endl;
  switch(fringerotationorder) {
    case 2: // Quadratic
      if(piecewiserotator) vectorFree(piecewiserotator);
      if(quadpiecerotator) vectorFree(quadpiecerotator);

      if(subquadxval) vectorFree(subquadxval);
      if(subquadphase) vectorFree(subquadphase);
      if(subquadarg) vectorFree(subquadarg);
      if(subquadsin) vectorFree(subquadsin);
      if(subquadcos) vectorFree(subquadcos);

      if(stepxoffsquared) vectorFree(stepxoffsquared);
      if(tempstepxval)  vectorFree(tempstepxval);
    case 1:
      if(subtoff) vectorFree(subtoff);
      if(subtval) vectorFree(subtval);
      if(subxoff) vectorFree(subxoff);
      if(subxval) vectorFree(subxval);
      if(subphase) vectorFree(subphase);
      if(subarg) vectorFree(subarg);
      if(subsin) vectorFree(subsin);
      if(subcos) vectorFree(subcos);

      if(steptoff) vectorFree(steptoff);
      if(steptval) vectorFree(steptval);
      if(stepxoff) vectorFree(stepxoff);
      if(stepxval) vectorFree(stepxval);
      if(stepphase) vectorFree(stepphase);
      if(steparg) vectorFree(steparg);
      if(stepsin) vectorFree(stepsin);
      if(stepcos) vectorFree(stepcos);
      if(stepcplx) vectorFree(stepcplx);

      if(complexunpacked) vectorFree(complexunpacked);
      if(complexrotator) vectorFree(complexrotator);
      if(fftd) vectorFree(fftd);
      break;
    case 0: //zeroth order interpolation, "post-F"
      if(isfft) {
	      if(pFFTSpecR) vectorFreeDFTR_f32(pFFTSpecR);
      }
      else{
	      if(pDFTSpecR) vectorFreeDFTR_f32(pDFTSpecR);
      }
      break;
  }
  //std::cout << "post fringe rotation block" << std::endl;


//  std::cout << "lookup" << std::endl;
  if(lookup)              vectorFree(lookup);
//  std::cout << "linearunpacked" << std::endl;
  if(linearunpacked)      vectorFree(linearunpacked);
  //std::cout << "fftbuffer DO I WORK NOW? " << std::endl;

  if (fftbuffer != nullptr) {
      //std::cout << "Why am I doing this?" << std::endl;
      vectorFree(fftbuffer);
  }
  //std::cout << "subfracsamparg" << std::endl;
  if(subfracsamparg)      vectorFree(subfracsamparg);
//  std::cout << "subfracsampsin" << std::endl;
  if(subfracsampsin)      vectorFree(subfracsampsin);
//  std::cout << "subfracsampcos" << std::endl;
  if(subfracsampcos)      vectorFree(subfracsampcos);
//  std::cout << "subchannelfreqs" << std::endl;
  if(subchannelfreqs)      vectorFree(subchannelfreqs);
//  std::cout << "ldsbsubchannelfreqs " << std::endl;
  if(ldsbsubchannelfreqs)      vectorFree(ldsbsubchannelfreqs);
//    std::cout << "stepfracsamparg" << std::endl;

  if(stepfracsamparg)      vectorFree(stepfracsamparg);
//    std::cout << "stepfracsampsin" << std::endl;
  if(stepfracsampsin)      vectorFree(stepfracsampsin);
//    std::cout << "stepfracsampcos" << std::endl;
  if(stepfracsampcos)      vectorFree(stepfracsampcos);
//    std::cout << "stepfracsampcplx" << std::endl;
  if(stepfracsampcplx)     vectorFree(stepfracsampcplx);
//    std::cout << "stepchannelfreqs" << std::endl;
  if(stepchannelfreqs)     vectorFree(stepchannelfreqs);
//    std::cout << "lsbstepchannelfreqs" << std::endl;
  if(lsbstepchannelfreqs)  vectorFree(lsbstepchannelfreqs);
//    std::cout << "dsbstepchannelfreqs" << std::endl;
  if(dsbstepchannelfreqs)  vectorFree(dsbstepchannelfreqs);
//    std::cout << "ldsbstepchannelfreqs" << std::endl;
  if(ldsbstepchannelfreqs) vectorFree(ldsbstepchannelfreqs);
//  std::cout << "fracsamprotatorA" << std::endl;
  if(fracsamprotatorA)      vectorFree(fracsamprotatorA);
//  std::cout << "fracsamprotatorB" << std::endl;
  if (deltapoloffsets && fracsamprotatorB) vectorFree(fracsamprotatorB);
  //vectorFree(fracmult);
  //vectorFree(fracmultcos);
  //vectorFree(fracmultsin);
  //vectorFree(complexfracmult);
  //vectorFree(channelfreqs);
//  std::cout << "autcorr loop" << std::endl;
  for(int i=0;i<autocorrwidth;i++)
  {
    for(int j=0;j<numrecordedbands;j++)
      if(autocorrelations[i][j]) vectorFree(autocorrelations[i][j]);
    delete [] autocorrelations[i];
    delete [] weights[i];
  }
  delete [] weights;
  delete [] autocorrelations;
//  std::cout << "Pcal stuff" << std::endl;
  if(config->getDPhaseCalIntervalMHz(configindex, datastreamindex))
  {
    for(int i=0;i<numrecordedbands;i++) {
       if(extractor[i]) delete extractor[i];
       if(pcalresults[i]) delete[] pcalresults[i];
    }
    delete[] pcalresults;
    delete[] extractor;
    delete[] pcalnbins;
  }
//  std::cout << "Kurtosis" << std::endl;
  if(sk != 0) //also need to delete kurtosis stuff
  {
    for(int i=0;i<numrecordedbands;i++)
    {
      if(s1[i]) vectorFree(s1[i]);
      if(s2[i]) vectorFree(s2[i]);
      if(sk[i]) vectorFree(sk[i]);
    }
    delete [] s1;
    delete [] s2;
    delete [] sk;
    if(kscratch) vectorFree(kscratch);
  }

  if (linear2circular) {
    delete [] tmpvec;
  }
  //std::cout << "At the end of mode destructor" << std::endl;
}

float Mode::unpack(int sampleoffset, int subloopindex)
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

void Mode::process(const int index, const int subloopindex) {
  std::cerr << "In mode::process." << std::endl
            << "You shouldn't call this from a 'Mode *', you must call it from a GPUMode * or CPUMode *. This seems perverse (what are virtual functions for?). However, because ::process is called in a hot loop, we can't make the function virtual (too expensive to have dynamic dispatch)." << std::endl
            << "I'll quit now." << std::endl;
  abort();
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
  //std::cout << "Setting data length as " << dbytes << std::endl;
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

    const bool pcal_debug_target =
      config->get_use_gpu() &&
      datastreamindex == 2 &&
      i == 5;
      
      
    const int pcal_tone_count = extractor[i]->getLength();
    // Grab pcal output from GPU if we are in GPU mode 
    if (config->get_use_gpu()) { 
      if(pcal_debug_target) {
        std::cerr
            << "PCAL-FINAL-STATE: "
            << "mode=" << this
            << " ds=" << datastreamindex
            << " band=" << i
            << " datasec=" << datasec
            << " datans=" << datans
            << " real_gpu_buffer="
            << pcal_output_real_gpu_mode
            << " complex_gpu_buffer="
            << pcal_output_complex_gpu_mode
            << " pcal_bin_stride_length="
            << pcal_bin_stride_length
            << std::endl;
      }      
        if (pcal_bin_stride_length <= 0) {
            vectorZero_cf32(pcalresults[i], pcal_tone_count);
            csevere << startl << "invalid pcal_bin_stride_length=" << pcal_bin_stride_length << endl;
            continue;
        }
        if (usecomplex) {
            if (pcal_output_complex_gpu_mode == nullptr) {
                vectorZero_cf32(pcalresults[i], pcal_tone_count);
                csevere << startl << "pcal_output_complex_gpu_mode is NULL" << endl;
                continue;
            }
        } else {
            if (pcal_output_real_gpu_mode == nullptr) {
                vectorZero_cf32(pcalresults[i], pcal_tone_count);
                csevere << startl << "pcal_output_real_gpu_mode is NULL" << endl;
                continue;
            }
        }

        if (usecomplex) {
          extractor[i]->setPcalComplex(pcal_output_complex_gpu_mode+(i*(pcal_bin_stride_length)));
        } else {     
          extractor[i]->setPcalReal(pcal_output_real_gpu_mode+(i*(pcal_bin_stride_length)));    

          if(pcal_debug_target) {
            const f32 *raw_pcal =
              pcal_output_real_gpu_mode +
              (i * pcal_bin_stride_length);

            int nonzero_raw_bin_count = 0;
            int nonfinite_raw_bin_count = 0;
            f32 largest_raw_absolute_value = 0.0f;

            for(int pcal_bin = 0;
                pcal_bin < pcal_bin_stride_length;
                pcal_bin++) {
              const f32 value =
                raw_pcal[pcal_bin];

              if(!std::isfinite(value)) {
                nonfinite_raw_bin_count++;
                continue;
              }

              const f32 absolute_value =
                std::fabs(value);

              if(absolute_value >
                 largest_raw_absolute_value) {
                largest_raw_absolute_value =
                  absolute_value;
              }

              if(value != 0.0f) {
                nonzero_raw_bin_count++;
              }
            }

            std::cerr
                << "PCAL-FINAL-RAW: "
                << "mode=" << this
                << " ds=" << datastreamindex
                << " band=" << i
                << " nonzero_raw_bin_count="
                << nonzero_raw_bin_count
                << " nonfinite_raw_bin_count="
                << nonfinite_raw_bin_count
                << " largest_raw_absolute_value="
                << largest_raw_absolute_value
                << std::endl;
          }
        }
    }
    uint64_t samples = extractor[i]->getFinalPCal(pcalresults[i]);

    if(pcal_debug_target) {
      const cf32 target_tone =
        pcalresults[i][6];

      std::cerr
          << "PCAL-FINAL-RESULT: "
          << "mode=" << this
          << " ds=" << datastreamindex
          << " band=" << i
          << " samples=" << samples
          << " tone_6_real=" << target_tone.re
          << " tone_6_imag=" << target_tone.im
          << std::endl;
    }    

    //if ((samples == 0) && (datasec != INVALID_SUBINT) && (datalengthbytes > 1)) {
        //cdebug << startl << "finalisepcal band " << i << " samples==0 over valid subint " << datasec << "s+" << datans << "ns" << endl;
    //}
  }
}

const float Mode::decorrelationpercentage[] = {0.63662, 0.88, 0.94, 0.96, 0.98, 0.99, 0.996, 0.998}; //note these are just approximate!!!


// vim: shiftwidth=2:softtabstop=2:expandtab
