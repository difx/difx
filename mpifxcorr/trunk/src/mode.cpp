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
#include "mode.h"
#include "math.h"
#include "architecture.h"
#include "datastream.h"
#include "mpifxcorr.h"
#include <iomanip>
#include <difxmessage.h>

//using namespace std;
const float Mode::TINY = 0.000000001;

Mode::Mode(Configuration * conf, int confindex, int dsindex, int nchan, int bpersend, int gblocks, int nfreqs, double bw, double * freqclkoffsets, int ninputbands, int noutputbands, int nbits, int unpacksamp, bool fbank, bool postffringe, bool quaddelayinterp, bool cacorrs, double bclock)
  : config(conf), configindex(confindex), datastreamindex(dsindex), numchannels(nchan), blockspersend(bpersend), guardblocks(gblocks), twicenumchannels(nchan*2), numfreqs(nfreqs), bandwidth(bw), freqclockoffsets(freqclkoffsets), numinputbands(ninputbands), numoutputbands(noutputbands), numbits(nbits), unpacksamples(unpacksamp), filterbank(fbank), postffringerot(postffringe), quadraticdelayinterp(quaddelayinterp), calccrosspolautocorrs(cacorrs), blockclock(bclock)
{
  int status;
  int decimationfactor = config->getDecimationFactor(configindex);

  sampletime = 1.0/(2.0*bandwidth); //microseconds
  processtime = twicenumchannels*sampletime; //microseconds
  fractionalLoFreq = false;
  for(int i=0;i<nfreqs;i++)
  {
    if(config->getDFreq(configindex, datastreamindex, i) - int(config->getDFreq(configindex, datastreamindex, i)) > TINY)
      fractionalLoFreq = true;
  }

  //now do the rest of the initialising
  samplesperblock = int(bandwidth*2/blockclock);
  if(samplesperblock == 0)
    cerr << "Error!!! Samplesperblock is < 1, current implementation cannot handle this situation.  Aborting!" << endl;
  bytesperblocknumerator = (numinputbands*samplesperblock*numbits*decimationfactor)/8;
  if(bytesperblocknumerator == 0)
  {
    bytesperblocknumerator = 1;
    bytesperblockdenominator = 8/(numinputbands*samplesperblock*numbits*decimationfactor);
    unpacksamples += bytesperblockdenominator*sizeof(u16)*samplesperblock;
  }
  else
  {
    bytesperblockdenominator = 1;
  }
  samplesperlookup = (numinputbands*sizeof(u16)*samplesperblock*bytesperblockdenominator)/bytesperblocknumerator;
  numlookups = (unpacksamples*bytesperblocknumerator)/(bytesperblockdenominator*sizeof(u16)*samplesperblock);
  if(samplesperblock > 1)
    numlookups++;
  unpackedarrays = new f32*[numinputbands];
  fftoutputs = new cf32*[numinputbands];
  conjfftoutputs = new cf32*[numinputbands];
  for(int i=0;i<numinputbands;i++)
  {
    unpackedarrays[i] = vectorAlloc_f32(unpacksamples);
    fftoutputs[i] = vectorAlloc_cf32(numchannels+1);
    conjfftoutputs[i] = vectorAlloc_cf32(numchannels+1);
  }

  lookup = vectorAlloc_s16((MAX_U16+1)*samplesperlookup);
  linearunpacked = vectorAlloc_s16(numlookups*samplesperlookup);
  xval = vectorAlloc_f64(twicenumchannels);
  xoffset = vectorAlloc_f64(twicenumchannels);
  for(int i=0;i<twicenumchannels;i++)
    xoffset[i] = double(i)/double(numchannels*blockspersend);

  //initialise the fft info
  order = 0;
  while((twicenumchannels) >> order != 1)
    order++;
  flag = vecFFT_NoReNorm;
  hint = vecAlgHintFast;

  if(postffringerot) //initialise the specific structures
  {
    cerr << "Warning - post-f fringe rotation not yet tested!!!" << endl;
    status = vectorInitFFTR_f32(&pFFTSpecR, order, flag, hint);
    if(status != vecNoErr)
      cerr << "Error in FFT initialisation!!!" << status << endl;
    status = vectorGetFFTBufSizeR_f32(pFFTSpecR, &fftbuffersize);
    if(status != vecNoErr)
      cerr << "Error in FFT buffer size calculation!!!" << status << endl;
  }
  else
  {
    rotateargument = vectorAlloc_f32(twicenumchannels);
    cosrotated = vectorAlloc_f32(twicenumchannels);
    cosrotatedoutput = vectorAlloc_f32(twicenumchannels);
    sinrotated = vectorAlloc_f32(twicenumchannels);
    sinrotatedoutput = vectorAlloc_f32(twicenumchannels);
    realfftd = vectorAlloc_f32(twicenumchannels);
    imagfftd = vectorAlloc_f32(twicenumchannels);
    fringedelayarray = vectorAlloc_f64(twicenumchannels);
    status = vectorInitFFTC_f32(&pFFTSpecC, order, flag, hint);
    if(status != vecNoErr)
      cerr << "Error in FFT initialisation!!!" << status << endl;
    status = vectorGetFFTBufSizeC_f32(pFFTSpecC, &fftbuffersize);
    if(status != vecNoErr)
      cerr << "Error in FFT buffer size calculation!!!" << status << endl;
    //zero the Nyquist channel for every band - that is where the weight will be stored on all
    //baselines (the imag part) so the datastream channel for it must be zeroed
    for(int i=0;i<numinputbands;i++)
    {
      if(config->getDLowerSideband(configindex, datastreamindex, config->getDLocalFreqIndex(configindex, datastreamindex, i))) {
        fftoutputs[i][0].re = 0.0;
        fftoutputs[i][0].im = 0.0;
      }
      else {
        fftoutputs[i][numchannels].re = 0.0;
        fftoutputs[i][numchannels].im = 0.0;
      }
    }
  }
  fftbuffer = vectorAlloc_u8(fftbuffersize);

  delaylength = blockspersend + guardblocks;
  delays = vectorAlloc_f64(delaylength);

  fracmult = vectorAlloc_f32(numchannels + 1);
  fracmultcos = vectorAlloc_f32(numchannels + 1);
  fracmultsin = vectorAlloc_f32(numchannels + 1);
  complexfracmult = vectorAlloc_cf32(numchannels + 1);

  channelfreqs = vectorAlloc_f32(numchannels + 1);
  for(int i=0;i<numchannels + 1;i++)
    channelfreqs[i] = (float)((TWO_PI*i*bandwidth)/numchannels);
  lsbchannelfreqs = vectorAlloc_f32(numchannels + 1);
  for(int i=0;i<numchannels + 1;i++)
    lsbchannelfreqs[i] = (float)((-TWO_PI*(numchannels-i)*bandwidth)/numchannels);

  //space for the autocorrelations
  if(calccrosspolautocorrs)
    autocorrwidth = 2;
  else
    autocorrwidth = 1;
  autocorrelations = new cf32**[autocorrwidth];
  for(int i=0;i<autocorrwidth;i++)
  {
    autocorrelations[i] = new cf32*[numinputbands];
    for(int j=0;j<numinputbands;j++)
      autocorrelations[i][j] = vectorAlloc_cf32(numchannels+1);
  }
}

Mode::~Mode()
{
  int status;
  difxMessageSendDifxAlert("Starting a mode destructor", DIFX_ALERT_LEVEL_DEBUG);

  for(int i=0;i<numinputbands;i++)
  {
    vectorFree(unpackedarrays[i]);
    vectorFree(fftoutputs[i]);
    vectorFree(conjfftoutputs[i]);
  }
  delete [] unpackedarrays;
  delete [] fftoutputs;
  delete [] conjfftoutputs;
  vectorFree(xoffset);
  vectorFree(xval);

  if(postffringerot)
  {
    status = vectorFreeFFTR_f32(pFFTSpecR);
    if(status != vecNoErr)
      cerr << "Error in freeing FFT spec!!!" << status << endl;
  }
  else
  {
    vectorFree(rotateargument);
    vectorFree(realfftd);
    vectorFree(imagfftd);
    vectorFree(cosrotated);
    vectorFree(cosrotatedoutput);
    vectorFree(sinrotated);
    vectorFree(sinrotatedoutput);
    vectorFree(fringedelayarray);
    status = vectorFreeFFTC_f32(pFFTSpecC);
    if(status != vecNoErr)
      cerr << "Error in freeing FFT spec!!!" << status << endl;
  }
  vectorFree(lookup);
  vectorFree(linearunpacked);
  vectorFree(fftbuffer);
  vectorFree(delays);
  vectorFree(fracmult);
  vectorFree(fracmultcos);
  vectorFree(fracmultsin);
  vectorFree(complexfracmult);
  vectorFree(channelfreqs);

  for(int i=0;i<autocorrwidth;i++)
  {
    for(int j=0;j<numinputbands;j++)
      vectorFree(autocorrelations[i][j]);
    delete [] autocorrelations[i];
  }
  delete [] autocorrelations;

  difxMessageSendDifxAlert("Ending a mode destructor", DIFX_ALERT_LEVEL_DEBUG);
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
      cerr << "Error in lookup for unpacking!!!" << status << endl;
      return 0;
    }
  }

  //split the linear unpacked array into the separate subbands
  status = vectorSplitScaled_s16f32(&(linearunpacked[stepin*numinputbands]), unpackedarrays, numinputbands, unpacksamples);
  if(status != vecNoErr) {
    cerr << "Error in splitting linearunpacked!!!" << status << endl;
    return 0;
  }

  return 1.0;
}

float Mode::process(int index)  //frac sample error, fringedelay and wholemicroseconds are in microseconds 
{
  double phaserotation, averagedelay, nearestsampletime, starttime, finaloffset, lofreq, distance;
  f32 phaserotationfloat, fracsampleerror, dataweight;
  int status, count, nearestsample, integerdelay, sidebandoffset;
  cf32* fftptr;
  f32* currentchannelfreqptr;
  int indices[10];
  
  if(datalengthbytes == 0 || !(delays[index] > MAX_NEGATIVE_DELAY) || !(delays[index+1] > MAX_NEGATIVE_DELAY))
  {
    for(int i=0;i<numinputbands;i++)
    {
      status = vectorZero_cf32(fftoutputs[i], numchannels+1);
      if(status != vecNoErr)
        cerr << "Error trying to zero fftoutputs when data is bad!" << endl;
      status = vectorZero_cf32(conjfftoutputs[i], numchannels+1);
      if(status != vecNoErr)
        cerr << "Error trying to zero fftoutputs when data is bad!" << endl;
    }
    return 0.0; //don't process crap data
  }
    
  averagedelay = (delays[index] + delays[index+1])/2.0;
  starttime = (offsetseconds-bufferseconds)*1000000.0 + (double(offsetns)/1000.0 + index*twicenumchannels*sampletime - buffermicroseconds) - averagedelay;
  nearestsample = int(starttime/sampletime + 0.5);
  
  //if we need to, unpack some more data - first check to make sure the pos is valid at all
  if(nearestsample < -1 || (((nearestsample + twicenumchannels)/samplesperblock)*bytesperblocknumerator)/bytesperblockdenominator > datalengthbytes)
  {
    cerr << "MODE error - trying to process data outside range - aborting!!! nearest sample was " << nearestsample << ", the max bytes should be " << datalengthbytes << ".  bufferseconds was " << bufferseconds << ", offsetseconds was " << offsetseconds << ", buffermicroseconds was " << buffermicroseconds << ", offsetns was " << offsetns << ", index was " << index << ", average delay was " << averagedelay << endl;
    return 0.0;
  }
  if(nearestsample == -1)
  {
    nearestsample = 0;
    dataweight = unpack(nearestsample);
  }
  else if(nearestsample < unpackstartsamples || nearestsample > unpackstartsamples + unpacksamples - twicenumchannels)
    //need to unpack more data
    dataweight = unpack(nearestsample);

  if(!(dataweight > 0.0))
    return 0.0;

  nearestsampletime = nearestsample*sampletime;
  fracsampleerror = float(starttime - nearestsampletime);

  if(postffringerot)
  {
    integerdelay = int(averagedelay);
  }
  else //doing pre-f so need to work out the delay arrays
  {
    if(dolinearinterp)
    {
      integerdelay = int(delays[index]);
      double rate = blockspersend*(delays[index+1]-delays[index])/2.0;
      //multiply the offset by the rate
      status = vectorMulC_f64(xoffset, rate, xval, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in linearinterpolate, multiplication" << endl;
      //add the starting delay
      status = vectorAddC_f64_I(delays[index] - integerdelay, xval, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in linearinterpolate, final offset add!!!" << endl;
    }
    else //we're doing quadratic interpolation
    {
      integerdelay = int(centredelay);
      finaloffset = toaddlast - integerdelay;
      distance = double(index*2)/double(blockspersend) - 1.0;

      //change x to x + b/a
      status = vectorAddC_f64(xoffset, distance + toaddfirst, xval, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in quadinterpolate, offset add!!!" << endl;
      //square
      status = vectorSquare_f64_I(xval, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in quadinterpolate, xval squaring" << endl;
      //multiply by a
      status = vectorMulC_f64_I(a, xval, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in quadinterpolate, multiplication" << endl;
      //add c - b^2/4a
      status = vectorAddC_f64_I(finaloffset, xval, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in quadinterpolate, final offset add!!!" << endl;
    }
  }

  for(int i=0;i<numfreqs;i++)
  {
    count = 0;

    //create the array for fractional sample error correction - including the post-f fringe rotation
    currentchannelfreqptr = (config->getDLowerSideband(configindex, datastreamindex, i))?lsbchannelfreqs:channelfreqs;
    status = vectorMulC_f32(currentchannelfreqptr, fracsampleerror - freqclockoffsets[i], fracmult, numchannels + 1);
    if(status != vecNoErr)
      cerr << "Error in frac sample correction!!!" << status << endl;

    lofreq = config->getDFreq(configindex, datastreamindex, i);
    if(postffringerot)
    {
      //work out the phase rotation to apply
      phaserotation = (averagedelay-integerdelay)*lofreq;
      if(fractionalLoFreq)
        phaserotation += integerdelay*(lofreq-int(lofreq));
      phaserotationfloat = (f32)(-TWO_PI*(phaserotation-int(phaserotation + 0.5)));

      status = vectorAddC_f32_I(phaserotationfloat, fracmult, numchannels+1);
      if(status != vecNoErr)
        cerr << "Error in post-f phase rotation addition!!!" << status << endl;
    }
    else //need to work out the time domain modulation
    {
      status = vectorMulC_f64(xval, lofreq, fringedelayarray, twicenumchannels);
      if(status != vecNoErr)
        cerr << "Error in delay multiplication!!!" << status << endl;
      if(fractionalLoFreq)
      {
          status = vectorAddC_f64_I((lofreq-int(lofreq))*double(integerdelay), fringedelayarray, twicenumchannels);
          if(status != vecNoErr)
              cerr << "Error in addition of fractional LO contribution to fringe rotation!!!" << status << endl;
      }

      //convert to angle in range 0->2pi
      for(int j=0;j<twicenumchannels;j++)
        rotateargument[j] = -TWO_PI*(fringedelayarray[j] - int(fringedelayarray[j]));

      //do the sin/cos
      status = vectorSinCos_f32(rotateargument, sinrotated, cosrotated, twicenumchannels);
      if(status != vecNoErr)
      {
        cerr << "Error in sin/cos of rotate argument!!! Status = " << status << endl;
      }
    }

    status = vectorSinCos_f32(fracmult, fracmultsin, fracmultcos, numchannels + 1);
    if(status != vecNoErr)
      cerr << "Error in frac sample correction!!!" << status << endl; 

    status = vectorRealToComplex_f32(fracmultcos, fracmultsin, complexfracmult, numchannels + 1);
    if(status != vecNoErr)
      cerr << "Error in frac sample correction!!!" << status << endl; 

    for(int j=0;j<numinputbands;j++)
    {
      if(config->matchingBand(configindex, datastreamindex, i, j))
      {
        indices[count++] = j;
        if(postffringerot)
        {
          fftptr = (config->getDLowerSideband(configindex, datastreamindex, i))?conjfftoutputs[j]:fftoutputs[j];
  
          //do the fft
          status = vectorFFT_RtoC_f32(&(unpackedarrays[j][nearestsample - unpackstartsamples]), (f32*)fftptr, pFFTSpecR, fftbuffer);
          if(status != vecNoErr)
            cerr << "Error in FFT!!!" << status << endl;
  
          //fix the lower sideband if required
          if(config->getDLowerSideband(configindex, datastreamindex, i))
          {
            status = vectorConjFlip_cf32(fftptr, fftoutputs[j], numchannels + 1);
            if(status != vecNoErr)
              cerr << "Error in conjugate!!!" << status << endl;
          }
        }
        else //doing pre-f fringe rot
        {
          //do the fringe rotation
          status = vectorMul_f32(sinrotated, &(unpackedarrays[j][nearestsample - unpackstartsamples]), sinrotatedoutput, twicenumchannels);
          if(status != vecNoErr)
            cerr << "Error in sine fringe rotation!!!" << status << endl;
          status = vectorMul_f32(cosrotated, &(unpackedarrays[j][nearestsample - unpackstartsamples]), cosrotatedoutput, twicenumchannels);
          if(status != vecNoErr)
            cerr << "Error in cosine fringe rotation!!!" << status << endl;

          //do the fft
          status = vectorFFT_CtoC_f32(cosrotatedoutput, sinrotatedoutput, realfftd, imagfftd, pFFTSpecC, fftbuffer);
          if(status != vecNoErr)
            cerr << "Error in FFT!!!" << status << endl;

          //assemble complex from the real and imaginary
          if(config->getDLowerSideband(configindex, datastreamindex, i)) {
            //updated to include "DC" channel at upper end of LSB band
            status = vectorRealToComplex_f32(&realfftd[numchannels+1], &imagfftd[numchannels+1], &(fftoutputs[j][1]), numchannels-1);
            fftoutputs[j][numchannels].re = realfftd[0];
            fftoutputs[j][numchannels].im = imagfftd[0];
          }
          else {
            //updated to include "Nyquist" channel
            status = vectorRealToComplex_f32(realfftd, imagfftd, fftoutputs[j], numchannels);
          }
          if(status != vecNoErr)
            cerr << "Error assembling complex fft result" << endl;
        }

        //do the frac sample correct (+ fringe rotate if its post-f)
        status = vectorMul_cf32_I(complexfracmult, fftoutputs[j], numchannels + 1);
        if(status != vecNoErr)
          cerr << "Error in frac sample correction!!!" << status << endl;

        //do the conjugation
        status = vectorConj_cf32(fftoutputs[j], conjfftoutputs[j], numchannels + 1);
        if(status != vecNoErr)
          cerr << "Error in conjugate!!!" << status << endl;

        //updated so that Nyquist channel is not accumulated for either USB or LSB data
        sidebandoffset = 0;
        if(config->getDLowerSideband(configindex, datastreamindex, i))
          sidebandoffset = 1;

        //do the autocorrelation (skipping Nyquist channel)
        status = vectorAddProduct_cf32(fftoutputs[j]+sidebandoffset, conjfftoutputs[j]+sidebandoffset, autocorrelations[0][j]+sidebandoffset, numchannels);
        if(status != vecNoErr)
          cerr << "Error in autocorrelation!!!" << status << endl;

        //Add the weight in magic location (imaginary part of Nyquist channel)
        autocorrelations[0][j][numchannels*(1-sidebandoffset)].im += dataweight;
      }
    }

    //if we need to, do the cross-polar autocorrelations
    if(calccrosspolautocorrs && count > 1)
    {
      //cout << "For frequency " << i << ", datastream " << datastreamindex << " has chosen bands " << indices[0] << " and " << indices[1] << endl; 
      status = vectorAddProduct_cf32(fftoutputs[indices[0]]+sidebandoffset, conjfftoutputs[indices[1]]+sidebandoffset, autocorrelations[1][indices[0]]+sidebandoffset, numchannels);
      if(status != vecNoErr)
        cerr << "Error in cross-polar autocorrelation!!!" << status << endl;
      status = vectorAddProduct_cf32(fftoutputs[indices[1]]+sidebandoffset, conjfftoutputs[indices[0]]+sidebandoffset, autocorrelations[1][indices[1]]+sidebandoffset, numchannels);
      if(status != vecNoErr)
        cerr << "Error in cross-polar autocorrelation!!!" << status << endl;
      //add the weight in magic location (imaginary part of Nyquist channel)
      autocorrelations[1][indices[0]][numchannels*(1-sidebandoffset)].im += dataweight;
      autocorrelations[1][indices[1]][numchannels*(1-sidebandoffset)].im += dataweight;
    }
  }

  return dataweight;
}

void Mode::zeroAutocorrelations()
{
  for(int i=0;i<autocorrwidth;i++)
  {
    for(int j=0;j<numinputbands;j++)
      vectorZero_cf32(autocorrelations[i][j], numchannels+1);
  }
}

void Mode::setDelays(f64 * d)
{
  int status = vectorCopy_f64(d, delays, delaylength);
  if(status != vecNoErr)
    cerr << "Error trying to copy delays!!!" << endl;
    
  dolinearinterp = !quadraticdelayinterp;
    
  //work out the twiddle factors used later to interpolate
  if(quadraticdelayinterp)
  {
    if(!(delays[0] > MAX_NEGATIVE_DELAY) || !(delays[blockspersend/2] > MAX_NEGATIVE_DELAY) || !(delays[blockspersend/2+1] > MAX_NEGATIVE_DELAY) || !(delays[blockspersend] > MAX_NEGATIVE_DELAY))
    {
      dolinearinterp = true; //no quadratic cause we don't have the info everywhere
    }
    else
    {
      centredelay = (blockspersend%2==1)?(delays[blockspersend/2]+delays[blockspersend/2+1])/2.0:delays[blockspersend/2];
      a = (delays[blockspersend]+delays[0])/2.0 - centredelay;
      b = (delays[blockspersend]-delays[0])/2.0;
      c = centredelay;
      toaddfirst = b/(2*a);
      toaddlast = c - (b*b)/(4*a);
      if(a == 0)
        dolinearinterp = true; //can't use the quadratic since that would involve divide by 0
    }
  }
}

void Mode::setData(u8 * d, int dbytes, double btime)
{
  data = d;
  datalengthbytes = dbytes;
  bufferseconds = int(btime);
  buffermicroseconds = (btime - int(btime))*1000000.0;
  unpackstartsamples = -999999999;
}

const float Mode::decorrelationpercentage[] = {0.63662, 0.88, 0.94, 0.96, 0.98, 0.99, 0.996, 0.998}; //note these are just approximate!!!


LBAMode::LBAMode(Configuration * conf, int confindex, int dsindex, int nchan, int bpersend, int gblocks, int nfreqs, double bw, double * freqclkoffsets, int ninputbands, int noutputbands, int nbits, bool fbank, bool postffringe, bool quaddelayinterp, bool cacorrs, const s16* unpackvalues)
    : Mode(conf,confindex,dsindex,nchan,bpersend,gblocks,nfreqs,bw,freqclkoffsets,ninputbands,noutputbands,nbits,nchan*2,fbank,postffringe,quaddelayinterp,cacorrs,(bw<16.0)?bw*2.0:32.0)
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
      for(int k=0;k<numinputbands;k++)
      {
        for(int l=0;l<samplesperblock;l++)
        {
          if(samplesperblock > 1 && numinputbands > 1) //32 MHz or 64 MHz dual pol
            if(samplesperblock == 4) //64 MHz
              outputshift = 3*(2-l) - 3*k;
            else
              outputshift = -k*samplesperblock + k + l;
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

const s16 LBAMode::stdunpackvalues[] = {MAX_S16/4, -MAX_S16/4 - 1, 3*MAX_S16/4, -3*MAX_S16/4 - 1};
const s16 LBAMode::vsopunpackvalues[] = {-3*MAX_S16/4 - 1, MAX_S16/4, -MAX_S16/4 - 1, 3*MAX_S16/4};
