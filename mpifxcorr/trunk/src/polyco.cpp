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
#include "polyco.h"
#include "mode.h"
#include "math.h"
#include "alert.h"

const double Polyco::BIN_TOLERANCE = 0.01;
const double Polyco::DM_CONSTANT_SECS = 1.0/0.000241;

Polyco::Polyco(string filename, int subcount, int confindex, int nbins, int nchans, double * bphases, double * bweights, double calcmins)
  : configindex(confindex), numbins(nbins), numchannels(nchans), numfreqs(-1), calclengthmins(calcmins)
{
  int status;

  //load the polyco file
  readok = loadPolycoFile(filename, subcount);
  if(!readok)
    cerror << startl << "Error trying to open polyco file " << filename << endl;
  else
  {
    //allocate the phase and weight arrays
    absolutephases = vectorAlloc_f64(numchannels + 1);

    binphases = vectorAlloc_f64(numbins);
    status = vectorCopy_f64(bphases, binphases, numbins);
    if(status != vecNoErr)
      csevere << startl << "Error copying binphases in Polyco!!" << endl;

    binweights = vectorAlloc_f64(numbins);
    status = vectorCopy_f64(bweights, binweights, numbins);
    if(status != vecNoErr)
      csevere << startl << "Error copying binweights in Polyco!!" << endl;
  
    cinfo << startl << "Polyco " << subcount << " from file " << filename << " created successfully" << endl;
  }
}

// FIXME: WFB: dt0 not copied -- is that OK?
Polyco::Polyco(const Polyco & tocopy)
  : pulsarname(tocopy.pulsarname), configindex(tocopy.configindex), numbins(tocopy.numbins), numchannels(tocopy.numchannels), numfreqs(tocopy.numfreqs), observatory(tocopy.observatory), timespan(tocopy.timespan), numcoefficients(tocopy.numcoefficients), mjd(tocopy.mjd), mjdfraction(tocopy.mjdfraction), dm(tocopy.dm), dopplershift(tocopy.dopplershift), logresidual(tocopy.logresidual), refphase(tocopy.refphase), f0(tocopy.f0), obsfrequency(tocopy.obsfrequency), binaryphase(tocopy.binaryphase), minbinwidth(tocopy.minbinwidth), bandwidth(tocopy.bandwidth), calclengthmins(tocopy.calclengthmins)
{
  int status;
  cinfo << startl << "Started copying a polyco" << endl;

  //copy as much information as is contained in the copy Polyco
  binphases = vectorAlloc_f64(numbins);
  status = vectorCopy_f64(tocopy.binphases, binphases, numbins);
  if(status != vecNoErr)
    csevere << startl << "Error copying binphases in Polyco!!" << endl;
    
  binweights = vectorAlloc_f64(numbins);
  status = vectorCopy_f64(tocopy.binweights, binweights, numbins);
  if(status != vecNoErr)
    csevere << startl << "Error copying binweights in Polyco!!" << endl;
  
  absolutephases = vectorAlloc_f64(numchannels + 1);
  coefficients = new double[numcoefficients];
  freqcoefficientarray = vectorAlloc_f64(numcoefficients);
  phasecoefficientarray = vectorAlloc_f64(numcoefficients);
  timepowerarray = vectorAlloc_f64(numcoefficients);
  timepowerarray[0] = 1.0;
  for(int i=0;i<numcoefficients;i++)
  {
    coefficients[i] = tocopy.coefficients[i];
    freqcoefficientarray[i] = tocopy.freqcoefficientarray[i];
    phasecoefficientarray[i] = tocopy.phasecoefficientarray[i];
  }
  
  if(numfreqs > 0)
  {
    lofrequencies = vectorAlloc_f32(numfreqs);
    dmPhaseOffsets = new double*[numfreqs];
    channeldmdelays = new double*[numfreqs];
    for(int i=0;i<numfreqs;i++)
    {
      lofrequencies[i] = tocopy.lofrequencies[i];
      dmPhaseOffsets[i] = vectorAlloc_f64(numchannels+1);
      channeldmdelays[i] = vectorAlloc_f64(numchannels+1);
      for(int j=0;j<numchannels+1;j++)
      {
        channeldmdelays[i][j] = tocopy.channeldmdelays[i][j];
      }
    }
    currentbincounts = new int**[numbins];
    for(int i=0;i<numbins;i++)
    {
      currentbincounts[i] = new int*[numfreqs];
      for(int j=0;j<numfreqs;j++)
      {
        currentbincounts[i][j] = vectorAlloc_s32(numchannels+1);
        for(int k=0;k<numchannels+1;k++)
          currentbincounts[i][j][k] = tocopy.currentbincounts[i][j][k];
      }
    }
  }
  
  cinfo << startl << "Finished copying a polyco!" << endl;
}

Polyco::~Polyco()
{
    delete [] coefficients;
    vectorFree(freqcoefficientarray);
    vectorFree(phasecoefficientarray);
    vectorFree(timepowerarray);
    vectorFree(absolutephases);
    vectorFree(binphases);

    if(numfreqs > 0)
    {
      vectorFree(lofrequencies);
      for(int i=0;i<numfreqs;i++)
      {
        vectorFree(dmPhaseOffsets[i]);
        vectorFree(channeldmdelays[i]);
      }
      delete [] dmPhaseOffsets;
      delete [] channeldmdelays;
      for(int i=0;i<numbins;i++)
      {
        for(int j=0;j<numfreqs;j++)
        {
          vectorFree(currentbincounts[i][j]);
        }
        delete [] currentbincounts[i];
      }
      delete [] currentbincounts;
    }
}

void Polyco::incrementBinCount()
{
    for(int i=0;i<numbins;i++)
    {
        for(int j=0;j<numfreqs;j++)
        {
            for(int k=0;k<numchannels+1;k++)
                currentbincounts[i][j][k] = 0;
        }
    }
}

void Polyco::getBins(double offsetmins, int **bins)
{
    int status;
    double averagephase, modulophase;
    double dt = dt0 + offsetmins;

    //calculate the average phase for this time
    for(int i=1;i<numcoefficients;i++)
    {
        timepowerarray[i] = dt*timepowerarray[i-1];
    }
    status = vectorDotProduct_f64(timepowerarray, phasecoefficientarray, numcoefficients, &averagephase);
    if(status != vecNoErr)
        csevere << startl << "Error!!! Problem calculating vector dot product in polyco::getBins" << endl;

    //loop through all the frequencies and subtract the phase offsets for this frequency
    for(int i=0;i<numfreqs;i++)
    {
        //now subtract the dmphaseoffsets for this observing frequency
        status = vectorAddC_f64(dmPhaseOffsets[i], averagephase, absolutephases, numchannels + 1);
        if(status != vecNoErr)
            csevere << startl << "Error!!! Problem adding dmPhaseOffsets to average phase!!!" << endl;

        //now work out the modulophase for each element and thence its bin
        for(int j=0;j<numchannels+1;j++)
        {
            modulophase = absolutephases[j] - floor(absolutephases[j]);
            if(modulophase < binphases[0] || modulophase >= binphases[numbins-1])
            {
                bins[i][j] = 0;
                currentbincounts[0][i][j]++;
            }
            else
            {
                for(int k=numbins-1;k>0;k--)
                {
                    if(modulophase < binphases[k] && modulophase >= binphases[k-1])
                    {
                        bins[i][j] = k;
                        currentbincounts[k][i][j]++;
                    }
                }
            }
        }
    }
}

bool Polyco::setFrequencyValues(int nfreqs, double * freqs, double bw)
{
    double channelfreq;
    bool ok = true;

    numfreqs = nfreqs;
    bandwidth = bw;

    lofrequencies = vectorAlloc_f32(numfreqs);
    for(int i=0;i<numfreqs;i++)
      lofrequencies[i] = freqs[i];

    //create the phase offsets and channeldmdelay arrays
    dmPhaseOffsets = new double*[numfreqs];
    channeldmdelays = new double*[numfreqs];
    for(int i=0;i<numfreqs;i++)
    {
        dmPhaseOffsets[i] = vectorAlloc_f64(numchannels+1);
        channeldmdelays[i] = vectorAlloc_f64(numchannels+1);
        for(int j=0;j<numchannels+1;j++)
        {
            channelfreq = lofrequencies[i] + j*bandwidth/double(numchannels);
            channeldmdelays[i][j] = -dm*DM_CONSTANT_SECS/(channelfreq*channelfreq) + dm*DM_CONSTANT_SECS/(obsfrequency*obsfrequency);
        }
    }

    //find the minimum bin width
    minbinwidth = binphases[0] + 1.0 - binphases[numbins-1];
    for(int i=1;i<numbins;i++)
    {
        if((binphases[i] - binphases[i-1]) < minbinwidth)
            minbinwidth = binphases[i] - binphases[i-1];
    }

    if(minbinwidth < 0.0)
    {
        cfatal << startl << "Error!! Bin phase breakpoints are not in linear ascending order!!!" << endl;
        ok = false;
    }

    //create the bincounts array
    currentbincounts = new int**[numbins];
    for(int i=0;i<numbins;i++)
    {
        currentbincounts[i] = new int*[numfreqs];
        for(int j=0;j<numfreqs;j++)
        {
            currentbincounts[i][j] = vectorAlloc_s32(numchannels+1);
            for(int k=0;k<numchannels+1;k++)
                currentbincounts[i][j][k] = 0;
        }
    }
    return ok;
}

void Polyco::setTime(int startmjd, double startmjdfraction)
{
    double startf, endf, freqgradient, endt, maxdeltaphase, maxdeltafreq, minlofreq;
    int status;

    //work out the length in minutes, and the end point
    dt0 = (startmjd-mjd)*1440 + (startmjdfraction-mjdfraction)*1440.0;
    endt = dt0 + calclengthmins;

    //work out the frequency at the start and the end of the period
    for(int i=1;i<numcoefficients;i++)
        timepowerarray[i] = dt0*timepowerarray[i-1];
    status = vectorDotProduct_f64(timepowerarray, freqcoefficientarray, numcoefficients, &startf);
    if(status != vecNoErr)
        csevere << startl << "Error!!! Problem calculating vector dot product in polyco" << endl;
    for(int i=1;i<numcoefficients;i++)
        timepowerarray[i] = endt*timepowerarray[i-1];
    status = vectorDotProduct_f64(timepowerarray, freqcoefficientarray, numcoefficients, &endf);
    if(status != vecNoErr)
        csevere << startl << "Error!!! Problem calculating vector dot product in polyco" << endl;

    freqgradient = (startf>endf)?(startf-endf)/calclengthmins:(endf-startf)/calclengthmins;

    //work out how often we'll need to recalculate the dmphase array - the max phase error we can allow is BIN_TOLERANCE*minbinwidth
    maxdeltaphase = BIN_TOLERANCE*minbinwidth;
    minlofreq = lofrequencies[0]; //first work out the minimum frequency
    for(int i=1;i<numfreqs;i++)
    {
        if(lofrequencies[i] < minlofreq) {
            minlofreq = lofrequencies[i];
        }
    }
    maxdeltafreq = maxdeltaphase/(dm*DM_CONSTANT_SECS/(minlofreq*minlofreq));

    //now, we need to make sure our required valid length is not longer than the validity time we've just worked out
    if(maxdeltafreq/freqgradient < calclengthmins)
      cerror << startl << "Error - Polyco will not be accurate over entire range of " << calclengthmins << " as the frequency is changing too rapidly.  The maximum safe calc length would be " << maxdeltafreq/freqgradient << " - try reducing blocks per send or numchannels" << ", maxdeltaphase is " << maxdeltaphase << ", minbinwidth is " << minbinwidth << ", freqgradient is " << freqgradient << ", maxdeltafreq is " << maxdeltafreq << ", dm is " << dm << ", minlofreq is " << minlofreq << endl;

    //work out the initial dmphasecorrect array and set the lastphasecalctime
    calculateDMPhaseOffsets(calclengthmins/2);
}

bool Polyco::includesTime(int incmjd, double incmjdfraction)
{
    double differencemins = (incmjd - mjd)*1440 + (incmjdfraction - mjdfraction)*1440;

    return (differencemins <= double(timespan/2.0)) && (differencemins >= -double(timespan/2.0));
}

Polyco * Polyco::getCurrentPolyco(int requiredconfig, int mjd, double mjdfraction, Polyco ** polycos, int npolycos)
{
    for(int i=0;i<npolycos;i++)
    {
        if(polycos[i]->includesTime(mjd, mjdfraction) && polycos[i]->getConfig() == requiredconfig)
	{
            return polycos[i]; //note exit from loop here!
	}
    }

    return NULL;
}

void Polyco::calculateDMPhaseOffsets(double offsetmins)
{
    double currentfreq, dt;
    int status;

    dt = dt0 + offsetmins;

    //work out the timepower array and calculate the current frequency
    for(int i=1;i<numcoefficients;i++)
        timepowerarray[i] = dt*timepowerarray[i-1];
    status = vectorDotProduct_f64(timepowerarray, freqcoefficientarray, numcoefficients, &currentfreq);
    if(status != vecNoErr)
        csevere << startl << "Error!!! Problem calculating vector dot product in calculateDMPhaseOffsets" << endl;

    //work out the phase offset due to dispersion at each channel of each frequency
    for(int i=0;i<numfreqs;i++)
    {
        status = vectorMulC_f64(channeldmdelays[i], currentfreq, dmPhaseOffsets[i], numchannels + 1);
        if(status != vecNoErr)
          csevere << startl << "Error!!! Problem calculating dmPhaseOffsets!!!" << endl;
    }
}

bool Polyco::loadPolycoFile(string filename, int subcount)
{
    string strbuffer;
    char buffer[80];
    int hour, minute;
    double timedouble, second, mjddouble;
    ifstream input(filename.c_str(), ios::in);
    if(!input.is_open() || input.bad())
        return false; //note return with failure here!!!

    coefficients = 0;

    cinfo << startl << "About to start processing the polyco file " << filename << ", looking for subcount " << subcount << endl;

    for(int s=0;s<=subcount;s++)
    {
      //process the first line
      input.get(buffer, 11);
      pulsarname = buffer;

      input.get(buffer, 11); //ignore the date - we'll use the mjd instead
      input.get(buffer, 13);
      timedouble = atof(buffer);
      hour = int(timedouble/10000);
      minute = int((timedouble - 10000*hour)/100);
      second = timedouble - (minute*100 + hour*10000);

      //get the mjd
      input.get(buffer, 20);
      mjddouble = atof(buffer);
      mjd = int(mjddouble);
      mjdfraction = double(hour)/24.0 + double(minute)/1440.0 + second/86400.0;

      //get the dm, dopplershift and logresidual
      input.get(buffer, 22);
      dm = atof(buffer);

      input.get(buffer, 8);
      dopplershift = atof(buffer);

      input.get(buffer, 8);
      logresidual = atof(buffer);

      //check if we missed anything
      if (input.fail()) {
        cwarn << startl << "Hit end of first line prematurely - check your polyco conforms to standard! Some values may not have been set properly, but likely everything is ok" << endl;
        input.clear();
      }

      getline(input, strbuffer); //skip over any remaining whitespace

      //process the second line
      //get the refphase, reference frequency, observatory, timespan, number of coefficients, observing frequency and binary phase
      input.get(buffer, 21);
      refphase = atof(buffer);

      input.get(buffer, 19);
      f0 = atof(buffer);

      input.get(buffer, 6);
      observatory = atoi(buffer);

      input.get(buffer, 7);
      timespan = atoi(buffer);

      input.get(buffer, 6);
      numcoefficients = atoi(buffer);

      input.get(buffer, 22);
      obsfrequency = atof(buffer);

      input.get(buffer, 6);
      binaryphase = atof(buffer);

      //check if the end of line was reached prematurely
      if (input.fail()) {
        cwarn << startl << "Hit end of second line prematurely - check your polyco conforms to standard! Some values may not have been set properly.  This often happens for non-binary pulsars.  Likely everything is ok." << endl;
        input.clear();
      }

      getline(input, strbuffer); //skip to next line

      //grab all the coefficients
      coefficients = new double[numcoefficients];

      for(int i=0;i<numcoefficients;i++)
      {
          input.get(buffer, 26);
          if(input.fail()) {
            csevere << startl << "Input related error processing polyco file " << filename << " coefficients!!! Will attempt clearing stream and continuing." << endl;
            input.clear();
          }
          //check for exponents given with D's rather than E's, and fix if necessary
          for (int j=0;j<26;j++)
          {
            if(buffer[j] == 'D' || buffer[j] == 'd')
              buffer[j] = 'E';
          }
          coefficients[i] = atof(buffer);
          if((((i+1)%3)==0) && (i>0)) // at the end of a line, need to skip
              getline(input, strbuffer);
      }
    }
    input.close();

    //generate the frequency and phase coefficient arrays
    freqcoefficientarray = vectorAlloc_f64(numcoefficients);
    freqcoefficientarray[0] = f0 + coefficients[1]/60.0;
    freqcoefficientarray[numcoefficients-1] = 0.0;
    for(int i=1;i<numcoefficients-1;i++)
        freqcoefficientarray[i] = (i+1)*coefficients[i+1];

    phasecoefficientarray = vectorAlloc_f64(numcoefficients);
    phasecoefficientarray[0] = refphase + coefficients[0];
    phasecoefficientarray[1] = 60.0*f0 + coefficients[1];
    for(int i=2;i<numcoefficients;i++)
        phasecoefficientarray[i] = coefficients[i];

    //create the timepowerarray and absolutephases array
    timepowerarray = vectorAlloc_f64(numcoefficients);
    timepowerarray[0] = 1.0;

    return true;
}
