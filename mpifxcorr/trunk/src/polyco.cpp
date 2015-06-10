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
#include <sstream>
#include "polyco.h"
#include "mode.h"
#include "math.h"
#include "alert.h"

const double Polyco::BIN_TOLERANCE = 0.01;
const double Polyco::DM_CONSTANT_SECS = 1.0/0.000241;

Polyco::Polyco(string filename, int subcount, int confindex, int nbins, int maxchans, double * bphases, double * bweights, double calcmins)
  : configindex(confindex), numbins(nbins), maxchannels(maxchans), numfreqs(-1), calclengthmins(calcmins)
{
  int status;
  estimatedbytes = 0;

  //load the polyco file
  readok = loadPolycoFile(filename, subcount);
  if(!readok)
    cerror << startl << "Error trying to open polyco file " << filename << endl;
  else
  {
    //allocate the phase and weight arrays
    absolutephases = vectorAlloc_f64(maxchannels);
    estimatedbytes += 8*(maxchannels);

    binphases = vectorAlloc_f64(numbins);
    estimatedbytes += 8*numbins;
    status = vectorCopy_f64(bphases, binphases, numbins);
    if(status != vecNoErr)
      csevere << startl << "Error copying binphases in Polyco!!" << endl;

    binweights = vectorAlloc_f64(numbins);
    estimatedbytes += 8*numbins;
    status = vectorCopy_f64(bweights, binweights, numbins);
    if(status != vecNoErr)
      csevere << startl << "Error copying binweights in Polyco!!" << endl;
  
    cinfo << startl << "Polyco " << subcount << " from file " << filename << " created successfully" << endl;
  }
}

Polyco::Polyco(const Polyco & tocopy)
  : pulsarname(tocopy.pulsarname), configindex(tocopy.configindex), numbins(tocopy.numbins), maxchannels(tocopy.maxchannels), numfreqs(tocopy.numfreqs), observatory(tocopy.observatory), timespan(tocopy.timespan), numcoefficients(tocopy.numcoefficients), mjd(tocopy.mjd), mjdfraction(tocopy.mjdfraction), dt0(tocopy.dt0), dm(tocopy.dm), dopplershift(tocopy.dopplershift), logresidual(tocopy.logresidual), refphase(tocopy.refphase), f0(tocopy.f0), obsfrequency(tocopy.obsfrequency), binaryphase(tocopy.binaryphase), minbinwidth(tocopy.minbinwidth), calclengthmins(tocopy.calclengthmins)
{
  int status;
  //cinfo << startl << "Started copying a polyco" << endl;

  //copy as much information as is contained in the copy Polyco
  binphases = vectorAlloc_f64(numbins);
  estimatedbytes += 8*numbins;
  status = vectorCopy_f64(tocopy.binphases, binphases, numbins);
  if(status != vecNoErr)
    csevere << startl << "Error copying binphases in Polyco!!" << endl;

  binweights = vectorAlloc_f64(numbins);
  estimatedbytes += 8*numbins;
  status = vectorCopy_f64(tocopy.binweights, binweights, numbins);
  if(status != vecNoErr)
    csevere << startl << "Error copying binweights in Polyco!!" << endl;

  absolutephases = vectorAlloc_f64(maxchannels);
  estimatedbytes += 8*(maxchannels);
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
    bandwidths    = vectorAlloc_f32(numfreqs);
    numchannels   = new int[numfreqs];
    computefor    = new bool[numfreqs];
    dmPhaseOffsets = new double*[numfreqs];
    channeldmdelays = new double*[numfreqs];
    for(int i=0;i<numfreqs;i++)
    {
      lofrequencies[i] = tocopy.lofrequencies[i];
      bandwidths[i] = tocopy.bandwidths[i];
      numchannels[i] = tocopy.numchannels[i];
      computefor[i] = tocopy.computefor[i];
      dmPhaseOffsets[i] = vectorAlloc_f64(numchannels[i]);
      channeldmdelays[i] = vectorAlloc_f64(numchannels[i]);
      estimatedbytes += 2*8*(numchannels[i]);
      for(int j=0;j<numchannels[i];j++)
      {
        channeldmdelays[i][j] = tocopy.channeldmdelays[i][j];
      }
    }
    /*currentbincounts = new int**[numbins];
    for(int i=0;i<numbins;i++)
    {
      currentbincounts[i] = new int*[numfreqs];
      for(int j=0;j<numfreqs;j++)
      {
        currentbincounts[i][j] = vectorAlloc_s32(numchannels[i]+1);
        estimatedbytes += 4*(numchannels[i]+1);
        for(int k=0;k<numchannels[i]+1;k++)
          currentbincounts[i][j][k] = tocopy.currentbincounts[i][j][k];
      }
    }*/
  }

  //cinfo << startl << "Finished copying a polyco!" << endl;
}

Polyco::~Polyco()
{
    delete [] coefficients;
    vectorFree(freqcoefficientarray);
    vectorFree(phasecoefficientarray);
    vectorFree(timepowerarray);
    vectorFree(absolutephases);
    vectorFree(binphases);
    vectorFree(binweights);

    if(numfreqs > 0)
    {
      vectorFree(lofrequencies);
      vectorFree(bandwidths);
      for(int i=0;i<numfreqs;i++)
      {
        vectorFree(dmPhaseOffsets[i]);
        vectorFree(channeldmdelays[i]);
      }
      delete [] dmPhaseOffsets;
      delete [] channeldmdelays;
      delete [] numchannels;
      delete [] computefor;
      /*for(int i=0;i<numbins;i++)
      {
        for(int j=0;j<numfreqs;j++)
        {
          vectorFree(currentbincounts[i][j]);
        }
        delete [] currentbincounts[i];
      }
      delete [] currentbincounts;*/
    }
}

/*void Polyco::incrementBinCount()
{
    for(int i=0;i<numbins;i++)
    {
        for(int j=0;j<numfreqs;j++)
        {
            for(int k=0;k<numchannels[i]+1;k++)
                currentbincounts[i][j][k] = 0;
        }
    }
}*/

void Polyco::getBins(double offsetmins, int **bins) const
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
      if(computefor[i])
      {
        //now subtract the dmphaseoffsets for this observing frequency
        status = vectorAddC_f64(dmPhaseOffsets[i], averagephase, absolutephases, numchannels[i]);
        if(status != vecNoErr)
          csevere << startl << "Error!!! Problem adding dmPhaseOffsets to average phase!!!" << endl;

        //now work out the modulophase for each element and thence its bin
        for(int j=0;j<numchannels[i];j++)
        {
          modulophase = absolutephases[j] - floor(absolutephases[j]);
          if(modulophase < binphases[0] || modulophase >= binphases[numbins-1])
          {
            bins[i][j] = 0;
            //currentbincounts[0][i][j]++;
          }
          else
          {
            for(int k=numbins-1;k>0;k--)
            {
              if(modulophase < binphases[k] && modulophase >= binphases[k-1])
              {
                bins[i][j] = k;
                //currentbincounts[k][i][j]++;
              }
            }
          }
        }
      }
    }
}

bool Polyco::setFrequencyValues(int nfreqs, double * freqs, double * bws, int * nchans, bool * compute)
{
    double channelfreq;
    bool ok = true;

    numfreqs = nfreqs;

    lofrequencies = vectorAlloc_f32(numfreqs);
    bandwidths    = vectorAlloc_f32(numfreqs);
    estimatedbytes += 8*numfreqs;
    numchannels   = new int[numfreqs];
    computefor    = new bool[numfreqs];
    for(int i=0;i<numfreqs;i++) {
      lofrequencies[i] = freqs[i];
      bandwidths[i]    = bws[i];
      numchannels[i]   = nchans[i];
      computefor[i]    = compute[i];
    }

    //create the phase offsets and channeldmdelay arrays
    dmPhaseOffsets = new double*[numfreqs];
    channeldmdelays = new double*[numfreqs];
    for(int i=0;i<numfreqs;i++)
    {
        dmPhaseOffsets[i] = vectorAlloc_f64(numchannels[i]);
        channeldmdelays[i] = vectorAlloc_f64(numchannels[i]);
        estimatedbytes += 2*8*numchannels[i];
        for(int j=0;j<numchannels[i];j++)
        {
            channelfreq = lofrequencies[i] + j*bandwidths[i]/double(numchannels[i]);
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
/*
    //create the bincounts array
    currentbincounts = new int**[numbins];
    for(int i=0;i<numbins;i++)
    {
        currentbincounts[i] = new int*[numfreqs];
        for(int j=0;j<numfreqs;j++)
        {
            currentbincounts[i][j] = vectorAlloc_s32(numchannels[i]+1);
            estimatedbytes += 4*(numchannels[i]+1);
            for(int k=0;k<numchannels[i]+1;k++)
                currentbincounts[i][j][k] = 0;
        }
    }*/
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
      cerror << startl << "Polyco will not be accurate over entire range of " << calclengthmins << " as the frequency is changing too rapidly.  The maximum safe calc length would be " << maxdeltafreq/freqgradient << " - try reducing blocks per send or numchannels" << ", maxdeltaphase is " << maxdeltaphase << ", minbinwidth is " << minbinwidth << ", freqgradient is " << freqgradient << ", maxdeltafreq is " << maxdeltafreq << ", dm is " << dm << ", minlofreq is " << minlofreq << endl;

    //work out the initial dmphasecorrect array and set the lastphasecalctime
    calculateDMPhaseOffsets(calclengthmins/2);
}

bool Polyco::includesTime(int incmjd, double incmjdfraction) const
{
    double differencemins = (incmjd - mjd)*1440 + (incmjdfraction - mjdfraction)*1440;
    double rangemins = double(timespan)/2.0 + 1.0/60.0; //allow an extra second range to avoid being bitten by roundoff

    return (differencemins <= rangemins) && (differencemins >= -rangemins);
}

Polyco * Polyco::getCurrentPolyco(int requiredconfig, int mjd, double mjdfraction, Polyco ** polycos, int npolycos, bool printtimes)
{
    for(int i=0;i<npolycos;i++)
    {
      if(printtimes) {
        cinfo << startl << "Polyco[" << i << "] has config " << polycos[i]->getConfig() << "; required config is " << requiredconfig << endl;
        cinfo << startl << "Center time MJD is " << polycos[i]->getMJD() << ", fraction " << polycos[i]->getMJDfraction() << " and timespan (mins) is " << polycos[i]->getSpanMinutes() << endl;
        cinfo << startl << "The requested time was MJD " << mjd << " + fraction " << mjdfraction << endl;
      }
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
        status = vectorMulC_f64(channeldmdelays[i], currentfreq, dmPhaseOffsets[i], numchannels[i]);
        if(status != vecNoErr)
          csevere << startl << "Error!!! Problem calculating dmPhaseOffsets!!!" << endl;
    }
}

bool Polyco::loadPolycoFile(string filename, int subcount)
{
    string strbuffer;
    istringstream iss;
    int hour, minute;
    double timedouble, second, mjddouble;

    ifstream input(filename.c_str(), ios::in);
    if(!input.is_open() || input.bad()) {
      cerror << startl << "Error trying to open polyco file " << filename << endl;
      return false; //note return with failure here!!!
    }

    coefficients = 0;
    iss.precision(20);

    cinfo << startl << "About to start processing the polyco file " << filename << ", looking for subcount " << subcount << endl;

    for(int s=0;s<=subcount;s++)
    {
      //process the first line
      getline(input, strbuffer); 
      iss.str(strbuffer);

      iss >> pulsarname; //pulsarname
      iss >> strbuffer; //date - ignore (will use mjd instead)
      iss >> strbuffer; //time
      timedouble = atof(strbuffer.c_str());
      hour = int(timedouble/10000);
      minute = int((timedouble - 10000*hour)/100);
      second = timedouble - (minute*100 + hour*10000);
      iss >> mjddouble; //mjd
      mjd = int(mjddouble);
      // David Nice says that the correct time is the fractional MJD, and the HHMMSS is rounded to the nearest second
      // and hence not guaranteed to be correct.  Hence the new check and changed value of mjdfraction below.
      mjdfraction = double(hour)/24.0 + double(minute)/1440.0 + second/86400.0;
      if(abs(mjdfraction - (mjddouble - mjd)) > 0.00002/86400) {
        csevere << startl << "Times in the polyco header (as given by HHMMSS vs fractional MJD) differ by more than 20 microseconds.  Using the fractional MJD." << endl;
      }
      mjdfraction = mjddouble - mjd;

      iss >> dm; //dm

      //check if we missed anything important
      if (iss.fail()) {
        cfatal << startl << "Hit end of first line prematurely - check that polyco conforms to standard! Aborting." << endl;
        return false;
      }
      iss >> dopplershift; //dopplershift
      iss >> logresidual; //logresidual

      //check if we missed anything non-important
      if (iss.fail()) {
        cwarn << startl << "Hit end of first line prematurely (missed dopplershift and/or logresidual).  Likely everything is ok - continuing." << endl;
      }
      iss.clear(); //in case we hit the end of the line

      //process the second line
      getline(input, strbuffer); 
      iss.str(strbuffer);

      iss >> refphase; //refphase
      refphase = refphase - floor(refphase);
      iss >> f0; //reference frequency
      iss >> strbuffer;
      observatory = atoi(strbuffer.c_str()); //observatory code (normally integer, but not used anyway)
      iss >> timespan; //timespan in minutes
      iss >> numcoefficients; //number of polynomial coefficients
      iss >> obsfrequency; //observing frequency (MHz)

      //check if we missed anything important
      if (iss.fail()) {
        cfatal << startl << "Hit end of second line prematurely - check that polyco conforms to standard! Aborting." << endl;
        return false;
      }

      iss >> binaryphase;
      //check if we missed anything non-important
      if (iss.fail()) {
        ; //non-binary pulsars don't have anything here, so no point actually issuing a warning
      }
      iss.clear(); //in case we hit the end of the line

      //grab all the coefficients
      coefficients = new double[numcoefficients];
      for(int i=0;i<numcoefficients;i++)
      {
        if(i%3 == 0) {
          iss.clear(); //clear the eof
          getline(input, strbuffer);
          iss.str(strbuffer);
        }
        iss >> strbuffer;
        if(iss.fail()) {
          cfatal << startl << "Input related error processing polyco file " << filename << " coefficients! Aborting." << endl;
          return false;
        }
        //replace any D or d characters with E, so atof works properly
        size_t lookHere = 0;
        size_t foundHere;
        while((foundHere = strbuffer.find("D", lookHere)) != string::npos)
        {
          strbuffer.replace(foundHere, 1, "E");
          lookHere = foundHere + 1;
        }
        while((foundHere = strbuffer.find("d", lookHere)) != string::npos)
        {
          strbuffer.replace(foundHere, 1, "E");
          lookHere = foundHere + 1;
        }
        coefficients[i] = atof(strbuffer.c_str());
      }
      iss.clear();
    }
    input.close();

    //generate the frequency and phase coefficient arrays
    freqcoefficientarray = vectorAlloc_f64(numcoefficients);
    freqcoefficientarray[0] = f0 + coefficients[1]/60.0;
    freqcoefficientarray[numcoefficients-1] = 0.0;
    for(int i=1;i<numcoefficients-1;i++)
        freqcoefficientarray[i] = (i+1)*coefficients[i+1]/60.0;

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
// vim: shiftwidth=2:softtabstop=2:expandtab
