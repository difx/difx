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
#include <mpi.h>
#include <string.h>
#include <climits>
#include <ctype.h>
#include <cmath>
#include "mk5mode.h"
#include "configuration.h"
#include "mode.h"
#include "visibility.h"
#include "alert.h"
#include "vdifio.h"
#include "mathutil.h"

int Configuration::MONITOR_TCP_WINDOWBYTES;

// finds the integer closest to but not less than the square root of fftchannels
static unsigned int calcstridelength(unsigned int arraylength)
{
  unsigned int guess;

  if(arraylength <= 16)
    return arraylength;

  // this is guaranteed to terminate when guess gets to 1
  for(guess = sqrt(arraylength+0.1); ; --guess)
  {
    if(arraylength % guess == 0)
      return arraylength / guess;
  }
}

Configuration::Configuration(const char * configfile, int id, double restartsec)
  : mpiid(id), consistencyok(true), restartseconds(restartsec)
{
  string configfilestring = configfile;
  size_t basestart = configfilestring.find_last_of('/');
  if(basestart == string::npos)
    basestart = 0;
  else
    basestart = basestart+1;
  jobname = configfilestring.substr(basestart, string(configfile).find_last_of('.')-basestart);
  char * difxmtu = getenv("DIFX_MTU");
  if(difxmtu == 0)
    mtu = 1500;
  else
    mtu = atoi(difxmtu);
  if (mtu > 9000) {
    cerror << startl << "DIFX_MTU was set to " << mtu << " - resetting to 9000 bytes (max)" << endl;
    mtu = 9000;
  }

  sectionheader currentheader = INPUT_EOF;
  commonread = false;
  datastreamread = false;
  configread = false;
  freqread = false;
  ruleread = false;
  baselineread = false;
  maxnumchannels = 0;
  estimatedbytes = 0;
  model = NULL;

  //open the file
  ifstream * input = new ifstream(configfile);
  if(input->fail() || !input->is_open())
  {
    //need to write this message from all processes - sometimes it is visible to head node but no-one else...
    cfatal << startl << "Cannot open file " << configfile << " - aborting!!!" << endl;
    consistencyok = false;
  }
  else
    currentheader = getSectionHeader(input);

  //go through all the sections and tables in the input file
  while(consistencyok && currentheader != INPUT_EOF)
  {
    switch(currentheader)
    {
      case COMMON:
        processCommon(input);
        break;
      case CONFIG:
        if(!commonread)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Input file out of order!  Attempted to read configuration details without knowledge of common settings - aborting!!!" << endl;
          consistencyok = false;
        }
        else
          consistencyok = processConfig(input);
        break;
      case RULE:
        if(!configread) {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Input file out of order!  Attempted to read rule details without knowledge of configurations - aborting!!!" << endl;
          consistencyok = false;
        }
        else
          consistencyok = processRuleTable(input);
        break;
      case FREQ:
        consistencyok = processFreqTable(input);
        break;
      case TELESCOPE:
        processTelescopeTable(input);
        break;
      case DATASTREAM:
        if(!configread || ! freqread)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Input file out of order!  Attempted to read datastreams without knowledge of one or both of configs/freqs - aborting!!!" << endl;
          consistencyok = false;
        }
        else
          consistencyok = processDatastreamTable(input);
        break;
      case BASELINE:
        if(!configread || !freqread)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Input file out of order! Attempted to read baselines without knowledge of freqs - aborting!!!" << endl;
          consistencyok = false;
        }
        consistencyok = processBaselineTable(input);
        break;
      case DATA:
        if(!datastreamread)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Input file out of order!  Attempted to read datastream data files without knowledge of datastreams - aborting!!!" << endl;
          consistencyok = false;
        }
        else
          processDataTable(input);
        break;
      case NETWORK:
        if(!datastreamread)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Input file out of order!  Attempted to read datastream network details without knowledge of datastreams - aborting!!!" << endl;
          consistencyok = false;
        }
        else
          processNetworkTable(input);
        break;
      default:
        break;
    }
    currentheader = getSectionHeader(input);
  }
  if(!configread || !ruleread || !commonread || !datastreamread || !freqread)
  {
    if(mpiid == 0) //only write one copy of this error message
    {
      if(!configread)
      {
        cerror << startl << "CONFIGURATION section not parsed" << endl;
      }
      if(!ruleread)
      {
        cerror << startl << "RULE section not parsed" << endl;
      }
      if(!commonread)
      {
        cerror << startl << "COMMON section not parsed" << endl;
      }
      if(!datastreamread)
      {
        cerror << startl << "DATASTREAM section not parsed" << endl;
      }
      if(!freqread)
      {
        cerror << startl << "FREQ section not parsed" << endl;
      }
      cfatal << startl << "One or more sections missing from input file - aborting!!!" << endl;
    }
    consistencyok = false;
  }
  input->close();
  delete input;

  if (consistencyok) {

    //work out which frequencies are used in each config, and the minimum #channels
    freqdata freq;
    // int oppositefreqindex;
    for(int i=0;i<numconfigs;i++)
    {
      freq = freqtable[getBFreqIndex(i,0,0)];
      configs[i].minpostavfreqchannels = freq.numchannels/freq.channelstoaverage;
      configs[i].frequsedbybaseline = new bool[freqtablelength]();
      configs[i].equivfrequsedbybaseline = new bool[freqtablelength]();
      for(int j=0;j<freqtablelength;j++) {
	configs[i].frequsedbybaseline[j] = false;
	configs[i].equivfrequsedbybaseline[j] = false;
      }
      for(int j=0;j<numbaselines;j++)
      {
	for(int k=0;k<baselinetable[configs[i].baselineindices[j]].numfreqs;k++)
        {
	  //cout << "Setting frequency " << getBFreqIndex(i,j,k) << " used to true, from baseline " << j << ", baseline frequency " << k << endl; 
	  freq = freqtable[getBFreqIndex(i,j,k)];
	  configs[i].frequsedbybaseline[getBFreqIndex(i,j,k)] = true;
	  if(freq.numchannels/freq.channelstoaverage < configs[i].minpostavfreqchannels)
	    configs[i].minpostavfreqchannels = freq.numchannels/freq.channelstoaverage;
	}
      }
    }
  
    //for each freq, check if an equivalent frequency is used, to ensure autocorrelations also get sent where required
    double bwdiff, freqdiff;
    for(int i=0;i<numconfigs;i++) {
      for(int j=0;j<freqtablelength;j++) {
	if(!configs[i].frequsedbybaseline[j]) {
	  for(int k=0;k<freqtablelength;k++) {
	    bwdiff = freqtable[j].bandwidth - freqtable[k].bandwidth;
	    freqdiff = freqtable[j].bandedgefreq - freqtable[k].bandedgefreq;
	    if(freqtable[j].lowersideband)
	      freqdiff -= freqtable[j].bandwidth;
	    if(freqtable[k].lowersideband)
	      freqdiff += freqtable[k].bandwidth;
	    if(bwdiff < Mode::TINY && freqdiff < Mode::TINY && freqtable[j].numchannels == freqtable[k].numchannels && 
	       freqtable[j].channelstoaverage == freqtable[k].channelstoaverage && 
	       freqtable[j].oversamplefactor == freqtable[k].oversamplefactor &&
	       freqtable[j].decimationfactor == freqtable[k].decimationfactor) {
	      if(configs[i].frequsedbybaseline[k])
		configs[i].equivfrequsedbybaseline[j] = true;
	    }
	  }
	}
      }
    }

    //set any opposite sideband freqs to be "used", to ensure their autocorrelations are not lost
    //for(int i=0;i<numconfigs;i++) {
    //  for(int j=0;j<freqtablelength;j++) {
    //    if(configs[i].frequsedbybaseline[j]) {
    //      oppositefreqindex = getOppositeSidebandFreqIndex(j);
    //      if(oppositefreqindex >= 0)
    //        configs[i].frequsedbybaseline[oppositefreqindex] = true;
    //    }
    //  }
    //}
    
    //process the pulsar configuration files
    for(int i=0;i<numconfigs;i++)
      {
	if(configs[i].pulsarbin)
	  {
	    if (consistencyok)
	      consistencyok = processPulsarConfig(configs[i].pulsarconfigfilename, i);
	    if (consistencyok)
	      consistencyok = setPolycoFreqInfo(i);
	  }
	
	if(configs[i].phasedarray)
	  {
	    if (consistencyok)
	      consistencyok = processPhasedArrayConfig(configs[i].phasedarrayconfigfilename, i);
	  }
      }
    if(consistencyok) {
      model = new Model(this, calcfilename);
      consistencyok = model->openSuccess();
    }
    for(int i=0;i<telescopetablelength;i++) {
      if(consistencyok)
	consistencyok = model->addClockTerms(telescopetable[i].name, telescopetable[i].clockrefmjd, telescopetable[i].clockorder, telescopetable[i].clockpoly, false);
    }
    if(consistencyok)
      consistencyok = setStrides();
    if(consistencyok)
      estimatedbytes += model->getEstimatedBytes();
    if(consistencyok)
      consistencyok = populateScanConfigList();
    if(consistencyok)
      consistencyok = populateModelDatastreamMap();
    if(consistencyok)
      consistencyok = populateResultLengths();
    if(consistencyok)
      consistencyok = consistencyCheck();
    if(consistencyok)
      consistencyok = populateRecordBandIndicies();
    commandthreadinitialised = false;
    dumpsta = false;
    dumplta = false;
    dumpkurtosis = false;
    stadumpchannels = DEFAULT_MONITOR_NUMCHANNELS;
    ltadumpchannels = DEFAULT_MONITOR_NUMCHANNELS;
    
    char *monitor_tcpwin = getenv("DIFX_MONITOR_TCPWINDOW");
    if (monitor_tcpwin!=0) {
      Configuration::MONITOR_TCP_WINDOWBYTES = atoi(monitor_tcpwin)*1024;
      cinfo << startl << "DIFX_MONITOR_TCPWINDOW set to" << Configuration::MONITOR_TCP_WINDOWBYTES/1024 << "kB" << endl;
    } else {
      Configuration::MONITOR_TCP_WINDOWBYTES = 262144;
    }
  }
}

Configuration::~Configuration()
{
  if(configread)
  {
    for(int i=0;i<numconfigs;i++)
    {
      delete [] configs[i].arraystridelen;
      delete [] configs[i].datastreamindices;
      delete [] configs[i].baselineindices;
      delete [] configs[i].ordereddatastreamindices;
      delete [] configs[i].frequsedbybaseline;
      delete [] configs[i].equivfrequsedbybaseline;
    }
    delete [] configs;
  }
  if(datastreamread)
  {
    for(int i=0;i<datastreamtablelength;i++)
    {
      delete [] datastreamtable[i].recordedfreqtableindices;
      delete [] datastreamtable[i].recordedfreqpols;
      delete [] datastreamtable[i].recordedfreqclockoffsets;
      delete [] datastreamtable[i].recordedfreqclockoffsetsdelta;
      delete [] datastreamtable[i].recordedfreqphaseoffset;
      delete [] datastreamtable[i].recordedfreqlooffsets;
      delete [] datastreamtable[i].zoomfreqtableindices;
      delete [] datastreamtable[i].zoomfreqpols;
      delete [] datastreamtable[i].zoomfreqparentdfreqindices;
      delete [] datastreamtable[i].zoomfreqchanneloffset;
      delete [] datastreamtable[i].recordedbandpols;
      delete [] datastreamtable[i].recordedbandlocalfreqindices;
      delete [] datastreamtable[i].zoombandpols;
      delete [] datastreamtable[i].zoombandlocalfreqindices;
      delete [] datastreamtable[i].datafilenames;
      if(datastreamtable[i].phasecalintervalmhz > 0) {
	for (int j=0;j<datastreamtable[i].numrecordedfreqs;j++)
	  delete [] datastreamtable[i].recordedfreqpcaltonefreqs[j];
	delete [] datastreamtable[i].numrecordedfreqpcaltones;
	delete [] datastreamtable[i].recordedfreqpcaltonefreqs;
	delete [] datastreamtable[i].recordedfreqpcaloffsetshz;
      }
    }
    delete [] datastreamtable;
  }
  if(model)
    delete model;
  delete [] freqtable;
  delete [] telescopetable;
  for(int i=0;i<baselinetablelength;i++)
  {
    for(int j=0;j<baselinetable[i].numfreqs;j++)
    {
      for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
        delete [] baselinetable[i].polpairs[j][k];
      delete [] baselinetable[i].polpairs[j];
      delete [] baselinetable[i].datastream1bandindex[j];
      delete [] baselinetable[i].datastream2bandindex[j];
      delete [] baselinetable[i].datastream1recordbandindex[j];
      delete [] baselinetable[i].datastream2recordbandindex[j];
    }
    delete [] baselinetable[i].datastream1bandindex;
    delete [] baselinetable[i].datastream2bandindex;
    delete [] baselinetable[i].datastream1recordbandindex;
    delete [] baselinetable[i].datastream2recordbandindex;
    delete [] baselinetable[i].numpolproducts;
    delete [] baselinetable[i].freqtableindices;
    delete [] baselinetable[i].polpairs;
  }
  delete [] baselinetable;
  delete [] numprocessthreads;
}

int Configuration::genMk5FormatName(dataformat format, int nchan, double bw, int nbits, datasampling sampling, int framebytes, int decimationfactor, int numthreads, char *formatname) const
{
  int fanout=1, mbps;

  mbps = int(2*nchan*bw*nbits + 0.5);

  switch(format)
  {
    case MKIV:
      fanout = framebytes*8/(20000*nbits*nchan);
      if(fanout*20000*nbits*nchan != framebytes*8)
      {
        cfatal << startl << "genMk5FormatName : MKIV format : framebytes = " << framebytes << " is not allowed" << endl;
        return -1;
      }
      if(decimationfactor > 1)	// Note, this conditional is to ensure compatibility with older mark5access versions
        sprintf(formatname, "MKIV1_%d-%d-%d-%d/%d", fanout, mbps, nchan, nbits, decimationfactor);
      else
        sprintf(formatname, "MKIV1_%d-%d-%d-%d", fanout, mbps, nchan, nbits);
      break;
    case VLBA:
      fanout = framebytes*8/(20160*nbits*nchan);
      if(fanout*20160*nbits*nchan != framebytes*8)
      {
        cfatal << startl << "genMk5FormatName : VLBA format : framebytes = " << framebytes << " is not allowed" << endl;
        return -1;
      }
      if(decimationfactor > 1)
        sprintf(formatname, "VLBA1_%d-%d-%d-%d/%d", fanout, mbps, nchan, nbits, decimationfactor);
      else
        sprintf(formatname, "VLBA1_%d-%d-%d-%d", fanout, mbps, nchan, nbits);
      break;
    case VLBN:
      fanout = framebytes*8/(20160*nbits*nchan);
      if(fanout*20160*nbits*nchan != framebytes*8)
      {
        cfatal << startl << "genMk5FormatName : VLBN format : framebytes = " << framebytes << " is not allowed" << endl;
        return -1;
      }
      if(decimationfactor > 1)
        sprintf(formatname, "VLBN1_%d-%d-%d-%d/%d", fanout, mbps, nchan, nbits, decimationfactor);
      else
        sprintf(formatname, "VLBN1_%d-%d-%d-%d", fanout, mbps, nchan, nbits);
      break;
    case MARK5B:
      if(decimationfactor > 1)
        sprintf(formatname, "Mark5B-%d-%d-%d/%d", mbps, nchan, nbits, decimationfactor);
      else
        sprintf(formatname, "Mark5B-%d-%d-%d", mbps, nchan, nbits);
      break;
    case KVN5B:
        sprintf(formatname, "KVN5B-%d-%d-%d", mbps, nchan, nbits);
      break;
    case INTERLACEDVDIF:
      //framebytes = (framebytes-VDIF_HEADER_BYTES)*numthreads + VDIF_HEADER_BYTES;
      //mbps /= nchan;
      //nchan = 1;
    case VDIF:
      if (sampling==COMPLEX) 
	if(decimationfactor > 1)
	  sprintf(formatname, "VDIFC_%d-%d-%d-%d/%d", framebytes-VDIF_HEADER_BYTES, mbps, nchan, nbits, decimationfactor);
	else
	  sprintf(formatname, "VDIFC_%d-%d-%d-%d", framebytes-VDIF_HEADER_BYTES, mbps, nchan, nbits);
      else
	if(decimationfactor > 1)
	  sprintf(formatname, "VDIF_%d-%d-%d-%d/%d", framebytes-VDIF_HEADER_BYTES, mbps, nchan, nbits, decimationfactor);
	else
	  sprintf(formatname, "VDIF_%d-%d-%d-%d", framebytes-VDIF_HEADER_BYTES, mbps, nchan, nbits);
      break;
    case VDIFL:
      if (sampling==COMPLEX) 
	if(decimationfactor > 1)
	  sprintf(formatname, "VDIFCL_%d-%d-%d-%d/%d", framebytes-VDIF_LEGACY_HEADER_BYTES, mbps, nchan, nbits, decimationfactor);
	else
	  sprintf(formatname, "VDIFCL_%d-%d-%d-%d", framebytes-VDIF_LEGACY_HEADER_BYTES, mbps, nchan, nbits);
      else
	if(decimationfactor > 1)
	  sprintf(formatname, "VDIFL_%d-%d-%d-%d/%d", framebytes-VDIF_LEGACY_HEADER_BYTES, mbps, nchan, nbits, decimationfactor);
	else
	  sprintf(formatname, "VDIFL_%d-%d-%d-%d", framebytes-VDIF_LEGACY_HEADER_BYTES, mbps, nchan, nbits);
      break;
    default:
      cfatal << startl << "genMk5FormatName : unsupported format encountered" << endl;
      return -1;
  }

  return fanout;
}

int Configuration::getFramePayloadBytes(int configindex, int configdatastreamindex) const
{
  int payloadsize;
  int framebytes = getFrameBytes(configindex, configdatastreamindex);
  dataformat format = getDataFormat(configindex, configdatastreamindex);
  
  switch(format)
  {
    case VLBA:
    case VLBN:
      payloadsize = (framebytes/2520)*2500;
      break;
    case MARK5B:
    case KVN5B:
      payloadsize = framebytes - 16;
      break;
    case INTERLACEDVDIF:
    case VDIF:
      payloadsize = framebytes - VDIF_HEADER_BYTES; 
      break;
    case VDIFL:
      payloadsize = framebytes - VDIF_LEGACY_HEADER_BYTES; 
      break;
    default:
      payloadsize = framebytes;
  }

  return payloadsize;
}

void Configuration::getFrameInc(int configindex, int configdatastreamindex, int &sec, int &ns) const
{
  int nchan, qb, decimationfactor;
  int payloadsize;
  double samplerate; /* in Hz */
  double seconds;

  nchan = getDNumRecordedBands(configindex, configdatastreamindex);
  samplerate = 2.0e6*getDRecordedBandwidth(configindex, configdatastreamindex, 0);
  qb = getDNumBits(configindex, configdatastreamindex);
  decimationfactor = getDDecimationFactor(configindex, configdatastreamindex);
  payloadsize = getFramePayloadBytes(configindex, configdatastreamindex);

  seconds = payloadsize*8/(samplerate*nchan*qb*decimationfactor); // Works for complex data, even if technically "wrong"
  sec = int(seconds);
  ns = int(1.0e9*(seconds - sec));
}

int Configuration::getFramesPerSecond(int configindex, int configdatastreamindex) const
{
  int nchan, qb, decimationfactor;
  int payloadsize;
  double samplerate; /* in Hz */

  nchan = getDNumRecordedBands(configindex, configdatastreamindex);
  samplerate = 2.0e6*getDRecordedBandwidth(configindex, configdatastreamindex, 0);
  qb = getDNumBits(configindex, configdatastreamindex);
  decimationfactor = getDDecimationFactor(configindex, configdatastreamindex);
  payloadsize = getFramePayloadBytes(configindex, configdatastreamindex);

  // This will always work out to be an integer
  return int(samplerate*nchan*qb*decimationfactor/(8*payloadsize) + 0.5); // Works for complex data
}

int Configuration::getMaxDataBytes() const
{
  int length;
  int maxlength = getDataBytes(0,0);

  for(int i=0;i<numconfigs;i++)
  {
    for(int j=0;j<numdatastreams;j++)
    {
      length = getDataBytes(i,j);
      if(length > maxlength)
        maxlength = length;
    }
  }

  return maxlength;
}

int Configuration::getMaxDataBytes(int datastreamindex) const
{
  int length;
  int maxlength = getDataBytes(0,datastreamindex);

  for(int i=1;i<numconfigs;i++)
  {
    length = getDataBytes(i,datastreamindex);
    if(length > maxlength)
      maxlength = length;
  }

  return maxlength;
}

int Configuration::getMaxBlocksPerSend() const
{
  int length;
  int maxlength = configs[0].blockspersend;

  for(int i=1;i<numconfigs;i++)
  {
    length = configs[i].blockspersend;
    if(length > maxlength)
      maxlength = length;
  }

  return maxlength;
}

int Configuration::getMaxNumRecordedFreqs() const
{
  int currentnumfreqs, maxnumfreqs = 0;
  
  for(int i=0;i<numconfigs;i++)
  {
    currentnumfreqs = getMaxNumRecordedFreqs(i);
    if(currentnumfreqs > maxnumfreqs)
      maxnumfreqs = currentnumfreqs;
  }
  
  return maxnumfreqs;
}

int Configuration::getMaxNumRecordedFreqs(int configindex) const
{
  int maxnumfreqs = 0;
  
  for(int i=0;i<numdatastreams;i++)
  {
    if(datastreamtable[configs[configindex].datastreamindices[i]].numrecordedfreqs > maxnumfreqs)
      maxnumfreqs = datastreamtable[configs[configindex].datastreamindices[i]].numrecordedfreqs;
  }

  return maxnumfreqs;
}

int Configuration::getMaxNumFreqDatastreamIndex(int configindex) const
{
  int maxindex = 0;
  int maxnumfreqs = datastreamtable[configs[configindex].datastreamindices[0]].numrecordedfreqs;
  
  for(int i=1;i<numdatastreams;i++)
  {
    if(datastreamtable[configs[configindex].datastreamindices[i]].numrecordedfreqs > maxnumfreqs)
    {
      maxnumfreqs = datastreamtable[configs[configindex].datastreamindices[i]].numrecordedfreqs;
      maxindex = i;
    }
  }
  
  return maxindex;
}

int Configuration::getMaxPhaseCentres(int configindex) const
{
  int maxphasecentres = 1;
  for(int i=0;i<model->getNumScans();i++) {
    if(scanconfigindices[i] == configindex) {
      if(model->getNumPhaseCentres(i) > maxphasecentres)
        maxphasecentres = model->getNumPhaseCentres(i);
    }
  }
  return maxphasecentres;
}

int Configuration::getOppositeSidebandFreqIndex(int freqindex) const
{
  int toreturn = -1;
  freqdata f1 = freqtable[freqindex];

  for(int i=0;i<freqtablelength;i++)
  {
    freqdata f = freqtable[i];
    if(f.bandwidth == f1.bandwidth && f.lowersideband != f1.lowersideband && f.numchannels == f1.numchannels && f.channelstoaverage == f1.channelstoaverage && f.oversamplefactor == f1.oversamplefactor && f.decimationfactor == f1.decimationfactor)
    {
      if(f.lowersideband && f1.bandedgefreq + f1.bandwidth == f.bandedgefreq)
        toreturn = i;
      if(!f.lowersideband && f.bandedgefreq + f.bandwidth == f1.bandedgefreq)
        toreturn = i;
    }
  }

  return toreturn;
}

int Configuration::getDataBytes(int configindex, int datastreamindex) const
{
  int validlength, payloadbytes, framebytes, numframes;
  const datastreamdata &currentds = datastreamtable[configs[configindex].datastreamindices[datastreamindex]];
  const freqdata &arecordedfreq = freqtable[currentds.recordedfreqtableindices[0]]; 
  validlength = (arecordedfreq.decimationfactor*configs[configindex].blockspersend*currentds.numrecordedbands*2*currentds.numbits*arecordedfreq.numchannels)/8;
  if(currentds.format == MKIV || currentds.format == VLBA || currentds.format == VLBN || currentds.format == MARK5B || currentds.format == KVN5B || currentds.format == VDIF ||currentds.format == VDIFL || currentds.format == INTERLACEDVDIF)
  {
    //must be an integer number of frames, with enough margin for overlap on either side
    validlength += (arecordedfreq.decimationfactor*(int)(configs[configindex].guardns/(1000.0/(freqtable[currentds.recordedfreqtableindices[0]].bandwidth*2.0))+1.0)*currentds.numrecordedbands*currentds.numbits)/8;
    payloadbytes = getFramePayloadBytes(configindex, datastreamindex);
    framebytes = currentds.framebytes;
    if(currentds.format == INTERLACEDVDIF) //account for change to larger packets after muxing
    {
      int nbands = 1;
      
      // round up to next power of 2 bands
      while(nbands < currentds.numrecordedbands)
      {
        nbands *= 2;
      }

      payloadbytes *= getDNumMuxThreads(configindex, datastreamindex);
      framebytes = payloadbytes + VDIF_HEADER_BYTES; // Assume INTERLACEDVDIF always non-legacy
    }
    numframes = (validlength/payloadbytes + 2);
    if(currentds.format == MARK5B || currentds.format == KVN5B) //be cautious in case the frame granularity is 2 (easier than checking)
    {
      numframes += numframes%2;
    }
    validlength = numframes*framebytes;
    //cout << "About to set databytes to " << validlength << " since currentds.framebytes is " << currentds.framebytes << " and blockspersend is " << configs[configindex].blockspersend << endl;
  }
  return validlength;
}

int Configuration::getMaxProducts(int configindex) const
{
  baselinedata current;
  int maxproducts = 0;
  for(int i=0;i<numbaselines;i++)
  {
    current = baselinetable[configs[configindex].baselineindices[i]];
    for(int j=0;j<current.numfreqs;j++)
    {
      if(current.numpolproducts[j] > maxproducts)
        maxproducts = current.numpolproducts[j];
    }
  }
  return maxproducts;
}

int Configuration::getMaxProducts() const
{
  int maxproducts = 0;

  for(int i=0;i<numconfigs;i++)
  {
    if(getMaxProducts(i) > maxproducts)
      maxproducts = getMaxProducts(i);
  }
  
  return maxproducts;
}

// returns number of polarisations recorded.  Lists the polarisations in the pols argument
// sort order is always R L X Y
int Configuration::getRecordedPolarisations(char *pols) const
{
  int nPol = 0;
  bool hasR=false;
  bool hasL=false;
  bool hasX=false;
  bool hasY=false;
  for(int i=0;i<4;i++)
  {
    pols[i] = 0;
  }
  for(int d=0;d<getDatastreamTableLength();++d)
  {
    for(int i=0;i<datastreamtable[d].numrecordedbands;i++)
    {
      char p = datastreamtable[d].recordedbandpols[i];
      switch(p)
      {
        case 'R': hasR=true; break;
        case 'L': hasL=true; break;
        case 'X': hasX=true; break;
        case 'Y': hasY=true; break;
      }
    }
  }
  if(hasR) pols[nPol++] = 'R';
  if(hasL) pols[nPol++] = 'L';
  if(hasX) pols[nPol++] = 'X';
  if(hasY) pols[nPol++] = 'Y';

  return nPol;
}

int Configuration::getDMatchingBand(int configindex, int datastreamindex, int bandindex) const
{
  datastreamdata ds = datastreamtable[configs[configindex].datastreamindices[datastreamindex]];
  if(bandindex >= ds.numrecordedbands) {
    for(int i=0;i<ds.numzoombands;i++)
    {
      if(ds.zoombandlocalfreqindices[bandindex] == ds.zoombandlocalfreqindices[i] && (i != bandindex))
        return i;
    }
  }
  else {
    for(int i=0;i<ds.numrecordedbands;i++)
    {
      if(ds.recordedbandlocalfreqindices[bandindex] == ds.recordedbandlocalfreqindices[i] && (i != bandindex))
        return i;
    }
  }

  return -1;
}

int Configuration::getCNumProcessThreads(int corenum) const
{
  if(numcoreconfs == 0)
    return 1;
  if(corenum < numcoreconfs)
    return numprocessthreads[corenum];
  cwarn << startl << "Trying to get a number of threads for core " << corenum+1 << " when only " << numcoreconfs << " provided in .threads file - setting num threads for this core to " << numprocessthreads[numcoreconfs-1] << ", which was the last value in the .threads file" << endl;
  return numprocessthreads[numcoreconfs-1];
}

bool Configuration::stationUsed(int telescopeindex) const
{
  bool toreturn = false;

  for(int i=0;i<numconfigs;i++)
  {
    for(int j=0;j<numdatastreams;j++)
    {
      if(datastreamtable[configs[i].datastreamindices[j]].telescopeindex == telescopeindex)
        toreturn = true;
    }
  }

  return toreturn;
}

Mode* Configuration::getMode(int configindex, int datastreamindex)
{
  configdata conf = configs[configindex];
  datastreamdata stream = datastreamtable[conf.datastreamindices[datastreamindex]];
  int framesamples, framebytes;
  int guardsamples = (int)(conf.guardns/(1000.0/(freqtable[stream.recordedfreqtableindices[0]].bandwidth*2.0)) + 0.5);
  int streamrecbandchan = freqtable[stream.recordedfreqtableindices[0]].numchannels;
  int streamdecimationfactor = freqtable[stream.recordedfreqtableindices[0]].decimationfactor;
  int streamchanstoaverage = freqtable[stream.recordedfreqtableindices[0]].channelstoaverage;
  double streamrecbandwidth = freqtable[stream.recordedfreqtableindices[0]].bandwidth;

  switch(stream.format)
  {
    case LBASTD:
      if(stream.numbits != 2)
        cerror << startl << "All LBASTD Modes must have 2 bit sampling - overriding input specification!!!" << endl;
      return new LBAMode(this, configindex, datastreamindex, streamrecbandchan, streamchanstoaverage, conf.blockspersend, guardsamples, stream.numrecordedfreqs, streamrecbandwidth,  stream.recordedfreqclockoffsets, stream.recordedfreqclockoffsetsdelta, stream.recordedfreqphaseoffset, stream.recordedfreqlooffsets, stream.numrecordedbands, stream.numzoombands, 2/*bits*/, stream.filterbank, stream.linear2circular, conf.fringerotationorder, conf.arraystridelen[datastreamindex], conf.writeautocorrs, LBAMode::stdunpackvalues);
      break;
    case LBAVSOP:
      if(stream.numbits != 2)
        cerror << startl << "All LBASTD Modes must have 2 bit sampling - overriding input specification!!!" << endl;
      return new LBAMode(this, configindex, datastreamindex, streamrecbandchan, streamchanstoaverage, conf.blockspersend, guardsamples, stream.numrecordedfreqs, streamrecbandwidth, stream.recordedfreqclockoffsets, stream.recordedfreqclockoffsetsdelta, stream.recordedfreqphaseoffset, stream.recordedfreqlooffsets, stream.numrecordedbands, stream.numzoombands, 2/*bits*/, stream.filterbank, stream.filterbank, conf.fringerotationorder, conf.arraystridelen[datastreamindex], conf.writeautocorrs, LBAMode::vsopunpackvalues);
      break;
    case LBA8BIT:
      if(stream.numbits != 8) {
        cerror << startl << "8BIT LBA mode must have 8 bits! aborting" << endl;
        return NULL;
      }
      return new LBA8BitMode(this, configindex, datastreamindex, streamrecbandchan, streamchanstoaverage, conf.blockspersend, guardsamples, stream.numrecordedfreqs, streamrecbandwidth, stream.recordedfreqclockoffsets, stream.recordedfreqclockoffsetsdelta, stream.recordedfreqphaseoffset, stream.recordedfreqlooffsets, stream.numrecordedbands, stream.numzoombands, 8/*bits*/, stream.filterbank, stream.filterbank, conf.fringerotationorder, conf.arraystridelen[datastreamindex], conf.writeautocorrs);
      break;
    case LBA16BIT:
      if(stream.numbits != 16) {
        cerror << startl << "16BIT LBA mode must have 16 bits! aborting" << endl;
        return NULL;
      }
      return new LBA16BitMode(this, configindex, datastreamindex, streamrecbandchan, streamchanstoaverage, conf.blockspersend, guardsamples, stream.numrecordedfreqs, streamrecbandwidth, stream.recordedfreqclockoffsets, stream.recordedfreqclockoffsetsdelta, stream.recordedfreqphaseoffset, stream.recordedfreqlooffsets, stream.numrecordedbands, stream.numzoombands, 16/*bits*/, stream.filterbank, stream.filterbank, conf.fringerotationorder, conf.arraystridelen[datastreamindex], conf.writeautocorrs);
      break;
    case MKIV:
    case VLBA:
    case VLBN:
    case MARK5B:
    case KVN5B:
    case VDIF:
    case VDIFL:
    case K5VSSP:
    case K5VSSP32:
    case INTERLACEDVDIF:
      framesamples = getFramePayloadBytes(configindex, datastreamindex)*8/(getDNumBits(configindex, datastreamindex)*getDNumRecordedBands(configindex, datastreamindex)*streamdecimationfactor);
      framebytes = getFrameBytes(configindex, datastreamindex);
      if (stream.sampling==COMPLEX) framesamples /=2;
      if(stream.format == INTERLACEDVDIF) { //separate frames for each subband - change numsamples, framebytes to the muxed version
        framesamples *= getDNumMuxThreads(configindex, datastreamindex);
        framebytes = (framebytes - VDIF_HEADER_BYTES)*getDNumMuxThreads(configindex, datastreamindex) + VDIF_HEADER_BYTES; // Assumed INTERLACED is never legacy
      }
      return new Mk5Mode(this, configindex, datastreamindex, streamrecbandchan, streamchanstoaverage, conf.blockspersend, guardsamples, stream.numrecordedfreqs, streamrecbandwidth, stream.recordedfreqclockoffsets, stream.recordedfreqclockoffsetsdelta, stream.recordedfreqphaseoffset, stream.recordedfreqlooffsets, stream.numrecordedbands, stream.numzoombands, stream.numbits, stream.sampling, stream.tcomplex, stream.filterbank, stream.filterbank, conf.fringerotationorder, conf.arraystridelen[datastreamindex], conf.writeautocorrs, framebytes, framesamples, stream.format);

      break;
    default:
      cerror << startl << "Unknown mode being requested!!!" << endl;
      return NULL;
  }
}

Configuration::sectionheader Configuration::getSectionHeader(ifstream * input)
{
  string line = "";

  while (line == "" && !input->eof())
    getline(*input, line); //skip the whitespace

  //return the type of section this is
  if(line.substr(0, 17) == "# COMMON SETTINGS")
    return COMMON;
  if(line.substr(0, 16) == "# CONFIGURATIONS")
    return CONFIG;
  if(line.substr(0, 7) == "# RULES")
    return RULE;
  if(line.substr(0, 12) == "# FREQ TABLE")
    return FREQ;
  if(line.substr(0, 17) == "# TELESCOPE TABLE")
    return TELESCOPE;
  if(line.substr(0, 18) == "# DATASTREAM TABLE")
    return DATASTREAM;
  if(line.substr(0, 16) == "# BASELINE TABLE")
    return BASELINE;
  if(line.substr(0, 12) == "# DATA TABLE")
    return DATA;
  if(line.substr(0, 15) == "# NETWORK TABLE")
    return NETWORK;

  if (input->eof())
    return INPUT_EOF;

  return UNKNOWN;
}

bool Configuration::processBaselineTable(ifstream * input)
{
  int tempint, dsband, findex, matchfindex;
  int ** tempintptr;
  string line;
  datastreamdata dsdata;
  baselinedata bldata;

  getinputline(input, &line, "BASELINE ENTRIES");
  baselinetablelength = atoi(line.c_str());
  baselinetable = new baselinedata[baselinetablelength]();
  estimatedbytes += baselinetablelength*sizeof(baselinedata);
  if(baselinetablelength < numbaselines)
  {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "Not enough baselines are supplied in the baseline table (" << baselinetablelength << ") compared to the number of baselines (" << numbaselines << ")!!!" << endl;
    return false;
  }

  for(int i=0;i<baselinetablelength;i++)
  {
    //read in the info for this baseline
    baselinetable[i].localfreqindices = new int[freqtablelength]();
    baselinetable[i].totalbands = 0;
    getinputline(input, &line, "D/STREAM A INDEX ", i);
    baselinetable[i].datastream1index = atoi(line.c_str());
    getinputline(input, &line, "D/STREAM B INDEX ", i);
    baselinetable[i].datastream2index = atoi(line.c_str());
    getinputline(input, &line, "NUM FREQS ", i);
    baselinetable[i].numfreqs = atoi(line.c_str());
    baselinetable[i].oddlsbfreqs = new int[baselinetable[i].numfreqs]();
    baselinetable[i].numpolproducts = new int[baselinetable[i].numfreqs]();
    baselinetable[i].datastream1bandindex = new int*[baselinetable[i].numfreqs]();
    baselinetable[i].datastream2bandindex = new int*[baselinetable[i].numfreqs]();
    baselinetable[i].datastream1recordbandindex = new int*[baselinetable[i].numfreqs]();
    baselinetable[i].datastream2recordbandindex = new int*[baselinetable[i].numfreqs]();
    baselinetable[i].freqtableindices = new int[baselinetable[i].numfreqs]();
    baselinetable[i].polpairs = new char**[baselinetable[i].numfreqs]();
    for(int j=0;j<baselinetable[i].numfreqs;j++)
    {
      baselinetable[i].oddlsbfreqs[j] = 0;
      getinputline(input, &line, "POL PRODUCTS ", i);
      baselinetable[i].numpolproducts[j] = atoi(line.c_str());
      baselinetable[i].datastream1bandindex[j] = new int[baselinetable[i].numpolproducts[j]]();
      baselinetable[i].datastream2bandindex[j] = new int[baselinetable[i].numpolproducts[j]]();
      baselinetable[i].datastream1recordbandindex[j] = new int[baselinetable[i].numpolproducts[j]]();
      baselinetable[i].datastream2recordbandindex[j] = new int[baselinetable[i].numpolproducts[j]]();
      estimatedbytes += baselinetable[i].numpolproducts[j]*2*4;
      baselinetable[i].polpairs[j] = new char*[baselinetable[i].numpolproducts[j]]();
      for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
      {
        baselinetable[i].totalbands++;
        getinputline(input, &line, "D/STREAM A BAND ", k);
        baselinetable[i].datastream1bandindex[j][k] = atoi(line.c_str());
        getinputline(input, &line, "D/STREAM B BAND ", k);
        baselinetable[i].datastream2bandindex[j][k] = atoi(line.c_str());
        baselinetable[i].polpairs[j][k] = new char[3]();
        estimatedbytes += 3;
      }
      dsdata = datastreamtable[baselinetable[i].datastream1index];
      dsband = baselinetable[i].datastream1bandindex[j][0];
      if(dsband >= dsdata.numrecordedbands) //it is a zoom band
        baselinetable[i].freqtableindices[j] = dsdata.zoomfreqtableindices[dsdata.zoombandlocalfreqindices[dsband-dsdata.numrecordedbands]];
      else
        baselinetable[i].freqtableindices[j] = dsdata.recordedfreqtableindices[dsdata.recordedbandlocalfreqindices[dsband]];
      for(int k=0;k<baselinetable[i].numpolproducts[j];k++) {
        dsdata = datastreamtable[baselinetable[i].datastream1index];
        dsband = baselinetable[i].datastream1bandindex[j][k];
        if(dsband >= dsdata.numrecordedbands) //it is a zoom band
          baselinetable[i].polpairs[j][k][0] = dsdata.zoombandpols[dsband-dsdata.numrecordedbands];
        else
          baselinetable[i].polpairs[j][k][0] = dsdata.recordedbandpols[dsband];
        dsdata = datastreamtable[baselinetable[i].datastream2index];
        dsband = baselinetable[i].datastream2bandindex[j][k];
        if(dsband >= dsdata.numrecordedbands) //it is a zoom band
          baselinetable[i].polpairs[j][k][1] = dsdata.zoombandpols[dsband-dsdata.numrecordedbands];
        else
          baselinetable[i].polpairs[j][k][1] = dsdata.recordedbandpols[dsband];
      }
    }
    if(datastreamtable[baselinetable[i].datastream1index].telescopeindex > datastreamtable[baselinetable[i].datastream2index].telescopeindex)
    {
      if(mpiid == 0) //only write one copy of this error message
        cerror << startl << "First datastream for baseline " << i << " has a higher number than second datastream - reversing!!!" << endl;
      tempint = baselinetable[i].datastream1index;
      baselinetable[i].datastream1index = baselinetable[i].datastream2index;
      baselinetable[i].datastream2index = tempint;
      tempintptr = baselinetable[i].datastream1bandindex;
      baselinetable[i].datastream1bandindex = baselinetable[i].datastream2bandindex;
      baselinetable[i].datastream2bandindex = tempintptr;
    }
  }
  for(int f=0;f<freqtablelength;f++)
  {
    if(!freqtable[f].lowersideband)
      continue; //only need to nadger lower sideband freqs here, and only when they are correlated against USB
    for(int i=0;i<baselinetablelength;i++)
    {
      bldata = baselinetable[i];
      for(int j=0;j<baselinetable[i].numfreqs;j++)
      {
        if(bldata.freqtableindices[j] == f) //its a match - check the other datastream for USB
        {
          dsdata = datastreamtable[bldata.datastream2index];
          dsband = bldata.datastream2bandindex[j][0];
          if(dsband >= dsdata.numrecordedbands) //it is a zoom band
            findex = dsdata.zoomfreqtableindices[dsdata.zoombandlocalfreqindices[dsband-dsdata.numrecordedbands]];
          else
            findex = dsdata.recordedfreqtableindices[dsdata.recordedbandlocalfreqindices[dsband]];
          if(!freqtable[findex].lowersideband)
          {
            //ahah! A cross LSB/USB.  Will need to change so the freqindex used is the USB
            freqtable[f].correlatedagainstupper = true;
          }
        }
      }
    }
    if(freqtable[f].correlatedagainstupper) //need to alter all freqindices to be the equivalent USB
    {
      matchfindex = 0;
      //first find the matching USB freq
      for(int i=0;i<freqtablelength;i++)
      {
        if(!freqtable[i].lowersideband && fabs(freqtable[f].bandwidth - freqtable[i].bandwidth) < Mode::TINY && fabs(freqtable[f].bandedgefreq - freqtable[f].bandwidth - freqtable[i].bandedgefreq) < Mode::TINY) //match
          matchfindex = i;
      }
      for(int i=0;i<baselinetablelength;i++)
      {
        bldata = baselinetable[i];
        for(int j=0;j<baselinetable[i].numfreqs;j++)
        {
          if(bldata.freqtableindices[j] == f) //this baseline had referred to the LSB - replace with the USB freqindex
          {
            bldata.freqtableindices[j] = matchfindex;
          }
        }
      }
    }
  }
  for(int f=0;f<freqtablelength;f++)
  {
    for(int i=0;i<baselinetablelength;i++)
    {
      bldata = baselinetable[i];
      bldata.localfreqindices[f] = -1;
      for(int j=0;j<bldata.numfreqs;j++)
      {
        if(bldata.freqtableindices[j] == f)
          bldata.localfreqindices[f] = j;
      }
    }
  }
  baselineread = true;
  return true;
}

// must be called after consistencyCheck()
bool Configuration::populateRecordBandIndicies()
{
  int baselinefreqindex, bandindex;
  int dsindex1, dsindex2;
  int numrecordedbands1, numrecordedbands2;

  for(int configindex=0;configindex<numconfigs;++configindex)
  {
    for(int f=0;f<getFreqTableLength();f++)
    {
      if(isFrequencyUsed(configindex, f))
      {
        for(int configbaselineindex=0;configbaselineindex<numbaselines;++configbaselineindex)
        {
          baselinefreqindex = getBLocalFreqIndex(configindex, configbaselineindex, f);
          if(baselinefreqindex >= 0)
          {
            dsindex1 = getBOrderedDataStream1Index(configindex, configbaselineindex);
            numrecordedbands1 = getDNumRecordedBands(configindex, dsindex1);
            dsindex2 = getBOrderedDataStream2Index(configindex, configbaselineindex);
            numrecordedbands2 = getDNumRecordedBands(configindex, dsindex2);
            for(int polproductindex=0;polproductindex<getBNumPolProducts(configindex, configbaselineindex, baselinefreqindex);++polproductindex)
            {
              // First the "1" index of the baseline
              bandindex = getBDataStream1BandIndex(configindex, configbaselineindex, baselinefreqindex, polproductindex);
              if(bandindex < numrecordedbands1)  // a record band so this is easy
              {
                baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1recordbandindex[baselinefreqindex][polproductindex] = bandindex;
              }
              else  // a zoom band
              {
                int localfreqindex; // within the datastream
                int parentfreqindex;
                int zoomindex;      // within the datastream

                localfreqindex = getDLocalZoomFreqIndex(configindex, dsindex1, bandindex-numrecordedbands1);
                parentfreqindex = getDZoomFreqParentFreqIndex(configindex, dsindex1, localfreqindex);
                zoomindex = bandindex - numrecordedbands1;

                baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1recordbandindex[baselinefreqindex][polproductindex] = -1;

                for(bandindex = 0; bandindex < numrecordedbands1; ++bandindex)
                {
                  if(getDLocalRecordedFreqIndex(configindex, dsindex1, bandindex) == parentfreqindex && getDZoomBandPol(configindex, dsindex1, zoomindex) == getDRecordedBandPol(configindex, dsindex1, bandindex))
                  {
                    baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1recordbandindex[baselinefreqindex][polproductindex] = bandindex;
                    break;
                  }
                }
              }

              // Then the "2" index of the baseline
              bandindex = getBDataStream2BandIndex(configindex, configbaselineindex, baselinefreqindex, polproductindex);
              if(bandindex < numrecordedbands2)  // a record band so this is easy
              {
                baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2recordbandindex[baselinefreqindex][polproductindex] = bandindex;
              }
              else  // a zoom band
              {
                int localfreqindex; // within the datastream
                int parentfreqindex;
                int zoomindex;      // within the datastream

                localfreqindex = getDLocalZoomFreqIndex(configindex, dsindex2, bandindex-numrecordedbands2);
                parentfreqindex = getDZoomFreqParentFreqIndex(configindex, dsindex2, localfreqindex);
                zoomindex = bandindex - numrecordedbands2;

                baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2recordbandindex[baselinefreqindex][polproductindex] = -1;

                for(bandindex = 0; bandindex < numrecordedbands2; ++bandindex)
                {
                  if(getDLocalRecordedFreqIndex(configindex, dsindex2, bandindex) == parentfreqindex && getDZoomBandPol(configindex, dsindex2, zoomindex) == getDRecordedBandPol(configindex, dsindex2, bandindex))
                  {
                    baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2recordbandindex[baselinefreqindex][polproductindex] = bandindex;
                    break;
                  }
                }
              }

            }
          }
        }
      }
    }
  }

  return true;
}

void Configuration::processCommon(ifstream * input)
{
  string line;

  getinputline(input, &calcfilename, "CALC FILENAME");
  getinputline(input, &coreconffilename, "CORE CONF FILENAME");
  getinputline(input, &line, "EXECUTE TIME (SEC)");
  executeseconds = atoi(line.c_str());
  getinputline(input, &line, "START MJD");
  startmjd = atoi(line.c_str());
  getinputline(input, &line, "START SECONDS");
  startseconds = atoi(line.c_str());
  startns = (int)((atof(line.c_str()) - ((double)startseconds))*1000000000.0 + 0.5);
  if(restartseconds > 0) {
    startseconds = (int)(atof(line.c_str()) + restartseconds);
    startns = (int)((atof(line.c_str()) + restartseconds - ((double)startseconds))*1000000000.0 + 0.5);
    executeseconds -= int(restartseconds);
  }
  getinputline(input, &line, "ACTIVE DATASTREAMS");
  numdatastreams = atoi(line.c_str());
  getinputline(input, &line, "ACTIVE BASELINES");
  numbaselines = atoi(line.c_str());
  getinputline(input, &line, "VIS BUFFER LENGTH");
  visbufferlength = atoi(line.c_str());
  getinputline(input, &line, "OUTPUT FORMAT");
  if(line == "SWIN" || line == "DIFX")
  {
    outformat = DIFX;
  }
  else if(line == "ASCII")
  {
    outformat = ASCII;
  }
  else
  {
    if(mpiid == 0) //only write one copy of this error message
      cerror << startl << "Unknown output format " << line << " (case sensitive choices are SWIN, DIFX (same thing) and ASCII), assuming SWIN/DIFX" << endl;
    outformat = DIFX;
  }
  getinputline(input, &outputfilename, "OUTPUT FILENAME");

  commonread = true;
}

bool Configuration::processConfig(ifstream * input)
{
  string line;
  int arraystridelenfrominputfile;

  maxnumpulsarbins = 0;
  maxnumbufferedffts = 0;

  getinputline(input, &line, "NUM CONFIGURATIONS");
  numconfigs = atoi(line.c_str());
  configs = new configdata[numconfigs];
  estimatedbytes += numconfigs*sizeof(configdata);
  for(int i=0;i<numconfigs;i++)
  {
    configs[i].arraystridelen = new int[numdatastreams]();
    configs[i].datastreamindices = new int[numdatastreams]();
    configs[i].baselineindices = new int [numbaselines]();
    getinputline(input, &(configs[i].name), "CONFIG NAME");
    getinputline(input, &line, "INT TIME (SEC)");
    configs[i].inttime = atof(line.c_str());
    getinputline(input, &line, "SUBINT NANOSECONDS");
    configs[i].subintns = atoi(line.c_str());
    getinputline(input, &line, "GUARD NANOSECONDS");
    configs[i].guardns = atoi(line.c_str());
    getinputline(input, &line, "FRINGE ROTN ORDER");
    configs[i].fringerotationorder = atoi(line.c_str());
    getinputline(input, &line, "ARRAY STRIDE LEN");
    arraystridelenfrominputfile = atoi(line.c_str());
    if(arraystridelenfrominputfile < 0)
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Invalid value for arraystridelength: " << arraystridelenfrominputfile << endl;
      consistencyok = false;
    }
    for(int j=0;j<numdatastreams;++j)
    {
      configs[i].arraystridelen[j] = arraystridelenfrominputfile;
    }
    getinputline(input, &line, "XMAC STRIDE LEN");
    configs[i].xmacstridelen = atoi(line.c_str());
    if(configs[i].xmacstridelen < 0)
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Invalid value for xmaclength: " << configs[i].xmacstridelen << endl;
      consistencyok = false;
    }
    getinputline(input, &line, "NUM BUFFERED FFTS");
    configs[i].numbufferedffts = atoi(line.c_str());
    if(configs[i].numbufferedffts > maxnumbufferedffts)
      maxnumbufferedffts = configs[i].numbufferedffts;
    getinputline(input, &line, "WRITE AUTOCORRS");
    configs[i].writeautocorrs = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    getinputline(input, &line, "PULSAR BINNING");
    configs[i].pulsarbin = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    if(configs[i].pulsarbin)
    {
      getinputline(input, &configs[i].pulsarconfigfilename, "PULSAR CONFIG FILE");
    }
    getinputline(input, &line, "PHASED ARRAY");
    configs[i].phasedarray = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    if(configs[i].phasedarray)
    {
      if(mpiid == 0) //only write one copy of this error message
        cwarn << startl << "PHASED ARRAY = TRUE but phased array mode is not yet supported!" << endl;
      getinputline(input, &configs[i].phasedarrayconfigfilename, "PHASED ARRAY CONFIG FILE");
    }
    for(int j=0;j<numdatastreams;j++)
    {
      getinputline(input, &line, "DATASTREAM ", j);
      configs[i].datastreamindices[j] = atoi(line.c_str());
    }
    for(int j=0;j<numbaselines;j++)
    {
      getinputline(input, &line, "BASELINE ", j);
      configs[i].baselineindices[j] = atoi(line.c_str());
    }
  }

  configread = true;
  return true;
}

bool Configuration::processDatastreamTable(ifstream * input)
{
  datastreamdata * dsdata;
  int configindex, freqindex, decimationfactor, tonefreq;
  double lofreq, parentlowbandedge, parenthighbandedge, lowbandedge, highbandedge, recbandwidth;
  string line = "";;
  string key = "";
  bool ok = true;

  getinputline(input, &line, "DATASTREAM ENTRIES");
  datastreamtablelength = atoi(line.c_str());
  datastreamtable = new datastreamdata[datastreamtablelength];
  estimatedbytes += datastreamtablelength*sizeof(datastreamdata);
  if(datastreamtablelength < numdatastreams)
  {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "Not enough datastreams are supplied in the datastream table (" << datastreamtablelength << ") compared to the number of datastreams (" << numdatastreams << "!!!" << endl;
    return false;
  }
  //create the ordereddatastream array
  for(int i=0;i<numconfigs;i++)
    configs[i].ordereddatastreamindices = new int[datastreamtablelength]();

  //get the information on the length of the internal buffer for the datastreams
  getinputline(input, &line, "DATA BUFFER FACTOR");
  databufferfactor = atoi(line.c_str());
  getinputline(input, &line, "NUM DATA SEGMENTS");
  numdatasegments = atoi(line.c_str());

  for(int i=0;i<datastreamtablelength;i++)
  {
    dsdata = &(datastreamtable[i]);
    configindex=-1;
    dsdata->maxnsslip = 0;
    datastreamtable[i].numdatafiles = 0; //default in case its a network datastream
    datastreamtable[i].tcpwindowsizekb = 0; //default in case its a file datastream
    datastreamtable[i].portnumber = -1; //default in case its a file datastream

    //get configuration index for this datastream
    for(int c=0; c<numconfigs; c++)
    {
      for(int d=0; d<numdatastreams; d++)
      {
        if(configs[c].datastreamindices[d] == i)
        {
          configindex = c;
          break;
        }
      }
      if(configindex >= 0) break;
    }

    //read all the info for this datastream
    getinputline(input, &line, "TELESCOPE INDEX");
    datastreamtable[i].telescopeindex = atoi(line.c_str());
    getinputline(input, &line, "TSYS");
    datastreamtable[i].tsys = atof(line.c_str());

    getinputline(input, &line, "DATA FORMAT");
    datastreamtable[i].ismuxed = false;
    if(line == "LBASTD")
      datastreamtable[i].format = LBASTD;
    else if(line == "LBAVSOP")
      datastreamtable[i].format = LBAVSOP;
    else if(line == "LBA8BIT")
      datastreamtable[i].format = LBA8BIT;
    else if(line == "LBA16BIT")
      datastreamtable[i].format = LBA16BIT;
    else if(line == "K5")
      datastreamtable[i].format = K5VSSP;
    else if(line == "K5VSSP")
      datastreamtable[i].format = K5VSSP;
    else if(line == "K5VSSP32")
      datastreamtable[i].format = K5VSSP32;
    else if(line == "MKIV")
      datastreamtable[i].format = MKIV;
    else if(line == "VLBA")
      datastreamtable[i].format = VLBA;
    else if(line == "VLBN")
      datastreamtable[i].format = VLBN;
    else if(line == "MARK5B")
      datastreamtable[i].format = MARK5B;
    else if(line == "KVN5B")
      datastreamtable[i].format = KVN5B;
    else if(line == "VDIF") {
      datastreamtable[i].format = VDIF;
      datastreamtable[i].nummuxthreads = 1;//In case 'vdifio' instead of 'mark5access' does decoding
      datastreamtable[i].muxthreadmap = new int[1]();
    } else if(line == "VDIFL") {
      datastreamtable[i].format = VDIFL;
      datastreamtable[i].nummuxthreads = 1;//In case 'vdifio' instead of 'mark5access' does decoding
      datastreamtable[i].muxthreadmap = new int[1]();
    } else if(line.substr(0,14) == "INTERLACEDVDIF") {
      if(line.length()<15) {
        cfatal << startl << "Data format " << line << " too short, expected thread information, see vex2difx documentation" << endl;
        return false;
      }
      datastreamtable[i].format = INTERLACEDVDIF;
      datastreamtable[i].ismuxed = true;
      setDatastreamMuxInfo(i, line.substr(15,string::npos));
    }
    else
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Unknown data format " << line << " (case sensitive choices are LBASTD, LBAVSOP, LBA8BIT, K5, MKIV, VLBA, VLBN, MARK5B, KVN5B, VDIF, VDIFL and INTERLACEDVDIF)" << endl;
      return false;
    }
    getinputline(input, &line, "QUANTISATION BITS");
    datastreamtable[i].numbits = atoi(line.c_str());

    getinputline(input, &line, "DATA FRAME SIZE");
    datastreamtable[i].framebytes = atoi(line.c_str());

    getinputline(input, &line, "DATA SAMPLING");
    if(line == "REAL")
      datastreamtable[i].sampling = REAL;
    else if(line == "COMPLEX" || line == "COMPLEX_IQ" || line == "COMPLEX_SSB") {
      datastreamtable[i].sampling = COMPLEX;
      datastreamtable[i].tcomplex = SINGLE;
    }
    else if(line == "COMPLEX_DBL" || line == "COMPLEX_DSB") {
      datastreamtable[i].sampling = COMPLEX;
      datastreamtable[i].tcomplex = DOUBLE;
    }
    else
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Unknown sampling type " << line << " (case sensitive choices are REAL or COMPLEX)" << endl;
      return false;
    }

    getinputline(input, &line, "DATA SOURCE");
    if(line == "FILE")
      datastreamtable[i].source = UNIXFILE;
    else if(line == "MODULE")
      datastreamtable[i].source = MK5MODULE;
    else if(line == "MARK6")
      datastreamtable[i].source = MK6MODULE;
    else if(line == "SHAREDMEMORY")
      datastreamtable[i].source = SHAREDMEMORYSTREAM;
    else if(line == "NETWORK")
      datastreamtable[i].source = NETWORKSTREAM;
    else if(line == "FAKE")
      datastreamtable[i].source = FAKESTREAM;
    else
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Unknown data source " << line << " (case sensitive choices are FILE, MODULE, MARK6, NETWORK, SHAREDMEMORY and FAKE)" << endl;
      return false;
    }

    datastreamtable[i].filterbank=false;
    datastreamtable[i].linear2circular=false;
    getinputkeyval(input, &key, &line);
    if (key=="FILTERBANK USED") {
      if ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t")) 
	datastreamtable[i].filterbank = true;
    } else if (key=="PROCESSING METHOD") {
      if (line == "FILTERBANK") 
      	datastreamtable[i].filterbank=true;
      else if (line=="L2C" || line=="LINEAR2CIRCULAR") {
	cinfo << startl << "Linear to Circular enabled" << endl;
      	datastreamtable[i].linear2circular=true;
      } else if (line != "NONE") 
	cerror << startl << "Unknown PROCESSING METHOD '" << line << "'. Ignoring." << endl;
    } else {
      cfatal << startl << "We thought we were reading something starting with 'FILTERBANK USED', when we actually got '" << key << "'" << endl;
      return false;
    }
    if (mpiid==0 && datastreamtable[i].filterbank)
      cwarn << startl << "Filterbank channelization requested but not yet supported!!!" << endl;

    getinputkeyval(input, &key, &line);
    if(key.find("TCAL FREQUENCY") != string::npos) {
      datastreamtable[i].switchedpowerfrequency = atoi(line.c_str());
      getinputline(input, &line, "PHASE CAL INT (MHZ)");
    }
    else {
      datastreamtable[i].switchedpowerfrequency = 0;
      if(key.find("PHASE CAL INT (MHZ)") == string::npos) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Went looking for PHASE CAL INT (MHZ) (or maybe TCAL FREQUENCY), but got " << key << endl;
        return false;
      }
    }
    datastreamtable[i].phasecalintervalmhz = atoi(line.c_str());

    getinputline(input, &line, "NUM RECORDED FREQS");
    datastreamtable[i].numrecordedfreqs = atoi(line.c_str());
    datastreamtable[i].recordedfreqpols = new int[datastreamtable[i].numrecordedfreqs]();
    datastreamtable[i].recordedfreqtableindices = new int[datastreamtable[i].numrecordedfreqs]();
    datastreamtable[i].recordedfreqclockoffsets = new double[datastreamtable[i].numrecordedfreqs]();
    datastreamtable[i].recordedfreqclockoffsetsdelta = new double[datastreamtable[i].numrecordedfreqs]();
    datastreamtable[i].recordedfreqphaseoffset = new double[datastreamtable[i].numrecordedfreqs]();
    datastreamtable[i].recordedfreqlooffsets = new double[datastreamtable[i].numrecordedfreqs]();
    estimatedbytes += 8*datastreamtable[i].numrecordedfreqs*3;
    datastreamtable[i].numrecordedbands = 0;
    for(int j=0;j<datastreamtable[i].numrecordedfreqs;j++)
    {
      getinputline(input, &line, "REC FREQ INDEX ", j);
      datastreamtable[i].recordedfreqtableindices[j] = atoi(line.c_str());
      getinputline(input, &line, "CLK OFFSET ", j);
      size_t found;
      found = line.find_first_of(':');
      if (found==std::string::npos) {
	// Just Delay offset
	datastreamtable[i].recordedfreqclockoffsets[j] = atof(line.c_str());
	datastreamtable[i].recordedfreqclockoffsetsdelta[j] = 0;
	datastreamtable[i].recordedfreqphaseoffset[j] = 0;
      } else {
	// Offset:LcpOffset[+Phaseoffset] (usec:usec:degrees)
	datastreamtable[i].recordedfreqclockoffsets[j] = atof(line.substr(0,found).c_str());

	size_t found2;
	found2 = line.substr(found+1).find_first_of(':');
	if (found2==std::string::npos) {
	  // Offset:LcpOffset
	  datastreamtable[i].recordedfreqclockoffsetsdelta[j] = atof(line.substr(found+1).c_str());
	} else {
	  // Offset:LcpOffset:PhaseOffset
	  datastreamtable[i].recordedfreqclockoffsetsdelta[j] = atof(line.substr(found+1).substr(0,found2).c_str());
	  datastreamtable[i].recordedfreqphaseoffset[j] = atof(line.substr(found+1).substr(found2+1).c_str());

	}
      }

      if(j == 0 && datastreamtable[i].recordedfreqclockoffsets[j] != 0.0 && mpiid == 0)
        cwarn << startl << "Model accountability is compromised if the first band of a telescope has a non-zero clock offset! If this is the first/only datastream for " << telescopetable[datastreamtable[i].telescopeindex].name << ", you should adjust the telescope clock so that the offset for this band is ZERO!" << endl;
      getinputline(input, &line, "FREQ OFFSET ", j); //Freq offset is positive if recorded LO frequency was higher than the frequency in the frequency table
      datastreamtable[i].recordedfreqlooffsets[j] = atof(line.c_str());
      getinputline(input, &line, "NUM REC POLS ", j);
      datastreamtable[i].recordedfreqpols[j] = atoi(line.c_str());
      datastreamtable[i].numrecordedbands += datastreamtable[i].recordedfreqpols[j];
    }
    decimationfactor = freqtable[datastreamtable[i].recordedfreqtableindices[0]].decimationfactor;
    recbandwidth = freqtable[datastreamtable[i].recordedfreqtableindices[0]].bandwidth;
    int numsamplebits = datastreamtable[i].numbits;
    /*int bytespersecond = numsamplebits*datastreamtable[i].numrecordedbands*decimationfactor*(((int)(1000000*recbandwidth*2))/8);
    switch(datastreamtable[i].format) {
      case VDIF:
        datastreamtable[i].framespersecond = bytespersecond / (datastreamtable[i].framebytes-VDIF_HEADER_BYTES);
        cout << "for normal VDIF, framespersecond is " << datastreamtable[i].framespersecond << endl;
        break;
      case INTERLACEDVDIF:
        datastreamtable[i].framespersecond = bytespersecond / ((datastreamtable[i].framebytes-VDIF_HEADER_BYTES)*datastreamtable[i].nummuxthreads);
        cout << "bytespersecond is " << bytespersecond << ", and payloadbytes is " << (datastreamtable[i].framebytes-VDIF_HEADER_BYTES)*datastreamtable[i].nummuxthreads << " s0 framespersecond is " << datastreamtable[i].framespersecond << endl;
        break;
      default:
        datastreamtable[i].framespersecond = bytespersecond / datastreamtable[i].framebytes; //not very useful for non-VDIF
        break;
    }*/
    if (datastreamtable[i].sampling==COMPLEX) numsamplebits *=2;
    datastreamtable[i].bytespersamplenum = (datastreamtable[i].numrecordedbands*numsamplebits*decimationfactor)/8;
    if(datastreamtable[i].bytespersamplenum == 0)
    {
      datastreamtable[i].bytespersamplenum = 1;
      datastreamtable[i].bytespersampledenom = 8/(datastreamtable[i].numrecordedbands*numsamplebits*decimationfactor);
    }
    else
      datastreamtable[i].bytespersampledenom = 1;

    datastreamtable[i].recordedbandpols = new char[datastreamtable[i].numrecordedbands]();
    datastreamtable[i].recordedbandlocalfreqindices = new int[datastreamtable[i].numrecordedbands]();
    estimatedbytes += datastreamtable[i].numrecordedbands*5;
    for(int j=0;j<datastreamtable[i].numrecordedbands;j++)
    {
      getinputline(input, &line, "REC BAND ", j);
      datastreamtable[i].recordedbandpols[j] = *(line.data());
      getinputline(input, &line, "REC BAND ", j);
      datastreamtable[i].recordedbandlocalfreqindices[j] = atoi(line.c_str());
      if(datastreamtable[i].recordedbandlocalfreqindices[j] >= datastreamtable[i].numrecordedfreqs) {
        if(mpiid == 0) //only write one copy of this error message
          cerror << startl << "Attempting to refer to freq outside local table!!!" << endl;
        return false;
      }
    }
    getinputline(input, &line, "NUM ZOOM FREQS");
    datastreamtable[i].numzoomfreqs = atoi(line.c_str());
    datastreamtable[i].zoomfreqtableindices = new int[datastreamtable[i].numzoomfreqs]();
    datastreamtable[i].zoomfreqpols = new int[datastreamtable[i].numzoomfreqs]();
    datastreamtable[i].zoomfreqparentdfreqindices = new int[datastreamtable[i].numzoomfreqs]();
    datastreamtable[i].zoomfreqchanneloffset = new int[datastreamtable[i].numzoomfreqs]();
    estimatedbytes += datastreamtable[i].numzoomfreqs*16;
    datastreamtable[i].numzoombands = 0;
    for(int j=0;j<datastreamtable[i].numzoomfreqs;j++)
    {
      getinputline(input, &line, "ZOOM FREQ INDEX ");
      datastreamtable[i].zoomfreqtableindices[j] = atoi(line.c_str());
      getinputline(input, &line, "NUM ZOOM POLS ", j);
      datastreamtable[i].zoomfreqpols[j] = atoi(line.c_str());
      datastreamtable[i].numzoombands += datastreamtable[i].zoomfreqpols[j];
      datastreamtable[i].zoomfreqparentdfreqindices[j] = -1;
      for (int k=0;k<datastreamtable[i].numrecordedfreqs;k++) {
        parentlowbandedge = freqtable[datastreamtable[i].recordedfreqtableindices[k]].bandedgefreq;
        parenthighbandedge = freqtable[datastreamtable[i].recordedfreqtableindices[k]].bandedgefreq + freqtable[datastreamtable[i].recordedfreqtableindices[k]].bandwidth;
        if(freqtable[datastreamtable[i].recordedfreqtableindices[k]].lowersideband) {
          parentlowbandedge -= freqtable[datastreamtable[i].recordedfreqtableindices[k]].bandwidth;
          parenthighbandedge -= freqtable[datastreamtable[i].recordedfreqtableindices[k]].bandwidth;
        }
        lowbandedge = freqtable[datastreamtable[i].zoomfreqtableindices[j]].bandedgefreq;
        highbandedge = freqtable[datastreamtable[i].zoomfreqtableindices[j]].bandedgefreq + freqtable[datastreamtable[i].zoomfreqtableindices[j]].bandwidth;
        if(freqtable[datastreamtable[i].zoomfreqtableindices[j]].lowersideband) {
          lowbandedge -= freqtable[datastreamtable[i].zoomfreqtableindices[j]].bandwidth;
          highbandedge -= freqtable[datastreamtable[i].zoomfreqtableindices[j]].bandwidth;
        }
        if (highbandedge <= parenthighbandedge && lowbandedge >= parentlowbandedge) {
          datastreamtable[i].zoomfreqparentdfreqindices[j] = k;
          datastreamtable[i].zoomfreqchanneloffset[j] = (int)(((lowbandedge - parentlowbandedge)/freqtable[datastreamtable[i].recordedfreqtableindices[0]].bandwidth)*freqtable[datastreamtable[i].recordedfreqtableindices[0]].numchannels + 0.5);
          //if (freqtable[datastreamtable[i].zoomfreqtableindices[j]].lowersideband)
          //  datastreamtable[i].zoomfreqchanneloffset[j] += freqtable[datastreamtable[i].zoomfreqtableindices[j]].numchannels;
        }
      }
    }
    datastreamtable[i].zoombandpols = new char[datastreamtable[i].numzoombands]();
    datastreamtable[i].zoombandlocalfreqindices = new int[datastreamtable[i].numzoombands]();
    estimatedbytes += 5*datastreamtable[i].numzoombands;
    for(int j=0;j<datastreamtable[i].numzoombands;j++)
    {
      getinputline(input, &line, "ZOOM BAND ", j);
      datastreamtable[i].zoombandpols[j] = *(line.data());
      getinputline(input, &line, "ZOOM BAND ", j);
      datastreamtable[i].zoombandlocalfreqindices[j] = atoi(line.c_str());
      if(datastreamtable[i].zoombandlocalfreqindices[j] >= datastreamtable[i].numzoomfreqs) {
        if(mpiid == 0) //only write one copy of this error message
          cerror << startl << "Attempting to refer to freq outside local table!!!" << endl;
        return false;
      }
    }
    if(dsdata->phasecalintervalmhz > 0)
    {
      dsdata->numrecordedfreqpcaltones = new int[dsdata->numrecordedfreqs]();
      dsdata->recordedfreqpcaltonefreqs = new int*[dsdata->numrecordedfreqs]();
      dsdata->recordedfreqpcaloffsetshz = new int[dsdata->numrecordedfreqs]();
      dsdata->maxrecordedpcaltones = 0;
      estimatedbytes += sizeof(int)*(dsdata->numrecordedfreqs);
      for(int j=0;j<dsdata->numrecordedfreqs;j++)
      {
        datastreamtable[i].numrecordedfreqpcaltones[j] = 0;
        freqindex = dsdata->recordedfreqtableindices[j];
        lofreq = freqtable[freqindex].bandedgefreq;

        if(freqtable[freqindex].lowersideband)
        {     // LSB
          tonefreq = (int(lofreq)/dsdata->phasecalintervalmhz)*dsdata->phasecalintervalmhz;
          if(tonefreq == lofreq)
            tonefreq -= dsdata->phasecalintervalmhz;
          if(tonefreq >= lofreq - freqtable[freqindex].bandwidth)
          {
            while(tonefreq - dsdata->numrecordedfreqpcaltones[j]*dsdata->phasecalintervalmhz >= lofreq - freqtable[freqindex].bandwidth)
              dsdata->numrecordedfreqpcaltones[j]++;
          }
          if(dsdata->numrecordedfreqpcaltones[j] > dsdata->maxrecordedpcaltones)
            dsdata->maxrecordedpcaltones = dsdata->numrecordedfreqpcaltones[j];
          if(datastreamtable[i].numrecordedfreqpcaltones[j] > 0)
          {
            datastreamtable[i].recordedfreqpcaltonefreqs[j] = new int[datastreamtable[i].numrecordedfreqpcaltones[j]]();
            estimatedbytes += sizeof(int)*datastreamtable[i].numrecordedfreqpcaltones[j];
            for(int k=0;k<datastreamtable[i].numrecordedfreqpcaltones[j];k++) {
              datastreamtable[i].recordedfreqpcaltonefreqs[j][k] = tonefreq - k*dsdata->phasecalintervalmhz;
            }
            dsdata->recordedfreqpcaloffsetshz[j] = long(1e6*lofreq - 1e6*datastreamtable[i].recordedfreqpcaltonefreqs[j][0] + 0.5);
          }
          else
          {
            datastreamtable[i].numrecordedfreqpcaltones[j] = 1;
            datastreamtable[i].recordedfreqpcaltonefreqs[j] = new int[1]();
            datastreamtable[i].recordedfreqpcaltonefreqs[j][0] = 0;

            dsdata->recordedfreqpcaloffsetshz[j] = -1; /* A flag to indicate this is not real */
          }
        }
        else
        {     // USB
          tonefreq = (int(lofreq)/dsdata->phasecalintervalmhz)*dsdata->phasecalintervalmhz;
          if(tonefreq <= lofreq)
            tonefreq += dsdata->phasecalintervalmhz;
          if(tonefreq <= lofreq + freqtable[freqindex].bandwidth)
          {
            while(tonefreq + dsdata->numrecordedfreqpcaltones[j]*dsdata->phasecalintervalmhz <= lofreq + freqtable[freqindex].bandwidth)
              dsdata->numrecordedfreqpcaltones[j]++;
          }
          if(dsdata->numrecordedfreqpcaltones[j] > dsdata->maxrecordedpcaltones)
            dsdata->maxrecordedpcaltones = dsdata->numrecordedfreqpcaltones[j];
          if(datastreamtable[i].numrecordedfreqpcaltones[j] > 0)
          {
            datastreamtable[i].recordedfreqpcaltonefreqs[j] = new int[datastreamtable[i].numrecordedfreqpcaltones[j]]();
            estimatedbytes += sizeof(int)*datastreamtable[i].numrecordedfreqpcaltones[j];
            for(int k=0;k<datastreamtable[i].numrecordedfreqpcaltones[j];k++) {
              datastreamtable[i].recordedfreqpcaltonefreqs[j][k] = tonefreq + k*dsdata->phasecalintervalmhz;
            }
            dsdata->recordedfreqpcaloffsetshz[j] = long(1e6*datastreamtable[i].recordedfreqpcaltonefreqs[j][0] - 1e6*lofreq + 0.5);
          }
          else
          {
            datastreamtable[i].numrecordedfreqpcaltones[j] = 1;
            datastreamtable[i].recordedfreqpcaltonefreqs[j] = new int[1]();
            datastreamtable[i].recordedfreqpcaltonefreqs[j][0] = 0;

            dsdata->recordedfreqpcaloffsetshz[j] = -1; /* A flag to indicate this is not real */
          }
        }  
      }
    }
    datastreamtable[i].tcpwindowsizekb = 0;
    datastreamtable[i].portnumber = 0;
  }

  for(int i=0;i<numconfigs;i++)
  {
    //work out blockspersend
    freqdata f = freqtable[datastreamtable[configs[i].datastreamindices[0]].recordedfreqtableindices[0]];
    double ffttime = 1000.0*f.numchannels/f.bandwidth;
    double bpersenddouble = configs[i].subintns/ffttime;
    configs[i].blockspersend = int(bpersenddouble + 0.5);
    if (fabs(bpersenddouble - configs[i].blockspersend) > Mode::TINY) {
      ok = false;
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "The supplied value of subint nanoseconds (" << configs[i].subintns << ") for config " << i << " does not yield an integer number of FFTs! (FFT time is " << ffttime << ") - aborting!!!" << endl;
    }
  }
  if(!ok)
    return false;

  //read in the core numthreads info
  ifstream coreinput(coreconffilename.c_str());
  numcoreconfs = 0;
  if(!coreinput.is_open() || coreinput.bad())
  {
    cwarn << startl << "Could not open " << coreconffilename << " - will set all numthreads to 1!" << endl;
  }
  else
  {
    getinputline(&coreinput, &line, "NUMBER OF CORES");
    int maxlines = atoi(line.c_str());
    numprocessthreads = new int[maxlines]();
    getline(coreinput, line);
    for(int i=0;i<maxlines;i++)
    {
      if(coreinput.eof())
      {
        cerror << startl << "Hit the end of the file! Setting the numthread for Core " << i << " to 1" << endl;
        numprocessthreads[numcoreconfs++] = 1;
      }
      else
      {
        numprocessthreads[numcoreconfs++] = atoi(line.c_str());
        getline(coreinput, line);
      }
    }
  }
  coreinput.close();

  datastreamread = true;
  return true;
}

bool Configuration::processRuleTable(ifstream * input)
{
  int count=0;
  string key, val;
  getinputline(input, &key, "NUM RULES");
  numrules = atoi(key.c_str());
  rules = new ruledata[numrules];
  estimatedbytes += numrules*sizeof(ruledata);
  for(int i=0;i<numrules;i++) {
    rules[i].configindex = -1;
    rules[i].sourcename = "";
    rules[i].scanId = "";
    rules[i].calcode = "";
    rules[i].qual = -1;
    rules[i].mjdStart = -999.9;
    rules[i].mjdStop = -999.9;
  }

  while(count<numrules && !input->eof())
  {
    getinputkeyval(input, &key, &val);
    if(strstr(key.c_str(), "CONFIG NAME")) {
      rules[count].configname = val;
      count++;
    }
    else if(strstr(key.c_str(), "SOURCE")) {
      rules[count].sourcename = val;
    }
    else if(strstr(key.c_str(), "SCAN ID")) {
      rules[count].scanId = val;
    }
    else if(strstr(key.c_str(), "CALCODE")) {
      rules[count].calcode = val;
    }
    else if(strstr(key.c_str(), "QUAL")) {
      rules[count].qual = atoi(val.c_str());
    }
    else if(strstr(key.c_str(), "MJD START")) {
      rules[count].mjdStart = atof(val.c_str());
    }
    else if(strstr(key.c_str(), "MJD STOP")) {
      rules[count].mjdStop = atof(val.c_str());
    }
    else {
      if(mpiid == 0) //only write one copy of this error message
        cwarn << startl << "Received unknown key " << key << " with val " << val << " in rule table - ignoring!" << endl;
    }
  }

  for(int i=0;i<numrules;i++) {
    for(int j=0;j<numconfigs;j++) {
      if(rules[i].configname.compare(configs[j].name) == 0) {
        rules[i].configindex = j;
      }
    }
    if(rules[i].configindex < 0 && rules[i].configname.compare("SKIP") != 0) {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Found a rule with config name " << rules[i].configname << " that doesn't match any configs - aborting!" << endl;
      return false;
    }
  }
  ruleread = true;
  return true;
}

void Configuration::processDataTable(ifstream * input)
{
  string line;

  for(int i=0;i<datastreamtablelength;i++)
  {
    getinputline(input, &line, "D/STREAM ", i);
    datastreamtable[i].numdatafiles = atoi(line.c_str());
    datastreamtable[i].datafilenames = new string[datastreamtable[i].numdatafiles];
    for(int j=0;j<datastreamtable[i].numdatafiles;j++)
      getinputline(input, &(datastreamtable[i].datafilenames[j]), "FILE ", i);
  }
}

bool Configuration::processFreqTable(ifstream * input)
{
  string line, key;

  getinputline(input, &line, "FREQ ENTRIES");
  freqtablelength = atoi(line.c_str());
  freqtable = new freqdata[freqtablelength];
  estimatedbytes += freqtablelength*sizeof(freqdata);
  for(int i=0;i<freqtablelength;i++)
  {
    getinputline(input, &line, "FREQ (MHZ) ", i);
    freqtable[i].bandedgefreq = atof(line.c_str());
    getinputline(input, &line, "BW (MHZ) ", i);
    freqtable[i].bandwidth = atof(line.c_str());
    getinputline(input, &line, "SIDEBAND ", i);
    freqtable[i].lowersideband = ((line == "L") || (line == "l") || (line == "LOWER") || (line == "lower"))?true:false;
    freqtable[i].correlatedagainstupper = false;
    getinputkeyval(input, &key, &line);
    if(key.find("RX NAME ") != string::npos) //look for optional Receiver Name
    {
      freqtable[i].rxName = line;
      getinputkeyval(input, &key, &line);
    }
    // Verify next line is NUM CHANNELS
    if(key.find("NUM CHANNELS ") == string::npos)
    {
      cfatal << startl << "Looking for NUM CHANNELS for FREQ table entry " << i << "failed" << endl;
      return false;
    }
    freqtable[i].numchannels = atoi(line.c_str());
    if(freqtable[i].numchannels > maxnumchannels)
      maxnumchannels = freqtable[i].numchannels;
    getinputline(input, &line, "CHANS TO AVG ");
    freqtable[i].channelstoaverage = atoi(line.c_str());
    if (freqtable[i].channelstoaverage <= 0 || (freqtable[i].channelstoaverage > 1 && freqtable[i].numchannels % freqtable[i].channelstoaverage != 0)) {
      if(mpiid == 0) //only write one copy of this error message
        cerror << startl << "Channels to average must be positive and the number of channels must be divisible by channels to average - not the case for frequency entry " << i << "(" << freqtable[i].channelstoaverage << ","<<freqtable[i].numchannels<<") - aborting!!!" << endl;
      return false;
    }
    getinputline(input, &line, "OVERSAMPLE FAC. ");
    freqtable[i].oversamplefactor = atoi(line.c_str());
    getinputline(input, &line, "DECIMATION FAC. ");
    freqtable[i].decimationfactor = atoi(line.c_str());
    getinputline(input, &line, "PHASE CALS ");
    int npcals = atoi(line.c_str()); //mpifxcorr doesn't need to store this information
    for(int j=0;j<npcals;j++)
    {
      getinputline(input, &line, "PHASE CAL ");
    }
    freqtable[i].matchingwiderbandindex = -1;
    freqtable[i].matchingwiderbandoffset = -1;
  }
  //now look for matching wider bands
  for(int i=freqtablelength-1;i>0;i--)
  {
    double f1chanwidth = freqtable[i].bandwidth/freqtable[i].numchannels;
    double f1loweredge = freqtable[i].bandedgefreq;
    if (freqtable[i].lowersideband)
      f1loweredge -= freqtable[i].bandwidth;
    for(int j=i-1;j>=0;j--)
    {
      double f2chanwidth = freqtable[j].bandwidth/freqtable[j].numchannels;
      double f2loweredge = freqtable[j].bandedgefreq;
      if (freqtable[j].lowersideband)
        f2loweredge -= freqtable[j].bandwidth;
      if((i != j) && (f1chanwidth == f2chanwidth) && (f1loweredge < f2loweredge) &&
          (f1loweredge + freqtable[i].bandwidth > f2loweredge + freqtable[j].bandwidth))
      {
        freqtable[j].matchingwiderbandindex = i;
        freqtable[j].matchingwiderbandoffset = int(((f2loweredge-f1loweredge)/freqtable[i].bandwidth)*freqtable[i].numchannels + 0.5);
      }
    }
  }
  freqread = true;
  return true;
}

void Configuration::processTelescopeTable(ifstream * input)
{
  string line;

  getinputline(input, &line, "TELESCOPE ENTRIES");
  telescopetablelength = atoi(line.c_str());
  telescopetable = new telescopedata[telescopetablelength];
  estimatedbytes += telescopetablelength*sizeof(telescopedata);
  for(int i=0;i<telescopetablelength;i++)
  {
    getinputline(input, &(telescopetable[i].name), "TELESCOPE NAME ", i);
    getinputline(input, &line, "CLOCK REF MJD ", i);
    telescopetable[i].clockrefmjd = atof(line.c_str());
    getinputline(input, &line, "CLOCK POLY ORDER ", i);
    telescopetable[i].clockorder = atoi(line.c_str());
    telescopetable[i].clockpoly = new double[telescopetable[i].clockorder+1]();
    for(int j=0;j<telescopetable[i].clockorder+1;j++) {
      getinputline(input, &line, "CLOCK COEFF ", i);
      telescopetable[i].clockpoly[j] = atof(line.c_str());
    }
  }
}

void Configuration::processNetworkTable(ifstream * input)
{
  string line;

  for(int i=0;i<datastreamtablelength;i++)
  {
    getinputline(input, &line, "PORT NUM ", i);
    if(isalpha(line[0]))
    {
      // sign that this is raw ethernet
      datastreamtable[i].portnumber = 0;
      datastreamtable[i].ethernetdevice = line;
    }
    else
    {
      datastreamtable[i].portnumber = atoi(line.c_str());
    }
    getinputline(input, &line, "TCP WINDOW (KB) ", i);
    datastreamtable[i].tcpwindowsizekb = atoi(line.c_str());
  }
}

bool Configuration::populateScanConfigList()
{
  bool applies, srcnameapplies, calcodeapplies, qualapplies;
  istringstream iss;
  string token;
  Model::source * src;
  ruledata r;

  scanconfigindices = new int[model->getNumScans()]();
  estimatedbytes += 4*model->getNumScans();
  for(int i=0;i<model->getNumScans();i++) {
    scanconfigindices[i] = -1;
    for(int j=0;j<numrules;j++) {
      applies = true;
      r = rules[j];
      if((r.scanId.compare("") != 0)) {
        applies = false;
        iss.str(r.scanId);
        while(getline(iss, token, ',')) {
          if(token.compare(model->getScanIdentifier(i)) == 0)
            applies = true;
        }
        iss.clear();
      }
      if(r.mjdStart > 0 && r.mjdStart > model->getScanStartMJD(i))
        applies = false;
      if(r.mjdStop > 0 && r.mjdStop > model->getScanEndMJD(i))
        applies = false;
      //cout << "Looking at scan " << i+1 << "/" << model->getNumScans() << endl;
      srcnameapplies = false;
      calcodeapplies = false;
      qualapplies = false;
      if(r.sourcename.compare("") == 0)
        srcnameapplies = true;
      if(r.calcode.compare("") == 0)
        calcodeapplies = true;
      if(r.qual < 0)
        qualapplies = true;
      src = model->getScanPointingCentreSource(i);
      iss.str(r.sourcename);
      while(getline(iss, token, ',')) {
         if(token.compare(src->name) == 0)
           srcnameapplies = true;
      }
      iss.clear();
      if(r.calcode.compare(src->calcode) == 0)
        calcodeapplies = true;
      if(r.qual == src->qual)
        calcodeapplies = true;
      for(int k=0;k<model->getNumPhaseCentres(i);k++) {
        //cout << "Looking at source " << k << " of scan " << i << endl;
        src = model->getScanPhaseCentreSource(i, k);
        iss.str(r.sourcename);
        while(getline(iss, token, ',')) {
          if(token.compare(src->name) == 0)
            srcnameapplies = true;
        }
        iss.clear();
        if(r.calcode.compare(src->calcode) == 0)
          calcodeapplies = true;
        if(r.qual == src->qual)
          calcodeapplies = true;
      }
      if(applies && srcnameapplies && calcodeapplies && qualapplies) {
        if(r.configname.compare("SKIP") == 0) {
          if(mpiid == 0) //only write one copy of this message
            cinfo << startl << "Scan " << i << " on source " << src->name << " will be skipped because rule " << j << " applied" << endl;
          scanconfigindices[i] = -1;
          break; //SKIPs take precedence over other rules, must break to ensure a subsequent rule is not applied
        }
        if(scanconfigindices[i] < 0 || scanconfigindices[i] == r.configindex) {
          scanconfigindices[i] = r.configindex;
        }
        else {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Conflicting rules apply to scan " << i << " - aborting!!!" << endl;
          return false;
        }
      }
    }
  }

  return true;
}

bool Configuration::populateModelDatastreamMap()
{
  Model::station s;
  string tname;

  for(int i=0;i<datastreamtablelength;i++) {
    datastreamtable[i].modelfileindex = -1;
    tname = telescopetable[datastreamtable[i].telescopeindex].name;
    for(int j=0;j<model->getNumStations();j++) {
      s = model->getStation(j);
      if(tname.compare(s.name) == 0)
        datastreamtable[i].modelfileindex = j;
    }
  }

  for(int i=0;i<numconfigs;i++) {
    for(int j=0;j<numdatastreams;j++) {
      if(datastreamtable[configs[i].datastreamindices[j]].modelfileindex < 0) {
        cfatal << startl << "Couldn't find datastream " << telescopetable[datastreamtable[configs[i].datastreamindices[j]].telescopeindex].name << " in the model file - aborting!!!" << endl;
        return false;
      }
    }
  }

  return true;
}

bool Configuration::populateResultLengths()
{
  datastreamdata dsdata;
  baselinedata bldata;
  bool found;
  int threadfindex, threadbindex, coreresultindex, toadd;
  int bandsperautocorr, freqindex, freqchans, chanstoaverage, maxconfigphasecentres, xmacstridelen, binloop;
  int numaccs;

  maxthreadresultlength = 0;
  maxcoreresultlength = 0;
  for(int c=0;c<numconfigs;c++)
  {
    //find a scan that matches this config
    found = false;
    maxconfigphasecentres = 1;
    for(int i=0;i<model->getNumScans();i++) {
      if(scanconfigindices[i] == c) {
        if(model->getNumPhaseCentres(i) > maxconfigphasecentres)
          maxconfigphasecentres = model->getNumPhaseCentres(i);
        found = true;
      }
    }
    if(!found) {
      if(mpiid == 0) //only write one copy of this error message
        cwarn << startl << "Did not find a scan matching config index " << c << endl;
    }
    if(getMaxProducts(c) > 2)
      bandsperautocorr = 2;
    else
      bandsperautocorr = 1;

    if(configs[c].phasedarray) //set up for phased array
    {
      //the thread space is just for one round of results
      threadfindex = 0;
      if(configs[c].padomain == FREQUENCY)
      {
      // WFB: from cppcheck, this {} block is the same as ...
        for(int f=0;f<freqtablelength;f++)
        {
          threadfindex += configs[c].numpafreqpols[f]*freqtable[f].numchannels;
        }
        configs[c].threadresultlength = threadfindex;
      }
      else
      {
      // WFB: ... this one!
        for(int f=0;f<freqtablelength;f++)
        {
          threadfindex += configs[c].numpafreqpols[f]*freqtable[f].numchannels;
        }
        configs[c].threadresultlength = threadfindex;
      }

      //the core space needs room for as many dumps as necessary
      numaccs = configs[c].subintns/configs[c].paaccumulationns;
      if(configs[c].padomain == FREQUENCY)
      {
        configs[c].coreresultlength = (configs[c].threadresultlength*numaccs*8)/configs[c].pabits;
      }
      else
      {
        configs[c].coreresultlength = (configs[c].threadresultlength*configs[c].blockspersend*8)/configs[c].pabits;
      }
    }
    else //set up for normal cross-correlations
    {
      xmacstridelen = configs[c].xmacstridelen;
      binloop = 1;
      if(configs[c].pulsarbin && !configs[c].scrunchoutput)
        binloop = configs[c].numbins;

      //work out the offsets for threadresult, and the total length too
      configs[c].completestridelength = new int[freqtablelength]();
      configs[c].numxmacstrides = new int[freqtablelength]();
      configs[c].threadresultfreqoffset = new int[freqtablelength]();
      configs[c].threadresultbaselineoffset = new int*[freqtablelength]();
      threadfindex = 0;
      for(int i=0;i<freqtablelength;i++)
      {
        if(configs[c].frequsedbybaseline[i])
        {
          configs[c].threadresultfreqoffset[i] = threadfindex;
          freqchans = freqtable[i].numchannels;
          configs[c].numxmacstrides[i] = freqtable[i].numchannels/xmacstridelen;
          configs[c].threadresultbaselineoffset[i] = new int[numbaselines]();
          threadbindex = 0;
          for(int j=0;j<numbaselines;j++)
          {
            configs[c].threadresultbaselineoffset[i][j] = threadbindex;
            bldata = baselinetable[configs[c].baselineindices[j]];
            if(bldata.localfreqindices[i] >= 0)
            {
              configs[c].threadresultbaselineoffset[i][j] = threadbindex;
              threadbindex += binloop*bldata.numpolproducts[bldata.localfreqindices[i]]*xmacstridelen;
            }
          }
          configs[c].completestridelength[i] = threadbindex;
          threadfindex += configs[c].numxmacstrides[i]*configs[c].completestridelength[i];
        }
      }
      configs[c].threadresultlength = threadfindex;

      //work out the offsets for coreresult, and the total length too
      configs[c].coreresultbaselineoffset = new int*[freqtablelength]();
      configs[c].coreresultbweightoffset  = new int*[freqtablelength]();
      configs[c].coreresultbshiftdecorroffset = new int*[freqtablelength];
      configs[c].coreresultautocorroffset = new int[numdatastreams]();
      configs[c].coreresultacweightoffset = new int[numdatastreams]();
      configs[c].coreresultpcaloffset     = new int[numdatastreams]();
      coreresultindex = 0;
      for(int i=0;i<freqtablelength;i++) //first the cross-correlations
      {
        if(configs[c].frequsedbybaseline[i])
        {
          freqchans = freqtable[i].numchannels;
          chanstoaverage = freqtable[i].channelstoaverage;
          configs[c].coreresultbaselineoffset[i] = new int[numbaselines]();
          for(int j=0;j<numbaselines;j++)
          {
            bldata = baselinetable[configs[c].baselineindices[j]];
            if(bldata.localfreqindices[i] >= 0)
            {
              configs[c].coreresultbaselineoffset[i][j] = coreresultindex;
              coreresultindex += maxconfigphasecentres*binloop*bldata.numpolproducts[bldata.localfreqindices[i]]*freqchans/chanstoaverage;
            }
          }
        }
      }
      for(int i=0;i<freqtablelength;i++) //then the baseline weights
      {
        if(configs[c].frequsedbybaseline[i])
        {
          configs[c].coreresultbweightoffset[i] = new int[numbaselines]();
          for(int j=0;j<numbaselines;j++)
          {
            bldata = baselinetable[configs[c].baselineindices[j]];
            if(bldata.localfreqindices[i] >= 0)
            {
              configs[c].coreresultbweightoffset[i][j] = coreresultindex;
              //baselineweights are only floats so need to divide by 2...
              toadd  = binloop*bldata.numpolproducts[bldata.localfreqindices[i]]/2;
              toadd += binloop*bldata.numpolproducts[bldata.localfreqindices[i]]%2;
              coreresultindex += toadd;
            }
          }
        }
      }
      for(int i=0;i<freqtablelength;i++) //then the shift decorrelation factors (multi-field only)
      {
        if(configs[c].frequsedbybaseline[i])
        {
          configs[c].coreresultbshiftdecorroffset[i] = new int[numbaselines]();
          for(int j=0;j<numbaselines;j++)
          {
            bldata = baselinetable[configs[c].baselineindices[j]];
            if(bldata.localfreqindices[i] >= 0 && maxconfigphasecentres > 1)
            {
              configs[c].coreresultbshiftdecorroffset[i][j] = coreresultindex;
              //shift decorrelation factors are only floats so need to divide by 2...
              toadd  = maxconfigphasecentres/2;
              toadd += maxconfigphasecentres%2;
              coreresultindex += toadd;
            }
          }
        }
      }
      for(int i=0;i<numdatastreams;i++) //then the autocorrelations
      {
        dsdata = datastreamtable[configs[c].datastreamindices[i]];
        configs[c].coreresultautocorroffset[i] = coreresultindex;
        for(int j=0;j<getDNumRecordedBands(c, i);j++) {
          if(isFrequencyUsed(c, getDRecordedFreqIndex(c, i, j)) || isEquivalentFrequencyUsed(c, getDRecordedFreqIndex(c, i, j))) {
            freqindex = getDRecordedFreqIndex(c, i, j);
            freqchans = getFNumChannels(freqindex);
            chanstoaverage = getFChannelsToAverage(freqindex);
            coreresultindex += bandsperautocorr*freqchans/chanstoaverage;
          }
        }
        for(int j=0;j<getDNumZoomBands(c, i);j++) {
          if(isFrequencyUsed(c, getDZoomFreqIndex(c, i, j)) || isEquivalentFrequencyUsed(c, getDZoomFreqIndex(c, i, j))) {
            freqindex = getDZoomFreqIndex(c, i, j);
            freqchans = getFNumChannels(freqindex);
            chanstoaverage = getFChannelsToAverage(freqindex);
            coreresultindex += bandsperautocorr*freqchans/chanstoaverage;
          }
        }
      }
      for(int i=0;i<numdatastreams;i++) //then the autocorrelation weights
      {
        dsdata = datastreamtable[configs[c].datastreamindices[i]];
        configs[c].coreresultacweightoffset[i] = coreresultindex;
        toadd = 0;
        for(int j=0;j<getDNumRecordedBands(c, i);j++) {
          if(isFrequencyUsed(c, getDRecordedFreqIndex(c, i, j)) || isEquivalentFrequencyUsed(c, getDRecordedFreqIndex(c, i, j))) {
            toadd += bandsperautocorr;
          }
        }
        for(int j=0;j<getDNumZoomBands(c, i);j++) {
          if(isFrequencyUsed(c, getDZoomFreqIndex(c, i, j)) || isEquivalentFrequencyUsed(c, getDZoomFreqIndex(c, i, j))) {
            toadd += bandsperautocorr;
          }
        }
        //this will also be just floats, not complex, so need to divide by 2
        toadd = (toadd + 1) / 2;    // ensure odd # gets whole complex space
        if(toadd == 0)
          toadd = 1;
        coreresultindex += toadd;
      }
      for(int i=0;i<numdatastreams;i++) //and finally the pcals
      {
        configs[c].coreresultpcaloffset[i] = coreresultindex;
        dsdata = datastreamtable[configs[c].datastreamindices[i]];
        if(dsdata.phasecalintervalmhz > 0)
        {
          for(int j=0;j<getDNumRecordedFreqs(c, i);j++)
            coreresultindex += dsdata.recordedfreqpols[j]*getDRecordedFreqNumPCalTones(c, i, j);
        }
      }
      configs[c].coreresultlength = coreresultindex;
    }
    if(configs[c].threadresultlength > maxthreadresultlength)
      maxthreadresultlength = configs[c].threadresultlength;
    if(configs[c].coreresultlength > maxcoreresultlength)
      maxcoreresultlength = configs[c].coreresultlength;
  }

  return true;
}

void Configuration::setDatastreamMuxInfo(int datastreamindex, string muxinfo)
{
  int threadindices[500];
  size_t at = muxinfo.find_first_of(":");
  datastreamtable[datastreamindex].nummuxthreads = 0;

  while(at != string::npos) {
    threadindices[datastreamtable[datastreamindex].nummuxthreads++] = atoi(muxinfo.substr(0,at).c_str());
    muxinfo = muxinfo.substr(at+1);
    at = muxinfo.find_first_of(":");
  }
  threadindices[datastreamtable[datastreamindex].nummuxthreads++] = atoi(muxinfo.substr(0,at).c_str());
  if ((muxinfo.substr(0,at).length() < 1) && (datastreamtable[datastreamindex].nummuxthreads == 1)) {
     cwarn << startl << "Possibly malformed datastream MUX info, found <=1 threads. Defaulting to 1 thread with ID 0" << endl;
     threadindices[0] = 0;
  }
  datastreamtable[datastreamindex].muxthreadmap = new int[datastreamtable[datastreamindex].nummuxthreads]();
  for(int i=0;i<datastreamtable[datastreamindex].nummuxthreads;i++)
    datastreamtable[datastreamindex].muxthreadmap[i] = threadindices[i];
}

int Configuration::calcgoodxmacstridelength(int configId) const
{
  int nchangcd = 0;
  int target;  /* ignoring numerology, the best possible xmac stride */
  double err = 1.0e12;
  int beststride = 0;

  // First get nchangcd : GCD of number of channels of each output frequency
  for(int j=0;j<numbaselines;j++)
  {
    baselinedata bl;
    bl = baselinetable[configs[configId].baselineindices[j]];
    for(int k=0;k<bl.numfreqs;k++)
    {
      int nchan;
      nchan = freqtable[bl.freqtableindices[k]].numchannels;
      if(nchangcd == 0)
      {
        nchangcd = nchan;
      }
      else
      {
        nchangcd = gcd((long)nchangcd, (long)nchan);
      }
    }
  }

  target = 150;

  // Now find a number that divides nchangcd and is as close as possible to target
  for(int guess = nchangcd; guess > 0; --guess)
  {
    double d;
    if(nchangcd % guess != 0)
    {
      continue;
    }
    d = fabs(guess - target);
    if(d < err)
    {
      err = d;
      beststride = guess;
    }
  }
  
  return beststride;
}

// sets arraystridelen, xmacstridelen, and rotatestridelen
bool Configuration::setStrides()
{
  int nchan;
  datastreamdata * dsdata;

  for(int i=0;i<numconfigs;i++)
  {
    for(int j=0;j<numdatastreams;j++) {
      dsdata = &(datastreamtable[configs[i].datastreamindices[j]]);
      for(int k=0;k<dsdata->numrecordedfreqs;k++) {
        nchan = freqtable[dsdata->recordedfreqtableindices[k]].numchannels;
        if(configs[i].arraystridelen[j] == 0)
        {
          configs[i].arraystridelen[j] = calcstridelength(nchan);
          if(mpiid == 0)
            cinfo << startl << "Config[" << i << "] datastream[" << j << "] had its array stride length automatically set to " << configs[i].arraystridelen[j] << " based on nchan = " << nchan << endl;
        }
      }
    }
    if(configs[i].xmacstridelen == 0)
    {
      configs[i].xmacstridelen = calcgoodxmacstridelength(i);
      if(mpiid == 0)
        cinfo << startl << "Config[" << i << "] had its xmac stride length automatically set to " << configs[i].xmacstridelen << " based on numbers of output channels" << endl;
    }

    // set rotate stride length to the smallest integer divisor >= sqrt(xmacstridelen)
    configs[i].rotatestridelen = calcstridelength(configs[i].xmacstridelen);
    if(mpiid == 0)
      cinfo << startl << "Config[" << i << "] had its rotate stride length automatically set to " << configs[i].rotatestridelen << " based on xmacstridelen = " << configs[i].xmacstridelen << endl;
  }

  return true;
}

bool Configuration::consistencyCheck()
{
  int tindex, count, freqindex, freq1index, freq2index, confindex, timesec, initscan, initsec, framebytes, numbits;
  double bandwidth, sampletimens, ffttime, numffts, f1, f2;
  bool ismuxed;
  datastreamdata ds1, ds2;
  baselinedata bl;
  datastreamdata * dsdata;

  //check length of the datastream table
  if(numdatasegments < 5)
  {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "Databuffer must have a minimum of 5 segments; " << numdatasegments << " specified" << endl;
    return false;
  }

  //check entries in the datastream table
  for(int i=0;i<datastreamtablelength;i++)
  {
    //check the telescope index is acceptable
    if(datastreamtable[i].telescopeindex < 0 || datastreamtable[i].telescopeindex >= telescopetablelength)
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Datastream table entry " << i << " has a telescope index (" << datastreamtable[i].telescopeindex << ") that refers outside the telescope table range (table length " << telescopetablelength << ") - aborting!!!" << endl;
      return false;
    }

    //check the recorded bands all refer to valid local freqs
    for(int j=0;j<datastreamtable[i].numrecordedbands;j++)
    {
      if(datastreamtable[i].recordedbandlocalfreqindices[j] < 0 || datastreamtable[i].recordedbandlocalfreqindices[j] >= datastreamtable[i].numrecordedfreqs)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has an recorded band local frequency index (band " << j << ") which is equal to " << datastreamtable[i].recordedbandlocalfreqindices[j] << " that refers outside the local frequency table range (" << datastreamtable[i].numrecordedfreqs << ") - aborting!!!" << endl;
        return false;
      }
    }

    //check that the zoom mode bands also refer to valid local freqs
    for(int j=0;j<datastreamtable[i].numzoombands;j++)
    {
      if(datastreamtable[i].zoombandlocalfreqindices[j] < 0 || datastreamtable[i].zoombandlocalfreqindices[j] >= datastreamtable[i].numzoomfreqs)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has an zoom band local frequency index (band " << j << ") which is equal to " << datastreamtable[i].zoombandlocalfreqindices[j] << " that refers outside the local frequency table range (" << datastreamtable[i].numzoomfreqs << ") - aborting!!!" << endl;
        return false;
      }
    }

    //check that all zoom freqs come later in the freq table than regular freqs
    for(int j=0;j<datastreamtable[i].numrecordedfreqs;j++)
    {
      int rfreqtableindex = datastreamtable[i].recordedfreqtableindices[j];
      for(int k=0;k<datastreamtable[i].numzoomfreqs;k++)
      {
        if(datastreamtable[i].zoomfreqtableindices[k] < rfreqtableindex)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Datastream table entry " << i << " has a zoom band (index " << k << ") which comes earlier in the freq table than a recorded band (index " << j << ") - aborting!!!" << endl;
          return false;
        }
      }
    }

    //check the frequency table indices are ok and all the bandwidths, number of channels, oversampling etc match for the recorded freqs
    bandwidth = freqtable[datastreamtable[i].recordedfreqtableindices[0]].bandwidth;
    int oversamp = freqtable[datastreamtable[i].recordedfreqtableindices[0]].oversamplefactor;
    int decim = freqtable[datastreamtable[i].recordedfreqtableindices[0]].decimationfactor;
    int toaver = freqtable[datastreamtable[i].recordedfreqtableindices[0]].channelstoaverage;
    if(oversamp < decim)
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Oversamplefactor (" << oversamp << ") is less than decimation factor (" << decim << ") - aborting!!!" << endl;
      return false;
    }
    for(int j=0;j<datastreamtable[i].numrecordedfreqs;j++)
    {
      if(datastreamtable[i].recordedfreqtableindices[j] < 0 || datastreamtable[i].recordedfreqtableindices[j] >= freqtablelength)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has a recorded frequency index (freq " << j << ") which is equal to " << datastreamtable[i].recordedfreqtableindices[j] << " that refers outside the frequency table range (" << freqtablelength << ") - aborting!!!" << endl;
        return false;
      }
      if(bandwidth != freqtable[datastreamtable[i].recordedfreqtableindices[j]].bandwidth)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "All recorded bandwidths for a given datastream must be equal - aborting!!!!" << endl;
        return false;
      }
      if(oversamp != freqtable[datastreamtable[i].recordedfreqtableindices[j]].oversamplefactor)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "All recorded oversample factors for a given datastream must be equal - aborting!!!!" << endl;
        return false;
      }
      if(decim != freqtable[datastreamtable[i].recordedfreqtableindices[j]].decimationfactor)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "All recorded decimations for a given datastream must be equal - aborting!!!!" << endl;
        return false;
      }
      if(toaver != freqtable[datastreamtable[i].recordedfreqtableindices[j]].channelstoaverage)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "All recorded channels to average for a given datastream must be equal - aborting!!!!" << endl;
        return false;
      }
    }

    //repeat for the zoom freqs, also check that they fit into a recorded freq and the channel widths match, and the polarisations match
    for(int j=0;j<datastreamtable[i].numzoomfreqs;j++)
    {
      if(datastreamtable[i].zoomfreqtableindices[j] < 0 || datastreamtable[i].zoomfreqtableindices[j] >= freqtablelength)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has a zoom frequency index (freq " << j << ") which is equal to " << datastreamtable[i].zoomfreqtableindices[j] << " that refers outside the frequency table range (" << freqtablelength << ") - aborting!!!" << endl;
        return false;
      }
      if(datastreamtable[i].zoomfreqparentdfreqindices[j] < 0) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has a zoom frequency index (freq " << j << ") which does not fit into any of the recorded bands - aborting!!!" << endl;
        return false;
      }
      double zoomfreqchannelwidth = freqtable[datastreamtable[i].zoomfreqtableindices[j]].bandwidth/freqtable[datastreamtable[i].zoomfreqtableindices[j]].numchannels;
      double parentfreqchannelwidth = freqtable[datastreamtable[i].recordedfreqtableindices[datastreamtable[i].zoomfreqparentdfreqindices[j]]].bandwidth/freqtable[datastreamtable[i].recordedfreqtableindices[datastreamtable[i].zoomfreqparentdfreqindices[j]]].numchannels;
      if(fabs(zoomfreqchannelwidth - parentfreqchannelwidth) > Mode::TINY) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has a zoom frequency index (freq " << j << ") whose channel width (" << zoomfreqchannelwidth << ") does not match its parents channel width (" << parentfreqchannelwidth << ") - aborting!!!" << endl;
        return false;
      }
    }

    //check that each zoom band has actually been recorded in the same polarisation
    for(int j=0;j<datastreamtable[i].numzoombands;j++) {
      bool matchingpol = false;
      for(int k=0;k<datastreamtable[i].numrecordedbands;k++) {
        if(datastreamtable[i].zoomfreqparentdfreqindices[datastreamtable[i].zoombandlocalfreqindices[j]] == datastreamtable[i].recordedbandlocalfreqindices[k]) {
          if (datastreamtable[i].zoombandpols[j] == datastreamtable[i].recordedbandpols[k])
            matchingpol = true;
        }
      }
      if(!matchingpol) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Datastream table entry " << i << " has a zoom band (band " << j << ") which does have have a parent band of the same polarisation (" << datastreamtable[i].zoombandpols[j] << ") - aborting!!!" << endl;
        return false;
      }
    }
  }

  //check that for all configs, the datastreams refer to the same telescope
  for(int i=0;i<numdatastreams;i++)
  {
    tindex = datastreamtable[configs[0].datastreamindices[i]].telescopeindex;
    for(int j=1;j<numconfigs;j++)
    {
      if(tindex != datastreamtable[configs[j].datastreamindices[i]].telescopeindex)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "All configs must have the same telescopes!  Config " << j << " datastream " << i << " refers to different telescopes - aborting!!!" << endl;
        return false;
      }
    }
  }

  //check that for all configs, if a datastream is muxed in one it is muxed in all, and frame size / num bits stays the same
  for(int i=0;i<numdatastreams;i++)
  {
    ismuxed = datastreamtable[configs[0].datastreamindices[i]].ismuxed;
    framebytes = datastreamtable[configs[0].datastreamindices[i]].framebytes;
    numbits = datastreamtable[configs[0].datastreamindices[i]].numbits;
    for(int j=1;j<numconfigs;j++)
    {
      if(ismuxed != datastreamtable[configs[j].datastreamindices[i]].ismuxed)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "If one config for a datastream is muxed, all must be! Config " << j << " datastream " << i << " differs from the first config - aborting!" << endl;
        return false;
      }
      if(ismuxed && framebytes != datastreamtable[configs[j].datastreamindices[i]].framebytes) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "If mux'ing, all configs must have the same frame bytes for a given datastream - not so for config " << j << " datastream " << i << " - aborting!" << endl;
        return false;
      }
      if(ismuxed && numbits != datastreamtable[configs[j].datastreamindices[i]].numbits) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "If mux'ing, all configs must have the same bits per sample for a given datastream - not so for config " << j << " datastream " << i << " - aborting!" << endl;
        return false;
      }
    }
  }

  //check entries in the config table, check that number of channels * sample time yields a whole number of nanoseconds and that the nanosecond increment is not too large for an int, and generate the ordered datastream indices array
  //also check that guardns is large enough
  int nchan, chantoav;
  int globalmaxnsslip = 0;
  double samplens;
  for(int i=0;i<numconfigs;i++)
  {
    //check the fringe rotation settings
    if(configs[i].fringerotationorder < 0 || configs[i].fringerotationorder > 2) {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Fringe rotation order must be 0, 1 or 2 for all configurations - aborting!!!" << endl;
      return false;
    }

    //fill in the maxnsslip for each datastream and calculate the maximum of these across all datastreams
    for(int j=0;j<numdatastreams;j++) {
      dsdata = &(datastreamtable[configs[i].datastreamindices[j]]);
      samplens = 500.0/freqtable[dsdata->recordedfreqtableindices[0]].bandwidth;
      if(dsdata->sampling == COMPLEX)
        samplens *= 2;
      double nsaccumulate = 0.0;
      do {
        nsaccumulate += dsdata->bytespersampledenom*samplens;
      } while (!(fabs(nsaccumulate - int(nsaccumulate)) < Mode::TINY));
      cdebug << startl << "NS accumulate is " << nsaccumulate << " and max geom slip is " << model->getMaxRate(dsdata->modelfileindex)*configs[i].subintns*0.000001 << ", maxnsslip is " << dsdata->maxnsslip << endl;
      nsaccumulate += model->getMaxRate(dsdata->modelfileindex)*configs[i].subintns*0.000001;
      if(nsaccumulate > dsdata->maxnsslip)
        dsdata->maxnsslip = int(nsaccumulate + 0.99);
      if(dsdata->maxnsslip > globalmaxnsslip)
        globalmaxnsslip = dsdata->maxnsslip;
    }

    //check that arraystridelen is ok, and guardns is ok
    for(int j=0;j<numdatastreams;j++) {
      dsdata = &(datastreamtable[configs[i].datastreamindices[j]]);
      for(int k=0;k<dsdata->numrecordedfreqs;k++) {
        nchan = freqtable[dsdata->recordedfreqtableindices[k]].numchannels;
        if(nchan % configs[i].arraystridelen[j] != 0) {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Config[" << i << "] datastream[" << j << "] has an array stride length of " << configs[i].arraystridelen[j] << " which is not an integral divisor of the number of channels in frequency[" << k << "] (which is " << nchan << ") - aborting!!!" << endl;
          return false;
        }
      }
      if(configs[i].guardns == 0)
      {
        configs[i].guardns = globalmaxnsslip;
        if(mpiid == 0)
          cinfo << startl << "Config[" << i << "] had its guardns automatically set to " << configs[i].guardns << " since it was not set in the configuration file" << endl;
      }
      if(configs[i].guardns < globalmaxnsslip) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Config[" << i << "] has a guard ns which is potentially too short (" << configs[i].guardns << ").  To be safe (against geometric rate slip and backwards shuffling of the start of a Datastream send to an integer nanosecond) guardns should be at least " << globalmaxnsslip << " - aborting!!!" << endl;
        return false;
      }
    }

    //work out the ordereddatastreamindices
    count = 0;
    for(int j=0;j<datastreamtablelength;j++)
    {
      configs[i].ordereddatastreamindices[j] = -1;
      for(int k=0;k<numdatastreams;k++)
      {
        if(configs[i].datastreamindices[k] == j)
          configs[i].ordereddatastreamindices[j] = count++;
      }
    }
    if(count != numdatastreams)
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Not all datastreams accounted for in the datastream table for config " << i << endl;
      return false;
    }

    //check that the subint time results in a whole number of FFTs for each datastream
    //also that the blockspersend is the same for all datastreams
    for(int j=0;j<numdatastreams;j++)
    {
      sampletimens = 1000.0/(2.0*freqtable[datastreamtable[configs[i].datastreamindices[j]].recordedfreqtableindices[0]].bandwidth);
      ffttime = sampletimens*freqtable[datastreamtable[configs[i].datastreamindices[j]].recordedfreqtableindices[0]].numchannels*2;
      numffts = configs[i].subintns/ffttime;
      if(ffttime - (int)(ffttime+0.5) > 0.00000001 || ffttime - (int)(ffttime+0.5) < -0.000000001)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "FFT chunk time for config " << i << ", datastream " << j << " is not a whole number of nanoseconds (" << ffttime << ") - aborting!!!" << endl;
        return false;
      }
      if(fabs(numffts - int(numffts+0.5)) > Mode::TINY) {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Send of size " << configs[i].subintns << " does not yield an integer number of FFTs for datastream " << j << " in config " << i << " - aborting!!!" << endl;
        return false;
      }
      if(((double)configs[i].subintns)*(databufferfactor/numdatasegments) > (((long long)1 << (sizeof(int)*8 - 1)) - 1))
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Increment per read in nanoseconds is " << ((double)configs[i].subintns)*(databufferfactor/numdatasegments) << " - too large to fit in an int - aborting!!!" << endl;
        return false;
      }
      for (int k=1;k<getDNumRecordedFreqs(i,j);k++) {
        freqdata f = freqtable[datastreamtable[configs[i].datastreamindices[j]].recordedfreqtableindices[k]];
        if (fabs((1000.0*f.numchannels)/f.bandwidth) - ffttime > Mode::TINY) {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Frequency " << k << " of datastream " << j << " of config " << i << " has a different bandwidth or num channels to the other freqs of this datastream - aborting!!!" << endl;
          return false;
        }
      }
    }

    //check that all baseline indices refer inside the table, and go in ascending order
    int b, lastt1 = 0, lastt2 = 0;
    for(int j=0;j<numbaselines;j++)
    {
      b = configs[i].baselineindices[j];
      if(b < 0 || b >= baselinetablelength) //bad index
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Config " << i << " baseline index " << j << " refers to baseline " << b << " which is outside the range of the baseline table - aborting!!!" << endl;
        return false;
      }
      if(datastreamtable[baselinetable[b].datastream2index].telescopeindex < lastt2 && datastreamtable[baselinetable[b].datastream1index].telescopeindex <= lastt1)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "Config " << i << " baseline index " << j << " refers to baseline " << datastreamtable[baselinetable[b].datastream2index].telescopeindex << "-" << datastreamtable[baselinetable[b].datastream1index].telescopeindex << " which is out of order with the previous baseline " << lastt1 << "-" << lastt2 << " - aborting!!!" << endl;
        return false;
      }
      lastt1 = datastreamtable[baselinetable[b].datastream1index].telescopeindex;
      lastt2 = datastreamtable[baselinetable[b].datastream2index].telescopeindex;
    }

    for(int j=0;j<numbaselines;j++)
    {
      bl = baselinetable[configs[i].baselineindices[j]];
      for(int k=0;k<bl.numfreqs;k++)
      {
        chantoav = freqtable[bl.freqtableindices[k]].channelstoaverage;
        nchan = freqtable[bl.freqtableindices[k]].numchannels;
        if(configs[i].xmacstridelen%chantoav != 0 && chantoav%configs[i].xmacstridelen != 0)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Config[" << i << "] has an xmac stride length of " << configs[i].xmacstridelen << " which is not 2^N x the channels to average in frequency [" << k << "] of baseline " << j << " (which is " << chantoav << ") - aborting!!!" << endl;
          return false;
        }
        if(nchan%configs[i].xmacstridelen != 0)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Config[" << i << "] has an xmac stride length of " << configs[i].xmacstridelen << " which is not an integer divisor of the number of channels in frequency[" << k << "] of baseline " << j << " (which is " << nchan << ") - aborting!!!" << endl;
          return false;
        }
      }
    }
  }

  //check the baseline table entries
  for(int i=0;i<baselinetablelength;i++)
  {
    //check the datastream indices
    if(baselinetable[i].datastream1index < 0 || baselinetable[i].datastream2index < 0 || baselinetable[i].datastream1index >= datastreamtablelength || baselinetable[i].datastream2index >= datastreamtablelength)
    {
      if(mpiid == 0) //only write one copy of this error message
        cfatal << startl << "Baseline table entry " << i << " has a datastream index outside the datastream table range! Its two indices are " << baselinetable[i].datastream1index << ", " << baselinetable[i].datastream2index << " - aborting!!!" << endl;
      return false;
    }

    ds1 = datastreamtable[baselinetable[i].datastream1index];
    ds2 = datastreamtable[baselinetable[i].datastream2index];
    for(int j=0;j<baselinetable[i].numfreqs;j++)
    {
      if(baselinetable[i].datastream1bandindex[j][0] >= ds1.numrecordedbands) //zoom band 
        freq1index = ds1.zoomfreqtableindices[ds1.zoombandlocalfreqindices[baselinetable[i].datastream1bandindex[j][0]-ds1.numrecordedbands]];
      else
        freq1index = ds1.recordedfreqtableindices[ds1.recordedbandlocalfreqindices[baselinetable[i].datastream1bandindex[j][0]]];
      if(baselinetable[i].datastream2bandindex[j][0] >= ds2.numrecordedbands) //zoom band
        freq2index = ds2.zoomfreqtableindices[ds2.zoombandlocalfreqindices[baselinetable[i].datastream2bandindex[j][0]-ds2.numrecordedbands]];
      else
        freq2index = ds2.recordedfreqtableindices[ds2.recordedbandlocalfreqindices[baselinetable[i].datastream2bandindex[j][0]]];
      if(freq1index != freq2index)
      {
        //these had better be compatible, otherwise bail
        f1 = freqtable[freq1index].bandedgefreq;
        f2 = freqtable[freq2index].bandedgefreq;
        if(freqtable[freq1index].lowersideband)
          f1 -= freqtable[freq1index].bandwidth;
        if(freqtable[freq2index].lowersideband)
          f2 -= freqtable[freq2index].bandwidth;
        if(freqtable[freq1index].bandedgefreq == freqtable[freq2index].bandedgefreq &&  freqtable[freq1index].bandwidth == freqtable[freq2index].bandwidth)
        {
          //different freqs, same value??
          if(mpiid == 0) //only write one copy of this error message
            cwarn << startl << "Baseline " << i << " frequency " << j << " points at two different frequencies that are apparently identical - this is not wrong, but very strange.  Check the input file" << endl;
        }
        else if(f1 == f2 && freqtable[freq1index].bandwidth == freqtable[freq2index].bandwidth)
        {
          //correlating a USB with an LSB
          if(mpiid == 0) //only write one copy of this error message
            cinfo << startl << "Baseline " << i << " frequency " << j << " is correlating an USB frequency with a LSB frequency" << endl;
          if(freqtable[freq1index].lowersideband)
            baselinetable[i].oddlsbfreqs[j] = 1; //datastream1 has the LSB (2 is USB)
          else
            baselinetable[i].oddlsbfreqs[j] = 2; //datastream2 has the LSB (1 is USB)
        }
        else
        {
          if(mpiid == 0) //only write one copy of this error message
            cwarn << startl << "Baseline table entry " << i << ", frequency " << j << " is trying to correlate two different frequencies!  Correlation will go on, but the results for these bands will probably be garbage!" << endl;
          if(freqtable[freq1index].lowersideband && !freqtable[freq2index].lowersideband)
            baselinetable[i].oddlsbfreqs[j] = 1;
          else if(freqtable[freq2index].lowersideband && !freqtable[freq1index].lowersideband)
            baselinetable[i].oddlsbfreqs[j] = 2;
        }
      }
      //catch the case of LSB against LSB where there is a USB somewhere
      else if(freqtable[freq1index].lowersideband && freqtable[freq2index].correlatedagainstupper)
      {
        baselinetable[i].oddlsbfreqs[j] = 3; //both are lower, but still need to be shifted
      }
      for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
      {
        //check the band indices
        if((baselinetable[i].datastream1bandindex[j][k] < 0) || (baselinetable[i].datastream1bandindex[j][k] >= (ds1.numrecordedbands + ds1.numzoombands)))
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Baseline table entry " << i << ", frequency " << j << ", polarisation product " << k << " for datastream 1 refers to a band outside datastream 1's range (" << baselinetable[i].datastream1bandindex[j][k] << ") - aborting!!!" << endl;
          return false;
        }
        if((baselinetable[i].datastream2bandindex[j][k] < 0) || (baselinetable[i].datastream2bandindex[j][k] >= (ds2.numrecordedbands + ds2.numzoombands)))
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Baseline table entry " << i << ", frequency " << j << ", polarisation product " << k << " for datastream 2 refers to a band outside datastream 2's range (" << baselinetable[i].datastream2bandindex[j][k] << ") - aborting!!!" << endl;
          return false;
        }

        //check that the freqs pointed at match
        if(baselinetable[i].datastream1bandindex[j][k] >= ds1.numrecordedbands) //zoom band
          freqindex = ds1.zoomfreqtableindices[ds1.zoombandlocalfreqindices[baselinetable[i].datastream1bandindex[j][k]-ds1.numrecordedbands]];
        else
          freqindex = ds1.recordedfreqtableindices[ds1.recordedbandlocalfreqindices[baselinetable[i].datastream1bandindex[j][k]]];
        if(freqindex != freq1index)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Baseline table entry " << i << ", frequency " << j << ", polarisation product " << k << " for datastream 1 does not match the frequency of the first polarisation product - aborting!!!" << endl;
          return false;
        }
        if(baselinetable[i].datastream2bandindex[j][k] >= ds2.numrecordedbands) //zoom band
          freqindex = ds2.zoomfreqtableindices[ds2.zoombandlocalfreqindices[baselinetable[i].datastream2bandindex[j][0]-ds2.numrecordedbands]];
        else
          freqindex = ds2.recordedfreqtableindices[ds2.recordedbandlocalfreqindices[baselinetable[i].datastream2bandindex[j][k]]];
        if(freqindex != freq2index)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Baseline table entry " << i << ", frequency " << j << ", polarisation product " << k << " for datastream 2 does not match the frequency of the first polarisation product - aborting!!!" << endl;
          return false;
        }
      }
    }
  }

  //for each config, check if there are any USB x LSB correlations 
  for(int i=0;i<numconfigs;i++)
  {
    configs[i].anyusbxlsb = false;
    for(int j=0;j<numbaselines;j++)
    {
      for(int k=0;k<baselinetable[j].numfreqs;k++)
      {
        if(baselinetable[j].oddlsbfreqs[k] > 0)
          configs[i].anyusbxlsb = true;
      }
    }
  }

  //for each config, if it is phased array and FILTERBANK, check that the
  //accumulation time is an integer number of FFTs and will divide evenly 
  //into the SUBINT/#threads for all Cores
  for(int i=0;i<numconfigs;i++)
  {
    if(configs[i].phasedarray == true)
    {
      if(configs[i].subintns%configs[i].paaccumulationns != 0)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "For config " << i << ", the requested phased array accumulation time (" << configs[i].paaccumulationns << " ns) does not fit evenly into a subintegration (" << configs[i].subintns << " ns) - aborting!!!" << endl;
        return false;
      }
      for(int j=0;j<numcoreconfs;j++)
      {
        if(configs[i].subintns%(configs[i].paaccumulationns*getCNumProcessThreads(j)) != 0)
        {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "For config " << i << ", the requested phased array accumulation time (" << configs[i].paaccumulationns << " ns) does not fit evenly into a thread subintegration (" << configs[i].subintns/getCNumProcessThreads(j) << " ns for thread " << j << ") - aborting!!!" << endl;
          return false;
        }
      }
      ffttime = ((double)configs[i].subintns)/configs[i].blockspersend;
      double accffts = configs[i].paaccumulationns/ffttime;
      if(fabs(accffts)-int(accffts+0.5) > 0.00000000001 || int(accffts+0.5)%configs[i].numbufferedffts != 0)
      {
        if(mpiid == 0) //only write one copy of this error message
          cfatal << startl << "For config " << i << ", the requested phased array accumulation time (" << configs[i].paaccumulationns << " ns) is not an integer number of FFTs - aborting!!!" << endl;
        return false;
      }
    }
  }

  if(databufferfactor % numdatasegments != 0)
  {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "There must be an integer number of sends per datasegment.  Presently databufferfactor is " << databufferfactor << ", and numdatasegments is " << numdatasegments << " - aborting!!!" << endl;
    return false;
  }

  //if there is any pulsar binning, make sure we have polycos
  initscan = 0;
  while(model->getScanEndSec(initscan, startmjd, startseconds) < 0)
    initscan++;
  initsec = -(model->getScanStartSec(initscan, startmjd, startseconds));
  if(initsec < 0)
    initsec = 0;
  for(int i=initscan;i<model->getNumScans();i++)
  {
    confindex =  scanconfigindices[i];
    if(configs[confindex].pulsarbin)
    {
      for(int j=initsec;j<=model->getScanDuration(i);j++)
      {
        timesec = model->getScanStartSec(i, startmjd, startseconds) + startseconds + j;
        if(Polyco::getCurrentPolyco(confindex, startmjd, timesec/86400.0, configs[confindex].polycos, configs[confindex].numpolycos, false) == NULL) {
          if(mpiid == 0) //only write one copy of this error message
            cfatal << startl << "Could not find a polyco to cover MJD " << startmjd + timesec/86400 << ", sec " << timesec % 86400 << " - aborting!!!" << endl;
          return false;
        }
      }
    }
    initsec = 0;
  }

  return true;
}

bool Configuration::processPhasedArrayConfig(string filename, int configindex)
{
  string line;

  if(mpiid == 0) //only write one copy of this info message
    cinfo << startl << "About to process phased array file " << filename << endl;
  ifstream phasedarrayinput(filename.c_str(), ios::in);
  if(!phasedarrayinput.is_open() || phasedarrayinput.bad())
  {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "Could not open phased array config file " << line << " - aborting!!!" << endl;
    return false;
  }
  getinputline(&phasedarrayinput, &line, "OUTPUT TYPE");
  if(line == "FILTERBANK")
    configs[configindex].padomain = FREQUENCY;
  else if (line == "TIMESERIES") {
    if(mpiid == 0) //only write one copy of this warning
      cwarn << startl << "Time series output is requested but not yet supported." << endl;
    configs[configindex].padomain = TIME;
  }
  else {
    if(mpiid == 0) //only write one copy of this error message
      cerror << startl << "Unknown phased array output type " << line << " - setting to FILTERBANK" << endl;
    configs[configindex].padomain = FREQUENCY;
  }
  getinputline(&phasedarrayinput, &line, "OUTPUT FORMAT");
  if(line == "DIFX") {
    if(configs[configindex].padomain == TIME) {
      if(mpiid == 0) //only write one copy of this error message
        cerror << startl << "Cannot produce DIFX format data with a time series - setting output format for phased array to VDIF!" << endl;
      configs[configindex].paoutputformat = VDIFOUT;
    }
    else {
      configs[configindex].paoutputformat = DIFX;
    }
  }
  else if (line == "VDIF") {
    if(configs[configindex].padomain == FREQUENCY) {
      if(mpiid == 0) //only write one copy of this warning
        cerror << startl << "Cannot produce VDIF format data with a filterbank - setting output format for phased array to DIFX!" << endl;
      configs[configindex].paoutputformat = DIFX;
    }
    else {
      configs[configindex].paoutputformat = VDIFOUT;
    }
  }
  else {
    if(configs[configindex].padomain == FREQUENCY) {
      if(mpiid == 0) //only write one copy of this error message
        cerror << startl << "Unknown phased array output format " << line << " - setting to DIFX" << endl;
      configs[configindex].paoutputformat = DIFX;
    }
    else {
      if(mpiid == 0) //only write one copy of this error message
        cerror << startl << "Unknown phased array output format " << line << " - setting to VDIF" << endl;
      configs[configindex].paoutputformat = VDIFOUT;
    }
  }
  getinputline(&phasedarrayinput, &line, "ACC TIME (NS)");
  configs[configindex].paaccumulationns = atoi(line.c_str());
  getinputline(&phasedarrayinput, &line, "COMPLEX OUTPUT");
  if(line == "TRUE" || line == "true" || line == "True")
    configs[configindex].pacomplexoutput = true;
  else
    configs[configindex].pacomplexoutput = false;
  getinputline(&phasedarrayinput, &line, "OUTPUT BITS");
  configs[configindex].pabits = atoi(line.c_str());
  configs[configindex].paweights = new double*[freqtablelength]();
  configs[configindex].numpafreqpols = new int[freqtablelength]();
  configs[configindex].papols = new char*[freqtablelength]();
  for(int i=0;i<freqtablelength;i++)
  {
    getinputline(&phasedarrayinput, &line, "NUM FREQ");
    configs[configindex].numpafreqpols[i] = atoi(line.c_str());
    if(configs[configindex].numpafreqpols[i] > 0)
    {
      configs[configindex].paweights[i] = new double[numdatastreams]();
      configs[configindex].papols[i] = new char[configs[configindex].numpafreqpols[i]]();
      for(int j=0;j<configs[configindex].numpafreqpols[i];j++)
      {
        getinputline(&phasedarrayinput, &line, "FREQ");
        configs[configindex].papols[i][j] = line.c_str()[0];
      }
      for(int j=0;j<numdatastreams;j++)
      {
        getinputline(&phasedarrayinput, &line, "FREQ");
        configs[configindex].paweights[i][j] = atof(line.c_str());
        if(configs[configindex].paweights[i][j] < 0.0)
        {
          if(mpiid == 0)
            cfatal << startl << "All phased array weights must be >= 0 - aborting!!!" << endl;
          return false;
        }
      }
    }
  }
  phasedarrayinput.close();
  return true;
}

bool Configuration::processPulsarConfig(string filename, int configindex)
{
  int numpolycofiles, ncoefficients, polycocount;
  string line;
  string * polycofilenames;
  double * binphaseends;
  double * binweights;
  int * numsubpolycos;
  char psrline[128];
  ifstream temppsrinput;

  if(mpiid == 0) //only write one copy of this info message
    cinfo << startl << "About to process pulsar file " << filename << endl;
  ifstream pulsarinput(filename.c_str(), ios::in);
  if(!pulsarinput.is_open() || pulsarinput.bad())
  {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "Could not open pulsar config file " << filename << " - aborting!!!" << endl;
    return false;
  }
  getinputline(&pulsarinput, &line, "NUM POLYCO FILES");
  numpolycofiles = atoi(line.c_str());
  polycofilenames = new string[numpolycofiles];
  numsubpolycos = new int[numpolycofiles]();
  configs[configindex].numpolycos = 0;
  for(int i=0;i<numpolycofiles;i++)
  {
    getinputline(&pulsarinput, &(polycofilenames[i]), "POLYCO FILE");
    numsubpolycos[i] = 0;
    temppsrinput.open(polycofilenames[i].c_str());
    if(!temppsrinput.is_open() || temppsrinput.bad()) {
      if(mpiid == 0) //only write one copy of this error message
        cerror << startl << "Could not open polyco file " << polycofilenames[i] << ", but continuing..." << endl;
      continue;
    }
    temppsrinput.getline(psrline, 128);
    temppsrinput.getline(psrline, 128);
    while(!(temppsrinput.eof() || temppsrinput.fail())) {
      psrline[54] = '\0';
      ncoefficients = atoi(&(psrline[49]));
      for(int j=0;j<ncoefficients/3 + 2;j++)
        temppsrinput.getline(psrline, 128);
      numsubpolycos[i]++;
      configs[configindex].numpolycos++;
    }
    temppsrinput.close();
  }
  if(configs[configindex].numpolycos == 0) {
    if(mpiid == 0) //only write one copy of this error message
      cfatal << startl << "No polycos were parsed from the binconfig file " << filename << " - aborting!!!" << endl;
    delete [] numsubpolycos;
    return false;
  }
  getinputline(&pulsarinput, &line, "NUM PULSAR BINS");
  configs[configindex].numbins = atoi(line.c_str());
  if(configs[configindex].numbins > maxnumpulsarbins)
    maxnumpulsarbins = configs[configindex].numbins;
  binphaseends = new double[configs[configindex].numbins]();
  binweights = new double[configs[configindex].numbins]();
  getinputline(&pulsarinput, &line, "SCRUNCH OUTPUT");
  configs[configindex].scrunchoutput = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
  for(int i=0;i<configs[configindex].numbins;i++)
  {
    getinputline(&pulsarinput, &line, "BIN PHASE END");
    binphaseends[i] = atof(line.c_str());
    getinputline(&pulsarinput, &line, "BIN WEIGHT");
    binweights[i] = atof(line.c_str());
  }

  //create the polycos
  configs[configindex].polycos = new Polyco*[configs[configindex].numpolycos]();
  polycocount = 0;
  for(int i=0;i<numpolycofiles;i++)
  {
    for(int j=0;j<numsubpolycos[i];j++)
    {
      //cinfo << startl << "About to create polyco file " << polycocount << " from filename " << polycofilenames[i] << ", subcount " << j << endl;
      configs[configindex].polycos[polycocount] = new Polyco(polycofilenames[i], j, configindex, configs[configindex].numbins, getMaxNumChannels(), binphaseends, binweights, double(configs[configindex].subintns)/60000000000.0);
      if (!configs[configindex].polycos[polycocount]->initialisedOK()) {
	delete [] numsubpolycos;
        return false;
      }
      estimatedbytes += configs[configindex].polycos[polycocount]->getEstimatedBytes();
      polycocount++;
    } 
  }
  
  delete [] binphaseends;
  delete [] binweights;
  delete [] polycofilenames;
  delete [] numsubpolycos;
  pulsarinput.close();
  return true;
}

bool Configuration::setPolycoFreqInfo(int configindex)
{
  bool ok = true;
  datastreamdata d = datastreamtable[getMaxNumFreqDatastreamIndex(configindex)];	/* FIXME: This value is never used */
  double * frequencies = new double[freqtablelength]();
  double * bandwidths = new double[freqtablelength]();
  int * numchannels = new int[freqtablelength]();
  bool * used = new bool[freqtablelength]();
  for(int i=0;i<freqtablelength;i++)
  {
    frequencies[i] = freqtable[i].bandedgefreq;
    if(freqtable[i].lowersideband)
      frequencies[i] -= ((double)(freqtable[i].numchannels-1))*freqtable[i].bandwidth/((double)freqtable[i].numchannels);
    bandwidths[i] = freqtable[i].bandwidth;
    numchannels[i] = freqtable[i].numchannels;
    used[i] = configs[configindex].frequsedbybaseline[i];
  }
  for(int i=0;i<configs[configindex].numpolycos;i++)
  {
    ok = ok && configs[configindex].polycos[i]->setFrequencyValues(freqtablelength, frequencies, bandwidths, numchannels, used);
  }
  delete [] frequencies;
  delete [] bandwidths;
  delete [] numchannels;
  delete [] used;
  return ok;
}

bool Configuration::updateClock(std::string clockstring)
{
  size_t at;
  int count, antennaindex;
  double deltaclock[Model::MAX_POLY_ORDER+1];
  double * poly;

  count = 0;
  for(int i=0;i<Model::MAX_POLY_ORDER+1;i++)
    deltaclock[i] = 0.0;
  at = clockstring.find_first_of(":");
  if(at == string::npos) {
    cerror << startl << "Received a dodgy deltaclock parameter string: " << clockstring << endl;
    return false;
  }
  antennaindex = atoi(clockstring.substr(0,at).c_str());
  while(at != string::npos && count < Model::MAX_POLY_ORDER+1) {
    clockstring = clockstring.substr(at+1);
    at = clockstring.find_first_of(":");
    deltaclock[count] = atof(clockstring.substr(0,at).c_str());
    count++;
  }
  if(count == Model::MAX_POLY_ORDER+1) {
    cerror << startl << "Tried to add a clock of too-high order! String was " << clockstring << ", leaving clocks unchanged" << endl;
    return false;
  }
  poly = new double[count]();
  for(int i=0;i<count;i++)
    poly[i] = deltaclock[i];

  model->updateClock(antennaindex, count-1, poly);
  delete [] poly;

  return true;
}

bool Configuration::fillHeaderData(ifstream * input, int & baselinenum, int & mjd, double & seconds, int & configindex, int & sourceindex, int & freqindex, char polpair[3], int & pulsarbin, double & dataweight, double uvw[3])
{
  unsigned int sync;
  int version;
  string line;

  input->read((char*)(&sync), 4);
  if(sync != Visibility::SYNC_WORD)
  {
    if(sync != 'BASE')
      return false;

    //if we get.d2 here, must be an old style ascii file
    getinputline(input, &line, "LINE NUM");
    baselinenum = atoi(line.c_str());
    getinputline(input, &line, "MJD");
    mjd = atoi(line.c_str());
    getinputline(input, &line, "SECONDS");
    seconds = atof(line.c_str());
    getinputline(input, &line, "CONFIG INDEX");
    configindex = atoi(line.c_str());
    getinputline(input, &line, "SOURCE INDEX");
    sourceindex = atoi(line.c_str());
    getinputline(input, &line, "FREQ INDEX");
    freqindex = atoi(line.c_str());
    getinputline(input, &line, "POLARISATION PAIR");
    polpair[0] = line.at(0);
    polpair[1] = line.at(1);
    getinputline(input, &line, "PULSAR BIN");
    pulsarbin = atoi(line.c_str());
    getinputline(input, &line, "FLAGGED");
    getinputline(input, &line, "DATA WEIGHT");
    dataweight = atof(line.c_str());
    getinputline(input, &line, "U (METRES)");
    uvw[0] = atof(line.c_str());
    getinputline(input, &line, "V (METRES)");
    uvw[1] = atof(line.c_str());
    getinputline(input, &line, "W (METRES)");
    uvw[2] = atof(line.c_str());
    return true;
  }
  input->read((char*)(&version), 4);
  if(version != Visibility::BINARY_HEADER_VERSION)
    return false;
  input->read((char*)(&baselinenum), 4);
  input->read((char*)(&mjd), 4);
  input->read((char*)(&seconds), 8);
  input->read((char*)(&configindex), 4);
  input->read((char*)(&sourceindex), 4);
  input->read((char*)(&freqindex), 4);
  input->read(polpair, 2);
  input->read((char*)(&pulsarbin), 4);
  input->read((char*)(&dataweight), 8);
  input->read((char*)(uvw), 3*8);
  return true;
}

/* FIXME: should not be a member function? */
void Configuration::makeFortranString(string line, int length, char * destination) const
{
  int linelength = line.length();
  
  if(linelength <= length)
  {
    strcpy(destination, line.c_str());
    for(int i=0;i<length-linelength;i++)
      destination[i+linelength] = ' ';
  }
  else
  {
    strcpy(destination, (line.substr(0, length-1)).c_str());
    destination[length-1] = line.at(length-1);
  }
}

void Configuration::getinputkeyval(ifstream * input, std::string * key, std::string * val) const
{
  if(input->eof())
    cerror << startl << "Trying to read past the end of file!" << endl;
  getline(*input, *key);
  while(key->length() > 0 && key->at(0) == COMMENT_CHAR) { // a comment
    //if(mpiid == 0) //only write one copy of this error message
    //  cverbose << startl << "Skipping comment " << key << endl;
    getline(*input, *key);
  }
  int keylength = key->find_first_of(':') + 1;
  if(keylength < DEFAULT_KEY_LENGTH)
    keylength = DEFAULT_KEY_LENGTH;
  *val = key->substr(keylength);
  *key = key->substr(0, key->find_first_of(':'));
}

void Configuration::getinputline(ifstream * input, std::string * line, std::string startofheader, bool verbose) const
{
  if(input->eof())
    cerror << startl << "Trying to read past the end of file!" << endl;
  getline(*input,*line);
  while(line->length() > 0 && line->at(0) == COMMENT_CHAR) { // a comment
    //if(mpiid == 0) //only write one copy of this error message
    //  cverbose << startl << "Skipping comment " << line << endl;
    getline(*input, *line);
  }
  int keylength = line->find_first_of(':') + 1;
  if(keylength < DEFAULT_KEY_LENGTH)
    keylength = DEFAULT_KEY_LENGTH;
  if(startofheader.compare((*line).substr(0, startofheader.length())) != 0) //not what we expected
  {
    if (verbose) 
      cerror << startl << "We thought we were reading something starting with '" << startofheader << "', when we actually got '" << (*line).substr(0, keylength) << "'" << endl;
    else {
      *line = "";
      return;
    }
  }
  *line = line->substr(keylength);
}

void Configuration::getinputline(ifstream * input, std::string * line, std::string startofheader) const
{
  getinputline(input, line, startofheader, true);
}


void Configuration::getinputline(ifstream * input, std::string * line, std::string startofheader, int intval) const
{
  char buffer[MAX_KEY_LENGTH+1];
  sprintf(buffer, "%s%i", startofheader.c_str(), intval);
  getinputline(input, line, string(buffer), true);
}

void Configuration::getMJD(int & d, int & s, int year, int month, int day, int hour, int minute, int second) const
{
  d = year*367 - int(7*(year + int((month + 9)/12))/4) + int(275*month/9) + day - 678987;

  s = 3600*hour + 60*minute + second;
}

void Configuration::mjd2ymd(int mjd, int & year, int & month, int & day) const
{
  int j = mjd + 32044 + 2400001;
  int g = j / 146097;
  int dg = j % 146097;
  int c = ((dg/36524 + 1)*3)/4;
  int dc = dg - c*36524;
  int b = dc / 1461;
  int db = dc % 1461;
  int a = ((db/365 + 1)*3)/4;
  int da = db - a*365;
  int y = g*400 + c*100 + b*4 + a;
  int m = (da*5 + 308)/153 - 2;
  int d = da - ((m + 4)*153)/5 + 122;
  
  year = y - 4800 + (m + 2)/12;
  month = (m + 2)%12 + 1;
  day = d + 1;
}

Configuration::filechecklevel Configuration::getFileCheckLevel()
{
  const char *v;

  v = getenv("DIFX_FILE_CHECK_LEVEL");
  if(v == 0)
  {
    return Configuration::FILECHECKSEEK;  // default
  }
  else if(strcmp(v, "NONE") == 0)
  {
    return Configuration::FILECHECKNONE;
  }
  else if(strcmp(v, "SEEK") == 0)
  {
    return Configuration::FILECHECKSEEK;
  }
  else
  {
    return Configuration::FILECHECKUNKNOWN;
  }
}


// vim: shiftwidth=2:softtabstop=2:expandtab
