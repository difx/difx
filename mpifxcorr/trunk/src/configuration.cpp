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
#include "configuration.h"
#include "mode.h"
#include "datastream.h"
#include "mk5.h"

Configuration::Configuration(const char * configfile)
{
  //open the file
  ifstream * input = new ifstream(configfile);
  if(input->fail() || !input->is_open())
  {
    cerr << "Error opening file " << configfile << " - aborting!!!" << endl;
    exit(1);
  }
  sectionheader currentheader = getSectionHeader(input);
  commonread = false;
  datastreamread = false;
  configread = false;
  uvw = NULL;

  //go through all the sections and tables in the input file
  while(currentheader != INPUT_EOF)
  {
    switch(currentheader)
    {
      case COMMON:
        processCommon(input);
        break;
      case CONFIG:
        if(!commonread)
        {
          cerr << "Error - input file out of order!  Attempted to read configuration details without knowledge of common settings - aborting!!!" << endl;
          exit(EXIT_FAILURE);
        }
        processConfig(input);
        break;
      case FREQ:
        processFreqTable(input);
        break;
      case TELESCOPE:
        processTelescopeTable(input);
        break;
      case DATASTREAM:
        if(!configread)
        {
          cerr << "Error - input file out of order!  Attempted to read datastreams without knowledge of configs - aborting!!!" << endl;
          exit(EXIT_FAILURE);
        }
        processDatastreamTable(input);
        break;
      case BASELINE:
        processBaselineTable(input);
        break;
      case DATA:
        if(!datastreamread)
        {
          cerr << "Error - input file out of order!  Attempted to read datastream data files without knowledge of datastreams - aborting!!!" << endl;
          exit(EXIT_FAILURE);
        }
        processDataTable(input);
        break;
      case NETWORK:
        if(!datastreamread)
        {
          cerr << "Error - input file out of order!  Attempted to read datastream network details without knowledge of datastreams - aborting!!!" << endl;
          exit(EXIT_FAILURE);
        }
        processNetworkTable(input);
        break;
    }
    currentheader = getSectionHeader(input);
  }
  if(!configread)
  {
    cerr << "Error - no config section in input file - aborting!!!" << endl;
    exit(EXIT_FAILURE);
  }
  input->close();
  delete input;
  consistencyCheck();
  cout << "Finished processing input file!!!" << endl;
}


Configuration::~Configuration()
{
  if(configread)
  {
    for(int i=0;i<numconfigs;i++)
    {
      delete [] configs[i].datastreamindices;
      delete [] configs[i].baselineindices;
      delete [] configs[i].ordereddatastreamindices;
    }
    delete [] configs;
  }
  if(datastreamread)
  {
    for(int i=0;i<datastreamtablelength;i++)
    {
      for(int j=0;j<datastreamtable[i].numfreqs;j++)
      {
        delete [] datastreamtable[i].freqtableindices;
        delete [] datastreamtable[i].freqpols;
        delete [] datastreamtable[i].freqclockoffsets;
      }
      for(int j=0;j<datastreamtable[i].numinputbands;j++)
      {
        delete [] datastreamtable[i].inputbandpols;
        delete [] datastreamtable[i].inputbandlocalfreqindices;
      }
      for(int j=0;j<datastreamtable[i].numdatafiles;j++)
        delete [] datastreamtable[i].datafilenames;
    }
    delete [] datastreamtable;
  }
  if(uvw)
    delete uvw;
  delete [] freqtable;
  delete [] telescopetable;
  for(int i=0;i<baselinetablelength;i++)
  {
    for(int j=0;j<baselinetable[i].numfreqs;j++)
    {
      delete [] baselinetable[i].datastream1bandindex[j];
      delete [] baselinetable[i].datastream2bandindex[j];
    }
    delete [] baselinetable[i].datastream1bandindex;
    delete [] baselinetable[i].datastream2bandindex;
    delete [] baselinetable[i].numpolproducts;
  }
  delete [] baselinetable;
  delete [] numprocessthreads;
  delete [] firstnaturalconfigindices;
}

int Configuration::getMaxResultLength()
{
  int length;
  int maxlength = getResultLength(0);

  for(int i=1;i<numconfigs;i++)
  {
    length = getResultLength(i);
    if(length > maxlength)
      maxlength = length;
  }

  return maxlength;
}

int Configuration::getMaxDataBytes()
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

int Configuration::getMaxDataBytes(int datastreamindex)
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

int Configuration::getMaxSendBlocks()
{
  int length;
  int maxlength = configs[0].blockspersend + configs[0].guardblocks;

  for(int i=1;i<numconfigs;i++)
  {
    length = configs[i].blockspersend + configs[i].guardblocks;
    if(length > maxlength)
      maxlength = length;
  }

  return maxlength;
}

int Configuration::getMaxNumFreqs()
{
  int currentnumfreqs, maxnumfreqs = 0;
  
  for(int i=0;i<numconfigs;i++)
  {
    currentnumfreqs = getMaxNumFreqs(i);
    if(currentnumfreqs > maxnumfreqs)
      maxnumfreqs = currentnumfreqs;
  }
  
  return maxnumfreqs;
}

int Configuration::getMaxNumFreqs(int configindex)
{
  int maxnumfreqs = 0;
  
  for(int i=0;i<numdatastreams;i++)
  {
    if(datastreamtable[configs[configindex].datastreamindices[i]].numfreqs > maxnumfreqs)
      maxnumfreqs = datastreamtable[configs[configindex].datastreamindices[i]].numfreqs;
  }
  
  return maxnumfreqs;
}

int Configuration::getMaxNumFreqDatastreamIndex(int configindex)
{
  int maxindex = 0;
  int maxnumfreqs = datastreamtable[configs[configindex].datastreamindices[0]].numfreqs;
  
  for(int i=1;i<numdatastreams;i++)
  {
    if(datastreamtable[configs[configindex].datastreamindices[i]].numfreqs > maxnumfreqs)
    {
      maxnumfreqs = datastreamtable[configs[configindex].datastreamindices[i]].numfreqs;
      maxindex = i;
    }
  }
  
  return maxindex;
}

int Configuration::getResultLength(int configindex)
{
  datastreamdata currentdatastream;
  int numbands = 0;
  int bandsperautocorr = (configs[configindex].writeautocorrs)?2:1;

  //add up all the bands in the baselines
  for(int i=0;i<numbaselines;i++)
    numbands += baselinetable[configs[configindex].baselineindices[i]].totalbands;

  //multiply this by number of pulsar bins if necessary
  if(configs[configindex].pulsarbin && !configs[configindex].scrunchoutput)
    numbands *= configs[configindex].numbins;

  //add all the bands from all the datastreams
  for(int i=0;i<numdatastreams;i++)
  {
    currentdatastream = datastreamtable[configs[configindex].datastreamindices[i]];
    numbands += currentdatastream.numoutputbands*bandsperautocorr;
  }

  return numbands*(configs[configindex].numchannels+1);
}

int Configuration::getDataBytes(int configindex, int datastreamindex)
{
  datastreamdata currentds = datastreamtable[configs[configindex].datastreamindices[datastreamindex]];
  int validlength = (configs[configindex].blockspersend*currentds.numinputbands*2*currentds.numbits*configs[configindex].numchannels)/8;
  if(currentds.format == MKV || currentds.format == MKV_MKIV || currentds.format == MKV_VLBA)
  {
    //must be an integer number of frames, with enough margin for overlap on either side
    validlength += (configs[configindex].guardblocks*currentds.numinputbands*2*currentds.numbits*configs[configindex].numchannels)/8;
    return ((validlength/currentds.framebytes)+2)*currentds.framebytes;
  }
  else
    return validlength;
}

int Configuration::getMaxProducts(int configindex)
{
  baselinedata current;
  int maxproducts = 0;
  for(int i=0;i<numbaselines;i++)
  {
    if(configs[configindex].baselineindices[i] >= baselinetablelength || configs[configindex].baselineindices[i] < 0)
    {
      cerr << "Error - baselinetable index of " << configs[configindex].baselineindices[i] << " from config " << configindex << ", baseline " << i << " is outside of table range!!!" << endl;
      exit(1);
    }
    current = baselinetable[configs[configindex].baselineindices[i]];
    for(int j=0;j<current.numfreqs;j++)
    {
      if(current.numpolproducts[j] > maxproducts)
        maxproducts = current.numpolproducts[j];
    }
  }
  return maxproducts;
}

int Configuration::getMaxProducts()
{
  int maxproducts = 0;

  for(int i=0;i<numconfigs;i++)
  {
    if(getMaxProducts(i) > maxproducts)
      maxproducts = getMaxProducts(i);
  }
  
  return maxproducts;
}

int Configuration::getDMatchingBand(int configindex, int datastreamindex, int bandindex)
{
  for(int i=0;i<datastreamtable[configs[configindex].datastreamindices[datastreamindex]].numoutputbands;i++)
  {
    if(datastreamtable[configs[configindex].datastreamindices[datastreamindex]].inputbandlocalfreqindices[bandindex] ==
       datastreamtable[configs[configindex].datastreamindices[datastreamindex]].inputbandlocalfreqindices[i] && (i != bandindex))
      return i;
  }

  return -1;
}

int Configuration::getCNumProcessThreads(int corenum)
{
  if(corenum < numcoreconfs)
    return numprocessthreads[corenum];
  return 1;
}

void Configuration::loaduvwinfo(bool sourceonly)
{
  uvw = new Uvw(this, uvwfilename, sourceonly);
}

bool Configuration::stationUsed(int telescopeindex)
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

void Configuration::findMkVFormat(int configindex, int configdatastreamindex)
{
  long long maxoffset;
  int lastoffset;
  int mboffset = 0;
  bool lastok = false;
  bool beforelastok = false;
  VLBA_stream * vs;
  datastreamdata * ds = &(datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]]);

  if(ds->numdatafiles > 0) //open up a file and sort some stuff out
  {
    //check every MB into the file for 10 MB until we get two in a row that agree - handles the case of possible sync errors
    while(mboffset < 10 && !(lastok && beforelastok))
    {
      beforelastok = lastok;
      vs = VLBA_stream_open((ds->datafilenames[0]).c_str(), ds->numbits, ds->fanout, mboffset*1048576/*offset*/);
      if(vs != 0) //found the sync
      {
        lastok = true;
        if(vs->format == FORMAT_VLBA)
          ds->format = MKV_VLBA;
        else if(vs->format == FORMAT_MARK4)
          ds->format = MKV_MKIV;
        else
        {
          lastok = false;
          ds->format = MKV;
        }
        if(ds->format == MKV_VLBA || ds->format == MKV_MKIV)
        {
          ds->framebytes = vs->gulpsize;
          if(((mboffset*1048576 + vs->fileoffset - lastoffset) % vs->gulpsize) != 0)
            beforelastok = false;

          lastoffset = mboffset*1048576 + vs->fileoffset;
        }
        VLBA_stream_close(vs);
      }
      else
        lastok = false;
      mboffset++;
    }
    if(!lastok || !beforelastok)
    {
      cerr << "Error opening MkV style file " << ds->datafilenames[0] << " for datastream " << configdatastreamindex << " - could not find sync in first 10 MB - aborting!!!";
      exit(1);
    }
  }
}

int Configuration::getMkVFormat(int configindex, int configdatastreamindex)
{
  if(datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].format != MKV_VLBA &&  datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].format != MKV_MKIV && 
  datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numdatafiles > 0)
  {
    cerr << "Error trying to retrieve MkV format for configindex " << configindex << ", datastream " << configdatastreamindex << ": had not been updated from general MkV!!" << endl;
    exit(1);
  }

  return (datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].format == MKV_VLBA)?FORMAT_VLBA:FORMAT_MARK4;
}

void Configuration::setMkVFormat(int configindex, int configdatastreamindex, int format)
{
  if(format != FORMAT_VLBA && format != FORMAT_MARK4)
  {
    cerr << "Error trying to set MkV format for configindex " << configindex << ", datastream " << configdatastreamindex << ": had not been updated from general MkV!!" << endl;
    exit(1);
  }
  datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].format = ((format == FORMAT_VLBA)?MKV_VLBA:MKV_MKIV);
}

int Configuration::getConfigIndex(int offsetseconds)
{
  int currentconfigindex;
  string currentsourcename;

  if(!uvw)
  {
    cerr << "UVW HAS NOT BEEN CREATED!!!" << endl;
    exit(1);
  }

  uvw->getSourceName(startmjd, startseconds + offsetseconds, currentsourcename);
  currentconfigindex = defaultconfigindex;
  for(int i=0;i<numconfigs;i++)
  {
    if(configs[i].sourcename == currentsourcename.substr(0, (configs[i].sourcename).length()))
      currentconfigindex = i;
  }

  return currentconfigindex;
}

Mode* Configuration::getMode(int configindex, int datastreamindex)
{
  configdata conf = configs[configindex];
  datastreamdata stream = datastreamtable[conf.datastreamindices[datastreamindex]];

  switch(stream.format)
  {
    case LBASTD:
      if(stream.numbits != 2)
        cerr << "ERROR! All LBASTD Modes must have 2 bit sampling - overriding input specification!!!" << endl;
      return new LBAMode(this, configindex, datastreamindex, conf.numchannels, conf.blockspersend, conf.guardblocks, stream.numfreqs, freqtable[stream.freqtableindices[0]].bandwidth, stream.freqclockoffsets, stream.numinputbands, stream.numoutputbands, 2/*bits*/, stream.filterbank, conf.pulsarbin, conf.scrunchoutput, conf.postffringerot, conf.quadraticdelayinterp, conf.writeautocorrs, LBAMode::stdunpackvalues);
      break;
    case LBAVSOP:
      if(stream.numbits != 2)
        cerr << "ERROR! All LBASTD Modes must have 2 bit sampling - overriding input specification!!!" << endl;
      return new LBAMode(this, configindex, datastreamindex, conf.numchannels, conf.blockspersend, conf.guardblocks, stream.numfreqs, freqtable[stream.freqtableindices[0]].bandwidth, stream.freqclockoffsets, stream.numinputbands, stream.numoutputbands, 2/*bits*/, stream.filterbank, conf.pulsarbin, conf.scrunchoutput, conf.postffringerot, conf.quadraticdelayinterp, conf.writeautocorrs, LBAMode::vsopunpackvalues);
      break;
    case MKV:
    case MKV_MKIV:
    case MKV_VLBA:
      return new Mk5Mode(this, configindex, datastreamindex, stream.fanout, conf.numchannels, conf.blockspersend, conf.guardblocks, stream.numfreqs, freqtable[stream.freqtableindices[0]].bandwidth, stream.freqclockoffsets, stream.numinputbands, stream.numoutputbands, stream.numbits, stream.filterbank, conf.pulsarbin, conf.scrunchoutput, conf.postffringerot, conf.quadraticdelayinterp, conf.writeautocorrs, stream.framebytes);
      break;
  }

  cerr << "Error - unknown Mode!!!" << endl;
  return NULL;
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

void Configuration::processBaselineTable(ifstream * input)
{
  int tempint;
  int ** tempintptr;
  string line;

  getinputline(input, &line, "BASELINE ENTRIES");
  baselinetablelength = atoi(line.c_str());
  baselinetable = new baselinedata[baselinetablelength];
  if(baselinetablelength < numbaselines)
  {
    cerr << "Error - not enough baselines are supplied in the baseline table (" << baselinetablelength << ") compared to the number of baselines (" << numbaselines << ")!!!" << endl;
    exit(1);
  }

  for(int i=0;i<baselinetablelength;i++)
  {
    //read in the info for this baseline
    baselinetable[i].totalbands = 0;
    getinputline(input, &line, "D/STREAM A INDEX ", i);
    baselinetable[i].datastream1index = atoi(line.c_str());
    getinputline(input, &line, "D/STREAM B INDEX ", i);
    baselinetable[i].datastream2index = atoi(line.c_str());
    getinputline(input, &line, "NUM FREQS ", i);
    baselinetable[i].numfreqs = atoi(line.c_str());
    baselinetable[i].numpolproducts = new int[baselinetable[i].numfreqs];
    baselinetable[i].datastream1bandindex = new int*[baselinetable[i].numfreqs];
    baselinetable[i].datastream2bandindex = new int*[baselinetable[i].numfreqs];
    for(int j=0;j<baselinetable[i].numfreqs;j++)
    {
      getinputline(input, &line, "POL PRODUCTS ", i);
      baselinetable[i].numpolproducts[j] = atoi(line.c_str());
      baselinetable[i].datastream1bandindex[j] = new int[baselinetable[i].numpolproducts[j]];
      baselinetable[i].datastream2bandindex[j] = new int[baselinetable[i].numpolproducts[j]];
      for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
      {
        baselinetable[i].totalbands++;
        getinputline(input, &line, "D/STREAM A BAND ", k);
        baselinetable[i].datastream1bandindex[j][k] = atoi(line.c_str());
        getinputline(input, &line, "D/STREAM B BAND ", k);
        baselinetable[i].datastream2bandindex[j][k] = atoi(line.c_str());
      }
    }
    if(datastreamtable[baselinetable[i].datastream1index].telescopeindex > datastreamtable[baselinetable[i].datastream2index].telescopeindex)
    {
      cerr << "Error - first datastream for baseline " << i << " has a higher number than second datastream - reversing!!!" << endl;
      tempint = baselinetable[i].datastream1index;
      baselinetable[i].datastream1index = baselinetable[i].datastream2index;
      baselinetable[i].datastream2index = tempint;
      tempintptr = baselinetable[i].datastream1bandindex;
      baselinetable[i].datastream1bandindex = baselinetable[i].datastream2bandindex;
      baselinetable[i].datastream2bandindex = tempintptr;
    }
  }
}

void Configuration::processCommon(ifstream * input)
{
  string line;

  getinputline(input, &delayfilename, "DELAY FILENAME");
  getinputline(input, &uvwfilename, "UVW FILENAME");
  getinputline(input, &coreconffilename, "CORE CONF FILENAME");
  getinputline(input, &line, "EXECUTE TIME (SEC)");
  executeseconds = atoi(line.c_str());
  getinputline(input, &line, "START MJD");
  startmjd = atoi(line.c_str());
  getinputline(input, &line, "START SECONDS");
  startseconds = atoi(line.c_str());
  getinputline(input, &line, "ACTIVE DATASTREAMS");
  numdatastreams = atoi(line.c_str());
  getinputline(input, &line, "ACTIVE BASELINES");
  numbaselines = atoi(line.c_str());
  getinputline(input, &line, "DATA HEADER O/RIDE");
  dataoverride = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
  getinputline(input, &line, "OUTPUT FORMAT");
  if(line == "RPFITS")
  {
    outformat = RPFITS;
  }
  else if(line == "SWIN")
  {
    outformat = DIFX;
  }
  else if(line == "ASCII")
  {
    outformat = ASCII;
  }
  else
  {
    cerr << "Unknown output format " << line << " (case sensitive choices are RPFITS, SWIN and ASCII), assuming RPFITS" << endl;
    outformat = RPFITS;
  }
  getinputline(input, &outputfilename, "OUTPUT FILENAME");

  commonread = true;
}

void Configuration::processConfig(ifstream * input)
{
  string line;
  bool found;

  maxnumchannels = 0;
  maxnumpulsarbins = 0;
  numindependentchannelconfigs = 0;

  getinputline(input, &line, "NUM CONFIGURATIONS");
  numconfigs = atoi(line.c_str());
  configs = new configdata[numconfigs];
  firstnaturalconfigindices = new int[numconfigs];
  defaultconfigindex = -1;
  for(int i=0;i<numconfigs;i++)
  {
    found = false;
    configs[i].datastreamindices = new int[numdatastreams];
    configs[i].baselineindices = new int [numbaselines];
    getinputline(input, &(configs[i].sourcename), "CONFIG SOURCE");
    if(configs[i].sourcename == "DEFAULT")
      defaultconfigindex = i;
    getinputline(input, &line, "INT TIME (SEC)");
    configs[i].inttime = atof(line.c_str());
    getinputline(input, &line, "NUM CHANNELS");
    configs[i].numchannels = atoi(line.c_str());
    configs[i].independentchannelindex = i;
    for(int j=numindependentchannelconfigs-1;j>=0;j--)
    {
      if(configs[i].numchannels == configs[firstnaturalconfigindices[j]].numchannels)
      {
        found = true;
        configs[i].independentchannelindex = j;
      }
    }
    if(!found)
      firstnaturalconfigindices[numindependentchannelconfigs++] = i;
    if(configs[i].numchannels > maxnumchannels)
      maxnumchannels = configs[i].numchannels;
    getinputline(input, &line, "BLOCKS PER SEND");
    configs[i].blockspersend = atoi(line.c_str());
    getinputline(input, &line, "GUARD BLOCKS");
    configs[i].guardblocks = atoi(line.c_str());
    getinputline(input, &line, "POST-F FRINGE ROT");
    configs[i].postffringerot = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    getinputline(input, &line, "QUAD DELAY INTERP");
    configs[i].quadraticdelayinterp = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    if(configs[i].postffringerot && configs[i].quadraticdelayinterp)
    {
      cerr << "ERROR - cannot quad interpolate delays with post-f fringe rotation - aborting!!!" << endl;
      exit(1);
    }
    getinputline(input, &line, "WRITE AUTOCORRS");
    configs[i].writeautocorrs = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    getinputline(input, &line, "PULSAR BINNING");
    configs[i].pulsarbin = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    if(configs[i].pulsarbin)
    {
      getinputline(input, &configs[i].pulsarconfigfilename, "PULSAR CONFIG FILE");
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
  if(defaultconfigindex < 0)
  {
    cerr << "Warning - no default config found - sources which were not specified will not be correlated!!!" << endl;
  }

  configread = true;
}

void Configuration::processDatastreamTable(ifstream * input)
{
  string line;

  getinputline(input, &line, "DATASTREAM ENTRIES");
  datastreamtablelength = atoi(line.c_str());
  datastreamtable = new datastreamdata[datastreamtablelength];
  if(datastreamtablelength < numdatastreams)
  {
    cerr << "Error - not enough datastreams are supplied in the datastream table (" << datastreamtablelength << ") compared to the number of datastreams (" << numdatastreams << "!!!" << endl;
    exit(1);
  }
  //create the ordereddatastream array
  for(int i=0;i<numconfigs;i++)
    configs[i].ordereddatastreamindices = new int[datastreamtablelength];

  //get the information on the length of the internal buffer for the datastreams
  getinputline(input, &line, "DATA BUFFER FACTOR");
  databufferfactor = atoi(line.c_str());
  getinputline(input, &line, "NUM DATA SEGMENTS");
  numdatasegments = atoi(line.c_str());

  for(int i=0;i<datastreamtablelength;i++)
  {
    //read all the info for this datastream
    getinputline(input, &line, "TELESCOPE INDEX");
    datastreamtable[i].telescopeindex = atoi(line.c_str());
    getinputline(input, &line, "TSYS");
    datastreamtable[i].tsys = atof(line.c_str());
    getinputline(input, &line, "DATA FORMAT");
    if(line == "LBASTD")
      datastreamtable[i].format = LBASTD;
    else if(line == "LBAVSOP")
      datastreamtable[i].format = LBAVSOP;
    else if(line == "MKV")
    {
      datastreamtable[i].format = MKV;
      getinputline(input, &line, "FANOUT");
      datastreamtable[i].fanout = atoi(line.c_str());
    }
    else if(line == "NZ")
      datastreamtable[i].format = NZ;
    else if(line == "K5")
      datastreamtable[i].format = K5;
    else
    {
      cerr << "Unnkown data format " << line << " (case sensitive choices are LBASTD, LBAVSOP, MKIV, NZ and K5) - assuming LBASTD!!!" << endl;
      datastreamtable[i].format = LBASTD;
    }
    getinputline(input, &line, "QUANTISATION BITS");
    datastreamtable[i].numbits = atoi(line.c_str());
    getinputline(input, &line, "FILTERBANK USED");
    datastreamtable[i].filterbank = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    if(datastreamtable[i].filterbank)
      cerr << "Error - filterbank not yet supported!!!" << endl;
    getinputline(input, &line, "READ FROM FILE");
    datastreamtable[i].readfromfile = ((line == "TRUE") || (line == "T") || (line == "true") || (line == "t"))?true:false;
    getinputline(input, &line, "NUM FREQS");
    datastreamtable[i].numfreqs = atoi(line.c_str());
    datastreamtable[i].freqpols = new int[datastreamtable[i].numfreqs];
    datastreamtable[i].freqtableindices = new int[datastreamtable[i].numfreqs];
    datastreamtable[i].freqclockoffsets = new double[datastreamtable[i].numfreqs];
    datastreamtable[i].numinputbands = 0;
    for(int j=0;j<datastreamtable[i].numfreqs;j++)
    {
      getinputline(input, &line, "FREQ TABLE INDEX ", j);
      datastreamtable[i].freqtableindices[j] = atoi(line.c_str());
      getinputline(input, &line, "CLK OFFSET ", j);
      datastreamtable[i].freqclockoffsets[j] = atof(line.c_str());
      getinputline(input, &line, "NUM POLS ", j);
      datastreamtable[i].freqpols[j] = atoi(line.c_str());
      datastreamtable[i].numinputbands += datastreamtable[i].freqpols[j];
    }
    datastreamtable[i].bytespersamplenum = (datastreamtable[i].numinputbands*datastreamtable[i].numbits)/8;
    if(datastreamtable[i].bytespersamplenum == 0)
    {
      datastreamtable[i].bytespersamplenum = 1;
      datastreamtable[i].bytespersampledenom = 8/(datastreamtable[i].numinputbands*datastreamtable[i].numbits);
    }
    else
      datastreamtable[i].bytespersampledenom = 1;
    if(!datastreamtable[i].filterbank)
      datastreamtable[i].numoutputbands = datastreamtable[i].numinputbands;
    datastreamtable[i].inputbandpols = new char[datastreamtable[i].numinputbands];
    datastreamtable[i].inputbandlocalfreqindices = new int[datastreamtable[i].numinputbands];
    for(int j=0;j<datastreamtable[i].numinputbands;j++)
    {
      getinputline(input, &line, "INPUT BAND ", j);
      datastreamtable[i].inputbandpols[j] = *(line.data());
      getinputline(input, &line, "INPUT BAND ", j);
      datastreamtable[i].inputbandlocalfreqindices[j] = atoi(line.c_str());
      if(datastreamtable[i].inputbandlocalfreqindices[j] >= datastreamtable[i].numfreqs)
        cerr << "Error - attempting to refer to freq outside local table!!!" << endl;
    }
    if(datastreamtable[i].format == MKV)
    {
      //put in a default worstcase for framesize
      datastreamtable[i].framebytes = (datastreamtable[i].fanout*datastreamtable[i].numbits*datastreamtable[i].numinputbands*FRAMESIZE)/8;
    }
  }

  for(int i=0;i<numconfigs;i++)
  {
    if(configs[i].pulsarbin)
    {
      //process the pulsar config file
      processPulsarConfig(configs[i].pulsarconfigfilename, i);
      setPolycoFreqInfo(i);
    }
  }
  
  //read in the core numthreads info
  ifstream coreinput(coreconffilename.c_str());
  numcoreconfs = 0;
  if(!coreinput.is_open() || coreinput.bad())
  {
    cerr << "Error - could not open " << coreconffilename << " - will set all numthreads to 1!!" << endl;
  }
  else
  {
    getinputline(&coreinput, &line, "NUMBER OF CORES");
    int maxlines = atoi(line.c_str());
    numprocessthreads = new int[maxlines];
    getline(coreinput, line);
    for(int i=0;i<maxlines;i++)
    {
      if(coreinput.eof())
      {
        cerr << "Warning - hit the end of the file! Setting the numthread for Core " << i << " to 1" << endl;
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

void Configuration::processFreqTable(ifstream * input)
{
  string line;

  getinputline(input, &line, "FREQ ENTRIES");
  freqtablelength = atoi(line.c_str());
  freqtable = new freqdata[freqtablelength];

  for(int i=0;i<freqtablelength;i++)
  {
    getinputline(input, &line, "FREQ (MHZ) ", i);
    freqtable[i].bandedgefreq = atof(line.c_str());
    getinputline(input, &line, "BW (MHZ) ", i);
    freqtable[i].bandwidth = atof(line.c_str());
    getinputline(input, &line, "SIDEBAND ", i);
    freqtable[i].lowersideband = ((line == "L") || (line == "l") || (line == "LOWER") || (line == "lower"))?true:false;
  }
}

void Configuration::processTelescopeTable(ifstream * input)
{
  string line;

  getinputline(input, &line, "TELESCOPE ENTRIES");
  telescopetablelength = atoi(line.c_str());
  telescopetable = new telescopedata[telescopetablelength];

  for(int i=0;i<telescopetablelength;i++)
  {
    getinputline(input, &(telescopetable[i].name), "TELESCOPE NAME ", i);
    getinputline(input, &line, "CLOCK DELAY (us) ", i);
    telescopetable[i].clockdelay = atof(line.c_str());
    getinputline(input, &line, "CLOCK RATE(us/s) ", i);
    telescopetable[i].clockrate = atof(line.c_str());
  }
}

void Configuration::processNetworkTable(ifstream * input)
{
  string line;

  for(int i=0;i<datastreamtablelength;i++)
  {
    getinputline(input, &line, "PORT NUM ", i);
    datastreamtable[i].portnumber = atoi(line.c_str());
    getinputline(input, &line, "TCP WINDOW (KB) ", i);
    datastreamtable[i].tcpwindowsizekb = atoi(line.c_str());
  }
}

void Configuration::consistencyCheck()
{
  int tindex, count;
  double bandwidth, sampletimens, ffttime, nsincrement;

  //check entries in the datastream table
  for(int i=0;i<datastreamtablelength;i++)
  {
    //check the telescope index is acceptable
    if(datastreamtable[i].telescopeindex < 0 || datastreamtable[i].telescopeindex >= telescopetablelength)
    {
      cerr << "Error!!! Datastream table entry " << i << " has a telescope index that refers outside the telescope table range (" << datastreamtable[i].telescopeindex << ")- aborting!!!" << endl;
      exit(1);
    }

    //check the local freq indices are all ok
    for(int j=0;j<datastreamtable[i].numinputbands;j++)
    {
      if(datastreamtable[i].inputbandlocalfreqindices[j] < 0 || datastreamtable[i].inputbandlocalfreqindices[j] >= datastreamtable[i].numfreqs)
      {
        cerr << "Error!!! Datastream table entry " << i << " has an input band local frequency index (band " << j << ") that refers outside the local frequency table range (" << datastreamtable[i].inputbandlocalfreqindices[j] << ")- aborting!!!" << endl;
        exit(1);
      }
    }

    //check the frequency table indices are ok and all the bandwidths all match
    bandwidth = freqtable[datastreamtable[i].freqtableindices[0]].bandwidth;
    for(int j=0;j<datastreamtable[i].numfreqs;j++)
    {
      if(datastreamtable[i].freqtableindices[j] < 0 || datastreamtable[i].freqtableindices[j] >= freqtablelength)
      {
        cerr << "Error!!! Datastream table entry " << i << " has a frequency index (freq " << j << ") that refers outside the frequency table range (" << datastreamtable[i].freqtableindices[j] << ")- aborting!!!" << endl;
        exit(1);
      }
      if(bandwidth != freqtable[datastreamtable[i].freqtableindices[j]].bandwidth)
      {
        cerr << "Error - all bandwidths for a given datastream must be equal - Aborting!!!!" << endl;
        exit(1);
      }
    }
  }

  //check that for all configs, the datastreams refer to the same telescope
  for(int i=0;i<numdatastreams;i++)
  {
    tindex = datastreamtable[configs[0].datastreamindices[i]].telescopeindex;
    for(int j=1;j<numconfigs;j++)
    {
      if(tindex != datastreamtable[configs[0].datastreamindices[i]].telescopeindex)
      {
        cerr << "Error - all configs must have the same telescopes!  Config " << j << " datastream " << i << " refers to different telescopes - aborting!!!" << endl;
        exit(1);
      }
    }
  }

  //check entries in the config table, check that number of channels * sample time yields a whole number of nanoseconds and that the nanosecond increment is not too large for an int, and generate the ordered datastream indices array
  for(int i=0;i<numconfigs;i++)
  {
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
      cerr << "Error - not all datastreams accounted for in the datastream table for config " << i << endl;
      exit(1);
    }

    //check that number of channels * sample time yields a whole number of nanoseconds for every datastream
    for(int j=0;j<numdatastreams;j++)
    {
      sampletimens = 1000.0/freqtable[datastreamtable[configs[i].datastreamindices[j]].freqtableindices[0]].bandwidth;
      ffttime = sampletimens*configs[i].numchannels*2;
      nsincrement = ffttime*configs[i].blockspersend*(databufferfactor/numdatasegments);
      if(ffttime - (int)(ffttime+0.5) > 0.00000001 || ffttime - (int)(ffttime+0.5) < -0.000000001)
      {
        cerr << "Error - FFT chunk time for config " << i << ", datastream " << j << " is not a whole number of nanoseconds (" << ffttime << ") - aborting!!!" << endl;
        exit(1);
      }
      if(nsincrement > ((1 << (sizeof(int)*8 - 1)) - 1))
      {
        cerr << "Error - increment per read in nanoseconds is " << nsincrement << " - too large to fit in an int.  ABORTING" << endl;
        exit(1);
      }
    }

    //check that all baseline indices refer inside the table, and go in ascending order
    int b, lastt1 = 0, lastt2 = 0;
    for(int j=0;j<numbaselines;j++)
    {
      b = configs[i].baselineindices[j];
      if(b < 0 || b >= baselinetablelength) //bad index
      {
        cerr << "Error - config " << i << " baseline index " << j << " refers to baseline " << b << " which is outside the range of the baseline table - aborting!!!" << endl;
        exit(1);
      }
      if(datastreamtable[baselinetable[b].datastream2index].telescopeindex < lastt2 && datastreamtable[baselinetable[b].datastream1index].telescopeindex <= lastt1)
      {
        cerr << "Error - config " << i << " baseline index " << j << " refers to baseline " << datastreamtable[baselinetable[b].datastream2index].telescopeindex << "-" << datastreamtable[baselinetable[b].datastream1index].telescopeindex << " which is out of order with the previous baseline " << lastt1 << "-" << lastt2 << " - aborting!!!" << endl;
        exit(1);
      }
      lastt1 = datastreamtable[baselinetable[b].datastream1index].telescopeindex;
      lastt2 = datastreamtable[baselinetable[b].datastream2index].telescopeindex;
    }
  }

  //check the baseline table entries
  for(int i=0;i<baselinetablelength;i++)
  {
    //check the datastream indices
    if(baselinetable[i].datastream1index < 0 || baselinetable[i].datastream2index < 0 || baselinetable[i].datastream1index >= datastreamtablelength || baselinetable[i].datastream2index >= datastreamtablelength)
    {
      cerr << "Error - baseline table entry " << i << " has a datastream index outside the datastream table range! Its two indices are " << baselinetable[i].datastream1index << ", " << baselinetable[i].datastream2index << ".  ABORTING" << endl;
      exit(1);
    }

    //check the band indices
    for(int j=0;j<baselinetable[i].numfreqs;j++)
    {
      for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
      {
        if(baselinetable[i].datastream1bandindex[j][k] < 0 || baselinetable[i].datastream1bandindex[j][k] >= datastreamtable[baselinetable[i].datastream1index].numinputbands)
        {
          cerr << "Error! Baseline table entry " << i << ", frequency " << j << ", polarisation product " << k << " for datastream 1 refers to a band outside datastream 1's range (" << baselinetable[i].datastream1bandindex[j][k] << ") - aborting!!!" << endl;
          exit(1);
        }
        if(baselinetable[i].datastream2bandindex[j][k] < 0 || baselinetable[i].datastream2bandindex[j][k] >= datastreamtable[baselinetable[i].datastream2index].numinputbands)
        {
          cerr << "Error! Baseline table entry " << i << ", frequency " << j << ", polarisation product " << k << " for datastream 2 refers to a band outside datastream 2's range (" << baselinetable[i].datastream2bandindex[j][k] << ") - aborting!!!" << endl;
          exit(1);
        }
      }
    }
  }

  if(databufferfactor % numdatasegments != 0)
  {
    cerr << "Error - there must be an integer number of sends per datasegment.  Presently databufferfactor is " << databufferfactor << ", and numdatasegments is " << numdatasegments << ".  ABORTING" << endl;
    exit(1);
  }
}

void Configuration::processPulsarConfig(string filename, int configindex)
{
  string line;
  string * polycofilenames;
  double * binphaseends;
  double * binweights;
  cout << "About to process pulsar file " << filename << endl;
  ifstream pulsarinput(filename.c_str(), ios::in);
  if(!pulsarinput.is_open() || pulsarinput.bad())
  {
    cerr << "Error - could not open pulsar config file " << line << " - aborting!!!" << endl;
    exit(1);
  }
  getinputline(&pulsarinput, &line, "NUM POLYCO FILES");
  configs[configindex].numpolycos = atoi(line.c_str());
  polycofilenames = new string[configs[configindex].numpolycos];
  for(int i=0;i<configs[configindex].numpolycos;i++)
  {
    getinputline(&pulsarinput, &(polycofilenames[i]), "POLYCO FILE");
  }
  getinputline(&pulsarinput, &line, "NUM PULSAR BINS");
  configs[configindex].numbins = atoi(line.c_str());
  if(configs[configindex].numbins > maxnumpulsarbins)
    maxnumpulsarbins = configs[configindex].numbins;
  binphaseends = new double[configs[configindex].numbins];
  binweights = new double[configs[configindex].numbins];
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
  configs[configindex].polycos = new Polyco*[configs[configindex].numpolycos];
  for(int i=0;i<configs[configindex].numpolycos;i++)
  {
    cout << "About to create polyco file " << i << " with filename " << polycofilenames[i] << endl;
    configs[configindex].polycos[i] = new Polyco(polycofilenames[i], configindex, configs[configindex].numbins, configs[configindex].numchannels, binphaseends, binweights, double(2*configs[configindex].numchannels*configs[configindex].blockspersend)/(60.0*2000000.0*getDBandwidth(configindex,0,0)));
  }
  
  delete [] binphaseends;
  delete [] binweights;
  delete [] polycofilenames;
  pulsarinput.close();
}

void Configuration::setPolycoFreqInfo(int configindex)
{
  /*datastreamdata d = datastreamtable[getMaxNumFreqDatastreamIndex(configindex)];
  double * frequencies = new double[d.numfreqs];
  double bandwidth = freqtable[d.freqtableindices[0]].bandwidth;
  for(int i=0;i<d.numfreqs;i++)
  {
    frequencies[i] = freqtable[d.freqtableindices[i]].bandedgefreq;
    if(freqtable[d.freqtableindices[i]].lowersideband)
      frequencies[i] -= bandwidth;
  }
  for(int i=0;i<configs[configindex].numpolycos;i++)
  {
    configs[configindex].polycos[i]->setFrequencyValues(d.numfreqs, frequencies, bandwidth);
  }
  delete [] frequencies;*/
  datastreamdata d = datastreamtable[getMaxNumFreqDatastreamIndex(configindex)];
  double * frequencies = new double[datastreamtablelength];
  double bandwidth = freqtable[d.freqtableindices[0]].bandwidth;
  for(int i=0;i<datastreamtablelength;i++)
  {
    frequencies[i] = freqtable[i].bandedgefreq;
    if(freqtable[i].lowersideband)
      frequencies[i] -= freqtable[i].bandwidth;
  }
  for(int i=0;i<configs[configindex].numpolycos;i++)
  {
    configs[configindex].polycos[i]->setFrequencyValues(freqtablelength, frequencies, bandwidth);
  }
  delete [] frequencies;
}

void Configuration::makeFortranString(string line, int length, char * destination)
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

void Configuration::getinputline(ifstream * input, std::string * line, std::string startofheader)
{
  if(input->eof())
    cerr << "Error - trying to read past the end of file!!!" << endl;
  input->get(header, HEADER_LENGTH);
  if(strncmp(header, startofheader.c_str(), startofheader.length()) != 0) //not what we expected
    cerr << "Error - we thought we were reading something starting with '" << startofheader << "', when we actually got '" << header << "'" << endl;
  getline(*input,*line);
}

void Configuration::getinputline(ifstream * input, std::string * line, std::string startofheader, int intval)
{
  char buffer[HEADER_LENGTH+1];
  sprintf(buffer, "%s%i", startofheader.c_str(), intval);
  getinputline(input, line, string(buffer));
}

void Configuration::getMJD(int & d, int & s, int year, int month, int day, int hour, int minute, int second)
{
  d = year*367 - int(7*(year + int((month + 9)/12))/4) + int(275*month/9) + day - 678987;

  s = 3600*hour + 60*minute + second;
}

void Configuration::mjd2ymd(int mjd, int & year, int & month, int & day)
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

