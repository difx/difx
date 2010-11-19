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
#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <fstream>
#include <cstdlib>
#include <iostream>
#include <math.h>
#include "polyco.h"
#include "model.h"
#include "mark5access.h"
#include "mpifxcorr.h"

//forward declaration of class Mode
class Mode;

using namespace std;

/**
@class Configuration 
@brief Stores all information provided in the input file that controls the correlation

This class stores all the information which was provided to direct the correlation via the input file.  It allows easy access
to this configuration information to the other objects which actually perform the correlation

@author Adam Deller
*/
class Configuration{
public:
  /// Enumeration for the format of the output than can be produced
  enum outputformat {ASCII, DIFX, VDIFOUT};

  ///Enumeration for the type of phased array output
  enum datadomain {TIME, FREQUENCY};

  /// Supported types of recorded data format
  enum dataformat {LBASTD, LBAVSOP, LBA8BIT, LBA16BIT, K5, MKIV, VLBA, MARK5B, VDIF, VLBN};

  /// Supported sources of data
  enum datasource {UNIXFILE, MK5MODULE, NETWORKSTREAM};

  /// Supported types of recorded data sampling types
  enum datasampling {REAL, COMPLEX};

  /// Constant for the TCP window size for monitoring
  static int MONITOR_TCP_WINDOWBYTES;

 /**
  * Constructor: Reads and stores the information in the input file
  * @param configfile The filename of the input file containing configuration information to be read
  */
  Configuration(const char * configfile, int id);

  ~Configuration();

/** @name Access methods to the data structures held internally
  * These methods simply allow other objects access to the configuration information held in tables in the input file for the correlation
  */
//@{
  inline int getMPIId() { return mpiid; }
  inline string getJobName() { return jobname; }
  inline int getEstimatedBytes() { return estimatedbytes; }
  inline int getVisBufferLength() { return visbufferlength; }
  inline bool consistencyOK() {return consistencyok; }
  inline bool anyUsbXLsb(int configindex) { return configs[configindex].anyusbxlsb; }
  inline bool phasedArrayOn(int configindex) { return configs[configindex].phasedarray; }
  inline int getArrayStrideLength(int configindex) { return configs[configindex].arraystridelen; }
  inline int getXmacStrideLength(int configindex) { return configs[configindex].xmacstridelen; }
  inline int getNumBufferedFFTs(int configindex) { return configs[configindex].numbufferedffts; }
  inline int getThreadResultLength(int configindex) { return configs[configindex].threadresultlength; }
  inline int getCoreResultLength(int configindex) { return configs[configindex].coreresultlength; }
  inline int getMaxThreadResultLength() { return maxthreadresultlength; }
  inline int getMaxCoreResultLength() { return maxcoreresultlength; }
  inline int getMaxNumBufferedFFTs() { return maxnumbufferedffts; }
  inline int getNumXmacStrides(int configindex, int freqindex) { return configs[configindex].numxmacstrides[freqindex]; }
  inline int getCompleteStrideLength(int configindex, int freqindex) { return configs[configindex].completestridelength[freqindex]; }
  inline int getThreadResultFreqOffset(int configindex, int freqindex) { return configs[configindex].threadresultfreqoffset[freqindex]; }
  inline int getThreadResultBaselineOffset(int configindex, int freqindex, int configbaselineindex) { return configs[configindex].threadresultbaselineoffset[freqindex][configbaselineindex]; }
  inline int getCoreResultBaselineOffset(int configindex, int freqindex, int configbaselineindex) { return configs[configindex].coreresultbaselineoffset[freqindex][configbaselineindex]; }
  inline int getCoreResultBWeightOffset(int configindex, int freqindex, int configbaselineindex) { return configs[configindex].coreresultbweightoffset[freqindex][configbaselineindex]; }
  inline int getCoreResultBShiftDecorrOffset(int configindex, int freqindex, int configbaselineindex) { return configs[configindex].coreresultbshiftdecorroffset[freqindex][configbaselineindex]; }
  inline int getCoreResultAutocorrOffset(int configindex, int configdatastreamindex) { return configs[configindex].coreresultautocorroffset[configdatastreamindex]; }
  inline int getCoreResultACWeightOffset(int configindex, int configdatastreamindex) { return configs[configindex].coreresultacweightoffset[configdatastreamindex]; }
  inline int getCoreResultPCalOffset(int configindex, int configdatastreamindex) { return configs[configindex].coreresultpcaloffset[configdatastreamindex]; }
  inline int getNumConfigs() { return numconfigs; }
  inline int getBlocksPerSend(int configindex) { return configs[configindex].blockspersend; }
  inline double getIntTime(int configindex) { return configs[configindex].inttime; }
  inline bool writeAutoCorrs(int configindex) { return configs[configindex].writeautocorrs; }
  inline int getMinPostAvFreqChannels(int configindex) { return configs[configindex].minpostavfreqchannels; }
  inline outputformat getOutputFormat() { return outformat; }
  inline string getOutputFilename() { return outputfilename; }
  inline bool pulsarBinOn(int configindex) { return configs[configindex].pulsarbin; }
  inline bool scrunchOutputOn(int configindex) { return configs[configindex].scrunchoutput; }
  inline int getNumPulsarBins(int configindex) { return configs[configindex].numbins; }
  inline int getNumPolycos(int configindex) { return configs[configindex].numpolycos; }
  inline Polyco ** getPolycos(int configindex) { return configs[configindex].polycos; }
  inline bool matchingRecordedBand(int configindex, int configdatastreamindex, int datastreamfreqindex, int datastreamrecordedbandindex)
    { return datastreamfreqindex == datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedbandlocalfreqindices[datastreamrecordedbandindex]; }
  inline int getDDataBufferFactor() { return databufferfactor; }
  inline int getDNumDataSegments() { return numdatasegments; }
  inline int getDTelescopeIndex(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex; }
  inline int getDModelFileIndex(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].modelfileindex; }
  inline int getDNumRecordedFreqs(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numrecordedfreqs; }
  inline int getDNumZoomFreqs(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numzoomfreqs; }
  inline double getDClockCoeff(int configindex, int configdatastreamindex, int coeff)
    { return telescopetable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex].clockpoly[coeff]; }
  inline int getDOversampleFactor(int configindex, int configdatastreamindex) 
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqtableindices[0]].oversamplefactor; }
  inline int getDChannelsToAverage(int configindex, int configdatastreamindex) { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqtableindices[0]].channelstoaverage; }
  inline int getDDecimationFactor(int configindex, int configdatastreamindex) 
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqtableindices[0]].decimationfactor; }
  inline string getDStationName(int configindex, int configdatastreamindex) 
    { return telescopetable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex].name; }
  inline double getDTsys(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].tsys; }
  inline int getDPhaseCalIntervalMHz(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].phasecalintervalmhz; }
  inline int getDMaxRecordedPCalTones(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].maxrecordedpcaltones; }
  inline int getDNumBits(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numbits; }
  inline datasampling getDSampling(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].sampling; }
  inline int getDRecordedFreqIndex(int configindex, int configdatastreamindex, int datastreamrecordedbandindex)
    { datastreamdata ds = datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]]; return ds.recordedfreqtableindices[ds.recordedbandlocalfreqindices[datastreamrecordedbandindex]]; }
  inline int getDZoomFreqIndex(int configindex, int configdatastreamindex, int datastreamzoombandindex)
    { datastreamdata ds = datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]];   return ds.zoomfreqtableindices[ds.zoombandlocalfreqindices[datastreamzoombandindex]]; }
  inline int getDTotalFreqIndex(int configindex, int configdatastreamindex, int datastreamtotalbandindex)
    { datastreamdata ds = datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]];
    if (datastreamtotalbandindex < ds.numrecordedbands)
      return ds.recordedfreqtableindices[ds.recordedbandlocalfreqindices[datastreamtotalbandindex]];
    else
      return ds.zoomfreqtableindices[ds.zoombandlocalfreqindices[datastreamtotalbandindex-ds.numrecordedbands]];
    }
  inline int getDLocalRecordedFreqIndex(int configindex, int configdatastreamindex, int datastreamrecordedbandindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedbandlocalfreqindices[datastreamrecordedbandindex]; }
  inline int getDLocalZoomFreqIndex(int configindex, int configdatastreamindex, int datastreamzoombandindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoombandlocalfreqindices[datastreamzoombandindex]; }
  inline char getDRecordedBandPol(int configindex, int configdatastreamindex, int datastreamrecordedbandindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedbandpols[datastreamrecordedbandindex]; }
  inline char getDZoomBandPol(int configindex, int configdatastreamindex, int datastreamzoombandindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoombandpols[datastreamzoombandindex]; }
  inline int getDBytesPerSampleNum(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].bytespersamplenum; }
  inline int getDBytesPerSampleDenom(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].bytespersampledenom; }
  inline int getDNumFiles(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numdatafiles; }
  inline int getDPortNumber(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].portnumber; }
  inline int getDTCPWindowSizeKB(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].tcpwindowsizekb; }
  inline int getDNumRecordedBands(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numrecordedbands; }
  inline int getDNumZoomBands(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numzoombands; }
  inline int getDNumTotalBands(int configindex, int configdatastreamindex)
    {  return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numzoombands + datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numrecordedbands; }
  inline string * getDDataFileNames(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].datafilenames; }
  inline int getDRecordedFreqNumPCalTones(int configindex, int configdatastreamindex, int recordedfreqindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numrecordedfreqpcaltones[recordedfreqindex]; }
  inline int getDRecordedFreqPCalToneFreq(int configindex, int configdatastreamindex, int recordedfreqindex, int tone)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqpcaltonefreqs[recordedfreqindex][tone]; }
  inline double getDRecordedFreqPCalOffsetsHz(int configindex, int configdatastreamindex, int recordedfreqindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqpcaloffsetshz[recordedfreqindex]; }
  inline double getDRecordedFreq(int configindex, int configdatastreamindex, int datastreamrecordedfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqtableindices[datastreamrecordedfreqindex]].bandedgefreq; }
  inline double getDZoomFreq(int configindex, int configdatastreamindex, int datastreamzoomfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoomfreqtableindices[datastreamzoomfreqindex]].bandedgefreq; }
  inline double getDRecordedBandwidth(int configindex, int configdatastreamindex, int datastreamrecordedfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqtableindices[datastreamrecordedfreqindex]].bandwidth; }
  inline double getDZoomBandwidth(int configindex, int configdatastreamindex, int datastreamzoomfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoomfreqtableindices[datastreamzoomfreqindex]].bandwidth; }
  inline bool getDRecordedLowerSideband(int configindex, int configdatastreamindex, int datastreamrecordedfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].recordedfreqtableindices[datastreamrecordedfreqindex]].lowersideband; }
  inline bool getDZoomLowerSideband(int configindex, int configdatastreamindex, int datastreamzoomfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoomfreqtableindices[datastreamzoomfreqindex]].lowersideband; }
  inline int getDZoomFreqChannelOffset(int configindex, int configdatastreamindex, int datastreamzoomfreqindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoomfreqchanneloffset[datastreamzoomfreqindex]; }
  inline int getDZoomFreqParentFreqIndex(int configindex, int configdatastreamindex, int datastreamzoomfreqindex)
    {  return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].zoomfreqparentdfreqindices[datastreamzoomfreqindex]; }
  inline int getBDataStream1Index(int configindex, int configbaselineindex)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1index; }
  inline int getBDataStream2Index(int configindex, int configbaselineindex)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2index; }
  inline int getBOrderedDataStream1Index(int configindex, int configbaselineindex)
    { return configs[configindex].ordereddatastreamindices[baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1index]; }
  inline int getBOrderedDataStream2Index(int configindex, int configbaselineindex)
    { return configs[configindex].ordereddatastreamindices[baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2index]; }
  inline int getBNumFreqs(int configindex, int configbaselineindex)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].numfreqs; }
  inline int getBFreqIndex(int configindex, int configbaselineindex, int baselinefreqindex)
  {  return baselinetable[configs[configindex].baselineindices[configbaselineindex]].freqtableindices[baselinefreqindex];
    }
  inline int getBLocalFreqIndex(int configindex, int configbaselineindex, int freqtableindex) { return baselinetable[configs[configindex].baselineindices[configbaselineindex]].localfreqindices[freqtableindex]; }
  inline int getBFreqOddLSB(int configindex, int configbaselineindex, int freqtableindex) { return baselinetable[configs[configindex].baselineindices[configbaselineindex]].oddlsbfreqs[freqtableindex]; }
  inline int getBNumPolProducts(int configindex, int configbaselineindex, int baselinefreqindex)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].numpolproducts[baselinefreqindex]; }
  inline int getBDataStream1BandIndex(int configindex, int configbaselineindex, int baselinefreqindex, int baselinefreqdatastream1index)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1bandindex[baselinefreqindex][baselinefreqdatastream1index]; }
  inline int getBDataStream2BandIndex(int configindex, int configbaselineindex, int baselinefreqindex, int baselinefreqdatastream2index)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2bandindex[baselinefreqindex][baselinefreqdatastream2index]; }
  inline void getBPolPair(int configindex, int configbaselineindex, int baselinefreqindex, int freqpolindex, char polpair[3])
    { char * tpp = baselinetable[configs[configindex].baselineindices[configbaselineindex]].polpairs[baselinefreqindex][freqpolindex]; polpair[0] = tpp[0]; polpair[1] = tpp[1];
    }
  inline char getOppositePol(char pol) 
    {
      if (pol == 'R') return 'L';
      if (pol == 'L') return 'R';
      if (pol == 'X') return 'Y';
      if (pol == 'Y') return 'X';
      return 'X';
    }
  inline int getBNumber(int configindex, int configbaselineindex)
    { return (datastreamtable[baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1index].telescopeindex + 1)*256 + (datastreamtable[baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2index].telescopeindex + 1); }
  inline int getNumDataStreams() { return numdatastreams; }
  inline int getNumBaselines() { return numbaselines; }
  inline int getMaxNumChannels() { return maxnumchannels; }
  inline int getMaxNumPulsarBins() { return maxnumpulsarbins; }
  inline int getExecuteSeconds() { return executeseconds; }
  inline int getStartMJD() { return startmjd; }
  inline int getStartSeconds() { return startseconds; }
  inline int getStartNS() { return startns; }
  inline int getSubintNS(int configindex) { return configs[configindex].subintns; }
  inline int getGuardNS(int configindex) { return configs[configindex].guardns; }
  inline int getFreqTableLength() { return freqtablelength; }
  inline double getFreqTableFreq(int index) { return freqtable[index].bandedgefreq; }
  inline double getFreqTableBandwidth(int index) { return freqtable[index].bandwidth; }
  inline bool getFreqTableLowerSideband(int index) { return freqtable[index].lowersideband; }
  inline int getFNumChannels(int index) { return freqtable[index].numchannels; }
  inline int getFChannelsToAverage(int index) { return freqtable[index].channelstoaverage; }
  inline int getFMatchingWiderBandIndex(int index) { return freqtable[index].matchingwiderbandindex; }
  inline int getFMatchingWiderBandOffset(int index) { return freqtable[index].matchingwiderbandoffset; }
  inline bool isFrequencyUsed(int configindex, int freqindex) 
    { return configs[configindex].frequsedbybaseline[freqindex]; }
  inline bool circularPolarisations() 
    { return datastreamtable[0].recordedbandpols[0] == 'R' || datastreamtable[0].recordedbandpols[0] == 'L'; }
  inline bool isReadFromFile(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].source != NETWORKSTREAM; }
  inline bool isMkV(int datastreamindex) 
  {
    dataformat f;
    datasource s;
    f = datastreamtable[configs[0].datastreamindices[datastreamindex]].format;
    s = datastreamtable[configs[0].datastreamindices[datastreamindex]].source;
    return ((f == MKIV || f == VLBA || f == VLBN || f == MARK5B || f == VDIF) && (s == UNIXFILE || s == NETWORKSTREAM)); 
  }
  inline bool isNativeMkV(int datastreamindex) 
  { 
    dataformat f;
    datasource s;
    f = datastreamtable[configs[0].datastreamindices[datastreamindex]].format;
    s = datastreamtable[configs[0].datastreamindices[datastreamindex]].source;
    return ((f == MKIV || f == VLBA || f == VLBN || f == MARK5B) && s == MK5MODULE); 
  }
  inline int getFrameBytes(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].framebytes; }
  inline dataformat getDataFormat(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].format; }
  inline datasource getDataSource(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].source; }
  inline string getTelescopeName(int telescopeindex)
    { return telescopetable[telescopeindex].name; }
  inline int getTelescopeTableLength()
    { return telescopetablelength; }
  inline int getDatastreamTableLength() { return datastreamtablelength; }
  inline bool isCoreProcess() { return mpiid >= fxcorr::FIRSTTELESCOPEID + numdatastreams; }
  inline bool isDatastreamProcess() { return mpiid >= fxcorr::FIRSTTELESCOPEID && mpiid < fxcorr::FIRSTTELESCOPEID + numdatastreams; }
  inline void setCommandThreadInitialised() { commandthreadinitialised = true; }
  inline bool commandThreadInitialised() { return commandthreadinitialised; }
  inline void setDumpSTAState(bool setval) { dumpsta = setval; }
  inline void setDumpLTAState(bool setval) { dumplta = setval; }
  inline void setDumpKurtosisState(bool setval) { dumpkurtosis = setval; }
  inline bool dumpSTA() { return dumpsta; }
  inline bool dumpLTA() { return dumplta; }
  inline bool dumpKurtosis() { return dumpkurtosis; }
  inline void setSTADumpChannels(int setval) { stadumpchannels = setval; } //shared with kurtosis
  inline void setLTADumpChannels(int setval) { ltadumpchannels = setval; }
  inline int getSTADumpChannels() { return stadumpchannels; } //shared with kurtosis
  inline int getLTADumpChannels() { return ltadumpchannels; }
  inline double getFPhasedArrayDWeight(int configindex, int freqindex, int ordereddsindex)
    { return configs[configindex].paweights[freqindex][ordereddsindex]; }
  inline int getFPhasedArrayNumPols(int configindex, int freqindex)
    { return configs[configindex].numpafreqpols[freqindex]; }
  inline char getFPhaseArrayPol(int configindex, int freqindex, int polindex)
    { return configs[configindex].papols[freqindex][polindex]; }

//@}

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @param &sec The output value, seconds portion, returned by reference
  * @param &ns The output value, nanosec portion, returned by reference
  */
  void getFrameInc(int configindex, int configdatastreamindex, int &sec, int &ns);

 /**
  * @return The number of data frames per second, always an integer for Mark5 formats
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  */
  int getFramesPerSecond(int configindex, int configdatastreamindex);

 /**
  * @return The number of payload bytes in a data frame
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  */
  int getFramePayloadBytes(int configindex, int configdatastreamindex);

 /**
  * @return The fanout, or -1 if an error occurred
  * @param format The type of mark5 format
  * @param nchan The number of channels
  * @param bw The bandwidth (MHz)
  * @param nbits The number of bits per sample
  * @param framebytes The number of bytes in a frame
  * @param decimationfactor The number of samples to throw away during unpacking
  * @param formatname character array representing format name (set during method)
  */
  int genMk5FormatName(dataformat format, int nchan, double bw, int nbits, datasampling sampling, int framebytes, int decimationfactor, char *formatname);

 /**
  * @return The Model object which contains geometric model information
  */
  inline Model * getModel() { return model; }

 /**
  * Creates and returns the appropriate mode object
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @return A new Mode object for that datastream, in that configuration
  */
  Mode * getMode(int configindex, int datastreamindex);

 /**
  * @param scan The scan index
  * @return The index of the configuration in use for that scan
  */
  inline int getScanConfigIndex(int scan) { return scanconfigindices[scan]; }

 /**
  * @param freqindex The index of the frequency to find the opposite sideband for
  * @return The frequency table index of the freq which matches freqindex but is opposite sideband (-1 if not found)
  */
  int getOppositeSidebandFreqIndex(int freqindex);

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @return The maximum number of frequencies being used by any datastream during this configuration
  */
  int getMaxNumRecordedFreqs(int configindex);

 /**
  * @return The maximum number of frequencies being used by any datastream during any configuration
  */
  int getMaxNumRecordedFreqs();

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @return The index of the Datastream that uses the maximum number of frequencies during this configuration
  */
  int getMaxNumFreqDatastreamIndex(int configindex);

 /**
  * @return The maximum length of a data message from any telescope, for any configuration
  */
  int getMaxDataBytes();

 /**
  * @param configindex The index of the relevant entry in the configuration table
  * @return The maximum number of phase centres in any scan for this config
  */
  int getMaxPhaseCentres(int configindex);

 /**
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @return The maximum length of a data message from the specified datastream, for any configuration
  */
  int getMaxDataBytes(int datastreamindex);

 /**
  * @return The maximum number of whole blocks sent for any configuration (excluding guard time)
  */
  int getMaxBlocksPerSend();

 /**
  * @return The maximum number of products (1, 2 or 4) for any baseline in any configuration
  */
  int getMaxProducts();

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @return The maximum number of products (1, 2 or 4) for any baseline, for the specified configuration
  */
  int getMaxProducts(int configindex);

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @return The maximum length of a data message from the specified datastream and configuration
  */
  int getDataBytes(int configindex, int datastreamindex);

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @param bandindex The index of the band within this datastream
  * @return The band index for this datastream in this configuration that is the same frequency, but other polarisation, as the specified band
  */
  int getDMatchingBand(int configindex, int datastreamindex, int bandindex);

 /**
  * @param corenum The core id (indexed from 0->numcores-1)
  * @return The number of processing threads for the specified Core
  */
  int getCNumProcessThreads(int corenum);

 /**
  * @param telescopeindex The index of the telescope (from the table in the input file)
  * @return Whether there are any active datastreams (ie datastreams used by an active configuration) that belong to this telescope
  */
  bool stationUsed(int telescopeindex);

  /// Utility function used to read header info for a difx output file record
  bool fillHeaderData(ifstream * input, int & baselinenum, int & mjd, double & seconds, int & configindex, int & sourceindex, int & freqindex, char polpair[3], int & pulsarbin, double & dataweight, double uvw[3]);

 /**
  * Update clock values during an observation
  * @param clockstring antennaindex:zeroorderoffset[:1storderoffset:2ndorderoffset...]
  */
  bool updateClock(std::string clockstring);

 /**
  * Utility method which reads a line from a file, extracts a value and checks the keyword matches that expected
  * @param input Open input stream to read from
  * @param line Existing string to store value in
  * @param startofheader The start of the expected keyword, to compare to the actual keyword which will be read
  */
  void getinputline(ifstream * input, string * line, string startofheader);

 /**
  * Utility method which reads a line from a file, extracts a value and checks the keyword matches that expected
  * @param input Open input stream to read from
  * @param line Existing string to store value in
  * @param startofheader The start of the expected keyword, to compare to the actual keyword which will be read
  * @param intval An integer value which should follow startofheader
  */
  void getinputline(ifstream * input, string * line, string startofheader, int intval);

 /**
  * Utility method which reads a line from a file, splitting it into a key and a value and storing both
  * @param input Open input stream to read from
  * @param key String to store key in
  * @param val String to store value in
  */
  void getinputkeyval(ifstream * input, std::string * key, std::string * val);

 /**
  * Utility method which converts a year,month,day into mjd and hour,minute,second into seconds from start of day
  * @param d The MJD, which will be set
  * @param s The seconds from start of day, which will be set
  * @param year Year of the date to be converted
  * @param month Month of the date to be converted
  * @param day Day of the date to be converted
  * @param hour Hour of the time to be converted
  * @param minute Minute of the time to be converted
  * @param second Second of the time to be converted
  */
  void getMJD(int & d, int & s, int year, int month, int day, int hour, int minute, int second);

 /**
  * Utility method which converts an mjd into year,month,day
  * @param mjd The MJD to convert
  * @param year Year, which will be set
  * @param month Month, which will be set
  * @param day Day, which will be set
  */
  void mjd2ymd(int mjd, int & year, int & month, int & day);

 /**
  * Utility method to create a Fortran style string in a character array.  The string is padded with spaces with no trailing null.
  * @param line The string to be copied
  * @param length The length of the destination character buffer
  * @param destination The buffer to store the Fortran-style string
  */
  void makeFortranString(string line, int length, char * destination);

private:
  ///types of sections that can occur within an input file
  enum sectionheader {COMMON, CONFIG, RULE, FREQ, TELESCOPE, DATASTREAM, BASELINE, DATA, NETWORK, INPUT_EOF, UNKNOWN};

  ///Storage struct for data from the frequency table of the input file
  typedef struct {
    double bandedgefreq;
    double bandwidth;
    bool lowersideband;
    bool correlatedagainstupper;
    int numchannels;
    int channelstoaverage;
    int oversamplefactor;
    int decimationfactor;
    int matchingwiderbandindex;
    int matchingwiderbandoffset;
  } freqdata;

  ///Storage struct for data from the baseline table of the input file
  typedef struct {
    int datastream1index;
    int datastream2index;
    int numfreqs;
    int totalbands;
    int * freqtableindices;
    int * oddlsbfreqs;
    int * localfreqindices; //given a freq table index, what local freq does it correspond to (-1 = none)
    int * numpolproducts;
    int ** datastream1bandindex;
    int ** datastream2bandindex;
    char *** polpairs;
  } baselinedata;

  ///Storage struct for data from the config table of the input file
  typedef struct {
    string name;
    double inttime;
    int blockspersend;
    int subintns;
    int guardns;
    int fringerotationorder;
    int arraystridelen;
    int xmacstridelen;
    int numbufferedffts;
    bool writeautocorrs;
    bool pulsarbin;
    bool phasedarray;
    int numpolycos;
    int numbins;
    int minpostavfreqchannels;
    int threadresultlength;
    int coreresultlength;
    bool scrunchoutput;
    int numphasecentres;
    bool anyusbxlsb;
    string pulsarconfigfilename;
    string phasedarrayconfigfilename;
    outputformat paoutputformat;
    int pabits;
    bool pacomplexoutput;
    int paaccumulationns;
    double ** paweights; //[freq][datastream]
    char   ** papols;    //[freq][pol]
    int * numpafreqpols; //[freq]
    datadomain padomain;
    Polyco ** polycos;
    int  * datastreamindices;
    int  * ordereddatastreamindices;
    int  * baselineindices;
    bool * frequsedbybaseline;
    //bookkeeping info for thread results
    int  * numxmacstrides;              //[freq]
    int  * completestridelength;        //[freq]
    int  * threadresultfreqoffset;      //[freq]
    int ** threadresultbaselineoffset;  //[freq][baseline]
    //bookkeeping info for core results
    int ** coreresultbaselineoffset;    //[freq][baseline]
    int ** coreresultbweightoffset;     //[freq][baseline]
    int ** coreresultbshiftdecorroffset;//[freq][baseline]
    int  * coreresultautocorroffset;    //[datastream]
    int  * coreresultacweightoffset;    //[datastream]
    int  * coreresultpcaloffset;        //[datastream]
  } configdata;

  ///storage struct for data from the rule table of the input file
  typedef struct {
    string configname;
    int configindex; //derived, into configs
    string sourcename;
    string scanId;
    string calcode;
    int qual;
    double mjdStart, mjdStop;
  } ruledata;

  ///Storage struct for data from the telescope table of the input file
  typedef struct {
    string name;
    int clockorder;
    double clockrefmjd;
    double * clockpoly;
  } telescopedata;

  ///Storage struct for data from the datastream table of the input file
  typedef struct {
    int telescopeindex;
    int modelfileindex;
    double tsys;
    dataformat format;
    datasource source;
    datasampling sampling;
    int phasecalintervalmhz;
    int numbits;
    int bytespersamplenum;
    int bytespersampledenom;
    int framesamples;
    int framebytes;
    int framens;
    bool filterbank;
    int numrecordedfreqs;
    int numzoomfreqs;
    int maxrecordedpcaltones;
    int *  recordedfreqpols;
    int *  recordedfreqtableindices;
    int *  numrecordedfreqpcaltones;
    int ** recordedfreqpcaltonefreqs; 
    int * recordedfreqpcaloffsetshz;
    double * recordedfreqclockoffsets;
    double * recordedfreqlooffsets;
    int * zoomfreqpols;
    int * zoomfreqtableindices;
    int * zoomfreqparentdfreqindices;
    int * zoomfreqchanneloffset;
    int numrecordedbands;
    int numzoombands;
    char * recordedbandpols;
    int * recordedbandlocalfreqindices;
    char * zoombandpols;
    int * zoombandlocalfreqindices;
    int numdatafiles;
    string * datafilenames;
    int portnumber;
    int tcpwindowsizekb;
  } datastreamdata;

 /**
  * Reads through the input file and locates the next section header
  * @param input Open file stream for the input file
  * @return The kind of section encountered
  */
  sectionheader getSectionHeader(ifstream * input);

 /**
  * Checks a loaded file for consistency - ensuring all frequencies for a given datastream have the same bandwidth etc
  * @return If the configuration file was parsed without problems, true, else false
  */
  bool consistencyCheck();

 /**
  * Goes through scans applying rules to figure out configs apply, and checking
  * for inconsistencies
  * @return True if the rules are all consistent, else false
  */
  bool populateScanConfigList();

 /**
  * Goes through configs working out the result length for each
  * @return True if all result lengths make sense
  */
  bool populateResultLengths();

 /**
  * Goes through Model file and maps datastreams to the telescope names
  * @return True if all datastreams used in this correlation are found
  */
  bool populateModelDatastreamMap();

 /**
  * Loads the baseline table from the file into memory
  * @param input Open file stream for the input file
  * @return Whether the baseline table was successfully parsed (failure should abort)
  */
  bool processBaselineTable(ifstream * input);

 /**
  * Loads the common settings from the file into memory
  * @param input Open file stream for the input file
  */
  void processCommon(ifstream * input);

 /**
  * Loads the config table from the file into memory
  * @param input Open file stream for the input file
  * @return Whether the config table was successfully parsed (failure should abort)
  */
  bool processConfig(ifstream * input);

 /**
  * Loads the rule table from the file into memory
  * @param input Open file stream for the input file
  * @return Whether the rule table was successfully parsed (failure should abort)
  */
  bool processRuleTable(ifstream * input);

 /**
  * Loads the datastream table from the file into memory
  * @param input Open file stream for the input file
  * @return Whether the datastream table was successfully parsed (failure should abort)
  */
  bool processDatastreamTable(ifstream * input);

 /**
  * Loads the data table from the file into memory
  * @param input Open file stream for the input file
  */
  void processDataTable(ifstream * input);

 /**
  * Loads the frequency table from the file into memory
  * @param input Open file stream for the input file
  * @return Whether the freq table was successfully parsed (failure should abort)
  */
  bool processFreqTable(ifstream * input);

 /**
  * Loads the telescope table from the file into memory
  * @param input Open file stream for the input file
  */
  void processTelescopeTable(ifstream * input);

 /**
  * Loads the network table from the file into memory
  * @param input Open file stream for the input file
  */
  void processNetworkTable(ifstream * input);

 /**
  * Loads the pulsar setup data for the specified config and creates the Polyco objects
  * @param filename The file containing pulsar configuration data to be loaded
  * @param configindex The config index in the configuration table that this pulsar setup belongs to
  * @return Whether the pulsar config was successfully parsed (failure should abort)
  */
  bool processPulsarConfig(string filename, int configindex);

 /**
  * Loads the phased array setup data for the specified config
  * @param filename The file containing phased array configuration data to be loaded
  * @param configindex The config index in the configuration table that this phased array belongs to
  * @return Whether the phased array config was successfully parsed (failure should abort)
  */
  bool processPhasedArrayConfig(string filename, int configindex);

 /**
  * Once the input file has been completely processed, provide all frequency info to the generated Polyco files
  * @param configindex The index of the configuration to be set up (from the table in the input file)
  * @return Whether the polyco frequency values were set successfully (false should abort)
  */
  bool setPolycoFreqInfo(int configindex);

  ///The length of keywords in all input files
  static const int DEFAULT_KEY_LENGTH = 20;
  static const int MAX_KEY_LENGTH = 128;

  ///The character used to denote a comment line, to be ignored
  static const char COMMENT_CHAR = '@';

  /// Constant for the default number of channels for visibilities sent to monitor (STA or LTA)
  static const int DEFAULT_MONITOR_NUMCHANNELS = 32;

  int mpiid;
  char header[MAX_KEY_LENGTH];
  bool commonread, configread, datastreamread, freqread, ruleread, baselineread;
  bool consistencyok, commandthreadinitialised, dumpsta, dumplta, dumpkurtosis;
  int visbufferlength, databufferfactor, numdatasegments;
  int numdatastreams, numbaselines, numcoreconfs;
  int executeseconds, startmjd, startseconds, startns;
  int maxnumchannels, maxnumpulsarbins, maxthreadresultlength, maxcoreresultlength, maxnumbufferedffts;
  int stadumpchannels, ltadumpchannels;
  int numconfigs, numrules, baselinetablelength, telescopetablelength, datastreamtablelength, freqtablelength, estimatedbytes;
  string calcfilename, modelfilename, coreconffilename, outputfilename, jobname;
  int * numprocessthreads;
  int * scanconfigindices;
  configdata * configs;
  ruledata * rules;
  freqdata * freqtable;
  telescopedata * telescopetable;
  baselinedata * baselinetable;
  datastreamdata * datastreamtable;
  Model * model;
  outputformat outformat;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
