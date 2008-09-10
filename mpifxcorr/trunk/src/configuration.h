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
#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <string>
#include <fstream>
#include <cstdlib>
#include <iostream>
#include "polyco.h"
#include "uvw.h"
#include "mark5access.h"

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
  /// Enumeration for the kind of output than can be produced
  enum outputformat {RPFITS, ASCII, DIFX};

  /// Supported types of recorded data format
  enum dataformat {LBASTD, LBAVSOP, NZ, K5, MKIV, VLBA, MARK5B};

  /// Supported sources of data
  enum datasource {UNIXFILE, MK5MODULE, EVLBI};

  /// Constant for the TCP window size for monitoring
  static const int MONITOR_TCP_WINDOWBYTES = 65536;

 /**
  * Constructor: Reads and stores the information in the input file
  * @param configfile The filename of the input file containing configuration information to be read
  */
  Configuration(const char * configfile);

  ~Configuration();

/** @name Access methods to the data structures held internally
  * These methods simply allow other objects access to the configuration information held in tables in the input file for the correlation
  */
//@{
  inline int getVisBufferLength() { return visbufferlength; }
  inline bool consistencyOK() {return consistencyok; }
  inline int getNumConfigs() { return numconfigs; }
  inline int getNumIndependentChannelConfigs() { return numindependentchannelconfigs; }
  inline int getFirstNaturalConfigIndex(int independentchannelindex) { return firstnaturalconfigindices[independentchannelindex]; }
  inline int getIndependentChannelIndex(int naturalconfigindex) { return configs[naturalconfigindex].independentchannelindex; }
  inline int getNumChannels(int configindex) { return configs[configindex].numchannels; }
  inline int getBlocksPerSend(int configindex) { return configs[configindex].blockspersend; }
  inline int getGuardBlocks(int configindex) { return configs[configindex].guardblocks; }
  inline int getOversampleFactor(int configindex) { return configs[configindex].oversamplefactor; }
  inline int getDecimationFactor(int configindex) { return configs[configindex].decimationfactor; }
  inline double getIntTime(int configindex) { return configs[configindex].inttime; }
  inline bool writeAutoCorrs(int configindex) { return configs[configindex].writeautocorrs; }
  inline outputformat getOutputFormat() { return outformat; }
  inline string getOutputFilename() { return outputfilename; }
  inline bool pulsarBinOn(int configindex) { return configs[configindex].pulsarbin; }
  inline bool scrunchOutputOn(int configindex) { return configs[configindex].scrunchoutput; }
  inline int getNumPulsarBins(int configindex) { return configs[configindex].numbins; }
  inline int getNumPolycos(int configindex) { return configs[configindex].numpolycos; }
  inline Polyco ** getPolycos(int configindex) { return configs[configindex].polycos; }
  inline bool matchingBand(int configindex, int configdatastreamindex, int datastreamfreqindex, int datastreaminputbandindex)
    { return datastreamfreqindex == datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].inputbandlocalfreqindices[datastreaminputbandindex]; }
  inline int getDDataBufferFactor() { return databufferfactor; }
  inline int getDNumDataSegments() { return numdatasegments; }
  inline int getDTelescopeIndex(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex; }
  inline int getDNumFreqs(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numfreqs; }
  inline double getDClockOffset(int configindex, int configdatastreamindex)
    { return telescopetable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex].clockdelay; }
  inline double getDClockRate(int configindex, int configdatastreamindex)
    { return telescopetable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex].clockrate; }
  inline string getDStationName(int configindex, int configdatastreamindex) 
    { return telescopetable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].telescopeindex].name; }
  inline double getDTsys(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].tsys; }
  inline int getDNumBits(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numbits; }
  inline int getDFreqIndex(int configindex, int configdatastreamindex, int datastreambandindex)
    { datastreamdata ds = datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]]; return ds.freqtableindices[ds.inputbandlocalfreqindices[datastreambandindex]]; }
  inline int getDLocalFreqIndex(int configindex, int configdatastreamindex, int datastreambandindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].inputbandlocalfreqindices[datastreambandindex]; }
  inline char getDBandPol(int configindex, int configdatastreamindex, int datastreambandindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].inputbandpols[datastreambandindex]; }
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
  inline int getDNumOutputBands(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numoutputbands; }
  inline int getDNumInputBands(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].numinputbands; }
  inline string * getDDataFileNames(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].datafilenames; }
  inline double getDFreq(int configindex, int configdatastreamindex, int datastreamfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].freqtableindices[datastreamfreqindex]].bandedgefreq; }
  inline double getDBandwidth(int configindex, int configdatastreamindex, int datastreamfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].freqtableindices[datastreamfreqindex]].bandwidth; }
  inline bool getDLowerSideband(int configindex, int configdatastreamindex, int datastreamfreqindex)
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].freqtableindices[datastreamfreqindex]].lowersideband; }
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
    {  baselinedata b = baselinetable[configs[configindex].baselineindices[configbaselineindex]]; datastreamdata ds = datastreamtable[b.datastream1index]; return ds.freqtableindices[ds.inputbandlocalfreqindices[b.datastream1bandindex[baselinefreqindex][0]]];
    }
  inline int getBNumPolProducts(int configindex, int configbaselineindex, int baselinefreqindex)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].numpolproducts[baselinefreqindex]; }
  inline int getBDataStream1BandIndex(int configindex, int configbaselineindex, int baselinefreqindex, int baselinefreqdatastream1index)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream1bandindex[baselinefreqindex][baselinefreqdatastream1index]; }
  inline int getBDataStream2BandIndex(int configindex, int configbaselineindex, int baselinefreqindex, int baselinefreqdatastream2index)
    { return baselinetable[(configs[configindex].baselineindices[configbaselineindex])].datastream2bandindex[baselinefreqindex][baselinefreqdatastream2index]; }
  inline void getBPolPair(int configindex, int configbaselineindex, int baselinefreqindex, int freqpolindex, char polpair[3])
    { baselinedata b = baselinetable[configs[configindex].baselineindices[configbaselineindex]]; polpair[0] = datastreamtable[b.datastream1index].inputbandpols[b.datastream1bandindex[baselinefreqindex][freqpolindex]]; polpair[1] = datastreamtable[b.datastream2index].inputbandpols[b.datastream2bandindex[baselinefreqindex][freqpolindex]];
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
  inline string getDelayFileName() { return delayfilename; }
  inline int getFreqTableLength() { return freqtablelength; }
  inline double getFreqTableFreq(int index) { return freqtable[index].bandedgefreq; }
  inline double getFreqTableBandwidth(int index) { return freqtable[index].bandwidth; }
  inline bool getFreqTableLowerSideband(int index) { return freqtable[index].lowersideband; }
  inline bool circularPolarisations() 
    { return datastreamtable[0].inputbandpols[0] == 'R' || datastreamtable[0].inputbandpols[0] == 'L'; }
  inline int getSourceIndex(int mjd, int sec) { return uvw->getSourceIndex(mjd, sec); }
  inline bool isReadFromFile(int configindex, int configdatastreamindex) 
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].source != EVLBI; }
  inline bool isMkV(int datastreamindex) 
  {
    dataformat f;
    datasource s;
    f = datastreamtable[configs[0].datastreamindices[datastreamindex]].format;
    s = datastreamtable[configs[0].datastreamindices[datastreamindex]].source;
    return ((f == MKIV || f == VLBA || f == MARK5B) && (s == UNIXFILE || s == EVLBI)); 
  }
  inline bool isNativeMkV(int datastreamindex) 
  { 
    dataformat f;
    datasource s;
    f = datastreamtable[configs[0].datastreamindices[datastreamindex]].format;
    s = datastreamtable[configs[0].datastreamindices[datastreamindex]].source;
    return ((f == MKIV || f == VLBA || f == MARK5B) && s == MK5MODULE); 
  }
  inline int getFrameBytes(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].framebytes; }
  inline dataformat getDataFormat(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].format; }
  inline datasource getDataSource(int configindex, int configdatastreamindex)
    { return datastreamtable[configs[configindex].datastreamindices[configdatastreamindex]].source; }
  inline double getConfigBandwidth(int configindex) 
    { return freqtable[datastreamtable[configs[configindex].datastreamindices[0]].freqtableindices[0]].bandwidth; }
  inline string getTelescopeName(int telescopeindex)
    { return telescopetable[telescopeindex].name; }
  inline int getTelescopeTableLength()
    { return telescopetablelength; }
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
  * @return The UVW object which contains geometric model information
  */
  inline Uvw * getUVW() { return uvw; }

 /**
  * Loads the UVW information from file into memory
  * @param nameonly Whether to only load the scan names (true) or all model information (false)
  */
  void loaduvwinfo(bool nameonly);

 /**
  * Creates and returns the appropriate mode object
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @return A new Mode object for that datastream, in that configuration
  */
  Mode * getMode(int configindex, int datastreamindex);

 /**
  * Works out which kind of MkV format is used (Mk4 or VLBA) for a given datastream configuration
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @param configdatastreamindex The index of the datastream in order for that configuration (from the table in the input file)
  */

 /**
  * @param offsetseconds The offset from the start of the correlation in seconds
  * @return The index of the configuration in use at that time
  */
  int getConfigIndex(int offsetseconds);

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @return The maximum number of frequencies being used by any datastream during this configuration
  */
  int getMaxNumFreqs(int configindex);

 /**
  * @return The maximum number of frequencies being used by any datastream during any configuration
  */
  int getMaxNumFreqs();

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @return The index of the Datastream that uses the maximum number of frequencies during this configuration
  */
  int getMaxNumFreqDatastreamIndex(int configindex);

 /**
  * @return The maximum length of a cf32 vector containing all visibilities and autocorrelations, for any configuration
  */
  int getMaxResultLength();

 /**
  * @return The maximum length of a data message from any telescope, for any configuration
  */
  int getMaxDataBytes();

 /**
  * @param datastreamindex The index of the datastream (from the table in the input file)
  * @return The maximum length of a data message from the specified datastream, for any configuration
  */
  int getMaxDataBytes(int datastreamindex);

 /**
  * @return The maximum number of blocks sent in a single message (blocks + guard), for any configuration
  */
  int getMaxSendBlocks();

 /**
  * @return The maximum number of products (1, 2 or 4) for any baseline in any configuration
  */
  int getMaxProducts();

 /**
  * @param configindex The index of the configuration being used (from the table in the input file)
  * @return The maximum length of a cf32 vector containing all visibilities and autocorrelations, for the specified configuration
  */
  int getResultLength(int configindex);

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
  enum sectionheader {COMMON, CONFIG, FREQ, TELESCOPE, DATASTREAM, BASELINE, DATA, NETWORK, INPUT_EOF, UNKNOWN};

  ///Storage struct for data from the frequency table of the input file
  typedef struct {
    double bandedgefreq;
    double bandwidth;
    bool lowersideband;
  } freqdata;

  ///Storage struct for data from the baseline table of the input file
  typedef struct {
    int datastream1index;
    int datastream2index;
    int numfreqs;
    int totalbands;
    int * numpolproducts;
    int ** datastream1bandindex;
    int ** datastream2bandindex;
  } baselinedata;

  ///Storage struct for data from the config table of the input file
  typedef struct {
    string sourcename;
    double inttime;
    int numchannels;
    int channelstoaverage;
    int oversamplefactor;
    int decimationfactor;
    int independentchannelindex;
    int blockspersend;
    int guardblocks;
    bool postffringerot;
    bool quadraticdelayinterp;
    bool writeautocorrs;
    bool pulsarbin;
    int numpolycos;
    int numbins;
    bool scrunchoutput;
    string pulsarconfigfilename;
    Polyco ** polycos;
    int * datastreamindices;
    int * ordereddatastreamindices;
    int * baselineindices;
  } configdata;

  ///Storage struct for data from the telescope table of the input file
  typedef struct {
    string name;
    double clockdelay;
    double clockrate;
  } telescopedata;

  ///Storage struct for data from the datastream table of the input file
  typedef struct {
    int telescopeindex;
    double tsys;
    dataformat format;
    datasource source;
    int numbits;
    int bytespersamplenum;
    int bytespersampledenom;
    int framesamples;
    int framebytes;
    int framens;
    bool filterbank;
    int numfreqs;
    int * freqpols;
    int * freqtableindices;
    double * freqclockoffsets;
    int numinputbands;
    int numoutputbands;
    char * inputbandpols;
    int * inputbandlocalfreqindices;
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
  * Loads the baseline table from the file into memory
  * @param input Open file stream for the input file
  */
  void processBaselineTable(ifstream * input);

 /**
  * Loads the common settings from the file into memory
  * @param input Open file stream for the input file
  */
  void processCommon(ifstream * input);

 /**
  * Loads the config table from the file into memory
  * @param input Open file stream for the input file
  */
  void processConfig(ifstream * input);

 /**
  * Loads the datastream table from the file into memory
  * @param input Open file stream for the input file
  */
  void processDatastreamTable(ifstream * input);

 /**
  * Loads the data table from the file into memory
  * @param input Open file stream for the input file
  */
  void processDataTable(ifstream * input);

 /**
  * Loads the frequency table from the file into memory
  * @param input Open file stream for the input file
  */
  void processFreqTable(ifstream * input);

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
  */
  void processPulsarConfig(string filename, int configindex);

 /**
  * Once the input file has been completely processed, provide all frequency info to the generated Polyco files
  * @param configindex The index of the configuration to be set up (from the table in the input file)
  */
  void setPolycoFreqInfo(int configindex);

  ///The length of keywords in all input files
  static const int HEADER_LENGTH = 21;

  char header[HEADER_LENGTH];
  bool commonread, configread, datastreamread, consistencyok;
  int visbufferlength;
  int executeseconds, startmjd, startseconds, startns, numdatastreams, numbaselines, numconfigs, defaultconfigindex, baselinetablelength, telescopetablelength, datastreamtablelength, freqtablelength, databufferfactor, numdatasegments, numcoreconfs, maxnumchannels, maxnumpulsarbins, numindependentchannelconfigs;
  string delayfilename, uvwfilename, coreconffilename, outputfilename;
  int * numprocessthreads;
  int * firstnaturalconfigindices;
  configdata * configs;
  freqdata * freqtable;
  telescopedata * telescopetable;
  baselinedata * baselinetable;
  datastreamdata * datastreamtable;
  Uvw * uvw;
  outputformat outformat;
};

#endif
