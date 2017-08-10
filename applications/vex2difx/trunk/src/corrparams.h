/***************************************************************************
 *   Copyright (C) 2009-2017 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __CORRPARAM_H__
#define __CORRPARAM_H__

#include <string>
#include <set>
#include <vector>
#include <map>
#include <list>
#include <iostream>
#include <difxio.h>

#include "interval.h"
#include "freq.h"
#include "vex_data.h"

extern const double MJD_UNIX0;	// MJD at beginning of unix time
extern const double SEC_DAY;
extern const double MUSEC_DAY;

#define AXIS_OFFSET_NOT_SET	-999
#define ANTENNA_COORD_NOT_SET	-9e10

#define MAX_DX_ORDER	4

enum V2D_Mode
{
	V2D_MODE_NORMAL = 0,	// for almost all purposes
	V2D_MODE_PROFILE = 1	// to produce pulsar profiles
};

// see http://cira.ivec.org/dokuwiki/doku.php/difx/configuration

class PhaseCentre
{
public:
	//constants
	static const std::string DEFAULT_NAME;
	static const double DEFAULT_RA;
	static const double DEFAULT_DEC;

	//constructors
	PhaseCentre(double ra, double dec, std::string name);
	PhaseCentre();

	//methods
	void initialise(double ra, double dec, std::string name);
	bool isFixedSource() const { return (X != 0.0 || Y != 0.0 || Z != 0.0); };
	bool isSpacecraft() const { return (!ephemFile.empty()); };

	//variables
	double ra;	//radians
	double dec;	//radians
	std::string difxName;
	char calCode;
	int qualifier;
	// ephemeris
	std::string ephemObject;	// name of the object in the ephemeris
	std::string ephemFile;	// file containing a JPL ephemeris
	std::string naifFile;	// file containing naif time data
	double ephemDeltaT;	// tabulated ephem. nterval (seconds, default 60)
	double ephemStellarAber;	// 0 = don't apply (default), 1 = apply, other: scale correction accordingly
	double ephemClockError;		// (sec) 0.0 is no error
	double X, Y, Z;			// For geosync satellite [0, 0, 0 means not a geosync]
};

class SourceSetup
{
public:
	SourceSetup(const std::string &name);
	int setkv(const std::string &key, const std::string &value);
	int setkv(const std::string &key, const std::string &value, PhaseCentre * pc);

	bool doPointingCentre;		// Whether or not to correlate the pointing centre
	std::string vexName;		// Source name as appears in vex file
	PhaseCentre pointingCentre;	// The source which is at the pointing centre
	std::vector<PhaseCentre> phaseCentres; // Additional phase centres to be correlated
};

class ZoomFreq
{
public:
	//constructor
	ZoomFreq();

	//method
	void initialise(double freq, double bw, bool corrparent, int specavg);

	//variables
	double frequency, bandwidth;
	int spectralaverage;
	bool correlateparent;
};

class GlobalZoom
{
public:
	GlobalZoom(const std::string &name) : difxName(name) {};
	int setkv(const std::string &key, const std::string &value, ZoomFreq * zoomFreq);
	int setkv(const std::string &key, const std::string &value);

	std::string difxName;	// Name in .v2d file of this global zoom band set
	std::vector<ZoomFreq> zoomFreqs;
};

/* Datastreams...

How do multi-datastreams work in vex2difx?

1. Datastreams contain format and data source information for a subset of channels produced by an antenna
2. If an antenna has no representative ANTENNA section, then the vex file must provide all of the information and only one datastream can be configured for that antenna
3. ANTENNA sections can reference zero or more DATASTREAM sections with the datastreams parameter
4. If no DATASTREAM is provided, then the datastream-specific info comes from the vex file with possible overrides from the ANTENNA section.  After file loading a single entry in the datastreams vector is populated.
5. If multiple datastreams are created, each datastream (currently) must contain a single contiguous block of baseband channels (bands) from the VEX file.  If the datastream baseband channels in the vex file are not contiguous they will need to be manually reordered.
6. If the number of baseband channels (bands) is not specified for datastreams of an antenna, it will be assumed that the datastreams evenly divide the number of recorded baseband channels in the vex file.  It is an error if the number of baseband channels in datastreams does not equal the number of recorded baseband channels in the vex file.
7. Information that applies to all datastreams for an antenna can be provided in the ANTENNA section.
8. It is not allowed to specify conflicting information in the ANTENNA section and a related DATASTREAM section; i.e., there is no overriding specified values.
9. If multiple datastreams are defined then there is no mechanism to support modes with varying numbers of recorded channels.
10. It is likely problematic for two streams to have identical channels or overlapping channels with zoom bands entirely within the overlap

TODO:
* DATA table needs populating (file lists or network info)
* Test all non-fake modes
*/

/* NOTE! If something is added to this structure, you might need to update DatastreamSetup::merge() */
class DatastreamSetup
{
public:
	DatastreamSetup(const std::string &name);
	int setkv(const std::string &key, const std::string &value);
	bool hasBasebandData(const Interval &interval) const;	// Returns true if at least one baseband file exists over the provided interval
	int merge(const DatastreamSetup *dss);

	std::string difxName;
	std::string vsn;	// if provided manually
	std::vector<VexBasebandData> basebandFiles;	// files to correlate
	std::string networkPort;// For eVLBI : port for this antenna.  A non-number indicates raw mode attached to an ethernet interface
	int windowSize;		// For eVLBI : TCP window size
	std::string machine;	// If set, use specified machine as datastream node for this antenna
	std::string format;	// Override format from .vex file.
				// This is sometimes needed because format not known always at scheduling time
				// Possible values: S2 VLBA MkIV/Mark4 Mark5B . Is converted to all caps on load
	std::set<int> recorderIds;	// List of recorder ids to associate with this datastream
	enum DataSource dataSource;
	enum SamplingType dataSampling;

	int startBand;		// within the antenna's baseband channels (defined in vex order), where does this datastream start? [0-based]; -1 indicates not initialized
	int nBand;		// number of baseband channels provided by this datastream
	double tSys;		// The TSYS parameter of the .input file.  Normally this is zero
};

class AntennaSetup
{
public:
	AntennaSetup(const std::string &name);
	int setkv(const std::string &key, const std::string &value);
	int setkv(const std::string &key, const std::string &value, ZoomFreq * zoomFreq);
	void copyGlobalZoom(const GlobalZoom &globalZoom);
	bool hasBasebandData(const Interval &interval) const;	// Returns true if at least one baseband file exists over the provided interval
	enum DataSource getDataSource() const;
	const std::string &getFormat() const;

	std::string vexName;	// Antenna name as it appears in vex file
	std::string difxName;	// Antenna name (if different) to appear in difx
	double X, Y, Z;		// [m] Station coordinates to override vex
	double axisOffset;	// [m]
	int clockorder;		// Order of clock poly (if overriding)
	double clock2, clock3, clock4, clock5;	// Clock coefficients (if overriding)
	std::vector<double> freqClockOffs;	// Clock offsets for the individual frequencies
	std::vector<double> freqClockOffsDelta; // Clock offsets between pols for the individual frequencies
	std::vector<double> freqPhaseDelta;	// Phase difference between pols for each frequency
	std::vector<double> loOffsets;		// LO offsets for each individual frequency
	VexClock clock;
	double deltaClock;	// [sec]
	double deltaClockRate;	// [sec/sec]
	// flag
	bool polSwap;		// If true, swap polarizations
	int phaseCalIntervalMHz;// 0 if no phase cal extraction, positive gives interval between tones to extract
	enum ToneSelection toneSelection;	// Which tones to propagate to FITS
	double toneGuardMHz;	// to avoid getting tones too close to band edges; default = bandwidth/8
	int tcalFrequency;	// [Hz] (= 80 for VLBA)

	// No more than one of the following can be used at a time:
	std::vector<ZoomFreq> zoomFreqs;//List of zoom freqs to add for this antenna
	std::string globalZoom;	// A reference to a global zoom table

	// antenna-specific start and stop times
	double mjdStart;
	double mjdStop;

	DatastreamSetup defaultDatastreamSetup;	// only used to contain defaults used in creating datastreamSetups
	std::list<std::string> datastreamList;	// list of datastreams provided in v2d file
	std::vector<DatastreamSetup> datastreamSetups;
private:
	void addDatastream(const std::string &dsName);
};

class CorrSetup
{
public:
	CorrSetup(const std::string &name = "setup_default");
	int setkv(const std::string &key, const std::string &value);
	bool correlateFreqId(int freqId) const;
	double bytesPerSecPerBLPerBand() const;
	int checkValidity() const;

	double getMinRecordedBandwidth() const { return minRecordedBandwidth; }
	double getMaxRecordedBandwidth() const { return maxRecordedBandwidth; }
	void addRecordedBandwidth(double bw);
	int nInputChans(double bw) const { return static_cast<int>(bw / FFTSpecRes + 0.5); }
	int nOutputChans(double bw) const { return static_cast<int>(bw / outputSpecRes + 0.5); }
	int minInputChans() const { return static_cast<int>(getMinRecordedBandwidth() / FFTSpecRes + 0.5); }
	int maxInputChans() const { return static_cast<int>(getMaxRecordedBandwidth() / FFTSpecRes + 0.5); }
	int minOutputChans() const { return static_cast<int>(getMinRecordedBandwidth() / outputSpecRes + 0.5); }
	int maxOutputChans() const { return static_cast<int>(getMaxRecordedBandwidth() / outputSpecRes + 0.5); }
	int testSpectralResolution() const;
	int testXMACLength() const;
	int testStrideLength() const;
	int specAvg() const;


	std::string corrSetupName;

	bool explicitFFTSpecRes;// Whether .v2d set the resolution of FFTs
	bool explicitOutputSpecRes; // Whether .v2d set the output resolution
	double tInt;		// integration time
	bool doPolar;		// false for no cross pol, true for full pol
	bool doAuto;		// write autocorrelations
	int subintNS;		// Duration of a subintegration in nanoseconds
	int guardNS;		// Number of "guard" ns tacked on to end of a send
	double FFTSpecRes;	// Hz; resolution of initial FFTs
	double outputSpecRes;	// Hz; resolution of averaged output FFTs
	double suppliedSpecAvg;	// specAvg supplied by .v2d file
	int nFFTChan;		// This and the next parameter can be used to override the above two if all channels are the same width
	int nOutputChan;	//
	int maxNSBetweenUVShifts; //Mostly for multi-phase centres
	int maxNSBetweenACAvg;	// Mostly for sending STA dumps
	int fringeRotOrder;	// 0, 1, or 2
	int strideLength;	// The number of channels to do at a time
				// when fringeRotOrder > 0
	int xmacLength;		// Number of channels to do at a time when xmac'ing
	int numBufferedFFTs;	// Number of FFTs to do in Mode before XMAC'ing
	std::set<int> freqIds;	// which bands to correlate
	std::string binConfigFile;
	std::string phasedArrayConfigFile;
	char onlyPol;		// which polarization to correlate
private:
	void addFreqId(int freqId);

	std::set<double> recordedBandwidths;	// Hz; list of all unique recorded bandwidths using this setup
	double minRecordedBandwidth;		// Hz; cached by addRecordedBandwidth
	double maxRecordedBandwidth;		// Hz; ""
};


class CorrRule
{
public:
	CorrRule(const std::string &name = "rule_default");

	int setkv(const std::string &key, const std::string &value);
	bool match(const std::string &scan, const std::string &source, const std::string &mode) const;

	std::string ruleName;

	std::list<std::string> scanName;
	std::list<std::string> sourceName;
	std::list<std::string> modeName;
	std::list<char> calCode;
	std::list<int> qualifier;

	std::string corrSetupName;	/* pointer to CorrSetup */
};

class CorrParams : public Interval
{
public:
	CorrParams();
	CorrParams(const std::string &fileName);
	int checkSetupValidity();

	int setkv(const std::string &key, const std::string &value);
	int load(const std::string &fileName);
	void defaults();
	void defaultSetup();
	void defaultRule();
	void example();
	int sanityCheck();
	void addSourceSetup(const SourceSetup &toAdd);

	bool useAntenna(const std::string &antName) const;
	bool useBaseline(const std::string &ant1, const std::string &ant2) const;
	bool swapPol(const std::string &antName) const;
	const CorrSetup *getCorrSetup(const std::string &name) const;
	CorrSetup *getNonConstCorrSetup(const std::string &name);
	const SourceSetup *getSourceSetup(const std::string &name) const;
	const SourceSetup *getSourceSetup(const std::vector<std::string> &names) const;
	const PhaseCentre *getPhaseCentre(const std::string &difxname) const;
	const DatastreamSetup *getDatastreamSetup(const std::string &name) const;
	const AntennaSetup *getAntennaSetup(const std::string &name) const;
	AntennaSetup *getNonConstAntennaSetup(const std::string &name);
	const GlobalZoom *getGlobalZoom(const std::string &name) const;
	const VexClock *getAntennaClock(const std::string &antName) const;

	const std::string &findSetup(const std::string &scan, const std::string &source, const std::string &mode) const;
	const std::string &getNewSourceName(const std::string &origName) const;
	
	/* global parameters */
	int parseWarnings;
	std::string vexFile;
	std::string threadsFile;
	std::string delayModel;	// if set, add parameter to .calc file instructing calcif2 to choose the model program to run
	unsigned int minSubarraySize;
	double maxGap;		// days
	bool singleScan;
	bool fakeDatasource;	// if true, configure all datasources as FAKE
	bool singleSetup;
	bool allowOverlap;
	bool mediaSplit;	// split jobs on media change
	bool padScans;
	bool simFXCORR;		// set integration and start times to match VLBA HW correlator
	bool tweakIntTime;	// nadger the integration time to make values nice
	bool sortAntennas;	// generally a good idea (defaults to true). Sorts alphabetically.
	int nCore;
	int nThread;
	double maxLength;	// [days]
	double minLength;	// [days]
	double maxSize;		// [bytes] -- break jobs for output filesize
	std::string jobSeries;	// prefix name to job files
	int startSeries;	// start job series at this number
	int dataBufferFactor;
	int nDataSegments;
	int maxReadSize;	// Max (Bytes) amount of data to read into datastream at a time 
	int minReadSize;	// Min (Bytes) amount of data to read into datastream at a time
	unsigned int invalidMask;
	int visBufferLength;
	enum OutputFormatType outputFormat; // DIFX or ASCII
	std::string v2dComment;
	std::string outPath;	// If supplied, put the .difx/ output within the supplied directory rather in ./ .

	std::list<std::string> antennaList;
	std::list<std::pair<std::string,std::string> > baselineList;

	/* manual forced job breaks */
	std::vector<double> manualBreaks;

	/* setups to apply */
	std::vector<CorrSetup> corrSetups;

	/* source setups to apply */
	std::vector<SourceSetup> sourceSetups;

	/* manually provided EOPs */
	std::vector<VexEOP> eops;

	/* datastream setup definitions */
	std::vector<DatastreamSetup> datastreamSetups;

	/* antenna setups to apply */
	std::vector<AntennaSetup> antennaSetups;

	/* rules to determine which setups to apply */
	std::vector<CorrRule> rules;

	/* global zoom bands (referenced from a setup; applies to all antennas) */
	std::vector<GlobalZoom> globalZooms;

	enum V2D_Mode v2dMode;

	std::list<std::string> machines;	// List of computers for generation of .machines file

private:
	void addAntenna(const std::string &antName);
	void addBaseline(const std::string &baselineName);
};

std::ostream& operator << (std::ostream &os, const DatastreamSetup &x);
std::ostream& operator << (std::ostream &os, const AntennaSetup &x);
std::ostream& operator << (std::ostream &os, const CorrSetup &x);
std::ostream& operator << (std::ostream &os, const CorrRule &x);
std::ostream& operator << (std::ostream &os, const CorrParams &x);

bool areCorrSetupsCompatible(const CorrSetup *A, const CorrSetup *B, const CorrParams *C);

#endif
