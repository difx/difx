/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken                             *
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
#include <difxio.h>

#include "vextables.h"

using namespace std;

extern const double MJD_UNIX0;	// MJD at beginning of unix time
extern const double SEC_DAY;
extern const double MUSEC_DAY;

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
	static const string DEFAULT_NAME;
	static const double DEFAULT_RA  = -999.9;
	static const double DEFAULT_DEC = -999.9;

	//constructors
	PhaseCentre(double ra, double dec, string name);
	PhaseCentre();

	//methods
	void initialise(double ra, double dec, string name);

	//variables
	double ra;	  //radians
	double dec;	  //radians
	string difxName;
        char calCode;
	int qualifier;
	// ephemeris
	string ephemObject;     // name of the object in the ephemeris
	string ephemFile;       // file containing a JPL ephemeris
	string naifFile;        // file containing naif time data
	double ephemDeltaT;     // tabulated ephem. interval (seconds, default 60)
};

class SourceSetup
{
public:
	SourceSetup(const string &name);
	int setkv(const string &key, const string &value);
	int setkv(const string &key, const string &value, PhaseCentre * pc);

	bool doPointingCentre;       	  // Whether or not to correlate the pointing centre
	string vexName;		     	  // Source name as appears in vex file
	PhaseCentre pointingCentre;  	  // The source which is at the pointing centre
	vector<PhaseCentre> phaseCentres; // Additional phase centres to be correlated
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
	GlobalZoom(const string name) { difxName = name; }
	int setkv(const string &key, const string &value, ZoomFreq * zoomFreq);
	int setkv(const string &key, const string &value);

	string difxName;	// Name in .v2d file of this global zoom band set
	vector<ZoomFreq> zoomFreqs;
};

class AntennaSetup
{
public:
	AntennaSetup(const string &name);
	int setkv(const string &key, const string &value);
	int setkv(const string &key, const string &value, ZoomFreq * zoomFreq);
	void copyGlobalZoom(const GlobalZoom &globalZoom);

	string vexName;		// Antenna name as it appears in vex file
	string difxName;	// Antenna name (if different) to appear in difx
	double X, Y, Z;		// Station coordinates to override vex
	int clockorder;		// Order of clock poly (if overriding)
	double clock2, clock3, clock4, clock5;	// Clock coefficients (if overriding)
        vector<double> freqClockOffs; // clock offsets for the individual frequencies
	vector<double> loOffsets; //LO offsets for each individual frequency
	VexClock clock;
	double deltaClock;	// sec
	double deltaClockRate;	// sec/sec
	// flag
	// media
	bool polSwap;		// If true, swap polarizations
	string format;		// Override format from .v2d file.  
				// This is sometimes needed because format not known always at scheduling time
				// Possible values: S2 VLBA MkIV/Mark4 Mark5B . Is converted to all caps on load
	enum DataSource dataSource;
	enum SamplingType dataSampling;
	vector<VexBasebandFile> basebandFiles;	// files to correlate
	int networkPort;	// For eVLBI : port for this antenna
	int windowSize;		// For eVLBI : TCP window size
	int phaseCalIntervalMHz;// 0 if no phase cal extraction, positive gives interval between tones to extract
	enum ToneSelection toneSelection;	// Which tones to propagate to FITS
	double toneGuardMHz;	// to avoid getting tones too close to band edges; default = bandwidth/8
	int tcalFrequency;	// Hz (= 80 for VLBA)

	// No more than one of the following can be used at a time:
	vector<ZoomFreq> zoomFreqs;//List of zoom freqs to add for this antenna
	string globalZoom;	// A reference to a global zoom table
};

class CorrSetup
{
public:
	CorrSetup(const string &name = "setup_default");
	int setkv(const string &key, const string &value);
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


	string corrSetupName;

	bool explicitXmacLength;// Whether the xmacLength parameter was explicitly set
	bool explicitStrideLength;// Whether the strideLength parameter was explicitly set
	bool explicitFFTSpecRes;// Whether .v2d set the resolution of FFTs
	bool explicitOutputSpecRes; // Whether .v2d set the output resolution
	bool explicitGuardNS;	// Whether the guardNS parameter was explicitly set
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
	set<int> freqIds;       // which bands to correlate
	string binConfigFile;
	string phasedArrayConfigFile;
        char onlyPol;		// which polarization to correlate
private:
	void addFreqId(int freqId);

	set<double> recordedBandwidths;	// Hz; list of all unique recorded bandwidths using this setup
	double minRecordedBandwidth;	// Hz; cached by addRecordedBandwidth
	double maxRecordedBandwidth;	// Hz; ""
};


class CorrRule
{
public:
	CorrRule(const string &name = "rule_default");

	int setkv(const string &key, const string &value);
	bool match(const string &scan, const string &source, const string &mode, char cal, int qual) const;

	string ruleName;

	list<string> scanName;
	list<string> sourceName;
	list<string> modeName;
	list<char> calCode;
	list<int> qualifier;

	string corrSetupName;	/* pointer to CorrSetup */
};

class CorrParams : public VexInterval
{
public:
	CorrParams();
	CorrParams(const string &fileName);
	int checkSetupValidity();

	int loadShelves(const string &fileName);
	const char *getShelf(const string &vsn) const;

	int setkv(const string &key, const string &value);
	int load(const string &fileName);
	void defaults();
	void defaultSetup();
	void defaultRule();
	void example();
	int sanityCheck();
	void addSourceSetup(const SourceSetup &toAdd);

	bool useAntenna(const string &antName) const;
	bool useBaseline(const string &ant1, const string &ant2) const;
	bool swapPol(const string &antName) const;
	const CorrSetup *getCorrSetup(const string &name) const;
	CorrSetup *getNonConstCorrSetup(const string &name);
	const SourceSetup *getSourceSetup(const string &name) const;
	const SourceSetup *getSourceSetup(const vector<string> &names) const;
	const PhaseCentre *getPhaseCentre(const string &difxname) const;
	const AntennaSetup *getAntennaSetup(const string &name) const;
	const GlobalZoom *getGlobalZoom(const string &name) const;
	const VexClock *getAntennaClock(const string &antName) const;

	const string &findSetup(const string &scan, const string &source, const string &mode, char cal, int qual) const;
	const string &getNewSourceName(const string &origName) const;
	
	/* global parameters */
	int parseWarnings;
	string vexFile;
	string threadsFile;
	unsigned int minSubarraySize;
	double maxGap;		// days
	bool singleScan;
	bool fakeDatasource;	// if true, configure all datasources as FAKE
	bool singleSetup;
	bool allowOverlap;
	bool mediaSplit;	// split jobs on media change
	bool padScans;
	bool simFXCORR;		// set integration and start times to match VLBA HW correlator
	bool tweakIntTime;      // nadger the integration time to make values nice
	int nCore;
	int nThread;
	double maxLength;	// [days]
	double minLength;	// [days]
	double maxSize;		// [bytes] -- break jobs for output filesize
	string jobSeries;	// prefix name to job files
	int startSeries;	// start job series at this number
	int dataBufferFactor;
	int nDataSegments;
	int maxReadSize;        // Max (Bytes) amount of data to read into datastream at a time 
	int minReadSize;        // Min (Bytes) amount of data to read into datastream at a time
	unsigned int invalidMask;
	int visBufferLength;
	int overSamp;		// A user supplied override to oversample factor
	enum OutputFormatType outputFormat; // DIFX or ASCII

	list<string> antennaList;
	list<pair<string,string> > baselineList;

	/* manual forced job breaks */
	vector<double> manualBreaks;

	/* setups to apply */
	vector<CorrSetup> corrSetups;

	/* source setups to apply */
	vector<SourceSetup> sourceSetups;

	/* manually provided EOPs */
	vector<VexEOP> eops;

	/* antenna setups to apply */
	vector<AntennaSetup> antennaSetups;

	/* rules to determine which setups to apply */
	vector<CorrRule> rules;

	/* global zoom bands (referenced from a setup; applies to all antennas) */
	vector<GlobalZoom> globalZooms;

	enum V2D_Mode v2dMode;

	list<string> machines;	// List of computers for generation of .machines file

private:
	void addAntenna(const string &antName);
	void addBaseline(const string &baselineName);
	map<string,string> shelves;
};

ostream& operator << (ostream &os, const CorrSetup &x);
ostream& operator << (ostream &os, const CorrRule &x);
ostream& operator << (ostream &os, const CorrParams &x);

bool areCorrSetupsCompatible(const CorrSetup *A, const CorrSetup *B, const CorrParams *C);

#endif
