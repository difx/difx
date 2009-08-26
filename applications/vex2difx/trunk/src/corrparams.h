/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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

class PhaseCenter
{
public:
	double ra;
	double dec;
	string name;
	// pulsar index
	// ephemeris
	char calCode;
	int qualifier;
};

class SourceSetup
{
public:
	SourceSetup(const string &name);
	void setkv(const string &key, const string &value);

	string vexName;		// Source name as appears in vex file
	string difxName;	// Source name (if different) to appear in difx
	char calCode;
	double ra, dec;		// in radians
	string ephemObject;	// name of the object in the ephemeris
	string ephemFile;	// file containing a JPL ephemeris
	string naifFile;	// file containing naif time data
	double ephemDeltaT;	// tabulated ephem. interval (seconds, default 60)

	//vector<PhaseCenter> centers;
};

class AntennaSetup
{
public:
	AntennaSetup(const string &name);
	void setkv(const string &key, const string &value);

	string vexName;		// Antenna name as it appears in vex file
	string difxName;	// Antenna name (if different) to appear in difx
	double X, Y, Z;		// Station coordinates to override vex
	VexClock clock;
	// flag
	// media
	bool polSwap;		// If true, swap polarizations
	string format;		// Override format from .v2d file.  
				// This is sometimes needed because format not known always at scheduling time
				// Possible values: S2 VLBA MkIV/Mark4 Mark5B .  Is converted to all caps on load
	vector<VexBasebandFile> basebandFiles;	// files to correlate
};

class CorrSetup
{
public:
	CorrSetup(const string &name = "setup_default");
	void setkv(const string &key, const string &value);
	bool correlateFreqId(int freqId) const;
	double bytesPerSecPerBLPerBand() const;

	string corrSetupName;

	double tInt;		// integration time
	int nChan;		// channels per sub-band
	bool doPolar;		// false for no cross pol, true for full pol
	bool doAuto;		// write autocorrelations
	int blocksPerSend;	// literal
	int specAvg;
	int startChan;
	int nOutChan;
	bool postFFringe;	// fringe after FFT?
	string binConfigFile;
	set<int> freqIds;	// which bands to correlate
private:
	void addFreqId(int freqId);
};


class CorrRule
{
public:
	CorrRule(const string &name = "rule_default");

	void setkv(const string &key, const string &value);
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
	CorrParams(const string& fileName);

	void loadShelves(const string& fileName);
	const char *getShelf(const string& vsn) const;

	void setkv(const string &key, const string &value);
	void load(const string& fileName);
	void defaults();
	void defaultSetup();
	void example();

	bool useAntenna(const string &antName) const;
	bool useBaseline(const string &ant1, const string &ant2) const;
	bool swapPol(const string &antName) const;
	const CorrSetup *getCorrSetup(const string &name) const;
	const SourceSetup *getSourceSetup(const string &name) const;
	const AntennaSetup *getAntennaSetup(const string &name) const;
	const VexClock *getAntennaClock(const string &antName) const;

	const string &findSetup(const string &scan, const string &source, const string &mode, char cal, int qual) const;
	
	/* global parameters */
	string vexFile;
	unsigned int minSubarraySize;
	double maxGap;		// days
	bool singleScan;
	bool singleSetup;
	bool allowOverlap;
	bool mediaSplit;	// split jobs on media change
	bool padScans;
	bool simFXCORR;		// set integration and start times to match VLBA HW correlator
	double maxLength;	// [days]
	double minLength;	// [days]
	double maxSize;		// [bytes] -- break jobs for output filesize
	string jobSeries;	// prefix name to job files
	int startSeries;	// start job series at this number
	int dataBufferFactor;
	int nDataSegments;
	double sendLength;	// (s) amount of data to send from datastream to core at a time
	unsigned int invalidMask;
	int visBufferLength;

	list<string> antennaList;
	list<pair<string,string> > baselineList;

	/* manual forced job breaks */
	vector<double> manualBreaks;

	/* setups to apply */
	vector<CorrSetup> corrSetups;

	/* source setups to apply */
	vector<SourceSetup> sourceSetups;

	/* antenna setups to apply */
	vector<AntennaSetup> antennaSetups;

	/* rules to determine which setups to apply */
	vector<CorrRule> rules;

	enum V2D_Mode v2dMode;

private:
	void addAntenna(const string& antName);
	void addBaseline(const string& baselineName);
	map<string,string> shelves;
};

ostream& operator << (ostream& os, const CorrSetup& x);
ostream& operator << (ostream& os, const CorrRule& x);
ostream& operator << (ostream& os, const CorrParams& x);

bool areCorrSetupsCompatible(const CorrSetup *A, const CorrSetup *B, const CorrParams *C);

#endif
