/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken & Adam Deller               *
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

#ifndef __VEXTABLES_H__
#define __VEXTABLES_H__

#include <vector>
#include <list>
#include <string>
#include <iostream>
#include <map>
#include <difxio.h>

extern const double RAD2ASEC;

using namespace std;

class VexData;

class VexEvent
{
public:
	enum EventType	// NOTE! keep VexEvent::eventName up to date
	{
		NO_EVENT,
		ANT_SCAN_STOP,
		SCAN_STOP,
		JOB_STOP,
		OBSERVE_STOP,
		RECORD_STOP,
		CLOCK_BREAK,
		LEAP_SECOND,
		MANUAL_BREAK,
		RECORD_START,
		OBSERVE_START,
		JOB_START,
		SCAN_START,
		ANT_SCAN_START
	};

	static const char eventName[][20];

	double mjd;
	enum EventType eventType;
	string name;
	string scan;

	VexEvent() : mjd(0.0), eventType(NO_EVENT), name("") {}
	VexEvent(double m, enum EventType e, const string &a) : mjd(m), eventType(e), name(a), scan("") {}
	VexEvent(double m, enum EventType e, const string &a, const string &b) : mjd(m), eventType(e), name(a), scan(b) {}
};

class VexInterval
{
public:
	double mjdStart;
	double mjdStop;

	VexInterval(double start=0.0, double end=0.0) : mjdStart(start), mjdStop(end) {}
	VexInterval(const VexInterval &vi) : mjdStart(vi.mjdStart), mjdStop(vi.mjdStop) {}
	double duration() const { return mjdStop-mjdStart; }
	double duration_seconds() const { return 86400.0*(mjdStop-mjdStart); }
	double overlap(const VexInterval &v) const;
	double overlap_seconds(const VexInterval &v) const { return 86400.0*overlap(v); }
	double center() const { return 0.5*(mjdStart+mjdStop); }
	void shift(double deltaT) { mjdStart += deltaT; mjdStop += deltaT; }
	void setTimeRange(double start, double stop) { mjdStart = start; mjdStop = stop; }
	void setTimeRange(const VexInterval &v) { mjdStart = v.mjdStart; mjdStop = v.mjdStop; }
	void logicalAnd(double start, double stop);
	void logicalAnd(const VexInterval &v);
	void logicalOr(double start, double stop);
	void logicalOr(const VexInterval &v);
	bool contains(double mjd) const { return (mjdStart <= mjd) && (mjd <= mjdStop); }
	bool contains(const VexInterval &v) const { return (mjdStart <= v.mjdStart) && (mjdStop >= v.mjdStop); }
	bool containsAbsolutely(double mjd) const { return (mjdStart < mjd) && (mjd < mjdStop); }
	bool containsAbsolutely(const VexInterval &v) const { return (mjdStart < v.mjdStart) && (mjdStop > v.mjdStop); }
	bool isCausal() const { return (mjdStart <= mjdStop); }
};

class VexBasebandFile : public VexInterval
{
	public:
	string filename;

	VexBasebandFile(const string &name, const VexInterval &timeRange) : VexInterval(timeRange), filename(name) {} 
	VexBasebandFile(const string &name, double start=-1.0e9, double stop=1.0e9) : VexInterval(start, stop), filename(name) {}
};

class VexScan : public VexInterval
{
public:
	string defName;			// name of this scan
	string intent;			// intent of this scan

	string modeDefName;
	string sourceDefName;	
	map<string,VexInterval> stations;
	map<string,bool> recordEnable;	// This is true of the drive number is non-zero
	string corrSetupName;		// points to CorrSetup entry
	double size;			// [bytes] approx. correlated size
	double mjdVex;			// The start time listed in the vex file

	VexScan(): size(0), mjdVex(0.0) {};
	unsigned int nAntennasWithRecordedData(const VexData *V) const;
	unsigned int nRecordChan(const VexData *V, const string &antName) const;
};

class VexSource
{
public:
	VexSource() : calCode(' '), qualifier(0), ra(0.0), dec(0.0) {}
	bool hasSourceName(const string &name) const;

	string defName;		// in the "def ... ;" line in Vex
	
	vector<string> sourceNames;	// from source_name statements
	char calCode;
	int qualifier;
	double ra;		// (rad)
	double dec;		// (rad)

	static const unsigned int MAX_SRCNAME_LENGTH = 12;
};

class VexSubband		// Mode-specific frequency details for all antennas
{
public:
	VexSubband(double f=0.0, double b=0.0, char s=' ', char p=' ', string name="") : freq(f), bandwidth(b), sideBand(s), pol(p) {}

	double freq;		// (Hz)
	double bandwidth;	// (Hz)
	char sideBand;		// net side band of channel (U or L)
	char pol;		// R or L
};

class VexChannel		// Antenna-specific baseband channel details
{
public:
	VexChannel() : recordChan(-1), subbandId(-1), bbcFreq(0.0), bbcBandwidth(0.0), bbcSideBand(' ') {}
	void selectTones(int toneIntervalMHz, enum ToneSelection selection, double guardBandMHz);
	char bandCode() const;
	friend bool operator ==(const VexChannel &c1, const VexChannel &c2);
	friend bool operator <(const VexChannel &c1, const VexChannel &c2);

	int recordChan;		// channel number on recorded media	(< 0 indicates non-recording)
	int subbandId;		// 0-based index; -1 means unset
	string ifName;		// name of the IF this channel came from
	double bbcFreq;		// sky frequency tuning of the BBC (Hz)
	double bbcBandwidth;	// bandwidth (Hz)
	char bbcSideBand;	// sideband of the BBC
	string bbcName;		// name given in VEX of this channel in the BBC table
	vector<int> tones;	// pulse cal tones to extract, directly from PHASE_CAL_DETECT
	int threadId;		// thread Id for this channel (assigned based on channel names)
};

class VexIF
{
public:
	VexIF() : ifSSLO(0.0), ifSideBand(' '), pol(' '), phaseCalIntervalMHz(0) {}
	string VLBABandName() const;
	double getLowerEdgeFreq() const;

	string name;
	double ifSSLO;		// SSLO of the IF
	char ifSideBand;	// U or L
	char pol;		// R or L
	int phaseCalIntervalMHz; // MHz, typically 1 or 5 (or 0 if none)

        // special values needed for VLBA, extracted from comment line
        string comment;
};

class VexSetup	// Container for all antenna-specific settings
{
public:
	VexSetup() : sampRate(0.0), nBit(0), nRecordChan(0) {}
	int phaseCalIntervalMHz() const;
	const VexIF *getIF(const string &ifName) const;
	double firstTuningForIF(const string &ifName) const;	// returns Hz

	map<string,VexIF> ifs;		// Indexed by name in the vex file, such as IF_A
	vector<VexChannel> channels;

	double sampRate;		// (Hz)
	unsigned int nBit;
	unsigned int nRecordChan;	// number of recorded channels
	string formatName;		// e.g. VLBA, MKIV, Mk5B, VDIF, LBA, K5, ...
};

class VexMode
{
public:
	VexMode() {}

	int addSubband(double freq, double bandwidth, char sideband, char pol);
	int getPols(char *pols) const;
	int getBits() const;
	const VexSetup* getSetup(const string &antName) const;
	double getLowestSampleRate() const;
	double getHighestSampleRate() const;
	double getAverageSampleRate() const;
#if 0
	int getOversampleFactor() const;
#endif

	string defName;

	vector<VexSubband> subbands;
	map<string,VexSetup> setups;	// indexed by antenna name
	list<int> overSamp;		// list of the oversample factors used by this mode
};

class VexClock
{
public:
	VexClock() : mjdStart(0.0), offset(0.0), rate(0.0), offset_epoch(50000.0) {}
	void flipSign() 
	{ 
		offset = -offset;
		rate = -rate;
	}

	double mjdStart;	// (mjd)
	double offset;		// (sec)
	double rate;		// (sec/sec)
	double offset_epoch;	// (mjd)
};

class VexAntenna
{
public:
	VexAntenna() : x(0.0), y(0.0), z(0.0), dx(0.0), dy(0.0), dz(0.0), posEpoch(0.0), axisOffset(0.0), dataSource(DataSourceNone) {}

	double getVexClocks(double mjd, double * coeffs) const;

	string name;
	string defName;		// Sometimes names get changed

	double x, y, z;		// (m) antenna position
	double dx, dy, dz;	// (m/sec) antenna velocity
	double posEpoch;	// mjd
	string axisType;
	double axisOffset;	// (m)
	vector<VexClock> clocks;
	vector<VexBasebandFile> basebandFiles;
	enum DataSource	dataSource;
};

class VexEOP
{
public:
	VexEOP() : mjd(0), tai_utc(0), ut1_utc(0.0), xPole(0.0), yPole(0.0) {}

	double mjd;		// days
	double tai_utc;		// seconds
	double ut1_utc;		// seconds
	double xPole, yPole;	// radian

	int setkv(const string &key, const string &value);
};

class VexExper : public VexInterval
{
public:
	VexExper() : VexInterval(0.0, 1000000.0) {}

	string name;
};

class VexJob : public VexInterval
{
public:
	VexJob() : VexInterval(0.0, 1000000.0), jobSeries("Bogus"), jobId(-1), dutyCycle(1.0), dataSize(0.0) {}

	void assignVSNs(const VexData &V);
	string getVSN(const string &antName) const;
	bool hasScan(const string &scanName) const;
	int generateFlagFile(const VexData &V, const char *fileName, unsigned int invalidMask=0xFFFFFFFF) const;

	// return the approximate number of Operations required to compute this scan
	double calcOps(const VexData *V, int fftSize, bool doPolar) const;
	double calcSize(const VexData *V) const;

	string jobSeries;
	int jobId;
	vector<string> scans;
	map<string,string> vsns;	// vsn, indexed by antenna name
	double dutyCycle;		// fraction of job spent in scans
	double dataSize;		// [bytes] estimate of data output size
};

class VexJobFlag : public VexInterval
{
public:
	static const unsigned int JOB_FLAG_RECORD = 1 << 0;
	static const unsigned int JOB_FLAG_POINT  = 1 << 1;
	static const unsigned int JOB_FLAG_TIME   = 1 << 2;
	static const unsigned int JOB_FLAG_SCAN   = 1 << 3;
	VexJobFlag() : antId(-1) {}
	VexJobFlag(double start, double stop, int ant) : VexInterval(start, stop), antId(ant) {}

	int antId;
};

class VexJobGroup : public VexInterval
{
public:
	vector<string> scans;
	list<VexEvent> events;

	bool hasScan(const string &scanName) const;
	void genEvents(const list<VexEvent> &eventList);
	void createJobs(vector<VexJob> &jobs, VexInterval &jobTimeRange, const VexData *V, double maxLength, double maxSize) const;
};

class VexData
{
private:
	VexExper exper;
	vector<VexSource> sources;
	vector<VexScan> scans;
	vector<VexMode> modes;
	vector<VexAntenna> antennas;
	vector<VexEOP> eops;

	list<VexEvent> events;
	string directory;

public:
	int sanityCheck();

	VexSource *newSource();
	VexScan *newScan();
	VexMode *newMode();
	VexAntenna *newAntenna();
	VexEOP *newEOP();
	void findLeapSeconds();
	void addBreaks(const vector<double> &breaks);

	double obsStart() const
	{
		return events.front().mjd;
	}

	double obsStop() const
	{
		return events.back().mjd;
	}

	const string &getDirectory() const { return directory; }
	void setDirectory(const string &dir) { directory = dir; }

	unsigned int nSource() const { return sources.size(); }
	int getSourceIdByDefName(const string &defName) const;
	const VexSource *getSource(unsigned int num) const;
	const VexSource *getSourceByDefName(const string &defName) const;
	const VexSource *getSourceBySourceName(const string &name) const;

	unsigned int nScan() const { return scans.size(); }
	const VexScan *getScan(unsigned int num) const;
	const VexScan *getScanByDefName(const string &defName) const;
	void setScanSize(unsigned int num, double size);
	void getScanList(list<string> &scans) const;

	unsigned int nAntenna() const { return antennas.size(); }
	const VexAntenna *getAntenna(unsigned int num) const;
	const VexAntenna *getAntenna(const string &name) const;

	unsigned int nMode() const { return modes.size(); }
	int getModeIdByDefName(const string &defName) const;
	const VexMode *getMode(unsigned int num) const;
	const VexMode *getModeByDefName(const string &defName) const;

	unsigned int nEOP() const { return eops.size(); }
	const VexEOP *getEOP(unsigned int num) const;
	const vector<VexEOP> &getEOPs() const { return eops; }

	bool usesAntenna(const string &antennaName) const;
	bool usesMode(const string &modeDefName) const;

	unsigned int nVSN(const string &antName) const;
	void addVSN(const string &antName, const string &vsn, const VexInterval &timeRange);
	string getVSN(const string &antName, const VexInterval &timeRange) const;

	unsigned int nEvent() const { return events.size(); }
	const list<VexEvent> *getEvents() const;
	void addEvent(double mjd, VexEvent::EventType eventType, const string &name);
	void addEvent(double mjd, VexEvent::EventType eventType, const string &name, const string &scanName);
	void sortEvents();

	const VexExper *getExper() const { return &exper; }
	void setExper(const string &name, const VexInterval &experTimeRange);
};

bool operator < (const VexEvent &a, const VexEvent &b);
ostream& operator << (ostream &os, const VexInterval &x);
ostream& operator << (ostream &os, const VexSource &x);
ostream& operator << (ostream &os, const VexScan &x);
ostream& operator << (ostream &os, const VexAntenna &x);
ostream& operator << (ostream &os, const VexSubband &x);
ostream& operator << (ostream &os, const VexChannel &x);
ostream& operator << (ostream &os, const VexIF &x);
ostream& operator << (ostream &os, const VexSetup &x);
ostream& operator << (ostream &os, const VexMode &x);
ostream& operator << (ostream &os, const VexEOP &x);
ostream& operator << (ostream &os, const VexBasebandFile &x);
ostream& operator << (ostream &os, const VexClock &x);
ostream& operator << (ostream &os, const VexJob &x);
ostream& operator << (ostream &os, const VexJobGroup &x);
ostream& operator << (ostream &os, const VexEvent &x);
ostream& operator << (ostream &os, const VexJobFlag &x);
ostream& operator << (ostream &os, const VexData &x);
bool operator == (const VexSubband &s1, const VexSubband &s2);

#endif
