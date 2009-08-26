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

#ifndef __VEXTABLES_H__
#define __VEXTABLES_H__

#include <vector>
#include <list>
#include <string>
#include <iostream>
#include <map>

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
	VexEvent(double m, enum EventType e, const string& a) : mjd(m), eventType(e), name(a), scan("") {}
	VexEvent(double m, enum EventType e, const string& a, const string& b) : mjd(m), eventType(e), name(a), scan(b) {}
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
	void setTimeRange(double start, double stop) { mjdStart = start; mjdStop = stop; }
	void setTimeRange(const VexInterval &v) { mjdStart = v.mjdStart; mjdStop = v.mjdStop; }
	void logicalAnd(double start, double stop);
	void logicalAnd(const VexInterval &v);
	void logicalOr(double start, double stop);
	void logicalOr(const VexInterval &v);
	bool contains(double mjd) { return (mjdStart <= mjd) && (mjd <= mjdStop); }
	bool isCausal() { return (mjdStart <= mjdStop); }
};

class VexBasebandFile : public VexInterval
{
	public:
	string filename;

	VexBasebandFile(string name, double start=-1.0e9, double stop=1.0e9) :
		VexInterval(start, stop), filename(name) {}
};

class VexScan : public VexInterval
{
public:
	string name;		// name of this scan

	string modeName;
	string sourceName;
	map<string,VexInterval> stations;
	string corrSetupName;	// points to CorrSetup entry
	double size;		// [bytes] approx. correlated size
};

class VexSource
{
public:
	VexSource() : calCode(' '), qualifier(0), ra(0.0), dec(0.0) {}

	string name;
	
	vector<string> sourceNames;
	char calCode;
	int qualifier;
	double ra;		// (rad)
	double dec;		// (rad)
};

class VexSubband	// Mode-specific frequency details
{
public:
	VexSubband(double f=0.0, double b=0.0, char s=' ', char p=' ') : 
		freq(f), bandwidth(b), sideBand(s), pol(p) {}

	double freq;		// (Hz)
	double bandwidth;	// (Hz)
	char sideBand;		// U or L
	char pol;		// R or L
};

class VexIF		// Antenna-specific baseband channel details
{
public:
	VexIF() : recordChan(0), subbandId(-1), phaseCal(0) {}

	int recordChan;		// channel number on recorded media
	int subbandId;		// 0-based index
	int phaseCal;		// (Hz), typically 1,000,000 or 5,000,000
};

class VexFormat
{
public:
	VexFormat() : nBit(0), nRecordChan(0) {}

	string name;

	string format;		// e.g. VLBA, MKIV, Mk5B, VDIF, LBA, K5, ...
	int nBit;
	int nRecordChan;	// number of recorded channels
	vector<VexIF> ifs;
};

class VexMode
{
public:
	VexMode() : sampRate(0.0) {}

	int addSubband(double freq, double bandwidth, char sideband, char pol);
	int getPols(char *pols) const;
	int getBits() const;
	const VexFormat &getFormat(const string antName) const;

	string name;

	double sampRate;		// (Hz)
	vector<VexSubband> subbands;
	map<string,VexFormat> formats;	// indexed by antenna name
};

class VexVSN : public VexInterval
{
public:
	string name;
	VexVSN() {}
	VexVSN(const string &c_name, const VexInterval& timeRange) : VexInterval(timeRange), name(c_name) {} 
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
	VexAntenna() : x(0.0), y(0.0), z(0.0), axisOffset(0.0) {}

	void getClock(double mjd, double &offset, double &rate) const;

	string name;
	string nameInVex;	// Sometimes names get changed

	double x, y, z;		// (m) antenna position
	double dx, dy, dz;	// (m/?) antenna position	//FIXME
	double posEpoch;	// mjd
	string axisType;
	double axisOffset;	// (m)
	vector<VexClock> clocks;
	vector<VexVSN> vsns;
	vector<VexBasebandFile> basebandFiles;
};

class VexEOP
{
public:
	VexEOP() : mjd(0), tai_utc(0), ut1_utc(0.0), xPole(0.0), yPole(0.0) {}

	double mjd;		// days
	double tai_utc;		// seconds
	double ut1_utc;		// seconds
	double xPole, yPole;	// radian

	void setkv(const string &key, const string &value);
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
	VexJob() : VexInterval(0.0, 1000000.0), jobSeries("Bogus"), jobId(-1), dataSize(0.0) {}

	void assignVSNs(const VexData& V);
	string getVSN(const string &antName) const;
	bool hasScan(const string &scanName) const;
	int generateFlagFile(const VexData& V, const string &fileName, unsigned int invalidMask=0xFFFFFFFF) const;

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
	VexJobFlag() {}
	VexJobFlag(double start, double stop, int ant) : VexInterval(start, stop), antId(ant) {}

	int antId;
};

class VexJobGroup : public VexInterval
{
public:
	vector<string> scans;
	list<VexEvent> events;

	bool hasScan(const string& scanName) const;
	void genEvents(const list<VexEvent>& eventList);
	void createJobs(vector<VexJob>& jobs, VexInterval& jobTimeRange, const VexData *V, double maxLength, double maxSize) const;
};

class VexData
{
private:
	VexAntenna &getAntenna(string name);

	VexExper exper;
	vector<VexSource> sources;
	vector<VexScan> scans;
	vector<VexMode> modes;
	vector<VexAntenna> antennas;
	vector<VexEOP> eops;

	list<VexEvent> events;
	string directory;

public:
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


	int nSource() const { return sources.size(); }
	const VexSource *getSource(const string name) const;
	const VexSource *getSource(int num) const;

	int nScan() const { return scans.size(); }
	const VexScan *getScan(const string name) const;
	const VexScan *getScan(int num) const;
	void setScanSize(int num, double size);
	void getScanList(list<string> &scans) const;

	int nAntenna() const { return antennas.size(); }
	const VexAntenna *getAntenna(const string name) const;
	const VexAntenna *getAntenna(int num) const;

	int nMode() const { return modes.size(); }
	const VexMode *getMode(const string name) const;
	const VexMode *getMode(int num) const;

	int nEOP() const { return eops.size(); }
	const VexEOP *getEOP(int num) const;
	const vector<VexEOP> &getEOPs() const { return eops; }

	bool usesAntenna(const string& antennaName) const;
	bool usesMode(const string& modeName) const;

	void addVSN(const string& antName, const string& vsn, const VexInterval& timeRange);
	string getVSN(const string& antName, const VexInterval& timeRange) const;

	int nEvent() const { return events.size(); }
	const list<VexEvent> *getEvents() const;
	void addEvent(double mjd, VexEvent::EventType eventType, const string &name);
	void addEvent(double mjd, VexEvent::EventType eventType, const string &name, const string &scanName);

	const VexExper *getExper() const { return &exper; }
	void setExper(const string& name, const VexInterval& experTimeRange);
};

bool operator<(const VexEvent &a, const VexEvent &b);
ostream& operator << (ostream& os, const VexInterval& x);
ostream& operator << (ostream& os, const VexSource& x);
ostream& operator << (ostream& os, const VexScan& x);
ostream& operator << (ostream& os, const VexAntenna& x);
ostream& operator << (ostream& os, const VexSubband& x);
ostream& operator << (ostream& os, const VexIF& x);
ostream& operator << (ostream& os, const VexFormat& x);
ostream& operator << (ostream& os, const VexMode& x);
ostream& operator << (ostream& os, const VexEOP& x);
ostream& operator << (ostream& os, const VexBasebandFile& x);
ostream& operator << (ostream& os, const VexVSN& x);
ostream& operator << (ostream& os, const VexClock& x);
ostream& operator << (ostream& os, const VexJob& x);
ostream& operator << (ostream& os, const VexJobGroup& x);
ostream& operator << (ostream& os, const VexEvent& x);
ostream& operator << (ostream& os, const VexJobFlag& x);
ostream& operator << (ostream& os, const VexData& x);
bool operator == (VexSubband& s1, VexSubband& s2);

#endif
