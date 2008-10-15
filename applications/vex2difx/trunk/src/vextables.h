#ifndef __VEXTABLES_H__
#define __VEXTABLES_H__

#include <vector>
#include <list>
#include <string>
#include <iostream>
#include <map>

using namespace std;

class VexEvent
{
public:
	enum EventType
	{
		NO_EVENT,
		OBSERVE_START,
		JOB_START,
		RECORD_START,
		SCAN_START,
		ANT_SCAN_START,
		ANT_SCAN_STOP,
		SCAN_STOP,
		RECORD_STOP,
		JOB_STOP,
		OBSERVE_STOP
	};

	static const char eventName[][20];

	double mjd;
	enum EventType eventType;
	string name;

	VexEvent() : mjd(0.0), eventType(NO_EVENT), name("") {}
	VexEvent(double m, enum EventType e, const string& a) : mjd(m), eventType(e), name(a) {}
};

class VexInterval
{
public:
	double mjdStart;
	double mjdEnd;
	VexInterval(double start=0.0, double end=0.0) : mjdStart(start), mjdEnd(end) {}
};

class VexScan
{
public:
	string name;		// name of this scan

	VexInterval timeRange;
	string modeName;
	string sourceName;
	map<string,VexInterval> stations;
	string setupName;	// points to CorrSetup entry
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
	VexIF() : phaseCal(0), recordChan(0), subbandId(-1) {}

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

	string name;

	double sampRate;
	vector<VexSubband> subbands;
	map<string,VexFormat> formats;	// indexed by antenna name
};

class VexVSN
{
public:
	VexVSN() : mjdStart(0.0), mjdEnd(0.0) {}

	string name;
	double mjdStart;
	double mjdEnd;
};

class VexAntenna
{
public:
	VexAntenna() : x(0.0), y(0.0), z(0.0), axisOffset(0.0), clockOffset(0.0), clockRate(0.0) {}

	string name;

	double x, y, z;		// (m) antenna position
	string axisType;
	double axisOffset;	// (m)
	double clockOffset;
	double clockRate;
	vector<VexVSN> vsns;
};

class VexEOP
{
public:
	VexEOP() : mjd(0), tai_utc(0), ut1_utc(0.0), xPole(0.0), yPole(0.0) {}

	double mjd;		// days
	double tai_utc;		// seconds
	double ut1_utc;		// seconds
	double xPole, yPole;	// radian
};

class VexExper
{
public:
	VexExper() : mjdStart(0.0), mjdStop(1000000.0) {}

	string name;
	double mjdStart;
	double mjdStop;
};

class VexJob
{
public:
	VexJob() : mjdStart(0.0), mjdStop(1000000.0), jobSeries("Bogus"), jobId(-1) {}

	string jobSeries;
	int jobId;
	vector<string> scans;
	map<string,string> vsns;	// vsn, indexed by antenna name
	double mjdStart;
	double mjdStop;
	double dutyCycle;		// fraction of job spent in scans
};

class VexJobGroup
{
public:
	vector<string> scans;
	list<VexEvent> events;

	bool hasScan(const string& scanName) const;
	void genEvents(const list<VexEvent>& eventList);
	void createJob(vector<VexJob>& jobs, double start, double stop) const;
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

public:
	VexSource *newSource();
	VexScan *newScan();
	VexMode *newMode();
	VexAntenna *newAntenna();
	VexEOP *newEOP();

	double obsStart() const
	{
		return events.front().mjd;
	}

	double obsStop() const
	{
		return events.back().mjd;
	}

	int nSource() const { return sources.size(); }
	const VexSource *getSource(string name) const;
	const VexSource *getSource(int num) const;

	int nScan() const { return scans.size(); }
	const VexScan *getScan(string name) const;
	const VexScan *getScan(int num) const;
	const VexScan *getScanByAntenna(string antName, double mjd) const;
	void getScanList(list<string> &scans) const;

	int nAntenna() const { return antennas.size(); }
	const VexAntenna *getAntenna(string name) const;
	const VexAntenna *getAntenna(int num) const;

	int nMode() const { return modes.size(); }
	const VexMode *getMode(string name) const;
	const VexMode *getMode(int num) const;

	int nEOP() const { return eops.size(); }
	const VexEOP *getEOP(int num) const;

	bool usesAntenna(const string& antennaName) const;
	bool usesMode(const string& modeName) const;

	void addVSN(const string& antName, const string& vsn, double mjdStart, double mjdStop);

	int nEvent() const { return events.size(); }
	const list<VexEvent> *getEvents() const;
	void addEvent(double mjd, VexEvent::EventType eventType, const string &antName);

	const VexExper *getExper() const { return &exper; }
	void setExper(const string& name, double start, double stop);
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
ostream& operator << (ostream& os, const VexVSN& x);
ostream& operator << (ostream& os, const VexJob& x);
ostream& operator << (ostream& os, const VexEvent& x);
ostream& operator << (ostream& os, const VexData& x);
bool operator == (VexSubband& s1, VexSubband& s2);

#endif
