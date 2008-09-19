
#include <vector>
#include <string>
#include <iostream>
#include <map>

#ifndef __VEXTABLES_H__
#define __VEXTABLES_H__

using namespace std;

class VexExper
{
public:
	string name;
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
};

class VexEOP
{
public:
	VexEOP() : mjd(0), tai_utc(0), ut1_utc(0.0), xPole(0.0), yPole(0.0) {}

	int mjd;
	int tai_utc;
	double ut1_utc;
	double xPole, yPole;
};

class VexData
{
public:
	VexSource *newSource();
	VexScan *newScan();
	VexMode *newMode();
	VexAntenna *newAntenna();
	VexEOP *newEOP();

	int nSource() const { return sources.size(); }
	const VexSource &getSource(string name) const;
	const VexSource &getSource(int num) const;

	int nScan() const { return scans.size(); }
	const VexScan &getScan(string name) const;
	const VexScan &getScan(int num) const;

	int nAntenna() const { return antennas.size(); }
	const VexAntenna &getAntenna(string name) const;
	const VexAntenna &getAntenna(int num) const;

	int nMode() const { return modes.size(); }
	const VexMode &getMode(string name) const;
	const VexMode &getMode(int num) const;

	int nEOP() const { return eops.size(); }
	const VexEOP &getEOP(int num) const;

	bool usesAntenna(const string& antennaName) const;
	bool usesMode(const string& modeName) const;
private:
	vector<VexSource> sources;
	vector<VexScan> scans;
	vector<VexMode> modes;
	vector<VexAntenna> antennas;
	vector<VexEOP> eops;

};

ostream& operator << (ostream& os, const VexInterval& x);
ostream& operator << (ostream& os, const VexSource& x);
ostream& operator << (ostream& os, const VexScan& x);
ostream& operator << (ostream& os, const VexAntenna& x);
ostream& operator << (ostream& os, const VexSubband& x);
ostream& operator << (ostream& os, const VexIF& x);
ostream& operator << (ostream& os, const VexFormat& x);
ostream& operator << (ostream& os, const VexMode& x);
ostream& operator << (ostream& os, const VexEOP& x);
ostream& operator << (ostream& os, const VexData& x);
bool operator == (VexSubband& s1, VexSubband& s2);

#endif
