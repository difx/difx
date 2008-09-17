
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
	VexInterval(double start=0.0, double end=0.0) : mjdStart(start), mjdEnd(end) {};
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
	double freq;		// (Hz)
	double bandwidth;	// (Hz)
	char sideBand;		// U or L
	char pol;		// R or L
	bool isDummy;		// TRUE if this is included for padding
};

class VexIF		// Antenna-specific baseband channel details
{
public:
	int phaseCal;		// (Hz), typically 1,000,000 or 5,000,000
	int recordChan;		// channel number on recorded media
	int subBandId;
};

class VexFormat
{
public:
	string name;

	string format;		// e.g. VLBA, MKIV, Mk5B, VDIF, LBA, K5, ...
	int nBit;
	int nRecordChan;	// number of recorded channels
	vector<VexIF> ifs;
};

class VexMode
{
public:
	string name;

	double sampRate;
	vector<VexSubband> subBands;
	map<string,VexFormat> formats;	// indexed by antenna name

};

class VexAntenna
{
public:
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
private:
	vector<VexSource> sources;
	vector<VexScan> scans;
	vector<VexMode> modes;
	vector<VexAntenna> antennas;
	vector<VexEOP> eops;

};

ostream& operator << (ostream& os, const VexInterval& x);
ostream& operator << (ostream& os, const VexScan& x);
ostream& operator << (ostream& os, const VexData& x);

#endif
