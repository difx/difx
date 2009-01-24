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

#include <vector>
#include <list>
#include <map>
#include <string>

using namespace std;

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
	void set(const string &key, const string &value);

	string vexName;		// Source name as appears in vex file
	string difxName;	// Source name (if different) to appear in difx
	char calCode;
	double ra, dec;		// in radians
	//vector<PhaseCenter> centers;
};

class AntennaSetup
{
public:
	AntennaSetup(const string &name);
	void set(const string &key, const string &value);

	string vexName;		// Antenna name as it appears in vex file
	string difxName;	// Antenna name (if different) to appear in difx
	double X, Y, Z;		// Station coordinates to override vex
	double clockOffset;	// [us] Override clock values in vex file.  All 3 clock ...
	double clockRate;	// [us/s]  ... parameters are required if any is present.
	double clockEpoch;	// [MJD]
	// flag
	// media
	double polSwap;		// If true, swap polarizations
};

class CorrSetup
{
public:
	CorrSetup(const string &name = "setup_default");
	void set(const string &key, const string &value);

	string setupName;

	double tInt;		// integration time
	int nChan;		// channels per sub-band
	bool doPolar;		// false for no cross pol, true for full pol
	bool doAuto;		// write autocorrelations
	int blocksPerSend;	// literal
	int specAvg;
	int startChan;
	bool postFFringe;	// fringe after FFT?
	string binConfigFile;
};


class CorrRule
{
public:
	CorrRule(const string &name = "rule_default");

	void set(const string &key, const string &value);
	bool match(const string &scan, const string &source, const string &mode, char cal, int qual) const;

	string ruleName;

	list<string> scanName;
	list<string> sourceName;
	list<string> modeName;
	list<char> calCode;
	list<int> qualifier;

	string setupName;	/* pointer to CorrSetup */
};

class CorrParams
{
public:
	CorrParams();
	CorrParams(const string& fileName);

	void loadShelves(const string& fileName);
	const char *getShelf(const string& vsn) const;

	void set(const string &key, const string &value);
	void load(const string& fileName);
	void defaults();
	void defaultSetup();
	void example();

	bool useAntenna(const string &antName) const;
	const CorrSetup *getCorrSetup(const string &name) const;
	const SourceSetup *getSourceSetup(const string &name) const;
	const AntennaSetup *getAntennaSetup(const string &name) const;

	const string &findSetup(const string &scan, const string &source, const string &mode, char cal, int qual) const;
	
	/* global parameters */
	string vexFile;
	double mjdStart;
	double mjdStop;
	unsigned int minSubarraySize;
	double maxGap;		// days
	bool singleScan;
	bool singleSetup;
	bool allowOverlap;
	bool mediaSplit;	// split jobs on media change
	bool padScans;
	double maxLength;	// days
	string jobSeries;	// prefix name to job files
	int startSeries;	// start job series at this number
	int dataBufferFactor;
	int nDataSegments;
	double sendLength;	// (s) amount of data to send from datastream to core at a time
	unsigned int invalidMask;

	list<string> antennaList;

	/* setups to apply */
	vector<CorrSetup> setups;

	/* rules to determine which setups to apply */
	vector<CorrRule> rules;

	/* source setups to apply */
	vector<SourceSetup> sourceSetups;

	/* antenna setups to apply */
	vector<AntennaSetup> antennaSetups;

private:
	void addAntenna(const string& antName);
	map<string,string> shelves;
};

ostream& operator << (ostream& os, const CorrSetup& x);
ostream& operator << (ostream& os, const CorrRule& x);
ostream& operator << (ostream& os, const CorrParams& x);

bool areCorrSetupsCompatible(const CorrSetup *A, const CorrSetup *B, const CorrParams *C);

#endif
