/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
 * $Id: mark5_stream.c 777 2008-09-10 14:48:08Z HelgeRottmann $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision: 777 $
 * $Author: HelgeRottmann $
 * $LastChangedDate: 2008-09-10 08:48:08 -0600 (Wed, 10 Sep 2008) $
 *
 *==========================================================================*/

#ifndef __CORRPARAM_H__
#define __CORRPARAM_H__

#include <vector>
#include <list>
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
	char calcode;
	int qualifier;
};

class CorrSetup
{
public:
	CorrSetup(const string &name = "setup_default");

	string setupName;

	double tInt;		// integration time
	int nChan;		// channels per sub-band
	bool doPolar;		// false for no cross pol, true for full pol
	bool doAuto;		// write autocorrelations
	int blocksPerSend;	// literal
	int specAvg;
	int startChan;
	bool postFFringe;	// fringe after FFT?
	vector<PhaseCenter> centers;
};


class CorrRule
{
public:
	CorrRule(const string &name = "rule_default");

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

	void defaultSetup();
	void example();

	bool useAntenna(const string &antName) const;
	const CorrSetup *getCorrSetup(const string &name) const;

	const string &findSetup(const string &scan, const string &source, const string &mode, char cal, int qual) const;
	
	/* global parameters */
	double mjdStart;
	double mjdStop;
	int minSubarraySize;
	double maxGap;		// days
	bool singleScan;
	bool singleSetup;
	bool allowOverlap;
	bool mediaSplit;	// split jobs on media change
	double maxLength;	// days
	string jobSeries;	// prefix name to job files
	int startSeries;	// start job series at this number

	list<string> antennaList;

	/* setups to apply */
	vector<CorrSetup> setups;

	/* rules to determine which setups to apply */
	vector<CorrRule> rules;

};

ostream& operator << (ostream& os, const CorrSetup& x);
ostream& operator << (ostream& os, const CorrRule& x);
ostream& operator << (ostream& os, const CorrParams& x);

bool areCorrSetupsCompatible(const CorrSetup *A, const CorrSetup *B, const CorrParams *C);

#endif
