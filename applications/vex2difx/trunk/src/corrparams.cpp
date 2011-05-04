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

#define _XOPEN_SOURCE
#include <iostream>
#include <iomanip>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <cstdio>
#include <cmath>
#include <cctype>
#include <ctime>
#include <string.h>
#include <unistd.h>
#include <difxio.h>
#include "util.h"
#include "corrparams.h"

const double MJD_UNIX0 = 40587.0;	// MJD at beginning of unix time
const double SEC_DAY = 86400.0;
const double MUSEC_DAY = 86400000000.0;

/* round to nearest second */
static double roundSeconds(double mjd)
{
	int intmjd, intsec;

	intmjd = static_cast<int>(mjd);
	intsec = static_cast<int>((mjd - intmjd)*86400.0 + 0.5);

	return intmjd + intsec/86400.0;
}

bool isTrue(const string &str)
{
	if(str[0] == '0' || str[0] == 'f' || str[0] == 'F' || str[0] == '-')
	{
		return false;
	}
	else
	{
		return true;
	}
}

// Turns a string into MJD 
// The following formats are allowd:
// 1. decimal mjd:                 
// 2. ISO 8601 dateTtime strings:  2009-03-08T12:34:56.121
// 3. VLBA-like time               2009MAR08-12:34:56.121
// 4. vex time
double parseTime(const string &timeStr)
{
	const int TimeLength=54;
	double mjd;
	char str[TimeLength];
	char *p;
	double t;
	int n;
	struct tm tm;
	char dummy;

	snprintf(str, TimeLength, "%s", timeStr.c_str());

	// Test for ISO 8601
	p = strptime(str, "%FT%T", &tm);
	if(!p)
	{
		//Test for VLBA-like
		p = strptime(str, "%Y%b%d-%T", &tm);
	}
	if(!p)
	{
		//Test for Vex
		p = strptime(str, "%Yy%jd%Hh%Mm%Ss", &tm);
	}
	if(p)
	{
		t = mktime(&tm);
		mjd = t/86400.0 + MJD_UNIX0;

		return mjd;
	}

	n = sscanf(str, "%lf%c", &mjd, &dummy);
	if(n == 1)
	{
		// Must be straight MJD value
		return mjd;
	}

	// No match
	cerr << endl;
	cerr << "Error: date not parsable: " << timeStr << endl;
	cerr << endl;
	cerr << "Allowable formats are:" << endl;
	cerr << "1. Straight MJD        54345.341944" << endl;
	cerr << "2. Vex formatted date  2009y245d08h12m24s" << endl;
	cerr << "3. VLBA-like format    2009SEP02-08:12:24" << endl;
	cerr << "4. ISO 8601 format     2009-09-02T08:12:24" << endl;
	cerr << endl;

	exit(0);
}

double parseCoord(const char *str, char type)
{
	int sign = 1, l, n;
	double a, b, c;
	double v = -999999.0;

	if(type != ' ' && type != 'R' && type != 'D')
	{
		cerr << "Programmer error: parseCoord: parameter 'type' has illegal value = " << type << endl;
		
		exit(0);
	}

	if(str[0] == '-')
	{
		sign = -1;
		str++;
	}
	else if(str[0] == '+')
	{
		str++;
	}

	l = strlen(str);

	if(sscanf(str, "%lf:%lf:%lf", &a, &b, &c) == 3)
	{
		v = sign*(a + b/60.0 + c/3600.0);
		if(type == 'D')
		{
			v *= M_PI/180.0;
		}
		else
		{
			v *= M_PI/12.0;
		}
	}
	else if(sscanf(str, "%lfh%lfm%lf", &a, &b, &c) == 3 && str[l-1] == 's' && type != 'D')
	{
		v = sign*(a + b/60.0 + c/3600.0);
		v *= M_PI/12.0;
	}
	else if(sscanf(str, "%lfd%lf'%lf\"", &a, &b, &c) == 3 && str[l-1] == '"' && type == 'D')
	{
		v = sign*(a + b/60.0 + c/3600.0);
		v *= M_PI/180.0;
	}
	else if(sscanf(str, "%lf%n", &a, &n) == 1)
	{
		if(n == l)
		{
			v = a;
		}
		else if(strcmp(str+n, "rad") == 0)
		{
			v = a;
		}
		else if(strcmp(str+n, "deg") == 0)
		{
			v = a*M_PI/180.0;
		}
		else
		{
			cerr << "Error parsing coordinate value " << str << endl;

			exit(0);
		}
		v *= sign;
	}

	return v;
}

// From http://oopweb.com/CPP/Documents/CPPHOWTO/Volume/C++Programming-HOWTO-7.html
void split(const string &str, vector<string> &tokens, const string &delimiters = " ")
{
	// Skip delimiters at beginning.
	string::size_type lastPos = str.find_first_not_of(delimiters, 0);
	// Find first "non-delimiter".
	string::size_type pos     = str.find_first_of(delimiters, lastPos);

	while (string::npos != pos || string::npos != lastPos)
	{
		// Found a token, add it to the vector.
		tokens.push_back(str.substr(lastPos, pos - lastPos));
		// Skip delimiters.  Note the "not_of"
		lastPos = str.find_first_not_of(delimiters, pos);
		// Find next "non-delimiter"
		pos = str.find_first_of(delimiters, lastPos);
	}
}


int loadBasebandFilelist(const string &fileName, vector<VexBasebandFile> &basebandFiles)
{
	const int MaxLineLength=1024;
	ifstream is;
	int n=0;
	char s[MaxLineLength];
	vector<string> tokens;

	is.open(fileName.c_str());

	if(is.fail())
	{
		cerr << "Error: cannot open " << fileName << endl;

		exit(0);
	}

	for(int line=1; ; line++)
	{
		is.getline(s, MaxLineLength);
		if(is.eof())
		{
			break;
		}

		for(int i = 0; s[i]; i++)
		{
			if(s[i] == '#')
			{
				s[i] = 0;
			}
		}

		tokens.clear();

		split(string(s), tokens);

		int l = tokens.size();

		if(l == 0)
		{
			continue;
		}
		else if(l == 1)
		{
			basebandFiles.push_back(VexBasebandFile(tokens[0]));
			n++;
		}
		else if(l == 3)
		{
			basebandFiles.push_back(VexBasebandFile(tokens[0],
				parseTime(tokens[1]),
				parseTime(tokens[2]) ));
			n++;
		}
		else
		{
			cerr << "Error: line " << line << " of file " << fileName << " is badly formatted" << endl;

			exit(0);
		}
	}

	return n;
}

CorrSetup::CorrSetup(const string &name) : corrSetupName(name)
{
	tInt = 2.0;
	specAvgDontUse = 0;		// If set, this will trigger a bail out condition
	nFFTChan = 128;
	nOutputChan = 16;
	doPolar = true;
	doAuto = true;
	fringeRotOrder = 1;
	strideLength = 0;
	xmacLength = 0;
	explicitXmacLength = false;
	explicitnFFTChan = false;
	numBufferedFFTs = 1;
	subintNS = 0;
	guardNS = 1000;
	maxNSBetweenUVShifts = 2000000000;
	maxNSBetweenACAvg = 0;		// zero means set to default in vex2difx.cpp
}

int CorrSetup::fftSize() const
{
	return nFFTChan*2;
}

int CorrSetup::nInputChan() const
{
	return nFFTChan;
}

int CorrSetup::setkv(const string &key, const string &value)
{
	stringstream ss;
	int nWarn = 0;
	char *ptr;

	ss << value;

	if(key == "VEX_rev")
	{
		cerr << "Error: You are running vex2difx on a vex file." << endl;
		cerr << "Please run on a vex2difx input file (.v2d) instead." << endl;

		exit(0);
	}
	else if(key == "tInt")
	{
		ss >> tInt;
	}
	else if(key == "nChan")
	{
		ss >> nOutputChan;
	}
	else if(key == "nFFTChan")
	{
		ss >> nFFTChan;
		explicitnFFTChan = true;
	}
	else if(key == "doPolar")
	{
		doPolar = isTrue(value);
	}
	else if(key == "doAuto")
	{
		doAuto = isTrue(value);
	}
	else if(key == "subintNS")
	{
		ss >> subintNS;
	}
	else if(key == "guardNS")
	{
		ss >> guardNS;
	}
	else if(key == "maxNSBetweenUVShifts")
	{
		ss >> maxNSBetweenUVShifts;
	}
	else if(key == "maxNSBetweenACAvg")
	{
		ss >> maxNSBetweenACAvg;
	}
	else if(key == "specAvg")
	{
		ss >> specAvgDontUse;
	}
	else if(key == "fringeRotOrder")
	{
		ss >> fringeRotOrder;
	}
	else if(key == "strideLength")
	{
		ss >> strideLength;
	}
	else if(key == "xmacLength")
	{
		ss >> xmacLength;
		explicitXmacLength = true;
	}
	else if(key == "numBufferedFFTs")
	{
		ss >> numBufferedFFTs;
	}
	else if(key == "binConfig")
	{
		ss >> binConfigFile;
		
		if(binConfigFile[0] != '/')
		{
			char cwd[DIFXIO_FILENAME_LENGTH];
			string inFile;

			ptr = getcwd(cwd, DIFXIO_FILENAME_LENGTH-1);
			if(ptr == 0)
			{
				cerr << "Cannot getcwd()" << endl;

				exit(0);
			}
			inFile = string(cwd);
			inFile += string("/");
			inFile += binConfigFile;
			binConfigFile = inFile;
		}
	}
	else if(key == "phasedArray")
	{
		ss >> phasedArrayConfigFile;

		if(phasedArrayConfigFile[0] != '/')
		{
			char cwd[DIFXIO_FILENAME_LENGTH];
			string inFile;

			ptr = getcwd(cwd, DIFXIO_FILENAME_LENGTH-1);
			if(ptr == 0)
			{
				cerr << "Cannot getcwd()" << endl;

				exit(0);
			}
			inFile = string(cwd);
			inFile += string("/");
			inFile += binConfigFile;
			binConfigFile = inFile;
		}
	}
	else if(key == "freqId" || key == "freqIds")
	{
		int freqId;
		ss >> freqId;
		addFreqId(freqId);
	}
	else
	{
		cerr << "Warning: SETUP: Unknown parameter '" << key << "'." << endl; 
		nWarn++;
	}

	return nWarn;
}

void CorrSetup::addFreqId(int freqId)
{
	freqIds.insert(freqId);
}

bool CorrSetup::correlateFreqId(int freqId) const
{
	if(freqIds.size() == 0)
	{
		return true;
	}
	else
	{
		return (freqIds.find(freqId) != freqIds.end());
	}
}

int CorrSetup::checkValidity() const
{
	int nwarn = 0;

	if(nInputChan() < strideLength)
	{
		cerr << "Array stride length " << strideLength << " is greater than the input number of channels " << nInputChan() << endl;
		cerr << "Probably you need to reduce the strideLength parameter" << endl;
		nwarn++;
	}

	if(strideLength > 0 && nInputChan() % strideLength != 0)
	{
		cerr << "Array stride length " << strideLength << " does not divide evenly into input number of channels " << nInputChan() << endl;
		cerr << "Probably you need to reduce the strideLength parameter" << endl;
		nwarn++;
	}

	if(nInputChan() < xmacLength)
	{
		cerr << "XMAC stride length " << xmacLength << " is greater than the input number of channels " << nInputChan() << endl;
		cerr << "Probably you need to reduce the xmacLength parameter" << endl;
		nwarn++;
	}

	if(specAvgDontUse != 0)
	{
		cerr << "Paramaeter specAvg is no longer supported starting with DiFX 2.0.1.  Instead use combinations of nChan and nFFTChan to achieve your goals." << endl;
		cerr << "If you meant nChan to be the number of output channels (after spectral averaging), then you should set nFFTChan=" << nOutputChan*specAvgDontUse << " and nChan=" << nOutputChan << "; if you meant for nChan to be the number of input channels (before spectral averaging), then you should set nFFTChan=" << nOutputChan << " and nChan=" << nOutputChan/specAvgDontUse << endl;
		exit(0);

	}

	if(nInputChan() % nOutputChan != 0)
	{
		cerr << "Error: nFFTChan must be an integer multiple of nChan.  Values found were nFFTChan=" << nFFTChan << " nChan=" << nOutputChan << endl;

		exit(0);
	}

	if(xmacLength > 0)
	{
		if(nInputChan() % xmacLength != 0)
		{
			cerr << "XMAC stride length " << xmacLength << " does not divide evenly into input number of channels " << nInputChan() << endl;
			cerr << "Probably you need to reduce the xmacLength parameter" << endl;
			nwarn++;
		}
	}

	return nwarn;
}

double CorrSetup::bytesPerSecPerBLPerBand() const
{
	int pols = doPolar ? 2 : 1;

	// assume 8 bytes per complex

	return 8*nOutputChan*pols/tInt;
}

CorrRule::CorrRule(const string &name) : ruleName(name)
{
}

bool CorrRule::match(const string &scan, const string &source, const string &mode, char cal, int qual) const
{
	if(!scanName.empty() && find(scanName.begin(), scanName.end(), scan) == scanName.end())
	{
		return false;
	}
	if(!sourceName.empty() && find(sourceName.begin(), sourceName.end(), source) == sourceName.end())
	{
		return false;
	}
	if(!modeName.empty() && find(modeName.begin(), modeName.end(), mode) == modeName.end())
	{
		return false;
	}
	if(!calCode.empty() && find(calCode.begin(), calCode.end(), cal) == calCode.end())
	{
		return false;
	}

	return true;
}

int CorrRule::setkv(const string &key, const string &value)
{
	stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "scanName" || key == "scan")
	{
		string s;

		ss >> s;
		scanName.push_back(s);
	}
	else if(key == "sourceName" || key == "source")
	{
		string s;
		
		ss >> s;
		sourceName.push_back(s);
	}
	else if(key == "modeName" || key == "mode")
	{
		string s;
		
		ss >> s;
		modeName.push_back(s);
	}
	else if(key == "calCode")
	{
		char c;
		
		ss >> c;
		calCode.push_back(c);
	}
	else if(key == "qualifier")
	{
		int i;
		
		ss >> i;
		qualifier.push_back(i);
	}
	else if(key == "setupName" || key == "setup")
	{
		ss >> corrSetupName;
	}
	else
	{
		cerr << "Warning: RULE: Unknown parameter '" << key << "'." << endl; 
		nWarn++;
	}

	return nWarn;
}

const string PhaseCentre::DEFAULT_NAME = "";

PhaseCentre::PhaseCentre()
{
	initialise(DEFAULT_RA, DEFAULT_DEC, DEFAULT_NAME);
}

PhaseCentre::PhaseCentre(double r, double d, string name)
{
	initialise(r, d, name);
}

void PhaseCentre::initialise(double r, double d, string name)
{
	ra = r;
	dec = d;
	difxName = name;
	calCode = ' ';
	ephemDeltaT = 60.0; //seconds
	qualifier = 0;
	string ephemObject = "";
	string ephemFile = "";
	string naifFile = "";  
}

SourceSetup::SourceSetup(const string &name) : vexName(name)
{
	doPointingCentre = true;
	pointingCentre.difxName = name;
}

int SourceSetup::setkv(const string &key, const string &value)
{
	return setkv(key, value, &pointingCentre);
}

int SourceSetup::setkv(const string &key, const string &value, PhaseCentre * pc)
{
	string::size_type at, last, splitat;
	string nestedkeyval;
	stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "ra" || key == "RA")
	{
		if(pc->ra > PhaseCentre::DEFAULT_RA)
		{
			cerr << "Warning: Source " << vexName << " has multiple RA assignments" << endl;
			nWarn++;
		}
		pc->ra = parseCoord(value.c_str(), 'R');
	}
	else if(key == "dec" || key == "Dec")
	{
		if(pc->dec > PhaseCentre::DEFAULT_DEC)
		{
			cerr << "Warning: Source " << vexName << " has multiple Dec assignments" << endl;
			nWarn++;
		}
		pc->dec = parseCoord(value.c_str(), 'D');
	}
	else if(key == "calCode")
	{
		ss >> pc->calCode;
	}
	else if(key == "name" || key == "newName")
	{
		ss >> pc->difxName;
	}
	else if(key == "ephemObject")
	{
		ss >> pc->ephemObject;
	}
	else if(key == "ephemFile")
	{
		ss >> pc->ephemFile;
	}
	else if(key == "naifFile")
	{
		ss >> pc->naifFile;
	}
	else if(key == "doPointingCentre" || key == "doPointingCenter")
	{
		if(value == "true" || value == "True" || value == "TRUE" || value == "t" || value == "T")
		{
			doPointingCentre = true;
		}
		else
		{
			doPointingCentre = false;
		}
	}
	else if(key == "addPhaseCentre" || key == "addPhaseCenter")
	{
		// This is a bit tricky.  All parameters must be together, with @ replacing =, and separated by /
		// e.g., addPhaseCentre = name@1010-1212/RA@10:10:21.1/Dec@-12:12:00.34
		phaseCentres.push_back(PhaseCentre());
		PhaseCentre * newpc = &(phaseCentres.back());
		last = 0;
		at = 0;
		while(at !=string::npos)
		{
			at = value.find_first_of('/', last);
			nestedkeyval = value.substr(last, at-last);
			splitat = nestedkeyval.find_first_of('@');
			setkv(nestedkeyval.substr(0,splitat), nestedkeyval.substr(splitat+1), newpc);
			last = at+1;
		}
	}
	else
	{
		cerr << "Warning: SOURCE: Unknown parameter '" << key << "'." << endl; 
		nWarn++;
	}

	return nWarn;
}

ZoomFreq::ZoomFreq()
{
        initialise(-999, -999, false, -1);
}

void ZoomFreq::initialise(double freq, double bw, bool corrparent, int specavg)
{
        frequency = freq*1000000; //convert to Hz
	bandwidth = bw*1000000; //convert to Hz
	correlateparent = corrparent;
	spectralaverage = specavg;
}

AntennaSetup::AntennaSetup(const string &name) : vexName(name)
{
	polSwap = false;
	X = 0.0;
	Y = 0.0;
	Z = 0.0;
	deltaClock = 0.0;
	deltaClockRate = 0.0;
	clock.mjdStart = -1e9;
	clockorder = 1;
	clock2 = 0.0;
	clock3 = 0.0;
	clock4 = 0.0;
	clock5 = 0.0;
	networkPort = 0;
	windowSize = 0;
	phaseCalIntervalMHz = -1;
	toneGuardMHz = -1.0;
	toneSelection = ToneSelectionSmart;
	tcalFrequency = -1;
	dataSource = DataSourceNone;
	dataSampling = NumSamplingTypes;	/* flag that no sampling is is identified here */
}

int AntennaSetup::setkv(const string &key, const string &value, ZoomFreq * zoomFreq)
{
	int nWarn = 0;

	if(key == "freq" || key == "FREQ")
        {
                zoomFreq->frequency = atof(value.c_str())*1000000; //convert to Hz
        }
	else if(key == "bw" || key == "BW")
        {
                zoomFreq->bandwidth = atof(value.c_str())*1000000; //convert to Hz
        }
	else if(key == "noparent" || key == "NOPARENT")
        {
		if(value == "TRUE" || value == "True" || value == "true")
	                zoomFreq->correlateparent = false;
		else
			zoomFreq->correlateparent = true;
        }
	else if(key == "specAvg" || key == "SPECAVG" || key == "specavg")
        {
                zoomFreq->spectralaverage = atoi(value.c_str());
        }
	else
	{
		cerr << "Warning: ANTENNA: Unknown parameter '" << key << "'." << endl; 
		nWarn++;
	}

	return nWarn;
}

int AntennaSetup::setkv(const string &key, const string &value)
{
	string::size_type at, last, splitat;
        string nestedkeyval;
	stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "name" || key == "newName")
	{
		if(vexName == "DEFAULT")
		{
			cerr << "Error: renaming the DEFAULT antenna setup is not allowed" << endl;
			
			exit(0);
		}
		ss >> difxName;
	}
	else if(key == "polSwap")
	{
		polSwap = isTrue(value);
	}
	else if(key == "clockOffset" || key == "clock0")
	{
		if(clock.offset != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clockOffset definitions" << endl;
			nWarn++;
		}
		ss >> clock.offset;
		clock.offset /= 1.0e6;	// convert from us to sec
		clock.mjdStart = 1;
	}
	else if(key == "sampling")
	{
		dataSampling = stringToSamplingType(value.c_str());
		if(dataSampling >= NumSamplingTypes)
		{
			cerr << "Error: antenna " << vexName << " has illegal samping type set: " << value << endl;

			exit(0);
		}
	}
	else if(key == "clockRate" || key == "clock1")
	{
		if(clock.rate != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clockRate definitions" << endl;
			nWarn++;
		}
		ss >> clock.rate;
		clock.rate /= 1.0e6;	// convert from us/sec to sec/sec
		clock.mjdStart = 1;
	}
	else if(key == "clock2")
	{
		if(clock2 != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clock2 definitions" << endl;
			nWarn++;
		}

		ss >> clock2;
		if(clockorder < 2)
		{
			clockorder = 2;
		}
		clock2 /= 1.0e6;	// convert from us/sec^2 to sec/sec^2
	}
	else if(key == "clock3")
	{
		if(clock3 != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clock3 definitions" << endl;
			nWarn++;
		}

		ss >> clock3;
		if(clockorder < 3)
		{
			clockorder = 3;
		}
		clock3 /= 1.0e6;	// convert from us/sec^3 to sec/sec^3
	}
	else if(key == "clock4")
	{
		if(clock4 != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clock4 definitions" << endl;
			nWarn++;
		}

		ss >> clock4;
		if(clockorder < 4)
		{
			clockorder = 4;
		}
		clock4 /= 1.0e6;	// convert from us/sec^4 to sec/sec^4
	}
	else if(key == "clock5")
	{
		if(clock5 != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clock5 definitions" << endl;
			nWarn++;
		}

		ss >> clock5;
		if(clockorder < 5)
		{
			clockorder = 5;
		}
		clock5 /= 1.0e6;	// convert from us/sec^5 to sec/sec^5
	}
	else if(key == "clockEpoch")
	{
		if(clock.offset_epoch > 50001.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple clockEpoch definitions" << endl;
			nWarn++;
		}
		clock.offset_epoch = parseTime(value);
		clock.mjdStart = 1;
	}
	else if(key == "deltaClock")
	{
		if(deltaClock != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple deltaClock definitions" << endl;
			nWarn++;
		}
		ss >> deltaClock;
		deltaClock /= 1.0e6;	// convert from us to sec
	}
	else if(key == "deltaClockRate")
	{
		if(deltaClockRate != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple deltaClockRate definitions" << endl;
			nWarn++;
		}
		ss >> deltaClockRate;
		deltaClockRate /= 1.0e6;	// convert from us/sec to sec/sec
	}
	else if(key == "X" || key == "x")
	{
		if(X != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple X definitions" << endl;
			nWarn++;
		}
		ss >> X;
	}
	else if(key == "Y" || key == "y")
	{
		if(Y != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple Y definitions" << endl;
			nWarn++;
		}
		ss >> Y;
	}
	else if(key == "Z" || key == "z")
	{
		if(Z != 0.0)
		{
			cerr << "Warning: antenna " << vexName << " has multiple Z definitions" << endl;
			nWarn++;
		}
		ss >> Z;
	}
	else if(key == "format")
	{
		string s;
		ss >> s;
		Upper(s);

		if(s == "MARK4")
		{
			s = "MKIV";
		}

		format = s;
	}
	else if(key == "file" || key == "files")
	{
		if(dataSource != DataSourceFile && dataSource != DataSourceNone)
		{
			cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceFile] << endl;
			nWarn++;
		}
		dataSource = DataSourceFile;
		basebandFiles.push_back(VexBasebandFile(value));
	}
	else if(key == "filelist")
	{
		if(dataSource != DataSourceFile && dataSource != DataSourceNone)
		{
			cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceFile] << endl;
			nWarn++;
		}
		dataSource = DataSourceFile;
		loadBasebandFilelist(value, basebandFiles);
	}
	else if(key == "networkPort")
	{
		if(dataSource != DataSourceNetwork && dataSource != DataSourceNone)
		{
			cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceNetwork] << endl;
			nWarn++;
		}
		dataSource = DataSourceNetwork;
		ss >> networkPort;
	}
	else if(key == "windowSize")
	{
		if(dataSource != DataSourceNetwork && dataSource != DataSourceNone)
		{
			cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceNetwork] << endl;
			nWarn++;
		}
		dataSource = DataSourceNetwork;
		ss >> windowSize;
	}
	else if(key == "UDP_MTU")
	{
		if(dataSource != DataSourceNetwork && dataSource != DataSourceNone)
		{
			cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceNetwork] << endl;
			nWarn++;
		}
		dataSource = DataSourceNetwork;
		ss >> windowSize;
		windowSize = -windowSize;
	}
	else if(key == "module" || key == "vsn")
	{
		if(dataSource == DataSourceModule)
		{
			cerr << "Warning: antenna " << vexName << " has multiple vsns assigned to it.  Only using the last one = " << value << " and discarding " << basebandFiles[0].filename << endl;
		}
		else if(dataSource != DataSourceNone)
		{
			cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceFile] << endl;
			nWarn++;
		}
		dataSource = DataSourceModule;
		basebandFiles.clear();
		basebandFiles.push_back(VexBasebandFile(value));
	}
	else if(key == "phaseCalInt")
	{
		ss >> phaseCalIntervalMHz;
	}
	else if(key == "toneGuard")
	{
		ss >> toneGuardMHz;
	}
	else if(key == "toneSelection")
	{
		string ts;
		ss >> ts;
		toneSelection = stringToToneSelection(ts.c_str());
		if(toneSelection == ToneSelectionUnknown)
		{
			cerr << "Error: antenna " << vexName << " unsupported value of toneSelection (" << ts << ") provided." << endl;
			nWarn++;
			toneSelection = ToneSelectionVex;
		}
	}
	else if(key == "tcalFreq")
	{
		ss >> tcalFrequency;
	}
	else if(key == "freqClockOffs")
	{
		double d;

		ss >> d;
		freqClockOffs.push_back(d);
	}
	else if(key =="loOffsets")
	{
		double d;
		
		ss >> d;
		loOffsets.push_back(d);
	}
	else if(key == "addZoomFreq")
	{
		// This is a bit tricky.  All parameters must be together, with @ replacing =, and separated by /
                // e.g., addZoomFreq = freq@1649.99/bw@1.0/correlateparent@TRUE/specAvg@8
		// only freq and bw are compulsory; default is parent values and don't correlate parent
                zoomFreqs.push_back(ZoomFreq());
                ZoomFreq * newfreq = &(zoomFreqs.back());
                last = 0;
                at = 0;
                while(at != string::npos)
                {
                        at = value.find_first_of('/', last);
                        nestedkeyval = value.substr(last, at-last);
                        splitat = nestedkeyval.find_first_of('@');
                        nWarn += setkv(nestedkeyval.substr(0,splitat), nestedkeyval.substr(splitat+1), newfreq);
                        last = at+1;
                }
	}
	else
	{
		cerr << "Warning: ANTENNA: Unknown parameter '" << key << "'." << endl; 
		nWarn++;
	}

	return nWarn;
}

CorrParams::CorrParams()
{
	defaults();
}

CorrParams::CorrParams(const string &fileName)
{
	size_t pos;

	defaults();

	pos = fileName.find(".");
	jobSeries = fileName.substr(0, pos);

	parseWarnings = load(fileName);

#ifdef DONT_USE_EXPER_AS_PASS
	pos = vexFile.find(".");
	string vexBase = jobSeries.substr(0, pos);
	if(vexBase == jobSeries)
	{
		jobSeries = "main";
	}
#endif
}

void CorrParams::defaults()
{
	jobSeries = "job";
	threadsFile = "";
	minSubarraySize = 2;
	maxGap = 180.0/86400.0;		// 3 minutes
	singleScan = false;
	singleSetup = true;
	allowOverlap = false;
	mediaSplit = true;
	padScans = true;
	simFXCORR = false;
	maxLength = 7200/86400.0;	// 2 hours
	minLength = 2/86400.0;		// 2 seconds
	maxSize = 2e9;			// 2 GB
	mjdStart = 0.0;
	mjdStop = 1.0e7;
	startSeries = 1;
	dataBufferFactor = 32;
	nDataSegments = 8;
	readSize = 25000000;		// Bytes
	invalidMask = ~0;		// write flags for all types of invalidity
	visBufferLength = 32;
	v2dMode = V2D_MODE_NORMAL;
	overSamp = 0;
	outputFormat = OutputFormatDIFX;
	nCore = 0;
	nThread = 0;
}

void pathify(string &filename)
{
	if(filename[0] == '/')
	{
		return;
	}

	char cwd[DIFXIO_FILENAME_LENGTH];
	string fn;

	if(getcwd(cwd, DIFXIO_FILENAME_LENGTH-1) == 0)
	{
		cerr << "Cannot getcwd()" << endl;

		exit(0);
	}
	fn = string(cwd) + string("/") + filename;

	filename = fn;
}

int CorrParams::setkv(const string &key, const string &value)
{
	stringstream ss;
	int nWarn = 0;

	ss << value;
	
	if(key == "vex")
	{
		ss >> vexFile;
		pathify(vexFile);
	}
	else if(key == "threadsFile")
	{
		ss >> threadsFile;
		pathify(threadsFile);
		
	}
	else if(key == "mjdStart" || key == "start")
	{
		mjdStart = parseTime(value);
	}
	else if(key == "mjdStop" || key == "stop")
	{
		mjdStop = parseTime(value);
	}
	else if(key == "break" || key == "breaks")
	{
		double mjd = parseTime(value);

		/* always break at integer second boundary */
		mjd = roundSeconds(mjd);

		manualBreaks.push_back(mjd);
	}
	else if(key == "minSubarray")
	{
		ss >> minSubarraySize;
	}
	else if(key == "maxGap")
	{
		ss >> maxGap;
		maxGap /= 86400.0;	// convert to seconds from days
	}
	else if(key == "singleScan")
	{
		singleScan = isTrue(value);
	}
	else if(key == "nCore")
	{
		ss >> nCore;
	}
	else if(key == "nThread")
	{
		ss >> nThread;
	}
	else if(key == "singleSetup")
	{
		singleSetup = isTrue(value);
	}
	else if(key == "allowOverlap")
	{
		allowOverlap = isTrue(value);
	}
	else if(key == "mediaSplit")
	{
		mediaSplit = isTrue(value);
	}
	else if(key == "maxLength")
	{
		ss >> maxLength;
		maxLength /= 86400.0;	// convert to seconds from days
	}
	else if(key == "minLength")
	{
		ss >> minLength;
		minLength /= 86400.0;	// convert to seconds from days
	}
	else if(key == "maxSize")
	{
		ss >> maxSize;
		maxSize *= 1000000.0;	// convert to bytes from MB
	}
	else if(key == "jobSeries" || key == "pass")
	{
		for(unsigned int i = 0; i < value.size(); i++)
		if(!isalnum(value[i]))
		{
			cerr << "Error: jobSeries must be purely alphanumeric" << endl;

			exit(0);	
		}
		ss >> jobSeries;
	}
	else if(key == "startSeries")
	{
		ss >> startSeries;
		if(startSeries < 0)
		{
			cerr << "Error: startSeries cannot be < 0" << endl;
			
			exit(0);
		}
	}
	else if(key == "dataBufferFactor")
	{
		ss >> dataBufferFactor;
	}
	else if(key == "nDataSegments")
	{
		ss >> nDataSegments;
	}
	else if(key == "readSize")
	{
		ss >> readSize;
	}
	else if(key == "padScans")
	{
		padScans = isTrue(value);
	}
	else if(key == "invalidMask")
	{
	  stringstream sss;
	  
	        sss << setbase(16) << value;
		sss >> invalidMask;
	}
	else if(key == "visBufferLength")
	{
		ss >> visBufferLength;
	}
	else if(key == "simFXCORR")
	{
		simFXCORR = isTrue(value);
	}
	else if(key == "tweakIntTime")
	{
		/* No longer does anything */
	}
	else if(key == "antennas")
	{
		string s;
		ss >> s;
		Upper(s);
		addAntenna(s);
	}
	else if(key == "baselines")
	{
		string s;
		ss >> s;
		Upper(s);
		addBaseline(s);
	}
	else if(key == "mode")
	{
		string s;
		ss >> s;
		Upper(s);
		if(s == "NORMAL")
		{
			v2dMode = V2D_MODE_NORMAL;
		}
		else if(s == "PROFILE")
		{
			v2dMode = V2D_MODE_PROFILE;
		}
		else
		{
			cerr << "Warning: Illegal value " << value << " for mode" << endl;
			nWarn++;
		}
	}
	else if(key == "outputFormat")
	{
	        string s;
	        ss >> s;
		Upper(s);
		if (s == "ASCII")
		{
		  outputFormat = OutputFormatASCII;
		}
	}
	else if(key == "overSamp")
	{
		ss >> overSamp;
	}
	else
	{
		cerr << "Warning: Unknown keyword " << key << " with value " << value << endl;
		nWarn++;
	}

	return nWarn;
}

void CorrParams::addAntenna(const string &antName)
{
	if(find(antennaList.begin(), antennaList.end(), antName) == antennaList.end())
	{
		antennaList.push_back(antName);
	}
}

void CorrParams::addBaseline(const string &baselineName)
{
	size_t pos;

	pos = baselineName.find("-");

	if(pos == string::npos)
	{
		cerr << "Error in baseline designation: " << baselineName << " -- a hyphen is required." << endl;

		exit(0);
	}

	if(pos == 0 || pos == baselineName.length()-1)
	{
		cerr << "Error in baseline designation: " << baselineName << " -- need characters before and after the hyphen." << endl;
		
		exit(0);
	}

	baselineList.push_back(pair<string,string>(
		baselineName.substr(0, pos),
		baselineName.substr(pos+1) ));
}

int CorrParams::load(const string &fileName)
{
	enum Parse_Mode
	{
		PARSE_MODE_GLOBAL,
		PARSE_MODE_SETUP,
		PARSE_MODE_RULE,
		PARSE_MODE_SOURCE,
		PARSE_MODE_ANTENNA,
		PARSE_MODE_EOP
	};

	const int MaxLineLength = 1024;

	ifstream is;
	vector<string> tokens;
	char s[MaxLineLength];
	CorrSetup   *corrSetup=0;
	CorrRule    *rule=0;
	SourceSetup *sourceSetup=0;
	AntennaSetup *antennaSetup=0;
	VexEOP       *eop=0;
	Parse_Mode parseMode = PARSE_MODE_GLOBAL;
	int nWarn = 0;

	is.open(fileName.c_str());

	if(is.fail())
	{
		cerr << "Error: cannot open " << fileName << endl;

		exit(0);
	}

	for(;;)
	{
		is.getline(s, MaxLineLength);
		if(is.eof())
		{
			break;
		}
		string ss = s;

		int l = ss.size();
		int t = 0;
		char last = ' ';
		for(int i = 0; i <= l; i++)
		{
			// comment
			if(last <= ' ' && s[i] == '#')
			{	
				break;
			}
			else if(s[i] == '{' || s[i] == '}' || 
			        s[i] == '=' || s[i] == ',')
			{
				if(t < i)
				{
					tokens.push_back(ss.substr(t, i-t));
				}
				tokens.push_back(ss.substr(i, 1));
				t = i+1;
			}
			else if(s[i] <= ' ')
			{
				if(t < i)
				{
					tokens.push_back(ss.substr(t, i-t));
				}
				t = i+1;
			}
			last = s[i];
		}
	}

	bool keyWaiting=false, keyWaitingTemp;
	string key(""), value, last("");
	for(vector<string>::const_iterator i = tokens.begin(); 
	    i != tokens.end();
	    i++)
	{
		keyWaitingTemp = false;
		if(*i == "SETUP")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				cerr << "Error: SETUP out of place." << endl;
				
				exit(0);
			}
			i++;
			corrSetups.push_back(CorrSetup(*i));
			corrSetup = &corrSetups.back();
			i++;
			if(*i != "{")
			{
				cerr << "Error: '{' expected." << endl;
				
				exit(0);
			}
			key = "";
			parseMode = PARSE_MODE_SETUP;
		}
		else if(*i == "RULE")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				cerr << "Error: RULE out of place." << endl;
				
				exit(0);
			}
			i++;
			rules.push_back(CorrRule(*i));
			rule = &rules.back();
			i++;
			if(*i != "{")
			{
				cerr << "Error: '{' expected." << endl;
				
				exit(0);
			}
			key = "";
			parseMode = PARSE_MODE_RULE;
		}
		else if(*i == "SOURCE")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				cerr << "Error: SOURCE out of place." << endl;
				
				exit(0);
			}
			i++;
			for(unsigned int j=0;j<sourceSetups.size();j++)
			{
				if(sourceSetups[j].vexName.compare(*i) == 0)
				{
					cerr << "Error: Trying to add a setup for source " << *i << " which already has one!" << endl;
					
					exit(0);
				}
			}
			sourceSetups.push_back(SourceSetup(*i));
			sourceSetup = &sourceSetups.back();
			i++;
			if(*i != "{")
			{
				cerr << "Error: '{' expected." << endl;

				exit(0);
			}
			key = "";
			parseMode = PARSE_MODE_SOURCE;
		}
		else if(*i == "ANTENNA")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				cerr << "Error: ANTENNA out of place." << endl;
				
				exit(0);
			}
			i++;
			string antName(*i);
			Upper(antName);
			antennaSetups.push_back(AntennaSetup(antName));
			antennaSetup = &antennaSetups.back();
			i++;
			if(*i != "{")
			{
				cerr << "Error: '{' expected." << endl;

				exit(0);
			}
			key = "";
			parseMode = PARSE_MODE_ANTENNA;
		}
		else if(*i == "EOP")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
                        {
                                cerr << "Error: ANTENNA out of place." << endl;

                                exit(0);
                        }
                        i++;
                        string eopDate(*i);
                        eops.push_back(VexEOP());
                        eop = &eops.back();
                        eop->mjd = parseTime(eopDate);
                        i++;
                        if(*i != "{")
                        {
                                cerr << "Error: '{' expected." << endl;

                                exit(0);
                        }
                        key = "";
                        parseMode = PARSE_MODE_EOP;
		}
		else if(*i == "}" && parseMode != PARSE_MODE_GLOBAL)
		{
			parseMode = PARSE_MODE_GLOBAL;
			key = "";
		}
		else if(*i == "=")
		{
			key = last;
		}
		else if(*i == "{" || *i == "}")
		{
			cerr << "Warning: unexpected character '" << *i << "'." << endl;
			nWarn++;
		}
		else if(last == "=" || last == ",")
		{
			if(key == "")
			{
				cerr << "Error: legal parameter name expected before " << *i << endl;

				exit(0);
			}
			value = *i;
			switch(parseMode)
                        {
                        case PARSE_MODE_GLOBAL:
                                nWarn += setkv(key, value);
                                break;
                        case PARSE_MODE_SETUP:
                                nWarn += corrSetup->setkv(key, value);
                                break;
                        case PARSE_MODE_RULE:
                                nWarn += rule->setkv(key, value);
                                break;
                        case PARSE_MODE_SOURCE:
                                nWarn += sourceSetup->setkv(key, value);
                                break;
                        case PARSE_MODE_ANTENNA:
                                nWarn += antennaSetup->setkv(key, value);
                                break;
                        case PARSE_MODE_EOP:
                                nWarn += eop->setkv(key, value);
                                break;
                        }
		}
		else
		{
			if(keyWaiting == true)
			{
				cerr << "Parse error in file " << fileName << " : Unused token: " << last << " before token: " << *i << endl;

				exit(0);
			}
			keyWaitingTemp = true;
		}
		if(*i == "{" || *i == "}")
		{
			last = "";
		}
		else
		{
			last = *i;
		}

		keyWaiting = keyWaitingTemp;
	}

	is.close();

	// if no setups or rules declared, make the default setup
	if(corrSetups.size() == 0)
	{
		defaultSetup();
	}

	if(rules.size() == 0)
	{
		defaultRule();
	}

	if(baselineList.size() == 0)
	{
		addBaseline("*-*");
	}

	// update nFFTChan if needed
	for(vector<CorrSetup>::iterator c = corrSetups.begin(); c != corrSetups.end(); c++)
	{
#warning "FIXME: This logic should consider number of antennas and possibly number of sub-bands"

		if(c->nFFTChan < c->nOutputChan)
		{
			if(c->explicitnFFTChan)
			{
				cerr << "You have explicitly requested " << c->nFFTChan << " channels in the FFT, but the number of output channels is set to " << c->nOutputChan << ".  This cannot be accommodated! If --force used, nFFTChan will be set to " << c->nOutputChan << endl;
				nWarn++;
			}
			c->nFFTChan = c->nOutputChan;
		}

		if(c->xmacLength == 0)
		{
			if(c->nInputChan() > 128)
			{
				c->xmacLength = 128;
			}
			else
			{
				c->xmacLength = c->nInputChan();
			}
		}
		if(c->strideLength == 0)
		{
			c->strideLength = 1;
			int tempcount = c->nInputChan();
			while(c->strideLength < tempcount)
			{
				c->strideLength *= 2;
				tempcount /= 2;
			}
		}
	}

	//check that all setups are sensible
	for(vector<CorrSetup>::const_iterator c = corrSetups.begin(); c != corrSetups.end(); c++)
	{
		nWarn += c->checkValidity();
	}

	if(v2dMode == V2D_MODE_PROFILE)
	{
		for(vector<CorrSetup>::iterator c = corrSetups.begin(); c != corrSetups.end(); c++)
		{
			c->doPolar = false;
		}
	}

	return nWarn;
}

void CorrParams::defaultSetup()
{
	corrSetups.push_back(CorrSetup("default"));
	rules.push_back(CorrRule("default"));
	rules.back().corrSetupName = "default";
}

void CorrParams::defaultRule()
{
	rules.push_back(CorrRule("default"));
	rules.back().corrSetupName = corrSetups.begin()->corrSetupName;
}

void CorrParams::example()
{
	singleSetup = false;
	corrSetups.push_back(CorrSetup("1413+15"));
	corrSetups.back().tInt = 1.0;
	corrSetups.back().nOutputChan = 16;
	corrSetups.back().nFFTChan = 128;
	corrSetups.push_back(CorrSetup("default"));
	rules.push_back(CorrRule("1413+15"));
	rules.back().sourceName.push_back(string("1413+15"));
	rules.back().corrSetupName = string("1413+15");
	rules.push_back(CorrRule("1713+07"));
	rules.back().sourceName.push_back(string("1713+07"));
	rules.back().corrSetupName = string("default");
	rules.push_back(CorrRule("X"));
	rules.back().scanName.push_back(string("No0006"));
	rules.back().corrSetupName = string("bogus");
}

int CorrParams::sanityCheck()
{
	int nWarn = 0;
	const AntennaSetup *a = 0;

	if(minSubarraySize > antennaList.size() && antennaList.size() > 0)
	{
		cerr << "Warning: the antenna list has fewer than minSubarray antennas.  No jobs will be made." << endl;
		nWarn++;
	}

	a = getAntennaSetup("DEFAULT");
	if(a)
	{
		if(a->X != 0.0 || a->Y != 0 || a->Z != 0)
		{
			cerr << "Warning: the default antenna's coordinates are set!" << endl;
			nWarn++;
		}
		if(a->clock.offset != 0 || a->clock.rate != 0 || a->clock2 != 0 || a->clock3 != 0 || a->clock4 != 0 || a->clock5 != 0 || a->clock.offset_epoch != 0 || a->deltaClock != 0 || a->deltaClockRate != 0)
		{
			cerr << "Warning: the default antenna's clock parameters are set!" << endl;
			nWarn++;
		}
	}

	if(nThread != 0 || nCore != 0)
	{
		if(nThread <= 0 || nCore <= 0)
		{
			cerr << "Warning: nThreads and nCore must either both or neither be set." << endl;
			nWarn++;
		}
	}

	return nWarn;
}

bool antennaMatch(const string &a1, const string &a2)
{
	if(a1 == "*" || a2 == "*")
	{
		return true;
	}
	if(a1 == a2)
	{
		return true;
	}
	if(a1.find(a2) != string::npos)
	{
		return true;
	}

	return false;
}

bool baselineMatch(const pair<string,string> &bl, const string &ant1, const string &ant2)
{
	if(antennaMatch(bl.first, ant1) &&
	   antennaMatch(bl.second, ant2) )
	{
		return true;
	}
	
	return false;
}

bool CorrParams::useAntenna(const string &antName) const
{
	list<string>::const_iterator it;

	if(antennaList.empty())
	{
		return true;
	}

	for(it = antennaList.begin(); it != antennaList.end(); it++)
	{
		if(antennaMatch(*it, antName))
		{
			return true;
		}
	}

	return false;
}

bool CorrParams::useBaseline(const string &ant1, const string &ant2) const
{
	list<pair<string,string> >::const_iterator it;

	if(baselineList.empty())
	{
		return true;
	}

	for(it = baselineList.begin(); it != baselineList.end(); it++)
	{
		if(baselineMatch(*it, ant1, ant2) ||
		   baselineMatch(*it, ant2, ant1))
		{
			return true;
		}
	}

	return false;
}

bool CorrParams::swapPol(const string &antName) const
{
	vector<AntennaSetup>::const_iterator a;

	for(a = antennaSetups.begin(); a != antennaSetups.end(); a++)
	{
		if(a->vexName == antName)
		{
			return a->polSwap;
		}
	}

	return false;
}

const VexClock *CorrParams::getAntennaClock(const string &antName) const
{
	vector<AntennaSetup>::const_iterator a;

	for(a = antennaSetups.begin(); a != antennaSetups.end(); a++)
	{
		if(a->vexName == antName)
		{
			if(a->clock.mjdStart > 0)
			{
				return &a->clock;
			}
			else
			{
				return 0;
			}
		}
	}
	
	return 0;
}

const AntennaSetup *CorrParams::getAntennaSetup(const string &name) const
{
	int i, n;
	const AntennaSetup *a = 0;

	n = antennaSetups.size();
	for(i = 0; i < n; i++)
	{
		if(antennaSetups[i].vexName == "DEFAULT")
		{
			// keep this as a placeholder in case nothing better is found
			a = &antennaSetups[i];
		}
		if(name == antennaSetups[i].vexName)
		{
			a = &antennaSetups[i];
			break;
		}
	}

	return a;
}

void CorrParams::addSourceSetup(SourceSetup toadd)
{
	for(unsigned int i=0;i<sourceSetups.size();i++)
	{
		if(toadd.vexName.compare(sourceSetups[i].vexName) == 0)
		{
			cerr << "Error: Trying to add a setup for source " << toadd.vexName << " which already has one!" << endl;

			exit(0);
		}
	}
	sourceSetups.push_back(toadd);
}

const CorrSetup *CorrParams::getCorrSetup(const string &name) const
{
	int i, n;

	n = corrSetups.size();
	for(i = 0; i < n; i++)
	{
		if(name == corrSetups[i].corrSetupName)
		{
			return &corrSetups[i];
		}
	}

	return 0;
}

const SourceSetup *CorrParams::getSourceSetup(const string &name) const
{
	int i, n;

	n = sourceSetups.size();
	for(i = 0; i < n; i++)
	{
		if(name == sourceSetups[i].vexName)
		{
			return &sourceSetups[i];
		}
	}

	return 0;
}

const SourceSetup *CorrParams::getSourceSetup(const vector<string> &names) const
{
	vector<string>::const_iterator s;
	int i, n;

	n = sourceSetups.size();
	for(i = 0; i < n; i++)
	{
		for(s = names.begin(); s != names.end(); s++)
		{
			if(*s == sourceSetups[i].vexName)
			{
				return &sourceSetups[i];
			}
		}
	}

	return 0;
}

const PhaseCentre * CorrParams::getPhaseCentre(const string  &difxName) const
{
	for(unsigned int i=0;i<sourceSetups.size();i++)
	{
		if(difxName == sourceSetups[i].pointingCentre.difxName)
			return &(sourceSetups[i].pointingCentre);
		for(unsigned int j=0;j<sourceSetups[i].phaseCentres.size();j++)
		{
			if(difxName == sourceSetups[i].phaseCentres[j].difxName)
				return &(sourceSetups[i].phaseCentres[j]);
		}
	}

	return 0;
}

const string &CorrParams::findSetup(const string &scan, const string &source, const string &mode, char cal, int qual) const
{
	vector<CorrRule>::const_iterator it;
	static const string def("default");
	static const string none("");

	for(it = rules.begin(); it != rules.end(); it++)
	{
		if(it->match(scan, source, mode, cal, qual))
		{
			return it->corrSetupName;
		}
	}

	// If here, no rule has been found.  Look for default
	if(getCorrSetup(def) != 0)
	{
		return def;
	}

	return none;
}

ostream& operator << (ostream &os, const CorrSetup &x)
{
	int p;

	p = os.precision();
	os.precision(6);

	os << "SETUP " << x.corrSetupName << endl;
	os << "{" << endl;
	os << "  tInt=" << x.tInt << endl;
	os << "  nChan=" << x.nOutputChan << endl;
	os << "  nFFTChan=" << x.nFFTChan << endl;
	os << "  doPolar=" << x.doPolar << endl;
	os << "  doAuto=" << x.doAuto << endl;
	os << "  subintNS=" << x.subintNS << endl;
	os << "  fringeRotOrder=" << x.fringeRotOrder << endl;
	if(x.binConfigFile.size() > 0)
	{
		os << "  binConfig=" << x.binConfigFile << endl;
	}
	os << "}" << endl;

	os.precision(p);

	return os;
}

ostream& operator << (ostream &os, const CorrRule &x)
{
	bool space = false;
	os << "RULE " << x.ruleName << endl;
	os << "{" << endl;
	if(!x.scanName.empty())
	{
		list<string>::const_iterator it;

		os << "  scan=";
		for(it = x.scanName.begin(); it != x.scanName.end(); it++)
		{
			os << " " << *it;
		}
		os << endl;
		space = true;
	}
	if(!x.sourceName.empty())
	{
		list<string>::const_iterator it;

		os << "  source=";
		for(it = x.sourceName.begin(); it != x.sourceName.end(); it++)
		{
			os << " " << *it;
		}
		os << endl;
		space = true;
	}
	if(!x.modeName.empty())
	{
		list<string>::const_iterator it;

		os << "  mode=";
		for(it = x.modeName.begin(); it != x.modeName.end(); it++)
		{
			os << " " << *it;
		}
		os << endl;
		space = true;
	}
	
	if(space)
	{
		os << endl;
	}
	os << "  correlator setup=" << x.corrSetupName << endl;
	
	os << "}" << endl;

	return os;
}

ostream& operator << (ostream &os, const SourceSetup &x)
{
	os << "SOURCE " << x.vexName << endl;
	os << "{" << endl;
	if(x.pointingCentre.difxName.size() > 0)
	{
		os << "  pointing centre name=" << x.pointingCentre.difxName << endl;
	}
	if(x.doPointingCentre)
	{
		os << "  pointing centre is correlated" << endl;
	}
	else
	{
		os << "  pointing centre is not correlated" << endl;
	}
	if(x.pointingCentre.ra > PhaseCentre::DEFAULT_RA)
	{
		os << "  pointing centre ra=" << x.pointingCentre.ra << " # J2000" << endl;
	}
	if(x.pointingCentre.dec > PhaseCentre::DEFAULT_DEC)
	{
		os << "  pointing centre dec=" << x.pointingCentre.dec << " # J2000" << endl;
	}
	if(x.pointingCentre.calCode != ' ')
	{
		os << "  pointing centre calCode=" << x.pointingCentre.calCode << endl;
	}
	os << "  Number of additional phase centres is " << x.phaseCentres.size() << endl;
	os << "}" << endl;

	return os;
}

ostream& operator << (ostream &os, const AntennaSetup &x)
{
        os << "ANTENNA " << x.vexName << endl;
        os << "{" << endl;
        if(x.difxName.size() > 0)
        {
                os << "  name=" << x.difxName << endl;
        }
        if(fabs(x.X) > 0.1 || fabs(x.Y) > 0.1 || fabs(x.Z) > 0.1)
        {
                os << "  X=" << x.X <<" Y=" << x.Y << " Z=" << x.Z << endl;
        }
        if(x.clock.mjdStart > 0.0)
        {
                os << "  clockOffset=" << x.clock.offset*1.0e6 << endl;
                os << "  clockRate=" << x.clock.rate*1.0e6 << endl;
                os << "  clockEpoch=" << x.clock.offset_epoch << endl;
        }
        os << "  polSwap=" << x.polSwap << endl;
        if(x.format.size() > 0)
        {
                os << "  format=" << x.format << endl;
        }
	os << "  # dataSource=" << dataSourceNames[x.dataSource] << endl;
	if(x.dataSource == DataSourceNetwork)
	{
		os << "  networkPort=" << x.networkPort << endl;
		os << "  windowSize=" << x.windowSize << endl;
	}
	os << "  phaseCalInt=" << x.phaseCalIntervalMHz << endl;
	os << "  tcalFreq=" << x.tcalFrequency << endl;

        os << "}" << endl;

        return os;
}

ostream& operator << (ostream &os, const CorrParams &x)
{
	int p;

	p = os.precision();
	os.precision(13);

	os << "# correlation parameters" << endl;

	os << "vex=" << x.vexFile << endl;
	switch(x.v2dMode)
	{
	case V2D_MODE_NORMAL:
		os << "mode=normal" << endl;
		break;
	case V2D_MODE_PROFILE:
		os << "mode=profile" << endl;
		break;
	}
	os << "mjdStart=" << x.mjdStart << endl;
	os << "mjdStop=" << x.mjdStop << endl;
	int n = x.manualBreaks.size();
	for(int i = 0; i < n; i++)
	{
		os << "break=" << x.manualBreaks[i] << endl;
	}
	os << "minSubarray=" << x.minSubarraySize << endl;
	os << "visBufferLength=" << x.visBufferLength << endl;

	os.precision(6);
	os << "maxGap=" << x.maxGap*86400.0 << " # seconds" << endl;
	os << "maxLength=" << x.maxLength*86400.0 << " # seconds" << endl;
	os << "minLength=" << x.minLength*86400.0 << " # seconds" << endl;
	os << "maxSize=" << x.maxSize/1000000.0 << " # MB" << endl;
	os.precision(13);

	if(x.threadsFile != "")
	{
		os << "threadsFile=" << x.threadsFile << endl;
	}
	os << "singleScan=" << x.singleScan << endl;
	os << "singleSetup=" << x.singleSetup << endl;
	if(x.nCore > 0 && x.nThread > 0)
	{
		os << "nCore=" << x.nCore << endl;
		os << "nThread=" << x.nCore << endl;
	}
	os << "mediaSplit=" << x.mediaSplit << endl;
	os << "jobSeries=" << x.jobSeries << endl;
	os << "startSeries=" << x.startSeries << endl;
	os << "dataBufferFactor=" << x.dataBufferFactor << endl;
	os << "nDataSegments=" << x.nDataSegments << endl;
	os << "readSize=" << x.readSize << " # Bytes" << endl;
	os << "overSamp=" << x.overSamp << endl;
	os << "outputFormat=" << x.outputFormat << endl;
	
	if(!x.antennaList.empty())
	{
		list<string>::const_iterator it;
		
		os << "antennas=";
		for(it = x.antennaList.begin(); it != x.antennaList.end(); it++)
		{
			if(it != x.antennaList.begin())
			{
				os << ",";
			}
			os << *it;
		}
		os << endl;
	}
	
	if(!x.baselineList.empty())
	{
		list<pair<string,string> >::const_iterator it;
		
		os << "baselines=";
		for(it = x.baselineList.begin(); it != x.baselineList.end(); it++)
		{
			if(it != x.baselineList.begin())
			{
				os << ",";
			}
			os << it->first << '-' << it->second;
		}
		os << endl;
	}

	if(!x.antennaSetups.empty())
        {
                vector<AntennaSetup>::const_iterator it;

                for(it = x.antennaSetups.begin(); it != x.antennaSetups.end(); it++)
                {
                        os << endl;
                        os << *it;
                }
        }

        if(!x.eops.empty())
        {
                os << endl;

                vector<VexEOP>::const_iterator it;

                for(it = x.eops.begin(); it != x.eops.end(); it++)
                {
                        os << "EOP " << it->mjd << " { ";
                        os << "tai_utc=" << it->tai_utc << " ";
                        os << "ut1_utc=" << it->ut1_utc << " ";
                        os << "xPole=" << it->xPole*RAD2ASEC << " ";
                        os << "yPole=" << it->yPole*RAD2ASEC << " }" << endl;
                }
        }

	if(!x.sourceSetups.empty())
	{
		vector<SourceSetup>::const_iterator it;

		for(it = x.sourceSetups.begin(); it != x.sourceSetups.end(); it++)
		{
			os << endl;
			os << *it;
		}
	}

	if(!x.corrSetups.empty())
	{
		vector<CorrSetup>::const_iterator it;

		for(it = x.corrSetups.begin(); it != x.corrSetups.end(); it++)
		{
			os << endl;
			os << *it;
		}
	}

	if(!x.rules.empty())
	{
		vector<CorrRule>::const_iterator it;

		for(it = x.rules.begin(); it != x.rules.end(); it++)
		{
			os << endl;
			os << *it;
		}
	}

	os.precision(p);

	return os;
}

bool areCorrSetupsCompatible(const CorrSetup *A, const CorrSetup *B, const CorrParams *C)
{
	if(C->singleScan)
	{
		return false;
	}
	if(C->singleSetup)
	{
		if(A->tInt        == B->tInt        &&
		   A->nOutputChan == B->nOutputChan &&
		   A->doPolar     == B->doPolar     &&
		   A->doAuto      == B->doAuto      &&
		   A->binConfigFile.compare(B->binConfigFile) == 0)
		{
			return true;
		}
		else
		{
			return false;
		}
	}
	else
	{
		return true;
	}
}

int CorrParams::loadShelves(const string &fileName)
{
	int nWarn = 0;
	ifstream is;
	bool doAntennas;
	char s[1024], a[32], v[32], ms[32];
	string vsn, shelf;
	vector<string> noShelf;

	is.open(fileName.c_str());

	if(is.fail())
	{
		return 0;
	}

	doAntennas = (antennaList.size() == 0);

	for(int lineNum = 1; ; lineNum++)
	{
		is.getline(s, 1024);
		if(is.eof())
		{
			break;
		}
		for(int i = 0; s[i]; i++)
		{
			if(s[i] == '#')
			{
				s[i] = 0;
				break;
			}
		}

		if(strlen(s) < 5)
		{
			continue;
		}

		if(sscanf(s, "%s%s%s", a, v, ms) != 3)
		{
			cerr << "Error: line " << lineNum << " of " << fileName << " not parsable." << endl;

			exit(0);
		}

		string antName(a);
		Upper(antName);

		if(doAntennas)
		{
			addAntenna(antName);
		}
		else if(!useAntenna(antName))
		{
			continue;
		}

		vsn = string(v);
		shelf = string(ms);

		Upper(vsn);
		Upper(shelf);

		if(shelf == string("NONE"))
		{
			noShelf.push_back(vsn);
		}
		else
		{
			shelves[vsn] = shelf;
		}
	}

	is.close();

	if(noShelf.size() > 0)
	{
		cerr << "Warning: " << noShelf.size() << " modules have no shelf location:";
		for(vector<string>::const_iterator s = noShelf.begin(); s != noShelf.end(); s++)
		{
			cerr << " " << *s;
		}
		cerr << endl;
	}

	return nWarn;
}

const char *CorrParams::getShelf(const string &vsn) const
{
	map<string,string>::const_iterator it;

	it = shelves.find(vsn);
	if(it == shelves.end())
	{
		return "NONE";
	}
	else
	{
		return it->second.c_str();
	}
}
