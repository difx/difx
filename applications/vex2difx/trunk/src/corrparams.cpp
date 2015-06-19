/***************************************************************************
 *   Copyright (C) 2009-2015 by Walter Brisken                             *
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
#include "timeutils.h"
#include "corrparams.h"

const double PhaseCentre::DEFAULT_RA  = -999.9;
const double PhaseCentre::DEFAULT_DEC = -999.9;

/* round to nearest second */
static double roundSeconds(double mjd)
{
	int intmjd, intsec;

	intmjd = static_cast<int>(mjd);
	intsec = static_cast<int>((mjd - intmjd)*86400.0 + 0.5);

	return intmjd + intsec/86400.0;
}

bool isTrue(const std::string &str)
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

/* check if an integer is a power of 2 */
bool isPower2(int n)
{
	if(!(n & (n - 1))) 
	{
		return true;
	}

	return false; // also true for zero but this shouldn't concern us
}

// Turns a string into MJD 
// The following formats are allowd:
// 1. decimal mjd:  55345.113521
// 2. ISO 8601 dateTtime strings:  2009-03-08T12:34:56.121
// 3. VLBA-like time:   2009MAR08-12:34:56.121
// 4. vex time: 2009y245d08h12m24s"
double parseTime(const std::string &timeStr)
{
	const int TimeLength=54;
	double mjd;
	char str[TimeLength];
	char *p;
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
		return mktime(&tm)/86400.0 + MJD_UNIX0;
	}

	n = sscanf(str, "%lf%c", &mjd, &dummy);
	if(n == 1)
	{
		// Must be straight MJD value
		return mjd;
	}

	// No match
	std::cerr << std::endl;
	std::cerr << "Error: date not parsable: " << timeStr << std::endl;
	std::cerr << std::endl;
	std::cerr << "Allowable formats are:" << std::endl;
	std::cerr << "1. Straight MJD        54345.341944" << std::endl;
	std::cerr << "2. Vex formatted date  2009y245d08h12m24s" << std::endl;
	std::cerr << "3. VLBA-like format    2009SEP02-08:12:24" << std::endl;
	std::cerr << "4. ISO 8601 format     2009-09-02T08:12:24" << std::endl;
	std::cerr << std::endl;

	exit(EXIT_FAILURE);
}

double parseCoord(const char *str, char type)
{
	int sign = 1, l, n;
	double a, b, c;
	double v = -999999.0;

	if(type != ' ' && type != 'R' && type != 'D')
	{
		std::cerr << "Programmer error: parseCoord: parameter 'type' has illegal value = " << type << std::endl;
		
		exit(EXIT_FAILURE);
	}

	if(str[0] == '-')
	{
		sign = -1;
		++str;
	}
	else if(str[0] == '+')
	{
		++str;
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
			std::cerr << "Error parsing coordinate value " << str << std::endl;

			exit(EXIT_FAILURE);
		}
		v *= sign;
	}

	return v;
}

// From http://oopweb.com/CPP/Documents/CPPHOWTO/Volume/C++Programming-HOWTO-7.html
void split(const std::string &str, std::vector<std::string> &tokens, const std::string &delimiters = " ")
{
	// Skip delimiters at beginning.
	std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
	// Find first "non-delimiter".
	std::string::size_type pos     = str.find_first_of(delimiters, lastPos);

	while(std::string::npos != pos || std::string::npos != lastPos)
	{
		// Found a token, add it to the vector.
		tokens.push_back(str.substr(lastPos, pos - lastPos));
		// Skip delimiters.  Note the "not_of"
		lastPos = str.find_first_not_of(delimiters, pos);
		// Find next "non-delimiter"
		pos = str.find_first_of(delimiters, lastPos);
	}
}

int loadBasebandFilelist(const std::string &fileName, std::vector<VexBasebandFile> &basebandFiles)
{
	const int MaxLineLength=1024;
	std::ifstream is;
	int n=0;
	char s[MaxLineLength];
	std::vector<std::string> tokens;

	is.open(fileName.c_str());

	if(is.fail())
	{
		std::cerr << "Error: cannot open " << fileName << std::endl;

		exit(EXIT_FAILURE);
	}

	for(unsigned int line = 1; ; ++line)
	{
		is.getline(s, MaxLineLength);
		if(is.eof())
		{
			break;
		}

		for(int i = 0; s[i]; ++i)
		{
			if(s[i] == '#')
			{
				s[i] = 0;
			}
		}

		tokens.clear();

		split(std::string(s), tokens);

		int l = tokens.size();

		if(l == 0)
		{
			continue;
		}
		else if(l == 1)
		{
			basebandFiles.push_back(VexBasebandFile(tokens[0]));
			++n;
		}
		else if(l == 3)
		{
			basebandFiles.push_back(VexBasebandFile(tokens[0],
				parseTime(tokens[1]),
				parseTime(tokens[2]) ));
			++n;
		}
		else
		{
			std::cerr << "Error: line " << line << " of file " << fileName << " is badly formatted" << std::endl;

			exit(EXIT_FAILURE);
		}
	}

	//sort(basebandFiles.begin(), basebandFiles.end(), sortStartMjdDescendingFunc);

	return n;
}

CorrSetup::CorrSetup(const std::string &name) : corrSetupName(name)
{
	tInt = 2.0;
	suppliedSpecAvg = 0;
	FFTSpecRes = 250000.0;		// Hz; spectral resolution at FFT stage
	outputSpecRes = 500000.0;	// Hz; spectral resolution in Output
	nFFTChan = 0;			// If this or nOutputChan != 0, these override SpecAvg params
	nOutputChan = 0;
	doPolar = true;
	doAuto = true;
	fringeRotOrder = 1;
	strideLength = 0;
	xmacLength = 0;
	explicitXmacLength = false;
	explicitStrideLength = false;
	explicitFFTSpecRes = false;
	explicitOutputSpecRes = false;
	explicitGuardNS = false;
	numBufferedFFTs = 10;
	subintNS = 0;
	guardNS = 1000;
	maxNSBetweenUVShifts = 2000000000;
	maxNSBetweenACAvg = 0;		// zero means set to default in vex2difx.cpp
	minRecordedBandwidth = 0.0;
	maxRecordedBandwidth = 0.0;
	onlyPol = ' ';
}

void CorrSetup::addRecordedBandwidth(double bw)
{
	recordedBandwidths.insert(bw);
	if(bw < minRecordedBandwidth || minRecordedBandwidth == 0.0)
	{
		minRecordedBandwidth = bw;
	}
	if(bw > maxRecordedBandwidth)
	{
		maxRecordedBandwidth = bw;
	}
}

int CorrSetup::setkv(const std::string &key, const std::string &value)
{
	std::stringstream ss;
	int nWarn = 0;
	char *ptr;

	ss << value;

	if(key == "VEX_rev")
	{
		std::cerr << "Error: You are running vex2difx on a vex file." << std::endl;
		std::cerr << "Please run on a vex2difx input file (.v2d) instead." << std::endl;

		exit(EXIT_FAILURE);
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
	}
	else if(key == "outputSpecRes" || key == "specRes")
	{
		ss >> outputSpecRes;
		outputSpecRes *= 1e6;	// Users use MHz, vex2difx uses Hz
		explicitOutputSpecRes = true;
	}
	else if(key == "FFTSpecRes" || key == "fftSpecRes")
	{
		ss >> FFTSpecRes;
		FFTSpecRes *= 1e6;	// Users use MHz, vex2difx uses Hz
		explicitFFTSpecRes = true;
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
		explicitGuardNS = true;
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
		ss >> suppliedSpecAvg;
	}
	else if(key == "fringeRotOrder")
	{
		ss >> fringeRotOrder;
	}
	else if(key == "strideLength")
	{
		ss >> strideLength;
		explicitStrideLength = true;
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
			std::string inFile;

			ptr = getcwd(cwd, DIFXIO_FILENAME_LENGTH-1);
			if(ptr == 0)
			{
				std::cerr << "Cannot getcwd()" << std::endl;

				exit(EXIT_FAILURE);
			}
			inFile = std::string(cwd);
			inFile += std::string("/");
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
			std::string inFile;

			ptr = getcwd(cwd, DIFXIO_FILENAME_LENGTH-1);
			if(ptr == 0)
			{
				std::cerr << "Cannot getcwd()" << std::endl;

				exit(EXIT_FAILURE);
			}
			inFile = std::string(cwd);
			inFile += std::string("/");
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
	else if(key == "onlyPol")
	{
		ss >> onlyPol;
	}
	else
	{
		std::cerr << "Warning: SETUP: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
	}

	return nWarn;
}

void CorrSetup::addFreqId(int freqId)
{
	freqIds.insert(freqId);
}

bool CorrSetup::correlateFreqId(int freqId) const
{
	if(freqIds.empty())
	{
		return true;
	}
	else
	{
		return (freqIds.find(freqId) != freqIds.end());
	}
}

// Returns ratio of output to input spectral resolution
int CorrSetup::specAvg() const
{
	double x = outputSpecRes / FFTSpecRes;
	int n = static_cast<int>(x + 0.5);

	if(fabs(x - n) > 0.0001)
	{
		n = -1;	// Signal illegal averaging
	}
	
	return n;
}

int CorrSetup::testSpectralResolution() const
{
	int nError = 0;

	for(std::set<double>::const_iterator it = recordedBandwidths.begin(); it != recordedBandwidths.end(); ++it)
	{
		double x = *it/FFTSpecRes;
		if(fabs(x - static_cast<int>(x + 0.5)) > 0.0001)
		{
			std::cerr << "Error: FFT spectral resolution (" << (FFTSpecRes*1.0e-6) << " MHz) does not divide nicely into sub-band bandwidth (" << (*it*1.0e-6) << " MHz)" << std::endl;
			
			++nError;
		}
	}

	return nError;
}

int CorrSetup::testXMACLength() const
{
	int nWarn = 0;

	if(xmacLength > 0)
	{
		for(std::set<double>::const_iterator it = recordedBandwidths.begin(); it != recordedBandwidths.end(); ++it)
		{
			if(nInputChans(*it) % xmacLength != 0)
			{
				std::cerr << "Warning: xmacLength=" << xmacLength << " does not divide evenly into " << minInputChans() << " which are requested for sub-bands with bandwidth " << (*it * 1.0e-6) << " MHz" << std::endl;
				std::cerr << "Probably you need to reduce the xmacLength parameter" << std::endl;

				++nWarn;
			}
		}
	}

	return nWarn;
}

int CorrSetup::testStrideLength() const
{
	int nWarn = 0;

	if(xmacLength > 0)
	{
		for(std::set<double>::const_iterator it = recordedBandwidths.begin(); it != recordedBandwidths.end(); ++it)
		{
			if(nInputChans(*it) % strideLength != 0)
			{
				std::cerr << "Warning: strideLength=" << strideLength << " does not divide evenly into " << minInputChans() << " which are requested for sub-bands with bandwidth " << (*it * 1.0e-6) << " MHz" << std::endl;
				std::cerr << "Probably you need to reduce the strideLength parameter" << std::endl;
				++nWarn;
			}
		}
	}

	return nWarn;
}

int CorrSetup::checkValidity() const
{
	int nWarn = 0;

	if(specAvg() < 0)
	{
		std::cerr << "Error: The FFT Spectral Resolution ( " << FFTSpecRes << " Hz) must be an integral multiple of the Output Spectral Resolution (" << outputSpecRes << " Hz)" << std::endl;

		exit(EXIT_FAILURE);
	}

	if(testSpectralResolution() > 0)
	{
		exit(EXIT_FAILURE);
	}

	nWarn += testXMACLength();

	nWarn += testStrideLength();

	return nWarn;
}

#warning "FIXME: this assumes worst case bandwidth here"
double CorrSetup::bytesPerSecPerBLPerBand() const
{
	int pols = doPolar ? 2 : 1;

	// assume 8 bytes per complex

	return 8*maxOutputChans()*pols/tInt;
}

CorrRule::CorrRule(const std::string &name) : ruleName(name)
{
}

bool CorrRule::match(const std::string &scan, const std::string &source, const std::string &mode, char cal, int qual) const
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

int CorrRule::setkv(const std::string &key, const std::string &value)
{
	std::stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "scanName" || key == "scan")
	{
		std::string s;

		ss >> s;
		scanName.push_back(s);
	}
	else if(key == "sourceName" || key == "source")
	{
		std::string s;
		
		ss >> s;
		sourceName.push_back(s);
	}
	else if(key == "modeName" || key == "mode")
	{
		std::string s;
		
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
		std::cerr << "Warning: RULE: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
	}

	return nWarn;
}

const std::string PhaseCentre::DEFAULT_NAME = "";

PhaseCentre::PhaseCentre()
{
	initialise(DEFAULT_RA, DEFAULT_DEC, DEFAULT_NAME);
}

PhaseCentre::PhaseCentre(double r, double d, std::string name)
{
	initialise(r, d, name);
}

void PhaseCentre::initialise(double r, double d, std::string name)
{
	ra = r;
	dec = d;
	difxName = name;
	calCode = ' ';
	ephemDeltaT = 24.0;	// seconds; 24 seconds is perfectly matched to the default behavior of calcif2
	ephemStellarAber = 0.0;	// 0 = don't correct; 1 = correct.  Other values interpolate/extrapolate correction by factor provided
	qualifier = 0;
	ephemClockError = 0.0;	// sec
	ephemObject = "";
	ephemFile = "";
	naifFile = "";  
	X = Y = Z = 0.0;	// not a geosyncronous satellite
}

SourceSetup::SourceSetup(const std::string &name) : vexName(name)
{
	doPointingCentre = true;
	pointingCentre.difxName = name;
}

int SourceSetup::setkv(const std::string &key, const std::string &value)
{
	return setkv(key, value, &pointingCentre);
}

int SourceSetup::setkv(const std::string &key, const std::string &value, PhaseCentre * pc)
{
	std::string::size_type at, last, splitat;
	std::string nestedkeyval;
	std::stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "ra" || key == "RA")
	{
		if(pc->ra > PhaseCentre::DEFAULT_RA)
		{
			std::cerr << "Warning: Source " << vexName << " has multiple RA assignments" << std::endl;
			++nWarn;
		}
		pc->ra = parseCoord(value.c_str(), 'R');
	}
	else if(key == "dec" || key == "Dec")
	{
		if(pc->dec > PhaseCentre::DEFAULT_DEC)
		{
			std::cerr << "Warning: Source " << vexName << " has multiple Dec assignments" << std::endl;
			++nWarn;
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
	else if(key == "ephemDeltaT")
	{
		ss >> pc->ephemDeltaT;
	}
	else if(key == "ephemStellarAber")
	{
		ss >> pc->ephemStellarAber;
	}
	else if(key == "ephemClockError")
	{
		ss >> pc->ephemClockError;
	}
	else if(key == "X" || key == "x")
	{
		if(pc->X != 0.0)
		{
			std::cerr << "Warning: phase centre " << pc->difxName << " has multiple X definitions" << std::endl;
			++nWarn;
		}
		ss >> pc->X;
	}
	else if(key == "Y" || key == "y")
	{
		if(pc->Y != 0.0)
		{
			std::cerr << "Warning: phase centre " << pc->difxName << " has multiple Y definitions" << std::endl;
			++nWarn;
		}
		ss >> pc->Y;
	}
	else if(key == "Z" || key == "z")
	{
		if(pc->Z != 0.0)
		{
			std::cerr << "Warning: phase centre " << pc->difxName << " has multiple Z definitions" << std::endl;
			++nWarn;
		}
		ss >> pc->Z;
	}
	else if(key == "naifFile")
	{
		ss >> pc->naifFile;
		if(pc->naifFile < "naif0010.tls")
		{
			if(time(0) > 1341100800)	// July 1, 2012
			{
				std::cout << "Error: naif0010.tls or newer is needed for correct ephemeris evaluation.  An old or unrecognized file, " << pc->naifFile << " was supplied." << std::endl;

				exit(EXIT_FAILURE);
			}
			else
			{
				std::cout << "Warning: Using old NAIF file: " << pc->naifFile << ".  Please upgrade to naif0010.tls or newer." << std::endl;
				std::cout << "After July 1, 2012, this will be an error and vex2difx will not run." << std::endl;
				nWarn++;
			}
		}
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
		while(at !=std::string::npos)
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
		std::cerr << "Warning: SOURCE: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
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

AntennaSetup::AntennaSetup(const std::string &name) : vexName(name)
{
	polSwap = false;
	X = 0.0;
	Y = 0.0;
	Z = 0.0;
	axisOffset = -1e6;
	deltaClock = 0.0;
	deltaClockRate = 0.0;
	clock.mjdStart = -1e9;
	clockorder = 1;
	clock2 = 0.0;
	clock3 = 0.0;
	clock4 = 0.0;
	clock5 = 0.0;
	networkPort = "0";
	windowSize = 0;
	phaseCalIntervalMHz = -1;
	toneGuardMHz = -1.0;
	toneSelection = ToneSelectionSmart;
	tcalFrequency = -1;
	dataSource = DataSourceNone;
	dataSampling = NumSamplingTypes;	// flag that no sampling is is identified here

	// antenna is by default not constrained in start time
	mjdStart = -1.0;
	mjdStop = -1.0;
}

int AntennaSetup::setkv(const std::string &key, const std::string &value, ZoomFreq *zoomFreq)
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
		{
			zoomFreq->correlateparent = false;
		}
		else
		{
			zoomFreq->correlateparent = true;
		}
	}
	else if(key == "specAvg" || key == "SPECAVG" || key == "specavg")
	{
		zoomFreq->spectralaverage = atoi(value.c_str());
	}
	else
	{
		std::cerr << "Warning: ANTENNA: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
	}

	return nWarn;
}

  enum charType {SIGN,DIGIT,DOT,E,SPACE,CHARERROR};

  enum charType whatChar(const char a) {
    if (a=='+'||a=='-') 
      return SIGN;
    else if (a=='E'||a=='e')
      return (E);
    else if (a>='0'&&a<='9')
      return DIGIT;
    else if (a==' ')
      return SPACE;
    else if (a=='.')
      return DOT;
    else
      return CHARERROR;
  }

  int getdouble(std::string &value, double &x) {
    enum stateType {START, STARTINT, INTEGER, DECIMAL, STARTEXP, EXPONENT, END, ERROR};
    enum stateType state = START;
    enum charType what;

    unsigned int i;
    for (i=0 ; i<value.length(); i++) {
      what = whatChar(value[i]);
      
      switch (state) {
      case START:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SIGN:
	  state = STARTINT;
	  break;
	case DIGIT:
	  state = INTEGER;
	  break;
	case SPACE:
	  break;
	case E:
	  state = ERROR;
	  break;
	case DOT:
	  state=DECIMAL;
	  break;
	}
	break;
	
      case STARTINT:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SIGN:
	case E:
	  state = ERROR;
	  break;
	case DIGIT:
	  state = INTEGER;
	  break;
	case SPACE:
	  break;
	case DOT:
	  state = DECIMAL;
	}
	break;

      case INTEGER:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case DIGIT:
	  break;
	case SIGN:
	case SPACE:
	  state = END;
	  break;
	case E:
	  state = STARTEXP;
	  break;
	case DOT:
	  state = DECIMAL;
	}
	break;
	
      case DECIMAL:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case DIGIT:
	  break;
	case SIGN:
	case SPACE:
	  state = END;
	  break;
	case E:
	  state = STARTEXP;
	  break;
	case DOT:
	  state = ERROR;
	  break;
	}
	break;
	
      case STARTEXP:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SIGN:
	case DIGIT:
	  state = EXPONENT;
	  break;
	case SPACE:
	case E:
	case DOT:
	  state = ERROR;
	  break;
	}
	break;
	
      case EXPONENT:
	switch (what) {
	case CHARERROR:
	  std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	  value = "";
	  return 1; 
	  break;
	case SPACE:
	case SIGN:
	  state = END;
	  break;
	case DIGIT:
	  break;
	case DOT:
	case E:
	  state = ERROR;
	  break;
	}
	break;

      case ERROR:
      case END:
	break;
	
      }
      
      if (state==ERROR) {
	std::cerr << "Error parsing \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	value = "";
	return 1; 
      }
      if (state==END) break;
    }

    std::stringstream ss;
    if (state==START) {
      value = "";
      return 1;
    } else if (state==END) {
    } else {
      i = value.length();
    }
    ss << value.substr(0,i);
    ss >> x;
    value  = value.substr(i);

    return 0;
  }
  
  int getOp(std::string &value, int &plus) {
    enum charType what;

    unsigned int i;
    for (i=0 ; i<value.length(); i++) {
      what = whatChar(value[i]);
      
      if (what==CHARERROR) {
	std::cerr << "Error parsing character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	value = "";
	return 1; 
      } else if (what==SPACE) {
	continue;
      } else if (what==SIGN) {
	if (value[i]=='+') {
	  plus = 1;
	} else {
	  plus = 0;
	} 
	value = value.substr(i+1);
	return(0);
      } else {
	std::cerr << "Unexpected character in \"" << value << "\" at : '" << value[i] << "':" << i << std::endl;
	value = "";
	return 1; 
      }
    }
    return(1); // Did not match anything
  }

double parseDouble(const std::string &value) {
  // Read a string consisting of a series of additions and subtrations (only) and return a double

  std::string str = value; // Copy as the procedure destroys the string
  
  int status, number=1, sign=-1;
  double thisvalue, result=0;
  while (str.length()) {
    if (number) {
      status = getdouble(str, thisvalue);
      if (status) break;
      if (sign==-1)
	result = thisvalue;
      else if (sign==1) 
	result += thisvalue;
      else
	result -= thisvalue;
      number = 0;
    
    } else  {
      status = getOp(str, sign);
      if (status) break;
      number = 1;
    }
  }

  return result;

}



int AntennaSetup::setkv(const std::string &key, const std::string &value)
{
	std::string::size_type at, last, splitat;
	std::string nestedkeyval;
	std::stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "name" || key == "newName")
	{
		if(vexName == "DEFAULT")
		{
			std::cerr << "Error: renaming the DEFAULT antenna setup is not allowed" << std::endl;
			
			exit(EXIT_FAILURE);
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
			std::cerr << "Warning: antenna " << vexName << " has multiple clockOffset definitions" << std::endl;
			++nWarn;
		}
		clock.offset = parseDouble(value);
		clock.offset /= 1.0e6;	// convert from us to sec
		clock.mjdStart = 1;
	}
	else if(key == "sampling")
	{
		dataSampling = stringToSamplingType(value.c_str());
		if(dataSampling >= NumSamplingTypes)
		{
			std::cerr << "Error: antenna " << vexName << " has illegal samping type set: " << value << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else if(key == "clockRate" || key == "clock1")
	{
		if(clock.rate != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple clockRate definitions" << std::endl;
			++nWarn;
		}
		clock.rate = parseDouble(value);
		clock.rate /= 1.0e6;	// convert from us/sec to sec/sec
		clock.mjdStart = 1;
	}
#warning "FIXME: this section of code could be made much nicer"
	else if(key == "clock2")
	{
		if(clock2 != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple clock2 definitions" << std::endl;
			++nWarn;
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
			std::cerr << "Warning: antenna " << vexName << " has multiple clock3 definitions" << std::endl;
			++nWarn;
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
			std::cerr << "Warning: antenna " << vexName << " has multiple clock4 definitions" << std::endl;
			++nWarn;
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
			std::cerr << "Warning: antenna " << vexName << " has multiple clock5 definitions" << std::endl;
			++nWarn;
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
			std::cerr << "Warning: antenna " << vexName << " has multiple clockEpoch definitions" << std::endl;
			++nWarn;
		}
		clock.offset_epoch = parseTime(value);
		clock.mjdStart = 1;
	}
	else if(key == "deltaClock")
	{
		if(deltaClock != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple deltaClock definitions" << std::endl;
			++nWarn;
		}
		deltaClock = parseDouble(value);
		deltaClock /= 1.0e6;	// convert from us to sec
	}
	else if(key == "deltaClockRate")
	{
		if(deltaClockRate != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple deltaClockRate definitions" << std::endl;
			++nWarn;
		}
		deltaClockRate = parseDouble(value);
		deltaClockRate /= 1.0e6;	// convert from us/sec to sec/sec
	}
	else if(key == "X" || key == "x")
	{
		if(X != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple X definitions" << std::endl;
			++nWarn;
		}
		ss >> X;
	}
	else if(key == "Y" || key == "y")
	{
		if(Y != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple Y definitions" << std::endl;
			++nWarn;
		}
		ss >> Y;
	}
	else if(key == "Z" || key == "z")
	{
		if(Z != 0.0)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple Z definitions" << std::endl;
			++nWarn;
		}
		ss >> Z;
	}
	else if(key == "axisOffset")
	{
		if(axisOffset > -1.0e5)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple axisOffset definitions" << std::endl;

			++nWarn;
		}
		ss >> axisOffset;
	}
	else if(key == "format")
	{
		std::string s;
		ss >> s;
		Upper(s);

		if(s == "MARK4")
		{
			s = "MKIV";
		}

		format = s;
	}
	else if(key == "machine")
	{
		ss >> machine;	// FIXME: when multiple datastreams per antenna are supported, this should be a list append
	}
	else if(key == "file" || key == "files")
	{
		if(dataSource != DataSourceFile && dataSource != DataSourceNone)
		{
			std::cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceFile] << std::endl;
			++nWarn;
		}
		dataSource = DataSourceFile;
		basebandFiles.push_back(VexBasebandFile(value));
	}
	else if(key == "filelist")
	{
		if(dataSource != DataSourceFile && dataSource != DataSourceNone)
		{
			std::cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceFile] << std::endl;
			++nWarn;
		}
		dataSource = DataSourceFile;
		loadBasebandFilelist(value, basebandFiles);
	}
	else if(key == "networkPort")
	{
		if(dataSource != DataSourceNetwork && dataSource != DataSourceNone)
		{
			std::cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceNetwork] << std::endl;
			++nWarn;
		}
		dataSource = DataSourceNetwork;
		ss >> networkPort;
	}
	else if(key == "windowSize")
	{
		if(dataSource != DataSourceNetwork && dataSource != DataSourceNone)
		{
			std::cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceNetwork] << std::endl;
			++nWarn;
		}
		dataSource = DataSourceNetwork;
		ss >> windowSize;
	}
	else if(key == "UDP_MTU")
	{
		if(dataSource != DataSourceNetwork && dataSource != DataSourceNone)
		{
			std::cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceNetwork] << std::endl;
			++nWarn;
		}
		dataSource = DataSourceNetwork;
		ss >> windowSize;
		windowSize = -windowSize;
	}
	else if(key == "module" || key == "vsn")
	{
		if(dataSource == DataSourceModule)
		{
			std::cerr << "Warning: antenna " << vexName << " has multiple vsns assigned to it.  Only using the last one = " << value << " and discarding " << basebandFiles[0].filename << std::endl;
		}
		else if(dataSource != DataSourceNone)
		{
			std::cerr << "Warning: antenna " << vexName << " had at least two kinds of data sources!: " << dataSourceNames[dataSource] << " and " << dataSourceNames[DataSourceFile] << std::endl;
			++nWarn;
		}
		dataSource = DataSourceModule;
		basebandFiles.clear();
		basebandFiles.push_back(VexBasebandFile(value));
	}
	else if(key == "fake")
	{
		dataSource = DataSourceFake;
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
		std::string ts;
		ss >> ts;
		toneSelection = stringToToneSelection(ts.c_str());
		if(toneSelection == ToneSelectionUnknown)
		{
			std::cerr << "Error: antenna " << vexName << " unsupported value of toneSelection (" << ts << ") provided." << std::endl;
			++nWarn;
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
	        size_t found;
		found = value.find_first_of(':');
		if(found == std::string::npos)
		{ 
			// No match
			// Just Delay offset
			ss >> d;
			freqClockOffs.push_back(d);
			freqClockOffsDelta.push_back(0);
			freqPhaseDelta.push_back(0);
		} 
		else
		{
			// Offset:LcpOffset[:Phaseoffset] (usec:usec:degrees)
			ss << value.substr(0,found);
			ss >> d;
			freqClockOffs.push_back(d);

			size_t found2;
			found2 = value.substr(found+1).find_first_of(':');
			if(found2==std::string::npos)
			{
				// Offset:LcpOffset

				std::stringstream ss2;
				ss2 << value.substr(found+1);
				ss2 >> d;
				freqClockOffsDelta.push_back(d);
				freqPhaseDelta.push_back(0);
			}
			else
			{
				// Offset:LcpOffset:PhaseOffset

				std::stringstream ss2;
				ss2 << value.substr(found+1).substr(0,found2);
				ss2 >> d;
				freqClockOffsDelta.push_back(d);

				std::stringstream ss3;
				ss3 << value.substr(found+1).substr(found2+1);
				ss3 >> d;
				freqPhaseDelta.push_back(d);
			}
		}
	}
	else if(key =="loOffsets")
	{
		double d;

		ss >> d;
		loOffsets.push_back(d);
	}
	else if(key == "zoom")
	{
		if(!zoomFreqs.empty())
		{
			std::cerr << "Error: cannot specify both ANTENNA-based and ZOOM-based zoom freqs for an antenna" << std::endl;

			exit(EXIT_FAILURE);
		}
		ss >> globalZoom;
	}
	else if(key == "addZoomFreq")
	{
		if(!globalZoom.empty())
		{
			std::cerr << "Error: cannot specify both ANTENNA-based and ZOOM-based zoom freqs for an antenna" << std::endl;

			exit(EXIT_FAILURE);
		}

		// This is a bit tricky.  All parameters must be together, with @ replacing =, and separated by /
		// e.g., addZoomFreq = freq@1649.99/bw@1.0/noparent@TRUE/specAvg@1
		// only freq and bw are compulsory; default is parent values and don't correlate parent
		zoomFreqs.push_back(ZoomFreq());
		ZoomFreq * newfreq = &(zoomFreqs.back());
		last = 0;
		at = 0;
		while(at != std::string::npos)
		{
			at = value.find_first_of('/', last);
			nestedkeyval = value.substr(last, at-last);
			splitat = nestedkeyval.find_first_of('@');
			nWarn += setkv(nestedkeyval.substr(0,splitat), nestedkeyval.substr(splitat+1), newfreq);
			last = at+1;
		}
	}
	else if(key == "mjdStart")
	{
		ss >> mjdStart;
	}
	else if(key == "mjdStop")
	{
		ss >> mjdStop;
	}
	else
	{
		std::cerr << "Warning: ANTENNA: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
	}

	return nWarn;
}

void AntennaSetup::copyGlobalZoom(const GlobalZoom &globalZoom)
{
	for(std::vector<ZoomFreq>::const_iterator it = globalZoom.zoomFreqs.begin(); it != globalZoom.zoomFreqs.end(); ++it)
	{
		zoomFreqs.push_back(*it);
	}
}

bool AntennaSetup::hasBasebandFile(const Interval &interval) const
{
	for(std::vector<VexBasebandFile>::const_iterator it = basebandFiles.begin(); it != basebandFiles.end(); ++it)
	{
		if(it->overlap(interval) > 0.0)
		{
			return true;
		}
	}

	return false;
}

int GlobalZoom::setkv(const std::string &key, const std::string &value, ZoomFreq *zoomFreq)
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
		{
			zoomFreq->correlateparent = false;
		}
		else
		{
			std::cerr << "Warning: Currently correlation of rec bands that are parents to globally defined zoom bands is not supported.  Results will be unpredictable." << std::endl;
			++nWarn;

			zoomFreq->correlateparent = true;
		}
	}
	else if(key == "specAvg" || key == "SPECAVG" || key == "specavg")
	{
		zoomFreq->spectralaverage = atoi(value.c_str());
	}
	else
	{
		std::cerr << "Warning: ZOOM: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
	}

	return nWarn;
	
}

int GlobalZoom::setkv(const std::string &key, const std::string &value)
{
	int nWarn = 0;

	if(key == "addZoomFreq" || key == "zoom" || key == "zoomFreq")
	{
		// This is a bit tricky.  All parameters must be together, with @ replacing =, and separated by /
		// e.g., addZoomFreq = freq@1649.99/bw@1.0/correlateparent@TRUE/specAvg@8
		// only freq and bw are compulsory; default is parent values and don't correlate parent
		zoomFreqs.push_back(ZoomFreq());
		ZoomFreq * newfreq = &(zoomFreqs.back());
		std::string::size_type last = 0;
		std::string::size_type at = 0;
		std::string::size_type splitat = 0;
		std::string nestedkeyval;
		while(at != std::string::npos)
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
		std::cerr << "Warning: ZOOM: Unknown parameter '" << key << "'." << std::endl; 
		++nWarn;
	}

	return nWarn;
}

CorrParams::CorrParams()
{
	defaults();

	parseWarnings = 0;
}

CorrParams::CorrParams(const std::string &fileName)
{
	size_t pos;

	defaults();

	pos = fileName.find(".");
	jobSeries = fileName.substr(0, pos);

	parseWarnings = load(fileName);

#ifdef DONT_USE_EXPER_AS_PASS
	pos = vexFile.find(".");
	std::string vexBase = jobSeries.substr(0, pos);
	if(vexBase == jobSeries)
	{
		jobSeries = "main";
	}
#endif
}

void CorrParams::defaults()
{
	jobSeries = "job";
	minSubarraySize = 2;
	maxGap = 180.0/86400.0;		// 3 minutes
	singleScan = false;
	fakeDatasource = false;
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
	maxReadSize = 25000000;		// Bytes	More and risk XLRRead problems
	minReadSize = 4000000;		// Bytes	Less and inefficiency is likely
	invalidMask = ~0;		// write flags for all types of invalidity
	visBufferLength = 80;
	v2dMode = V2D_MODE_NORMAL;
	overSamp = 0;
	outputFormat = OutputFormatDIFX;
	nCore = 0;
	nThread = 0;
	tweakIntTime = false;
}

void pathify(std::string &filename)
{
	if(filename[0] == '/')
	{
		return;
	}

	char cwd[DIFXIO_FILENAME_LENGTH];
	std::string fn;

	if(getcwd(cwd, DIFXIO_FILENAME_LENGTH-1) == 0)
	{
		std::cerr << "Cannot getcwd()" << std::endl;

		exit(EXIT_FAILURE);
	}
	fn = std::string(cwd) + std::string("/") + filename;

	filename = fn;
}

int CorrParams::setkv(const std::string &key, const std::string &value)
{
	std::stringstream ss;
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
	else if(key == "fake")
	{
		fakeDatasource = isTrue(value);
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
		unsigned int l = value.size();
		for(unsigned int i = 0; i < l; ++i)
		if(!isalnum(value[i]))
		{
			std::cerr << "Error: jobSeries must be purely alphanumeric" << std::endl;

			exit(EXIT_FAILURE);	
		}
		ss >> jobSeries;
	}
	else if(key == "startSeries")
	{
		ss >> startSeries;
		if(startSeries < 0)
		{
			std::cerr << "Error: startSeries cannot be < 0" << std::endl;
			
			exit(EXIT_FAILURE);
		}
	}
	else if(key == "outPath")
	{
		ss >> outPath;
	}
	else if(key == "dataBufferFactor")
	{
		ss >> dataBufferFactor;
	}
	else if(key == "nDataSegments")
	{
		ss >> nDataSegments;
	}
	else if(key == "maxReadSize")
	{
		ss >> maxReadSize;
	}
	else if(key == "minReadSize")
	{
		ss >> minReadSize;
	}
	else if(key == "padScans")
	{
		padScans = isTrue(value);
	}
	else if(key == "invalidMask")
	{
		std::stringstream sss;
	  
		sss << std::setbase(16) << value;
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
		tweakIntTime = isTrue(value);
	}
	else if(key == "antennas")
	{
		std::string s;
		ss >> s;
		Upper(s);
		addAntenna(s);
	}
	else if(key == "baselines")
	{
		std::string s;
		ss >> s;
		Upper(s);
		addBaseline(s);
	}
	else if(key == "mode")
	{
		std::string s;
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
			std::cerr << "Warning: Illegal value " << value << " for mode" << std::endl;
			++nWarn;
		}
	}
	else if(key == "outputFormat")
	{
		std::string s;
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
	else if(key == "machines")
	{
		std::string s;
		ss >> s;
		Lower(s);
		machines.push_back(s);
	}
	else
	{
		std::cerr << "Warning: Unknown keyword " << key << " with value " << value << std::endl;
		++nWarn;
	}

	return nWarn;
}

void CorrParams::addAntenna(const std::string &antName)
{
	if(find(antennaList.begin(), antennaList.end(), antName) == antennaList.end())
	{
		antennaList.push_back(antName);
	}
}

void CorrParams::addBaseline(const std::string &baselineName)
{
	size_t pos;

	pos = baselineName.find("-");

	if(pos == std::string::npos)
	{
		std::cerr << "Error in baseline designation: " << baselineName << " : a hyphen is required." << std::endl;

		exit(EXIT_FAILURE);
	}

	if(pos == 0 || pos == baselineName.length()-1)
	{
		std::cerr << "Error in baseline designation: " << baselineName << " : need characters before and after the hyphen." << std::endl;
		
		exit(EXIT_FAILURE);
	}

	baselineList.push_back(std::pair<std::string,std::string>(
		baselineName.substr(0, pos),
		baselineName.substr(pos+1) ));
}

int CorrParams::load(const std::string &fileName)
{
	enum Parse_Mode
	{
		PARSE_MODE_GLOBAL,
		PARSE_MODE_SETUP,
		PARSE_MODE_RULE,
		PARSE_MODE_SOURCE,
		PARSE_MODE_ANTENNA,
		PARSE_MODE_GLOBAL_ZOOM,
		PARSE_MODE_EOP,
		PARSE_MODE_COMMENT
	};

	const int MaxLineLength = 4*1024;

	std::ifstream is;
	std::vector<std::string> tokens;
	char s[MaxLineLength];
	CorrSetup   *corrSetup=0;
	CorrRule    *rule=0;
	SourceSetup *sourceSetup=0;
	AntennaSetup *antennaSetup=0;
	GlobalZoom  *globalZoom=0;
	VexEOP       *eop=0;
	Parse_Mode parseMode = PARSE_MODE_GLOBAL;
	int nWarn = 0;

	is.open(fileName.c_str());

	if(is.fail())
	{
		std::cerr << "Error: cannot open " << fileName << std::endl;

		exit(EXIT_FAILURE);
	}

	for(;;)
	{
		is.getline(s, MaxLineLength);
		if(is.eof())
		{
			break;
		}
		std::string ss = s;

		int l = ss.size();
		int t = 0;
		char last = ' ';
		for(int i = 0; i <= l; ++i)
		{
			// comment
			if(last <= ' ' && s[i] == '#')
			{	
				break;
			}
			else if(s[i] == '{' || s[i] == '}' || s[i] == '=' || s[i] == ',')
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
	std::string key(""), value, last("");
	for(std::vector<std::string>::const_iterator i = tokens.begin(); i != tokens.end(); ++i)
	{
		keyWaitingTemp = false;
		if(parseMode == PARSE_MODE_COMMENT)
		{
			if(*i == "}")
			{
				v2dComment += '\n';
				parseMode = PARSE_MODE_GLOBAL;
			}
			else
			{
				v2dComment += *i;
				v2dComment += ' ';
			}
		}
		else if(*i == "SETUP")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: SETUP out of place." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			++i;
			corrSetups.push_back(CorrSetup(*i));
			corrSetup = &corrSetups.back();
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: SETUP" << corrSetup->corrSetupName << ": '{' expected." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			key = "";
			parseMode = PARSE_MODE_SETUP;
		}
		else if(*i == "RULE")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: RULE out of place." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			++i;
			rules.push_back(CorrRule(*i));
			rule = &rules.back();
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: RULE " << rule->ruleName << ": '{' expected." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			key = "";
			parseMode = PARSE_MODE_RULE;
		}
		else if(*i == "SOURCE")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: SOURCE out of place." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			++i;
			if(getSourceSetup(*i) != 0)
			{
				std::cerr << "Error: Trying to add a setup for source " << *i << " which already has one!" << std::endl;

				exit(EXIT_FAILURE);
			}
			sourceSetups.push_back(SourceSetup(*i));
			sourceSetup = &sourceSetups.back();
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: SOURCE" << sourceSetup->vexName << ": '{' expected." << std::endl;

				exit(EXIT_FAILURE);
			}
			key = "";
			parseMode = PARSE_MODE_SOURCE;
		}
		else if(*i == "ANTENNA")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: ANTENNA out of place." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			++i;
			std::string antName(*i);
			Upper(antName);
			if(getAntennaSetup(antName) != 0)
			{
				std::cerr << "Error: two ANTENNA blocks set for antenna " << antName << std::endl;

				exit(EXIT_FAILURE);
			}
			antennaSetups.push_back(AntennaSetup(antName));
			antennaSetup = &antennaSetups.back();
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: ANTENNA " << antennaSetup->vexName << ": '{' expected." << std::endl;

				exit(EXIT_FAILURE);
			}
			key = "";
			parseMode = PARSE_MODE_ANTENNA;
		}
		else if(*i == "COMMENT")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: COMMENT out of place." << std::endl;

				exit(EXIT_FAILURE);
			}
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: COMMENT: '{' expected." << std::endl;

				exit(EXIT_FAILURE);
			}
			key = "";
			v2dComment += "\n";
			parseMode = PARSE_MODE_COMMENT;
		}
		else if(*i == "ZOOM")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: ZOOM out of place." << std::endl;
				
				exit(EXIT_FAILURE);
			}
			++i;
			std::string zoomName(*i);
			globalZooms.push_back(GlobalZoom(zoomName));
			globalZoom = &globalZooms.back();
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: ZOOM " << globalZoom->difxName << ": '{' expected." << std::endl;

				exit(EXIT_FAILURE);
			}
			key = "";
			parseMode = PARSE_MODE_GLOBAL_ZOOM;
		}
		else if(*i == "EOP")
		{
			if(parseMode != PARSE_MODE_GLOBAL)
			{
				std::cerr << "Error: ANTENNA out of place." << std::endl;

				exit(EXIT_FAILURE);
			}
			++i;
			std::string eopDate(*i);
			eops.push_back(VexEOP());
			eop = &eops.back();
			eop->mjd = parseTime(eopDate);
			++i;
			if(*i != "{")
			{
				std::cerr << "Error: '{' expected." << std::endl;

				exit(EXIT_FAILURE);
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
			std::cerr << "Warning: unexpected character '" << *i << "'." << std::endl;
			++nWarn;
		}
		else if(last == "=" || last == ",")
		{
			if(key == "")
			{
				std::cerr << "Error: legal parameter name expected before " << *i << std::endl;

				exit(EXIT_FAILURE);
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
			case PARSE_MODE_GLOBAL_ZOOM:
				nWarn += globalZoom->setkv(key, value);
				break;
			case PARSE_MODE_COMMENT:
				// nothing to do here
				break;
			}
		}
		else
		{
			if(keyWaiting == true)
			{
				std::cerr << "Parse error in file " << fileName << " : Unused token: " << last << " before token: " << *i << std::endl;

				exit(EXIT_FAILURE);
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
	if(corrSetups.empty())
	{
		defaultSetup();
	}

	if(rules.empty())
	{
		defaultRule();
	}

	if(baselineList.empty())
	{
		addBaseline("*-*");
	}

	// populate global zoom bands into antennas that want them
	for(std::vector<AntennaSetup>::iterator it = antennaSetups.begin(); it != antennaSetups.end(); ++it)
	{
		if(!it->globalZoom.empty())
		{
			const GlobalZoom *z = getGlobalZoom(it->globalZoom);

			if(!z)
			{
				std::cerr << "Error: referenced ZOOM " << it->globalZoom << " is not defined!" << std::endl;

				exit(EXIT_FAILURE);
			}

			it->copyGlobalZoom(*z);
		}
	}

	return nWarn;
}

int CorrParams::checkSetupValidity()
{
	int nWarn = 0;

	// if specAvg was used
	for(std::vector<CorrSetup>::iterator c = corrSetups.begin(); c != corrSetups.end(); ++c)
	{
		bool nChanSpecAvgOK = false;
		bool specAvgUsed = false;
		double minBW = c->getMinRecordedBandwidth();
		double maxBW = c->getMaxRecordedBandwidth();

		if(!c->nFFTChan && !c->nOutputChan && !c->explicitFFTSpecRes && !c->explicitOutputSpecRes)
		{
			std::cerr << "WARNING: No information was provided regarding spectral resolution.  Basic defaults WILL be used.  Please check that this suits your needs." << std::endl;
			++nWarn;
		}

		if(c->suppliedSpecAvg)
		{
			if(c->explicitFFTSpecRes)
			{
				if(!c->explicitOutputSpecRes)
				{
					specAvgUsed = true;
					c->outputSpecRes = c->FFTSpecRes * c->suppliedSpecAvg;
				}
			}
			else
			{
				if(c->explicitOutputSpecRes)
				{
					specAvgUsed = true;
					c->FFTSpecRes = c->outputSpecRes / c->suppliedSpecAvg;
				}
				else
				{
					nChanSpecAvgOK = true;
				}
			}
		}
	

		// if user wants to set channels explicitly

		if(c->nFFTChan || c->nOutputChan)
		{
			if(c->suppliedSpecAvg != 0)
			{
				if(c->nFFTChan && c->nOutputChan)
				{
					std::cerr << "Error: number of channels is overspecified.  Please don't use all three of: nFFTChan, nOutputChan and specAvg" << std::endl;

					exit(EXIT_FAILURE);
				}
				if(nChanSpecAvgOK)
				{
					specAvgUsed = true;
					if(c->nFFTChan)
					{
						c->nOutputChan = static_cast<int>(c->nFFTChan / c->suppliedSpecAvg + 0.5);
					}
					else
					{
						c->nFFTChan = static_cast<int>(c->nOutputChan * c->suppliedSpecAvg + 0.5);
					}
				}
			}

			if(c->explicitFFTSpecRes)
			{
				std::cerr << "Warning: number of channels and spectral resolutions provided!  Ignoring number of channels"  << std::endl;
				++nWarn;
			}
			else if(minBW != maxBW)
			{
				std::cerr << "Warning: cannot specify number of channels when different sub-band bandwidths exist within one setup." << std::endl;
				std::cerr << "Note! If you get this message while performing zoom-banding the fix is probably to specify specRes instead of nChan." << std::endl;
				++nWarn;
			}
			else
			{
				if(c->nFFTChan)
				{
					c->FFTSpecRes = minBW/c->nFFTChan;
					if(c->nOutputChan)
					{
						if(c->nFFTChan % c->nOutputChan != 0)
						{
							std::cerr << "Error: supplied number of FFT channels " << c->nFFTChan << " is not a multiple of number of supplied output channels " << c->nOutputChan << std::endl;

							exit(EXIT_FAILURE);
						}
						c->outputSpecRes = minBW/c->nOutputChan;
					}
					else
					{
						c->outputSpecRes = c->nFFTChan;
					}
				}
				else // here only nOutputChan is defined
				{
					int n = (c->nOutputChan < 128) ? 128 : c->nOutputChan;
					c->outputSpecRes = minBW/c->nOutputChan;
					c->FFTSpecRes = minBW/n;
				}
			}
		}
		if(c->suppliedSpecAvg && !specAvgUsed)
		{
			std::cerr << "Warning: the value 'specAvg' supplied in the .v2d file was not used because the averaging was already over specified.  Please verify the spectral resolutions being used are appropriate for this project!" << std::endl;
			++nWarn;
		}
	}

	// update spectral resolution and channels if needed
	for(std::vector<CorrSetup>::iterator c = corrSetups.begin(); c != corrSetups.end(); ++c)
	{
#warning "FIXME: This logic should consider number of antennas and possibly number of sub-bands"
		if(c->FFTSpecRes > c->outputSpecRes)
		{
			if(c->explicitFFTSpecRes)
			{
				std::cerr << "You have explicitly requested an FFT resolution of " << c->FFTSpecRes << " Hz, but the output spectral resolution is set to " << c->outputSpecRes << " Hz.  This cannot be accommodated! If --force used, FFTSpecRes will be set to " << c->outputSpecRes << std::endl;
				++nWarn;
			}
			c->FFTSpecRes = c->outputSpecRes;
		}

		if(c->xmacLength == 0)
		{
			if(c->minInputChans() > 0)
			{
				if(c->minInputChans() > 128)
				{
					const int trialXmacLength[] = {512, 400, 256, 250, 128, 125, 100, 80, 64, 50, 40, 25, 20, 10, 8, 5, 4, 2, 1};

					for(int i = 0; ; ++i)
					{
						if(c->minInputChans() % trialXmacLength[i] == 0)
						{
							c->xmacLength = trialXmacLength[i];
							break;
						}
					}
				}
				else
				{
					c->xmacLength = c->minInputChans();
				}
			}
		}
		if(c->strideLength == 0)
		{
			if(c->minInputChans() > 0)
			{
				const int trialStrideLength[] = {128, 125, 100, 80, 64, 50, 40, 25, 20, 10, 8, 5, 4, 2, 1};
				int goal;

				goal = static_cast<int>(sqrt(static_cast<double>(c->minInputChans())) + 0.5);

				for(int i = 0; ; ++i)
				{
					if(c->minInputChans() % trialStrideLength[i] == 0 && trialStrideLength[i] <= goal)
					{
						c->strideLength = trialStrideLength[i];
						break;
					}
				}
			}
		}
	}

	// check that all setups are sensible
	for(std::vector<CorrSetup>::const_iterator c = corrSetups.begin(); c != corrSetups.end(); ++c)
	{
		nWarn += c->checkValidity();
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
	corrSetups.back().FFTSpecRes = 250000.0;
	corrSetups.back().outputSpecRes = 500000.0;
	corrSetups.push_back(CorrSetup("default"));
	rules.push_back(CorrRule("1413+15"));
	rules.back().sourceName.push_back(std::string("1413+15"));
	rules.back().corrSetupName = std::string("1413+15");
	rules.push_back(CorrRule("1713+07"));
	rules.back().sourceName.push_back(std::string("1713+07"));
	rules.back().corrSetupName = std::string("default");
	rules.push_back(CorrRule("X"));
	rules.back().scanName.push_back(std::string("No0006"));
	rules.back().corrSetupName = std::string("bogus");
}

int CorrParams::sanityCheck()
{
	int nWarn = 0;

	nWarn += checkSetupValidity();

	if(minSubarraySize > antennaList.size() && !antennaList.empty())
	{
		std::cerr << "Warning: the antenna list has fewer than minSubarray antennas.  No jobs will be made." << std::endl;
		++nWarn;
	}

	const AntennaSetup *a = getAntennaSetup("DEFAULT");
	if(a)
	{
		if(a->X != 0.0 || a->Y != 0 || a->Z != 0)
		{
			std::cerr << "Warning: the default antenna's coordinates are set!" << std::endl;
			++nWarn;
		}
		if(a->clock.offset != 0 || a->clock.rate != 0 || a->clock2 != 0 || a->clock3 != 0 || a->clock4 != 0 || a->clock5 != 0 || a->clock.offset_epoch != 0 || a->deltaClock != 0 || a->deltaClockRate != 0)
		{
			std::cerr << "Warning: the default antenna's clock parameters are set!" << std::endl;
			++nWarn;
		}
	}

	if(nThread != 0 || nCore != 0)
	{
		if(nThread <= 0 || nCore <= 0)
		{
			std::cerr << "Warning: nThread and nCore must either both or neither be set." << std::endl;
			++nWarn;
		}
	}

	return nWarn;
}

bool antennaMatch(const std::string &a1, const std::string &a2)
{
	if(a1 == "*" || a2 == "*")
	{
		return true;
	}
	if(a1 == a2)
	{
		return true;
	}
	if(a1.find(a2) != std::string::npos)
	{
		return true;
	}

	return false;
}

bool baselineMatch(const std::pair<std::string,std::string> &bl, const std::string &ant1, const std::string &ant2)
{
	if(antennaMatch(bl.first, ant1) &&
	   antennaMatch(bl.second, ant2) )
	{
		return true;
	}
	
	return false;
}

bool CorrParams::useAntenna(const std::string &antName) const
{
	std::list<std::string>::const_iterator it;

	if(antennaList.empty())
	{
		return true;
	}

	for(it = antennaList.begin(); it != antennaList.end(); ++it)
	{
		if(antennaMatch(*it, antName))
		{
			return true;
		}
	}

	return false;
}

bool CorrParams::useBaseline(const std::string &ant1, const std::string &ant2) const
{
	std::list<std::pair<std::string,std::string> >::const_iterator it;

	if(baselineList.empty())
	{
		return true;
	}

	for(it = baselineList.begin(); it != baselineList.end(); ++it)
	{
		if(baselineMatch(*it, ant1, ant2) ||
		   baselineMatch(*it, ant2, ant1))
		{
			return true;
		}
	}

	return false;
}

bool CorrParams::swapPol(const std::string &antName) const
{
	std::vector<AntennaSetup>::const_iterator a;

	for(a = antennaSetups.begin(); a != antennaSetups.end(); ++a)
	{
		if(a->vexName == antName)
		{
			return a->polSwap;
		}
	}

	return false;
}

const VexClock *CorrParams::getAntennaClock(const std::string &antName) const
{
	std::vector<AntennaSetup>::const_iterator a;

	for(a = antennaSetups.begin(); a != antennaSetups.end(); ++a)
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

const AntennaSetup *CorrParams::getAntennaSetup(const std::string &name) const
{
	const AntennaSetup *a = 0;

	for(std::vector<AntennaSetup>::const_iterator it = antennaSetups.begin(); it != antennaSetups.end(); ++it)
	{
		if(it->vexName == "DEFAULT")
		{
			// keep this as a placeholder in case nothing better is found
			a = &(*it);
		}
		if(it->vexName == name)
		{
			a = &(*it);
			break;
		}
	}

	return a;
}

const GlobalZoom *CorrParams::getGlobalZoom(const std::string &name) const
{
	const GlobalZoom *z = 0;

	for(std::vector<GlobalZoom>::const_iterator it = globalZooms.begin(); it != globalZooms.end(); ++it)
	{
		if(it->difxName == name)
		{
			z = &(*it);
			break;
		}
	}

	return z;
}

void CorrParams::addSourceSetup(const SourceSetup &toAdd)
{
	for(std::vector<SourceSetup>::const_iterator it = sourceSetups.begin(); it != sourceSetups.end(); ++it)
	{
		if(it->vexName == toAdd.vexName)
		{
			std::cerr << "Error: Trying to add a setup for source " << toAdd.vexName << " which already has one!" << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	sourceSetups.push_back(toAdd);
}

const CorrSetup *CorrParams::getCorrSetup(const std::string &name) const
{
	for(std::vector<CorrSetup>::const_iterator it = corrSetups.begin(); it != corrSetups.end(); ++it)
	{
		if(it->corrSetupName == name)
		{
			return &(*it);
		}
	}

	return 0;
}

CorrSetup *CorrParams::getNonConstCorrSetup(const std::string &name)
{
	for(std::vector<CorrSetup>::iterator it = corrSetups.begin(); it != corrSetups.end(); ++it)
	{
		if(it->corrSetupName == name)
		{
			return &(*it);
		}
	}

	return 0;
}

const SourceSetup *CorrParams::getSourceSetup(const std::string &name) const
{
	for(std::vector<SourceSetup>::const_iterator it = sourceSetups.begin(); it != sourceSetups.end(); ++it)
	{
		if(it->vexName == name)
		{
			return &(*it);
		}
	}

	return 0;
}

const SourceSetup *CorrParams::getSourceSetup(const std::vector<std::string> &names) const
{
	for(std::vector<SourceSetup>::const_iterator it = sourceSetups.begin(); it != sourceSetups.end(); ++it)
	{
		if(find(names.begin(), names.end(), it->vexName) != names.end())
		{
			return &(*it);
		}
	}

	return 0;
}

const PhaseCentre *CorrParams::getPhaseCentre(const std::string &difxName) const
{
	for(std::vector<SourceSetup>::const_iterator ss = sourceSetups.begin(); ss != sourceSetups.end(); ++ss)
	{
		if(ss->pointingCentre.difxName == difxName)
		{
			return &(ss->pointingCentre);
		}
		for(std::vector<PhaseCentre>::const_iterator pc = ss->phaseCentres.begin(); pc != ss->phaseCentres.end(); ++pc)
		{
			if(pc->difxName == difxName)
			{
				return &(*pc);
			}
		}
	}

	return 0;
}

const std::string &CorrParams::findSetup(const std::string &scan, const std::string &source, const std::string &mode, char cal, int qual) const
{
	std::vector<CorrRule>::const_iterator it;
	static const std::string def("default");
	static const std::string none("");

	for(it = rules.begin(); it != rules.end(); ++it)
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

std::ostream& operator << (std::ostream &os, const CorrSetup &x)
{
	int p;

	p = os.precision();
	os.precision(6);

	os << "SETUP " << x.corrSetupName << std::endl;
	os << "{" << std::endl;
	os << "  tInt=" << x.tInt << std::endl;
	os << "  FFTSpecRes=" << (x.FFTSpecRes*1e-6) << std::endl;
	os << "  outputSpecRes=" << (x.outputSpecRes*1e-6) << std::endl;
	os << "  doPolar=" << x.doPolar << std::endl;
	os << "  doAuto=" << x.doAuto << std::endl;
	os << "  subintNS=" << x.subintNS << std::endl;
	os << "  fringeRotOrder=" << x.fringeRotOrder << std::endl;
	if(!x.binConfigFile.empty())
	{
		os << "  binConfig=" << x.binConfigFile << std::endl;
	}
	os << "}" << std::endl;

	os.precision(p);

	return os;
}

std::ostream& operator << (std::ostream &os, const CorrRule &x)
{
	bool space = false;
	os << "RULE " << x.ruleName << std::endl;
	os << "{" << std::endl;
	if(!x.scanName.empty())
	{
		std::list<std::string>::const_iterator it;

		os << "  scan=";
		for(it = x.scanName.begin(); it != x.scanName.end(); ++it)
		{
			os << " " << *it;
		}
		os << std::endl;
		space = true;
	}
	if(!x.sourceName.empty())
	{
		std::list<std::string>::const_iterator it;

		os << "  source=";
		for(it = x.sourceName.begin(); it != x.sourceName.end(); ++it)
		{
			os << " " << *it;
		}
		os << std::endl;
		space = true;
	}
	if(!x.modeName.empty())
	{
		std::list<std::string>::const_iterator it;

		os << "  mode=";
		for(it = x.modeName.begin(); it != x.modeName.end(); ++it)
		{
			os << " " << *it;
		}
		os << std::endl;
		space = true;
	}
	
	if(space)
	{
		os << std::endl;
	}
	os << "  correlator setup=" << x.corrSetupName << std::endl;
	
	os << "}" << std::endl;

	return os;
}

std::ostream& operator << (std::ostream &os, const SourceSetup &x)
{
	os << "SOURCE " << x.vexName << std::endl;
	os << "{" << std::endl;
	if(!x.pointingCentre.difxName.empty())
	{
		os << "  pointing centre name=" << x.pointingCentre.difxName << std::endl;
	}
	if(x.doPointingCentre)
	{
		os << "  pointing centre is correlated" << std::endl;
	}
	else
	{
		os << "  pointing centre is not correlated" << std::endl;
	}
	if(x.pointingCentre.ra > PhaseCentre::DEFAULT_RA)
	{
		os << "  pointing centre ra=" << x.pointingCentre.ra << " # J2000" << std::endl;
	}
	if(x.pointingCentre.dec > PhaseCentre::DEFAULT_DEC)
	{
		os << "  pointing centre dec=" << x.pointingCentre.dec << " # J2000" << std::endl;
	}
	if(x.pointingCentre.calCode != ' ')
	{
		os << "  pointing centre calCode=" << x.pointingCentre.calCode << std::endl;
	}
	os << "  Number of additional phase centres is " << x.phaseCentres.size() << std::endl;
	os << "}" << std::endl;

	return os;
}

std::ostream& operator << (std::ostream &os, const AntennaSetup &x)
{
	os << "ANTENNA " << x.vexName << std::endl;
	os << "{" << std::endl;
	if(!x.difxName.empty())
	{
		os << "  name=" << x.difxName << std::endl;
	}
	if(fabs(x.X) > 0.1 || fabs(x.Y) > 0.1 || fabs(x.Z) > 0.1)
	{
		os << "  X=" << x.X <<" Y=" << x.Y << " Z=" << x.Z << std::endl;
	}
	if(x.axisOffset > -1.0e5)
	{
		os << "  axisOffset=" << x.axisOffset << std::endl;
	}
	if(x.clock.mjdStart > 0.0)
	{
		os << "  clockOffset=" << x.clock.offset*1.0e6 << std::endl;
		os << "  clockRate=" << x.clock.rate*1.0e6 << std::endl;
		os << "  clockEpoch=" << x.clock.offset_epoch << std::endl;
	}
	os << "  polSwap=" << x.polSwap << std::endl;
	if(!x.format.empty())
	{
		os << "  format=" << x.format << std::endl;
	}
	os << "  # dataSource=" << dataSourceNames[x.dataSource] << std::endl;
	if(x.dataSource == DataSourceNetwork)
	{
		os << "  networkPort=" << x.networkPort << std::endl;
		os << "  windowSize=" << x.windowSize << std::endl;
	}
	os << "  phaseCalInt=" << x.phaseCalIntervalMHz << std::endl;
	os << "  tcalFreq=" << x.tcalFrequency << std::endl;

	os << "}" << std::endl;

	return os;
}

std::ostream& operator << (std::ostream &os, const CorrParams &x)
{
	int p;

	p = os.precision();
	os.precision(13);

	os << "# correlation parameters" << std::endl;

	os << "vex=" << x.vexFile << std::endl;
	switch(x.v2dMode)
	{
	case V2D_MODE_NORMAL:
		os << "mode=normal" << std::endl;
		break;
	case V2D_MODE_PROFILE:
		os << "mode=profile" << std::endl;
		break;
	}
	os << "mjdStart=" << x.mjdStart << std::endl;
	os << "mjdStop=" << x.mjdStop << std::endl;
	for(std::vector<double>::const_iterator mb = x.manualBreaks.begin(); mb != x.manualBreaks.end(); ++mb)
	{
		os << "break=" << *mb << std::endl;
	}
	os << "minSubarray=" << x.minSubarraySize << std::endl;
	os << "visBufferLength=" << x.visBufferLength << std::endl;

	os.precision(6);
	os << "maxGap=" << x.maxGap*86400.0 << " # seconds" << std::endl;
	os << "maxLength=" << x.maxLength*86400.0 << " # seconds" << std::endl;
	os << "minLength=" << x.minLength*86400.0 << " # seconds" << std::endl;
	os << "maxSize=" << x.maxSize/1000000.0 << " # MB" << std::endl;
	os.precision(13);

	if(x.threadsFile != "")
	{
		os << "threadsFile=" << x.threadsFile << std::endl;
	}
	os << "singleScan=" << x.singleScan << std::endl;
	os << "singleSetup=" << x.singleSetup << std::endl;
	if(x.nCore > 0 && x.nThread > 0)
	{
		os << "nCore=" << x.nCore << std::endl;
		os << "nThread=" << x.nCore << std::endl;
	}
	os << "mediaSplit=" << x.mediaSplit << std::endl;
	os << "jobSeries=" << x.jobSeries << std::endl;
	os << "startSeries=" << x.startSeries << std::endl;
	os << "dataBufferFactor=" << x.dataBufferFactor << std::endl;
	os << "nDataSegments=" << x.nDataSegments << std::endl;
	os << "maxReadSize=" << x.maxReadSize << " # Bytes" << std::endl;
	os << "minReadSize=" << x.minReadSize << " # Bytes" << std::endl;
	os << "overSamp=" << x.overSamp << std::endl;
	os << "outputFormat=" << x.outputFormat << std::endl;
	
	if(!x.antennaList.empty())
	{
		os << "antennas=";
		for(std::list<std::string>::const_iterator a = x.antennaList.begin(); a != x.antennaList.end(); ++a)
		{
			if(a != x.antennaList.begin())
			{
				os << ",";
			}
			os << *a;
		}
		os << std::endl;
	}
	
	if(!x.baselineList.empty())
	{
		os << "baselines=";
		for(std::list<std::pair<std::string,std::string> >::const_iterator bl = x.baselineList.begin(); bl != x.baselineList.end(); ++bl)
		{
			if(bl != x.baselineList.begin())
			{
				os << ",";
			}
			os << bl->first << '-' << bl->second;
		}
		os << std::endl;
	}

	if(!x.antennaSetups.empty())
	{
		for(std::vector<AntennaSetup>::const_iterator as = x.antennaSetups.begin(); as != x.antennaSetups.end(); ++as)
		{
			os << std::endl;
			os << *as;
		}
	}

	if(!x.eops.empty())
	{
		os << std::endl;

		for(std::vector<VexEOP>::const_iterator ve = x.eops.begin(); ve != x.eops.end(); ++ve)
		{
			os << "EOP " << ve->mjd << " { ";
			os << "tai_utc=" << ve->tai_utc << " ";
			os << "ut1_utc=" << ve->ut1_utc << " ";
			os << "xPole=" << ve->xPole*RAD2ASEC << " ";
			os << "yPole=" << ve->yPole*RAD2ASEC << " }" << std::endl;
		}
	}

	if(!x.sourceSetups.empty())
	{
		for(std::vector<SourceSetup>::const_iterator ss = x.sourceSetups.begin(); ss != x.sourceSetups.end(); ++ss)
		{
			os << std::endl;
			os << *ss;
		}
	}

	if(!x.corrSetups.empty())
	{
		for(std::vector<CorrSetup>::const_iterator cs = x.corrSetups.begin(); cs != x.corrSetups.end(); ++cs)
		{
			os << std::endl;
			os << *cs;
		}
	}

	if(!x.rules.empty())
	{
		for(std::vector<CorrRule>::const_iterator cr = x.rules.begin(); cr != x.rules.end(); ++cr)
		{
			os << std::endl;
			os << *cr;
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
	else if(C->singleSetup)
	{
		if(A->tInt          == B->tInt          &&
		   A->FFTSpecRes    == B->FFTSpecRes    &&
		   A->outputSpecRes == B->outputSpecRes &&
		   A->doPolar       == B->doPolar       &&
		   A->doAuto        == B->doAuto        &&
		   A->binConfigFile.compare(B->binConfigFile) == 0)
		{
			return true;
		}
		else
		{
			return false;
		}
	}

	return true;
}

int CorrParams::loadShelves(const std::string &fileName)
{
	int nWarn = 0;
	std::ifstream is;
	bool doAntennas;
	char s[1024], a[32], v[32], ms[32];
	std::string vsn, shelf;
	std::vector<std::string> noShelf;

	is.open(fileName.c_str());

	if(is.fail())
	{
		return 0;
	}

	// only change antenna selection if the antenna list is empty to start with
	doAntennas = antennaList.empty();

	for(int lineNum = 1; ; ++lineNum)
	{
		is.getline(s, 1024);
		if(is.eof())
		{
			break;
		}
		for(int i = 0; s[i]; ++i)
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

		if(sscanf(s, "%31s%31s%31s", a, v, ms) != 3)
		{
			std::cerr << "Error: line " << lineNum << " of " << fileName << " not parsable." << std::endl;

			exit(EXIT_FAILURE);
		}

		std::string antName(a);
		Upper(antName);

		if(doAntennas)
		{
			addAntenna(antName);
		}
		else if(!useAntenna(antName))
		{
			continue;
		}

		vsn = std::string(v);
		shelf = std::string(ms);

		Upper(vsn);
		Upper(shelf);

		if(shelf == std::string("NONE"))
		{
			noShelf.push_back(vsn);
		}
		else
		{
			shelves[vsn] = shelf;
		}
	}

	is.close();

	if(!noShelf.empty())
	{
		std::cerr << "Warning: " << noShelf.size() << " modules have no shelf location:";
		for(std::vector<std::string>::const_iterator s = noShelf.begin(); s != noShelf.end(); ++s)
		{
			std::cerr << " " << *s;
		}
		std::cerr << std::endl;
	}

	return nWarn;
}

const char *CorrParams::getShelf(const std::string &vsn) const
{
	std::map<std::string,std::string>::const_iterator it;

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

const std::string &CorrParams::getNewSourceName(const std::string &origName) const
{
	std::vector<SourceSetup>::const_iterator it;

	for(it = sourceSetups.begin(); it != sourceSetups.end(); ++it)
	{
		if(it->vexName == origName)
		{
			if(it->pointingCentre.difxName != PhaseCentre::DEFAULT_NAME)
			{
				return it->pointingCentre.difxName;
			}
		}
	}

	return origName;
}
