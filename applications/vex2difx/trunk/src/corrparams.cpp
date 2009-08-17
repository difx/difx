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

#include <iostream>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <cstdio>
#include <cmath>
#include <cctype>
#include <string.h>
#include "util.h"
#include "corrparams.h"

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

double parseCoord(const char *str, char type)
{
	int sign = 1, l, n;
	double a, b, c;
	double v = -999999.0;

	if(type != ' ' && type != 'R' && type != 'D')
	{
		cerr << "Programmer error: parseTime: parameter 'type' has illegal value = " << type << endl;
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
	}

	return v;
}

// From http://oopweb.com/CPP/Documents/CPPHOWTO/Volume/C++Programming-HOWTO-7.html
void split(const string& str, vector<string>& tokens, const string& delimiters = " ")
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
	ifstream is;
	int n=0;
	char s[1024];
	vector<string> tokens;

	is.open(fileName.c_str());

	if(is.fail())
	{
		cerr << "Error: cannot open " << fileName << endl;
		exit(0);
	}

	for(int line=1; ; line++)
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
				atof(tokens[1].c_str()),
				atof(tokens[2].c_str()) ));
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
	specAvg = 0;
	startChan = 0;
	nOutChan = 0;
	nChan = 16;
	doPolar = true;
	doAuto = true;
	postFFringe = false;
	blocksPerSend = 0;
}

void CorrSetup::setkv(const string &key, const string &value)
{
	stringstream ss;

	ss << value;

	if(key == "tInt")
	{
		ss >> tInt;
	}
	else if(key == "nChan")
	{
		ss >> nChan;
	}
	else if(key == "doPolar")
	{
		doPolar = isTrue(value);
	}
	else if(key == "doAuto")
	{
		doAuto = isTrue(value);
	}
	else if(key == "blocksPerSend")
	{
		ss >> blocksPerSend;
	}
	else if(key == "specAvg")
	{
		ss >> specAvg;
	}
	else if(key == "startChan")
	{
		ss >> startChan;
	}
	else if(key == "nOutChan")
	{
		ss >> nOutChan;
	}
	else if(key == "postFFringe")
	{
		postFFringe = isTrue(value);
	}
	else if(key == "binConfig")
	{
		ss >> binConfigFile;
		
		if(binConfigFile[0] != '/')
		{
			char cwd[1024];
			string inFile;

			getcwd(cwd, 1023);
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
	}
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

double CorrSetup::bytesPerSecPerBLPerBand() const
{
	int chans = nOutChan>0 ? nOutChan : nChan;
	int pols = doPolar ? 2 : 1;

	// assume 8 bytes per complex

	return 8*chans*pols/tInt;
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

void CorrRule::setkv(const string &key, const string &value)
{
	stringstream ss;

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
	}
}

SourceSetup::SourceSetup(const string &name) : vexName(name)
{
	ra = -999;
	dec = -999;
	calCode = ' ';
	ephemDeltaT = 60.0;	// seconds
}

void SourceSetup::setkv(const string &key, const string &value)
{
	stringstream ss;

	ss << value;

	if(key == "ra" || key == "RA")
	{
		ra = parseCoord(value.c_str(), 'R');
	}
	else if(key == "dec" || key == "Dec")
	{
		dec = parseCoord(value.c_str(), 'D');
	}
	else if(key == "calCode")
	{
		ss >> calCode;
	}
	else if(key == "name" || key == "newName")
	{
		ss >> difxName;
	}
	else if(key == "ephemObject")
	{
		ss >> ephemObject;
	}
	else if(key == "ephemFile")
	{
		ss >> ephemFile;
	}
	else if(key == "naifFile")
	{
		ss >> naifFile;
	}
	else
	{
		cerr << "Warning: SOURCE: Unknown parameter '" << key << "'." << endl; 
	}
}

AntennaSetup::AntennaSetup(const string &name) : vexName(name)
{
}

void AntennaSetup::setkv(const string &key, const string &value)
{
	stringstream ss;

	ss << value;

	if(key == "name" || key == "newName")
	{
		ss >> difxName;
	}
	else if(key == "polSwap")
	{
		polSwap = isTrue(value);
	}
	else if(key == "clockOffset")
	{
		ss >> clock.offset;
		clock.offset /= 1.0e6;
		clock.mjdStart = 1;
	}
	else if(key == "clockRate")
	{
		ss >> clock.rate;
		clock.rate /= 1.0e6;
		clock.mjdStart = 1;
	}
	else if(key == "clockEpoch")
	{
		ss >> clock.offset_epoch;
		clock.mjdStart = 1;
	}
	else if(key == "X")
	{
		ss >> X;
	}
	else if(key == "Y")
	{
		ss >> Y;
	}
	else if(key == "Z")
	{
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
		basebandFiles.push_back(VexBasebandFile(value));
	}
	else if(key == "filelist")
	{
		loadBasebandFilelist(value, basebandFiles);
	}
	else
	{
		cerr << "Warning: ANTENNA: Unknown parameter '" << key << "'." << endl; 
	}
}

CorrParams::CorrParams()
{
	defaults();
}

CorrParams::CorrParams(const string& fileName)
{
	size_t pos;

	defaults();

	pos = fileName.find(".");
	jobSeries = fileName.substr(0, pos);

	load(fileName);

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
	sendLength = 0.1;		// (s)
	invalidMask = ~0;		// write flags for all types of invalidity
	visBufferLength = 32;
	v2dMode = V2D_MODE_NORMAL;
}

void CorrParams::setkv(const string &key, const string &value)
{
	stringstream ss;

	ss << value;
	
	if(key == "vex")
	{
		ss >> vexFile;

		if(vexFile[0] != '/')
		{
			char cwd[1024];
			string inFile;

			getcwd(cwd, 1023);
			inFile = string(cwd);
			inFile += string("/");
			inFile += vexFile;
			vexFile = inFile;
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
	else if(key == "break" || key == "breaks")
	{
		double mjd;

		ss >> mjd;

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
	}
	else if(key == "singleScan")
	{
		singleScan = isTrue(value);
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
		for(int i = 0; i < value.size(); i++)
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
	}
	else if(key == "dataBufferFactor")
	{
		ss >> dataBufferFactor;
	}
	else if(key == "nDataSegments")
	{
		ss >> nDataSegments;
	}
	else if(key == "sendLength")
	{
		ss >> sendLength;
	}
	else if(key == "padScans")
	{
		padScans = isTrue(value);
	}
	else if(key == "invalidMask")
	{
		ss >> invalidMask;
	}
	else if(key == "visBufferLength")
	{
		ss >> visBufferLength;
	}
	else if(key == "simFXCORR")
	{
		simFXCORR = isTrue(value);
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
		}
	}
	else
	{
		cerr << "Warning: Unknown keyword " << key << " with value " << value << endl;
	}
}

void CorrParams::addAntenna(const string& antName)
{
	if(find(antennaList.begin(), antennaList.end(), antName) == antennaList.end())
	{
		antennaList.push_back(antName);
	}
}

void CorrParams::addBaseline(const string& baselineName)
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

void CorrParams::load(const string& fileName)
{
	ifstream is;
	vector<string> tokens;
	char s[1024];
	CorrSetup   *corrSetup=0;
	CorrRule    *rule=0;
	SourceSetup *sourceSetup=0;
	AntennaSetup *antennaSetup=0;
	int mode = 0;	// an internal concept, not observing mode!

	is.open(fileName.c_str());

	if(is.fail())
	{
		cerr << "Error: cannot open " << fileName << endl;
		exit(0);
	}

	for(;;)
	{
		is.getline(s, 1024);
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
			if(mode != 0)
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
			mode = 1;
		}
		else if(*i == "RULE")
		{
			if(mode != 0)
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
			mode = 2;
		}
		else if(*i == "SOURCE")
		{
			if(mode != 0)
			{
				cerr << "Error: SOURCE out of place." << endl;
				exit(0);
			}
			i++;
			sourceSetups.push_back(SourceSetup(*i));
			sourceSetup = &sourceSetups.back();
			i++;
			if(*i != "{")
			{
				cerr << "Error: '{' expected." << endl;
				exit(0);
			}
			key = "";
			mode = 3;
		}
		else if(*i == "ANTENNA")
		{
			if(mode != 0)
			{
				cerr << "Error: ANTENNA out of place." << endl;
				exit(0);
			}
			i++;
			antennaSetups.push_back(AntennaSetup(*i));
			antennaSetup = &antennaSetups.back();
			i++;
			if(*i != "{")
			{
				cerr << "Error: '{' expected." << endl;
				exit(0);
			}
			key = "";
			mode = 4;
		}
		else if(*i == "}" && mode != 0)
		{
			mode = 0;
			key = "";
		}
		else if(*i == "=")
		{
			key = last;
		}
		else if(*i == "{" || *i == "}")
		{
			cerr << "Warning: unexpected character '" << *i << "'." << endl;
		}
		else if(last == "=" || last == ",")
		{
			if(key == "")
			{
				cerr << "Error: legal parameter name expected before " << *i << endl;
				exit(0);
			}
			value = *i;
			if(mode == 0)
			{
				setkv(key, value);
			}
			else if(mode == 1)
			{
				corrSetup->setkv(key, value);
			}
			else if(mode == 2)
			{
				rule->setkv(key, value);
			}
			else if(mode == 3)
			{
				sourceSetup->setkv(key, value);
			}
			else if(mode == 4)
			{
				antennaSetup->setkv(key, value);
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

	if(corrSetups.size() == 0 && rules.size() == 0)
	{
		defaultSetup();
	}

	if(baselineList.size() == 0)
	{
		addBaseline("*-*");
	}

	if(v2dMode == V2D_MODE_PROFILE)
	{
		int n = corrSetups.size();
		for(int i = 0; i < n; i++)
		{
			corrSetups[i].doPolar = false;
		}
	}
}

void CorrParams::defaultSetup()
{
	corrSetups.push_back(CorrSetup("default"));
}

void CorrParams::example()
{
	singleSetup = false;
	corrSetups.push_back(CorrSetup("1413+15"));
	corrSetups.back().tInt = 1.0;
	corrSetups.back().nChan = 64;
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

	n = antennaSetups.size();
	for(i = 0; i < n; i++)
	{
		if(name == antennaSetups[i].vexName)
		{
			return &antennaSetups[i];
		}
	}

	return 0;
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

ostream& operator << (ostream& os, const CorrSetup& x)
{
	int p;

	p = os.precision();
	os.precision(6);

	os << "SETUP " << x.corrSetupName << endl;
	os << "{" << endl;
	os << "  tInt=" << x.tInt << endl;
	os << "  nChan=" << x.nChan << endl;
	os << "  doPolar=" << x.doPolar << endl;
	os << "  doAuto=" << x.doAuto << endl;
	os << "  blocksPerSend=" << x.blocksPerSend << endl;
	os << "  specAvg=" << x.specAvg << endl;
	os << "  startChan=" << x.startChan << endl;
	os << "  nOutChan=" << x.nOutChan << endl;
	os << "  postFFringe=" << x.postFFringe << endl;
	if(x.binConfigFile.size() > 0)
	{
		os << "  binConfig=" << x.binConfigFile << endl;
	}
	os << "}" << endl;

	os.precision(p);

	return os;
}

ostream& operator << (ostream& os, const CorrRule& x)
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

ostream& operator << (ostream& os, const SourceSetup& x)
{
	os << "SOURCE " << x.vexName << endl;
	os << "{" << endl;
	if(x.difxName.size() > 0)
	{
		os << "  name=" << x.difxName << endl;
	}
	if(x.ra > -990)
	{
		os << "  ra=" << x.ra << " # J2000" << endl;
	}
	if(x.dec > -990)
	{
		os << "  dec=" << x.dec << " # J2000" << endl;
	}
	if(x.calCode != ' ')
	{
		os << "  calCode=" << x.calCode << endl;
	}
	os << "}" << endl;

	return os;
}

ostream& operator << (ostream& os, const AntennaSetup& x)
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
	os << "  #FIXME clock=" << endl;
	os << "  polSwap=" << x.polSwap << endl;
	if(x.format.size() > 0)
	{
		os << "  format=" << x.format << endl;
	}

	os << "}" << endl;

	return os;
}

ostream& operator << (ostream& os, const CorrParams& x)
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

	os << "singleScan=" << x.singleScan << endl;
	os << "singleSetup=" << x.singleSetup << endl;
	os << "mediaSplit=" << x.mediaSplit << endl;
	os << "jobSeries=" << x.jobSeries << endl;
	os << "startSeries=" << x.startSeries << endl;
	os << "dataBufferFactor=" << x.dataBufferFactor << endl;
	os << "nDataSegments=" << x.nDataSegments << endl;
	os << "sendLength=" << x.sendLength << " # seconds" << endl;
	
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
		if(A->tInt    == B->tInt    &&
		   A->nChan   == B->nChan   &&
		   A->doPolar == B->doPolar &&
		   A->doAuto  == B->doAuto     )
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

void CorrParams::loadShelves(const string& fileName)
{
	ifstream is;
	bool doAntennas;
	char s[1024], a[32], v[32], ms[32];
	string vsn, shelf;
	vector<string> noShelf;

	is.open(fileName.c_str());

	if(is.fail())
	{
		return;
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
}

const char *CorrParams::getShelf(const string& vsn) const
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
