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
#include <string.h>
#include "util.h"
#include "corrparams.h"

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
	}

	return v;
}

CorrSetup::CorrSetup(const string &name) : setupName(name)
{
	tInt = 2.0;
	specAvg = 0;
	startChan = 0;
	nChan = 16;
	doPolar = true;
	doAuto = true;
	postFFringe = false;
	blocksPerSend = 0;
}

void CorrSetup::set(const string &key, const string &value)
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
	else if(key == "postFFringe")
	{
		postFFringe = isTrue(value);
	}
	else if(key == "binConfig")
	{
		ss >> binConfigFile;
	}
	else
	{
		cerr << "Warning: SETUP: Unknown parameter '" << key << "'." << endl; 
	}
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

void CorrRule::set(const string &key, const string &value)
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
		ss >> setupName;
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
}

void SourceSetup::set(const string &key, const string &value)
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
	else
	{
		cerr << "Warning: SOURCE: Unknown parameter '" << key << "'." << endl; 
	}
}

AntennaSetup::AntennaSetup(const string &name) : vexName(name)
{
}

void AntennaSetup::set(const string &key, const string &value)
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
	defaults();

	load(fileName);
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
	mjdStart = 0.0;
	mjdStop = 1.0e7;
	startSeries = 1;
	dataBufferFactor = 32;
	nDataSegments = 8;
	sendLength = 0.1;		// (s)
	invalidMask = ~0;		// write flags for all types of invalidity
	visBufferLength = 32;
}

void CorrParams::set(const string &key, const string &value)
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
	}
	else if(key == "jobSeries" || key == "pass")
	{
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

void CorrParams::load(const string& fileName)
{
	ifstream is;
	vector<string> tokens;
	char s[1024];
	CorrSetup   *setup=0;
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

	string key(""), value, last("");
	for(vector<string>::const_iterator i = tokens.begin(); 
	    i != tokens.end();
	    i++)
	{
		if(*i == "SETUP")
		{
			if(mode != 0)
			{
				cerr << "Error: SETUP out of place." << endl;
				exit(0);
			}
			i++;
			setups.push_back(CorrSetup(*i));
			setup = &setups.back();
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
				set(key, value);
			}
			else if(mode == 1)
			{
				setup->set(key, value);
			}
			else if(mode == 2)
			{
				rule->set(key, value);
			}
			else if(mode == 3)
			{
				sourceSetup->set(key, value);
			}
			else if(mode == 4)
			{
				antennaSetup->set(key, value);
			}
		}
		if(*i == "{" || *i == "}")
		{
			last = "";
		}
		else
		{
			last = *i;
		}
	}

	is.close();

	// if no setups or rules declared, make the default setup

	if(setups.size() == 0 && rules.size() == 0)
	{
		defaultSetup();
	}
}

void CorrParams::defaultSetup()
{
	setups.push_back(CorrSetup("default"));
}

void CorrParams::example()
{
	singleSetup = false;
	setups.push_back(CorrSetup("1413+15"));
	setups.back().tInt = 1.0;
	setups.back().nChan = 64;
	setups.push_back(CorrSetup("default"));
	rules.push_back(CorrRule("1413+15"));
	rules.back().sourceName.push_back(string("1413+15"));
	rules.back().setupName = string("1413+15");
	rules.push_back(CorrRule("1713+07"));
	rules.back().sourceName.push_back(string("1713+07"));
	rules.back().setupName = string("default");
	rules.push_back(CorrRule("X"));
	rules.back().scanName.push_back(string("No0006"));
	rules.back().setupName = string("bogus");
}

bool CorrParams::useAntenna(const string &antName) const
{
	if(antennaList.empty() || find(antennaList.begin(), antennaList.end(), antName) != antennaList.end())
	{
		return true;
	}
	else
	{
		return false;
	}
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

	n = setups.size();
	for(i = 0; i < n; i++)
	{
		if(name == setups[i].setupName)
		{
			return &setups[i];
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
			return it->setupName;
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

	os << "SETUP " << x.setupName << endl;
	os << "{" << endl;
	os << "  tInt=" << x.tInt << endl;
	os << "  nChan=" << x.nChan << endl;
	os << "  doPolar=" << x.doPolar << endl;
	os << "  doAuto=" << x.doAuto << endl;
	os << "  blocksPerSend=" << x.blocksPerSend << endl;
	os << "  specAvg=" << x.specAvg << endl;
	os << "  startChan=" << x.startChan << endl;
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
	os << "  setup=" << x.setupName << endl;
	
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

ostream& operator << (ostream& os, const CorrParams& x)
{
	int p;

	p = os.precision();
	os.precision(13);

	os << "# correlation parameters" << endl;

	os << "vex=" << x.vexFile << endl;
	os << "mjdStart=" << x.mjdStart << endl;
	os << "mjdStop=" << x.mjdStop << endl;
	os << "minSubarray=" << x.minSubarraySize << endl;
	os << "visBufferLength=" << x.visBufferLength << endl;

	os.precision(6);
	os << "maxGap=" << x.maxGap*86400.0 << " # seconds" << endl;
	os << "maxLength=" << x.maxLength*86400.0 << " # seconds" << endl;
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

	if(!x.sourceSetups.empty())
	{
		vector<SourceSetup>::const_iterator it;

		for(it = x.sourceSetups.begin(); it != x.sourceSetups.end(); it++)
		{
			os << endl;
			os << *it;
		}
	}

	if(!x.setups.empty())
	{
		vector<CorrSetup>::const_iterator it;

		for(it = x.setups.begin(); it != x.setups.end(); it++)
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
	int nNoShelf = 0;

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
			nNoShelf++;
		}
		else
		{
			shelves[vsn] = shelf;
		}
	}

	is.close();

	if(nNoShelf > 0)
	{
		cerr << "Warning: " << nNoShelf << " modules have no shelf location." << endl;
	}
}

const char *CorrParams::getShelf(const string& vsn) const
{
	map<string,string>::const_iterator it;

	it = shelves.find(vsn);
	if(it == shelves.end())
	{
		if(vsn != "None")
		{
			cerr << "Warning: cannot find shelf for " << vsn << endl;
		}
		return "NONE";
	}
	else
	{
		return it->second.c_str();
	}
}
