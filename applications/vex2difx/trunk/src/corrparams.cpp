#include <iostream>
#include <algorithm>

#include "corrparams.h"

CorrSetup::CorrSetup(const string &name) : setupName(name)
{
	tInt = 2.0;
	specAvg = 1;
	startChan = 0;
	nChan = 16;
	doPolar = true;
	doAuto = true;
	postFFringe = false;
	blocksPerSend = 3125;
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
}

CorrParams::CorrParams() : jobSeries("vex2difx")
{
	minSubarraySize = 2;
	maxGap = 180.0/86400.0;		// 3 minutes
	singleScan = false;
	singleSetup = true;
	allowOverlap = false;
	mediaSplit = true;
	maxLength = 7200/86400.0;	// 2 hours
	mjdStart = 0.0;
	mjdStop = 1.0e7;
	startSeries = 20;
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
	if(getCorrSetup("default") != 0)
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
	os << "  tInt = " << x.tInt << endl;
	os << "  nChan = " << x.nChan << endl;
	os << "  doPolar = " << x.doPolar << endl;
	os << "  doAuto = " << x.doAuto << endl;
	os << "  blocksPerSend = " << x.blocksPerSend << endl;
	// FIXME phasecenters
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

		os << "  scan =";
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

		os << "  source =";
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

		os << "  mode =";
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
	os << "  setup = " << x.setupName << endl;
	
	os << "}" << endl;

	return os;
}

ostream& operator << (ostream& os, const CorrParams& x)
{
	int p;

	p = os.precision();
	os.precision(13);

	os << "# correlation parameters" << endl;

	os << "mjdStart = " << x.mjdStart << endl;
	os << "mjdStop  = " << x.mjdStop << endl;
	os << "minSubarray = " << x.minSubarraySize << endl;

	os.precision(6);
	os << "maxGap = " << x.maxGap*86400.0 << " # seconds" << endl;
	os << "maxLength = " << x.maxLength*86400.0 << " # seconds" << endl;
	os.precision(13);

	os << "singleScan = " << x.singleScan << endl;
	os << "singleSetup = " << x.singleSetup << endl;
	
	if(!x.antennaList.empty())
	{
		list<string>::const_iterator it;
		
		os << "antennas =";
		for(it = x.antennaList.begin(); it != x.antennaList.end(); it++)
		{
			os << " " << *it;
		}
		os << endl;
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
