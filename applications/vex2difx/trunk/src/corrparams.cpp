#include <algorithm>

#include "corrparams.h"

CorrSetup::CorrSetup(const string &name) : setupName(name)
{
	tInt = 2.0;
	nChan = 16;
	doPolar = true;
	doAuto = true;
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

CorrParams::CorrParams()
{
	minSubarraySize = 2;
	maxGap = 180.0/86400.0;		// 3 minutes
	singleScan = false;
	singleSetup = true;
	maxLength = 7200/86400.0;	// 2 hours
	mjdStart = 0.0;
	mjdStop = 1.0e7;
}

void CorrParams::defaultSetup()
{
	setups.push_back(CorrSetup("default"));
}

void CorrParams::example()
{
	setups.push_back(CorrSetup("1413+15"));
	setups.back().tInt = 1.0;
	setups.push_back(CorrSetup("default"));
	rules.push_back(CorrRule("1413+15"));
	rules.back().sourceName.push_back(string("1413+15"));
	rules.back().setupName = string("1413+15");
	rules.push_back(CorrRule("1713+07"));
	rules.back().sourceName.push_back(string("1713+07"));
	rules.back().setupName = string("skip");
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

bool areCorrSetupsCompatible(const CorrSetup &A, const CorrSetup &B, const CorrParams &C)
{
	if(C.singleScan)
	{
		return false;
	}
	if(C.singleSetup)
	{
		if(A.tInt    == B.tInt    &&
		   A.nChan   == B.nChan   &&
		   A.doPolar == B.doPolar &&
		   A.doAuto  == B.doAuto     )
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
