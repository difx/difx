#include "corrparams.h"

CorrSetup::CorrSetup()
{
	setupName = string("vex2difx_default");
	tInt = 2.0;
	nChan = 16;
	doPolar = true;
	doAuto = true;
	blocksPerSend = 3125;
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

bool CorrParams::useAntenna(const string &antName) const
{
	int i, n;

	n = antennaList.size();
	if(n == 0)
	{
		return true;
	}
	for(i = 0; i < n; i++)
	{
		if(antName == antennaList[i])
		{
			return true;
		}
		// FIXME -- allow -antName
	}

	return false;
}
