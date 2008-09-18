#include "corrparams.h"

CorrParams::CorrParams()
{
	minSubarraySize = 2;
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
