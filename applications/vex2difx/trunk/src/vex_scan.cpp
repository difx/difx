#include "vex_scan.h"

const Interval *VexScan::getAntennaInterval(const std::string &antName) const
{
	std::map<std::string,Interval>::const_iterator it = stations.find(antName);
	if(it != stations.end())
	{
		return &it->second;
	}
	else
	{
		return 0;
	}
}

bool VexScan::getRecordEnable(const std::string &antName) const
{
	std::map<std::string,bool>::const_iterator it = recordEnable.find(antName);
	if(it != recordEnable.end())
	{
		return it->second;
	}
	else
	{
		return false;
	}
}

std::ostream& operator << (std::ostream &os, const VexScan &x)
{
	os << "Scan " << x.defName << 
		"\n  timeRange=" << (const Interval&)x <<
		"\n  mode=" << x.modeDefName <<
		"\n  source=" << x.sourceDefName << 
		"\n  size=" << x.size << " bytes \n";

	for(std::map<std::string,Interval>::const_iterator iter = x.stations.begin(); iter != x.stations.end(); ++iter)
	{
		os << "  " << iter->first << " range=" << iter->second << std::endl;
	}

	for(std::map<std::string,bool>::const_iterator iter = x.recordEnable.begin(); iter != x.recordEnable.end(); ++iter)
	{
		os << "  " << iter->first << " enable=" << iter->second << std::endl;
	}

	return os;
}
