#include <iostream>
#include "vextables.h"

using namespace std;

VexScan *VexData::newScan()
{
	VexScan *scan;

	scan = new VexScan;
	scans.push_back(*scan);
	return &scans.back();
}

const VexScan &VexData::getScan(string name) const
{
	int i;
	
	for(i = 0; i < nScan(); i++)
	{
		if(scans[i].name == name)
			return scans[i];
	}

	// FIXME -- throw exception
}

const VexScan &VexData::getScan(int num) const
{
	// FIXME -- throw exception if num < 0 || num >= nScan

	return scans[num];
}

ostream& operator << (ostream& os, const VexInterval& x)
{
	return os << "mjd(" << x.mjdStart << "," << x.mjdEnd << ")";
}

ostream& operator << (ostream& os, const VexScan& x)
{
	map<string,VexInterval>::const_iterator iter;

	os << "Scan " << x.name << 
		"\n  timeRange=" << x.timeRange <<
		"\n  mode=" << x.modeName <<
		"\n  source=" << x.sourceName << "\n";

	for(iter = x.stations.begin(); iter != x.stations.end(); iter++)
		cout << "  " << iter->first << " range=" << iter->second << endl;
	return os;
}

ostream& operator << (ostream& os, const VexData& x)
{
	int i, n;

	n = x.nScan();

	os << "Vex:" << endl;
	os << n << " scans:" << endl;
	for(i = 0; i < n; i++)
	{
		cout << x.getScan(i);
	}

	return os;
}

