#include <iostream>
#include "vextables.h"

using namespace std;

VexSource *VexData::newSource()
{
	VexSource *source;

	source = new VexSource;
	sources.push_back(*source);
	return &sources.back();
}

const VexSource &VexData::getSource(string name) const
{
	int i;
	
	for(i = 0; i < nSource(); i++)
	{
		if(sources[i].name == name)
			return sources[i];
	}

	// FIXME -- throw exception
}

const VexSource &VexData::getSource(int num) const
{
	// FIXME -- throw exception if num < 0 || num >= nScan

	return sources[num];
}

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

VexAntenna *VexData::newAntenna()
{
	VexAntenna *antenna;

	antenna = new VexAntenna;
	antennas.push_back(*antenna);
	return &antennas.back();
}

const VexAntenna &VexData::getAntenna(string name) const
{
	int i;
	
	for(i = 0; i < nAntenna(); i++)
	{
		if(antennas[i].name == name)
			return antennas[i];
	}

	// FIXME -- throw exception
}

const VexAntenna &VexData::getAntenna(int num) const
{
	// FIXME -- throw exception if num < 0 || num >= nScan

	return antennas[num];
}


ostream& operator << (ostream& os, const VexInterval& x)
{
	int p;

	p = os.precision();
	os.precision(12);
	os << "mjd(" << x.mjdStart << "," << x.mjdEnd << ")";
	os.precision(p);

	return os;
}

ostream& operator << (ostream& os, const VexSource& x)
{
	int i, n;

	os << "Source " << x.name << endl;
	n = x.sourceNames.size();
	for(i = 0; i < n; i++)
	{
		os << "  name=" << x.sourceNames[i] << endl;
	}
	os << "  ra=" << x.ra <<
		"\n  dec=" << x.dec <<
		"\n  calCode=" << x.calCode <<
		"\n  qual=" << x.qualifier << endl;

	return os;
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

ostream& operator << (ostream& os, const VexAntenna& x)
{
	os << "Antenna " << x.name <<
		"\n   x=" << x.x <<
		"\n   y=" << x.y <<
		"\n   z=" << x.z <<
		"\n   axisType=" << x.axisType <<
		"\n   axisOffset=" << x.axisOffset << endl;

	return os;
}

ostream& operator << (ostream& os, const VexData& x)
{
	int i, n;

	os << "Vex:" << endl;

	n = x.nSource();
	os << n << " sources:" << endl;
	for(i = 0; i < n; i++)
	{
		os << x.getSource(i);
	}

	n = x.nScan();
	os << n << " scans:" << endl;
	for(i = 0; i < n; i++)
	{
		os << x.getScan(i);
	}

	n = x.nAntenna();
	os << n << " antennas:" << endl;
	for(i = 0; i < n; i++)
	{
		os << x.getAntenna(i);
	}

	return os;
}

bool VexData::usesMode(string modeName) const
{
	int i, n;

	n = nScan();

	for(i = 0; i < n; i++)
	{
		if(getScan(i).modeName == modeName)
		{
			return true;
		}
	}

	return false;
}
