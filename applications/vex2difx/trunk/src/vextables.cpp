#include <iostream>
#include "vextables.h"

using namespace std;

const char VexEvent::eventName[][20] =
{
	"None",
	"Observe Start",
	"Job Start",
	"Record Start",
	"Scan Start",
	"Ant On-source",
	"Ant Off-source",
	"Scan Stop",
	"Record Stop",
	"Job Stop",
	"Observe Stop"
};


bool operator<(const VexEvent &a, const VexEvent &b)
{
	if(a.mjd < b.mjd)
	{
		return true;
	}
	else if(a.mjd > b.mjd)
	{
		return false;
	}
	if(a.eventType < b.eventType)
	{
		return true;
	}
	else if(a.eventType > b.eventType)
	{
		return false;
	}
	return a.name < b.name;
}



int VexMode::addSubband(double freq, double bandwidth, char sideband, char pol)
{
	int i, n;
	VexSubband S(freq, bandwidth, sideband, pol);

	n = subbands.size();

	for(i = 0; i < n; i++)
	{
		if(S == subbands[i])
		{
			return i;
		}
	}

	subbands.push_back(S);

	return n;
}

bool operator == (VexSubband& s1, VexSubband& s2)
{
	if(s1.freq != s2.freq) return false;
	if(s1.bandwidth != s2.bandwidth) return false;
	if(s1.sideBand != s2.sideBand) return false;
	if(s1.pol != s2.pol) return false;

	return true;
}

VexSource *VexData::newSource()
{
	sources.push_back(VexSource());
	return &sources.back();
}

const VexSource &VexData::getSource(string name) const
{
	for(int i = 0; i < nSource(); i++)
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
	scans.push_back(VexScan());
	return &scans.back();
}

const VexScan &VexData::getScan(string name) const
{
	for(int i = 0; i < nScan(); i++)
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
	antennas.push_back(VexAntenna());
	return &antennas.back();
}

const VexAntenna &VexData::getAntenna(string name) const
{
	for(int i = 0; i < nAntenna(); i++)
	{
		if(antennas[i].name == name)
			return antennas[i];
	}

	// FIXME -- throw exception
}

VexAntenna &VexData::getAntenna(string name)
{
	for(int i = 0; i < nAntenna(); i++)
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

VexMode *VexData::newMode()
{
	modes.push_back(VexMode());
	return &modes.back();
}

const VexMode &VexData::getMode(string name) const
{
	for(int i = 0; i < nMode(); i++)
	{
		if(modes[i].name == name)
			return modes[i];
	}

	// FIXME -- throw exception
}

const VexMode &VexData::getMode(int num) const
{
	// FIXME -- throw exception if num < 0 || num >= nScan

	return modes[num];
}

VexEOP *VexData::newEOP()
{
	eops.push_back(VexEOP());
	return &eops.back();
}

const VexEOP &VexData::getEOP(int num) const
{
	// FIXME -- throw exception if num < 0 || num >= nScan

	if(num > 1000)	// Look for mjd
	{
		int n = nEOP();
		for(int i = 0; i < n; i++)
		{
			if(getEOP(i).mjd == num)
			{
				return eops[i];
			}
		}
	}

	return eops[num];
}

bool VexData::usesAntenna(const string& antennaName) const
{
	int n = nAntenna();

	for(int i = 0; i < n; i++)
	{
		if(getAntenna(i).name == antennaName)
		{
			return true;
		}
	}

	return false;
}

bool VexData::usesMode(const string& modeName) const
{
	int n = nScan();

	for(int i = 0; i < n; i++)
	{
		if(getScan(i).modeName == modeName)
		{
			return true;
		}
	}

	return false;
}

void VexData::addVSN(const string& antName, const string& vsn, double mjdStart, double mjdStop)
{
	int n = nAntenna();

	for(int i = 0; i < n; i++)
	{
		if(antennas[i].name == antName)
		{
			antennas[i].vsns.push_back(VexVSN());
			antennas[i].vsns.back().name = vsn;
			antennas[i].vsns.back().mjdStart = mjdStart;
			antennas[i].vsns.back().mjdEnd = mjdStop;
		}
	}
}

const list<VexEvent> &VexData::getEvents() const
{
	// FIXME -- throw exception if num < 0 || num >= nScan

	return events;
}

void VexData::addEvent(double mjd, VexEvent::EventType eventType, const string &name)
{
	events.push_back(VexEvent(mjd, eventType, name));
	events.sort();
}

ostream& operator << (ostream& os, const VexInterval& x)
{
	int p = os.precision();
	os.precision(12);
	os << "mjd(" << x.mjdStart << "," << x.mjdEnd << ")";
	os.precision(p);

	return os;
}

ostream& operator << (ostream& os, const VexSource& x)
{
	os << "Source " << x.name << endl;
	int n = x.sourceNames.size();
	for(int i = 0; i < n; i++)
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
	{
		cout << "  " << iter->first << " range=" << iter->second << endl;
	}

	return os;
}

ostream& operator << (ostream& os, const VexAntenna& x)
{
	vector<VexVSN>::const_iterator iter;

	os << "Antenna " << x.name <<
		"\n   x=" << x.x <<
		"\n   y=" << x.y <<
		"\n   z=" << x.z <<
		"\n   axisType=" << x.axisType <<
		"\n   axisOffset=" << x.axisOffset << endl;

	for(iter = x.vsns.begin(); iter != x.vsns.end(); iter++)
	{
		cout << "   " << *iter << endl;
	}

	return os;
}

ostream& operator << (ostream& os, const VexSubband& x)
{
	os << "(" << x.freq << " Hz, " << x.bandwidth << " Hz, sb=" << x.sideBand << ", pol=" << x.pol << ")";
	
	return os;
}

ostream& operator << (ostream& os, const VexIF& x)
{
	os << "[r=" << x.recordChan << " -> s=" << x.subbandId << "]";

	return os;
}

ostream& operator << (ostream& os, const VexFormat& x)
{
	os << "(format=" << x.format << ", nBit=" << x.nBit << ", nChan=" << x.nRecordChan;
	for(int i = 0; i < x.ifs.size(); i++)
	{
		os << ", " << x.ifs[i];
	}
	os << ")";

	return os;
}

ostream& operator << (ostream& os, const VexMode& x)
{
	map<string,VexFormat>::const_iterator it;

	os << "Mode " << x.name << endl;
	for(int i = 0; i < x.subbands.size(); i++)
	{
		os << "  subband=" << x.subbands[i] << endl;
	}
	for(it = x.formats.begin(); it != x.formats.end(); ++it)
	{
		os << "  format[" << it->first << "] =" << it->second << endl;
	}
	
	return os;
}

ostream& operator << (ostream& os, const VexVSN& x)
{
	os << "VSN(" << x.name << ", " << x.mjdStart << ", " << x.mjdEnd << ")";

	return os;
}

ostream& operator << (ostream& os, const VexEvent& x)
{
	int d, s;
	d = static_cast<int>(x.mjd);
	s = static_cast<int>((x.mjd - d)*86400.0);

	os << "mjd=" << d << " sec=" << s << " : " << VexEvent::eventName[x.eventType] << " " << x.name;

	return os;
}

ostream& operator << (ostream& os, const VexData& x)
{
	os << "Vex:" << endl;

	int n = x.nSource();
	os << n << " sources:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << x.getSource(i);
	}

	n = x.nScan();
	os << n << " scans:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << x.getScan(i);
	}

	n = x.nAntenna();
	os << n << " antennas:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << x.getAntenna(i);
	}

	n = x.nMode();
	os << n << " modes:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << x.getMode(i);
	}

	const list<VexEvent>& events = x.getEvents();
	list<VexEvent>::const_iterator iter;
	os << " Events:" << endl;
	for(iter = events.begin(); iter != events.end(); iter++)
	{
		os << "   " << *iter << endl;
	}

	return os;
}
