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

#include <fstream>
#include <iostream>
#include <algorithm>
#include <cmath>
#include "vextables.h"

using namespace std;

// Note: the ordering here is crucial!
const char VexEvent::eventName[][20] =
{
	"None",
	"Ant Off-source",
	"Scan Stop",
	"Job Stop",
	"Observe Stop",
	"Record Stop",
	"Clock Break",
	"Record Start",
	"Observe Start",
	"Job Start",
	"Scan Start",
	"Ant On-source"
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

int VexMode::getPols(char *pols) const
{
	int n=0;
	bool L=false, R=false, X=false, Y=false;
	vector<VexSubband>::const_iterator it;

	for(it = subbands.begin(); it != subbands.end(); it++)
	{
		if(it->pol == 'R') R=true;
		if(it->pol == 'L') L=true;
		if(it->pol == 'X') X=true;
		if(it->pol == 'Y') Y=true;
	}

	if(R) 
	{
		*pols = 'R';
		pols++;
		n++;
	}
	if(L)
	{
		*pols = 'L';
		pols++;
		n++;
	}
	if(n)
	{
		return n;
	}
	if(X) 
	{
		*pols = 'X';
		pols++;
		n++;
	}
	if(Y)
	{
		*pols = 'Y';
		pols++;
		n++;
	}

	return n;
}

int VexMode::getBits() const
{
	int nBit;
	map<string,VexFormat>::const_iterator it;

	nBit = formats.begin()->second.nBit;

	for(it = formats.begin(); it != formats.end(); it++)
	{
		if(it->second.nBit != nBit)
		{
			cerr << "Warning: differing numbers of bits : FITS will not store this information perfectly" << endl;
			break;
		}
	}

	return nBit;
}

const VexFormat& VexMode::getFormat(const string antName) const
{
	map<string,VexFormat>::const_iterator it;

	for(it = formats.begin(); it != formats.end(); it++)
	{
		if(it->first == antName)
		{
			return it->second;
		}
	}

	cerr << "Error: antName=" << antName << " not found" << endl;
	exit(0);
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

// returned values in seconds
void VexAntenna::getClock(double mjd, double &offset, double &rate) const
{
	vector<VexClock>::const_iterator it;
	offset = 0.0;
	rate = 0.0;

	for(it = clocks.begin(); it != clocks.end(); it++)
	{
		if(it->mjdStart <= mjd)
		{
			offset = it->offset + (mjd - it->offset_epoch)*it->rate*86400.0;
			rate = it->rate;
		}
	}
}

const VexSource *VexData::getSource(string name) const
{
	for(int i = 0; i < nSource(); i++)
	{
		if(sources[i].name == name)
			return &sources[i];
	}

	return 0;
}

const VexSource *VexData::getSource(int num) const
{
	if(num < 0 || num >= nSource())
	{
		return 0;
	}

	return &sources[num];
}

VexScan *VexData::newScan()
{
	scans.push_back(VexScan());
	return &scans.back();
}

void VexJob::assignVSNs(const VexData& V)
{
	list<string> antennas;
	vector<string>::const_iterator s;
	map<string,VexInterval>::const_iterator a;
	list<string>::const_iterator i;

	for(s = scans.begin(); s != scans.end(); s++)
	{
		const VexScan* S = V.getScan(*s);
		for(a = S->stations.begin(); a != S->stations.end(); a++)
		{
			if(find(antennas.begin(), antennas.end(), a->first) == antennas.end())
			{
				antennas.push_back(a->first);
			}
		}
	}
	
	for(i = antennas.begin(); i != antennas.end(); i++)
	{
		const string &vsn = V.getVSN(*i, mjdStart, mjdStop);
		if(vsn != string("None"))
		{
			vsns[*i] = vsn;
		}
	}
}

bool VexJobGroup::hasScan(const string& scanName) const
{
	return find(scans.begin(), scans.end(), scanName) != scans.end();
}

void VexJobGroup::genEvents(const list<VexEvent>& eventList)
{
	list<VexEvent>::const_iterator it;
#if 0
	bool save = true;

	for(it = eventList.begin(); it != eventList.end(); it++)
	{
		if(it->eventType == VexEvent::SCAN_START)
		{
			if(!hasScan(it->name))
			{
				save = false;
			}
		}
		if(save || 
		   it->eventType == VexEvent::RECORD_START ||
		   it->eventType == VexEvent::RECORD_STOP)
		{
			events.push_back(*it);
		}
		if(it->eventType == VexEvent::SCAN_STOP)
		{
			save = true;
		}
	}
#endif
	for(it = eventList.begin(); it != eventList.end(); it++)
	{
		if(it->eventType == VexEvent::SCAN_START ||
		   it->eventType == VexEvent::SCAN_STOP ||
		   it->eventType == VexEvent::ANT_SCAN_START ||
		   it->eventType == VexEvent::ANT_SCAN_STOP)
		{
			if(hasScan(it->scan))
			{
				events.push_back(*it);
			}
		}
		else
		{
			events.push_back(*it);
		}
	}

	// Now remove any module changes that don't occur within scans

	list<VexEvent>::iterator rstart, rstop;
	map<string,bool> inScan;
	map<string,bool> inScanNow;

	// initialize inScan

	for(it = events.begin(); it != events.end(); it++)
	{
		if(it->eventType == VexEvent::RECORD_START)
		{
			inScan[it->name] = false;
			inScanNow[it->name] = false;
		}
	}

	for(rstart = events.begin(); rstart != events.end();)
	{
		if(rstart->eventType == VexEvent::ANT_SCAN_START)
		{
			inScan[rstart->name] = true;
			inScanNow[rstart->name] = true;
		}
		else if(rstart->eventType == VexEvent::ANT_SCAN_STOP)
		{
			inScanNow[rstart->name] = false;
		}
		if(rstart->eventType == VexEvent::RECORD_START && !inScanNow[rstart->name])
		{
			inScan[rstart->name] = inScanNow[rstart->name];
			for(rstop = rstart, rstop++; rstop != events.end(); rstop++)
			{
				if(rstart->name != rstop->name)
				{
					continue;
				}

				if(rstop->eventType == VexEvent::ANT_SCAN_START)
				{
					inScan[rstart->name] = true;
				}

				if(rstop->eventType == VexEvent::RECORD_STOP)
				{
					if(!inScan[rstop->name])
					{
						inScan[rstop->name] = inScanNow[rstop->name];
						events.erase(rstop);
						rstart = events.erase(rstart);
					}
					else
					{
						rstart++;
					}

					break;
				}
			}
		}
		else
		{
			rstart++;
		}
	}
}

int VexJob::generateFlagFile(const VexData& V, const string &fileName, unsigned int invalidMask) const
{
	vector<VexJobFlag> flags;
	map<string,int> antIds;
	map<string,string>::const_iterator a;
	list<VexEvent>::const_iterator e;
	map<string,VexInterval>::const_iterator sa;
	int nAnt;
	ofstream of;
	const list<VexEvent> &eventList = *V.getEvents();

	for(nAnt = 0, a = vsns.begin(); a != vsns.end(); nAnt++, a++)
	{
		antIds[a->first] = nAnt;
	}

	vector<unsigned int> flagMask(nAnt, 
		VexJobFlag::JOB_FLAG_RECORD | 
		VexJobFlag::JOB_FLAG_POINT | 
		VexJobFlag::JOB_FLAG_TIME | 
		VexJobFlag::JOB_FLAG_SCAN);
	vector<double> flagStart(nAnt, mjdStart);

	for(e = eventList.begin(); e != eventList.end(); e++)
	{
		if(e->eventType == VexEvent::RECORD_START)
		{
			flagMask[antIds[e->name]] &= ~VexJobFlag::JOB_FLAG_RECORD;
		}
		else if(e->eventType == VexEvent::RECORD_STOP)
		{
			flagMask[antIds[e->name]] |= VexJobFlag::JOB_FLAG_RECORD;
		}
		else if(e->eventType == VexEvent::SCAN_START)
		{
			const VexScan *scan = V.getScan(e->name);

			if(!scan)
			{
				cerr << "Developer error! generateFlagFile: SCAN_START, scan=0" << endl;
				exit(0);
			}
			for(sa = scan->stations.begin(); sa != scan->stations.end(); sa++)
			{
				flagMask[antIds[sa->first]] &= ~VexJobFlag::JOB_FLAG_SCAN;
			}
		}
		else if(e->eventType == VexEvent::SCAN_STOP)
		{
			const VexScan *scan = V.getScan(e->name);

			if(!scan)
			{
				cerr << "Developer error! generateFlagFile: SCAN_STOP, scan=0" << endl;
				exit(0);
			}
			for(sa = scan->stations.begin(); sa != scan->stations.end(); sa++)
			{
				flagMask[antIds[sa->first]] |= VexJobFlag::JOB_FLAG_SCAN;
			}
		}
		else if(e->eventType == VexEvent::ANT_SCAN_START)
		{
			flagMask[antIds[e->name]] &= ~VexJobFlag::JOB_FLAG_POINT;
		}
		else if(e->eventType == VexEvent::ANT_SCAN_STOP)
		{
			flagMask[antIds[e->name]] |= VexJobFlag::JOB_FLAG_POINT;
		}
		else if(e->eventType == VexEvent::JOB_START)
		{
			if(fabs(e->mjd - mjdStart) < 0.5/86400.0)
			{
				for(int antId = 0; antId < nAnt; antId++)
				{
					flagMask[antId] &= ~VexJobFlag::JOB_FLAG_TIME;
				}
			}
		}
		else if(e->eventType == VexEvent::JOB_STOP)
		{
			if(fabs(e->mjd - mjdStart) < 0.5/86400.0)
			{
				for(int antId = 0; antId < nAnt; antId++)
				{
					flagMask[antId] |= VexJobFlag::JOB_FLAG_TIME;
				}
			}
		}

		for(int antId = 0; antId < nAnt; antId++)
		{
			if( (flagMask[antId] & invalidMask) == 0)
			{
				if(flagStart[antId] > 0)
				{
					if(e->mjd - flagStart[antId] > 0.5/86400.0)
					{
						flags.push_back(VexJobFlag(flagStart[antId], e->mjd, antId));
					}
					flagStart[antId] = -1;
				}
			}
			else
			{
				if(flagStart[antId] <= 0)
				{
					flagStart[antId] = e->mjd;
				}
			}
		}
	}

	// add final flags if needed (probably not!)
	for(int antId = 0; antId < nAnt; antId++)
	{
		if( (flagMask[antId] & invalidMask) != 0)
		{
			if(mjdStop - flagStart[antId] > 0.5/86400.0)
			{
				flags.push_back(VexJobFlag(flagStart[antId], mjdStop, antId));
			}
		}
	}

	// write data to file
	of.open(fileName.c_str());
	of << flags.size() << endl;
	for(int f = 0; f < flags.size(); f++)
	{
		of << "  " << flags[f] << endl;
	}
	of.close();

	return flags.size();
}

// FIXME -- this does not allow concurrent scans
void VexJobGroup::createJob(vector<VexJob>& jobs, double start, double stop, double maxLength) const
{
	list<VexEvent>::const_iterator s, e;
	jobs.push_back(VexJob());
	VexJob *J = &jobs.back();
	double totalTime, scanTime = 0.0;
	string id("");

	// note these are backwards now -- will set these to minimum range covering scans
	J->mjdStart = stop;
	J->mjdStop = start;

	for(e = events.begin(); e != events.end(); e++)
	{
		if(e->eventType == VexEvent::SCAN_START)
		{
			s = e;
			id = e->name;
		}
		if(e->eventType == VexEvent::SCAN_STOP)
		{
			if(id != e->name)
			{
				cerr << "Programming error: id != e->name  (" << id << " != " << e->name << ")" << endl;
				cerr << "Contact developer" << endl;
				exit(0);
			}
			if(s->mjd >= start && e->mjd <= stop)
			{
				J->scans.push_back(e->name);
				if(J->mjdStart > s->mjd)
				{
					J->mjdStart = s->mjd;
				}
				if(J->mjdStop < e->mjd)
				{
					J->mjdStop = e->mjd;
				}
				scanTime += e->mjd - s->mjd;

				/* start a new job at scan boundary if maxLength exceeded */
				if(J->mjdStop - J->mjdStart > maxLength)
				{
					totalTime = J->mjdStop - J->mjdStart;
					J->dutyCycle = scanTime / totalTime;
					jobs.push_back(VexJob());
					J = &jobs.back();
					scanTime = 0.0;
					J->mjdStart = stop;
					J->mjdStop = start;
				}
			}
		}
	}

	totalTime = J->mjdStop - J->mjdStart;
	
	if(totalTime <= 0.0)
	{
		jobs.pop_back();
	}
	else
	{
		J->dutyCycle = scanTime / totalTime;
	}
}

const VexScan *VexData::getScan(string name) const
{
	for(int i = 0; i < nScan(); i++)
	{
		if(scans[i].name == name)
			return &scans[i];
	}

	return 0;
}

const VexScan *VexData::getScan(int num) const
{
	if(num < 0 || num >= nScan())
	{
		return 0;
	}

	return &scans[num];
}

const VexScan *VexData::getScanByAntenna(string antName, double mjd) const
{
	return 0;
}

void VexData::getScanList(list<string> &scanList) const
{
	vector<VexScan>::const_iterator it;

	for(it = scans.begin(); it != scans.end(); it++)
	{
		scanList.push_back(it->name);
	}
}

VexAntenna *VexData::newAntenna()
{
	antennas.push_back(VexAntenna());
	return &antennas.back();
}

const VexAntenna *VexData::getAntenna(string name) const
{
	for(int i = 0; i < nAntenna(); i++)
	{
		if(antennas[i].name == name)
			return &antennas[i];
	}

	return 0;
}

const VexAntenna *VexData::getAntenna(int num) const
{
	if(num < 0 || num >= nAntenna())
	{
		return 0;
	}

	return &antennas[num];
}

VexMode *VexData::newMode()
{
	modes.push_back(VexMode());
	return &modes.back();
}

const VexMode *VexData::getMode(string name) const
{
	for(int i = 0; i < nMode(); i++)
	{
		if(modes[i].name == name)
		{
			return &modes[i];
		}
	}

	return 0;
}

const VexMode *VexData::getMode(int num) const
{
	if(num < 0 || num >= nMode())
	{
		return 0;
	}

	return &modes[num];
}

VexEOP *VexData::newEOP()
{
	eops.push_back(VexEOP());
	return &eops.back();
}

const VexEOP *VexData::getEOP(int num) const
{
	if(num < 0 || num > nEOP())
	{
		return 0;
	}

	return &eops[num];
}

bool VexData::usesAntenna(const string& antennaName) const
{
	int n = nAntenna();

	for(int i = 0; i < n; i++)
	{
		if(getAntenna(i)->name == antennaName)
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
		if(getScan(i)->modeName == modeName)
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
			antennas[i].vsns.back().mjdStop = mjdStop;
		}
	}
}

string VexData::getVSN(const string& antName, double mjdStart, double mjdStop) const
{
	const VexAntenna *A;
	vector<VexVSN>::const_iterator v;
	double best = 0.0;
	string bestVSN("None");

	A = getAntenna(antName);
	if(!A)
	{
		return bestVSN;
	}

	for(v = A->vsns.begin(); v != A->vsns.end(); v++)
	{
		double overlap = min(mjdStop, v->mjdStop) - max(mjdStart, v->mjdStart);
		if(overlap > best)
		{
			best = overlap;
			bestVSN = v->name;
		}
	}

	return bestVSN;
}

void VexData::setExper(const string& name, double start, double stop)
{
	double a=1.0e7, b=0.0;
	list<VexEvent>::const_iterator it;

	for(it = events.begin(); it != events.end(); it++)
	{
		if(it->mjd < a && it->eventType != VexEvent::CLOCK_BREAK)
		{
			a = it->mjd;
		}
		if(it->mjd > b && it->eventType != VexEvent::CLOCK_BREAK)
		{
			b = it->mjd;
		}
	}

	if(start < 10000)
	{
		start = a;
	}
	if(stop < 10000)
	{
		stop = b;
	}

	exper.name = name;
	exper.mjdStart = start;
	exper.mjdStop = stop;
	addEvent(start, VexEvent::OBSERVE_START, name); 
	addEvent(stop, VexEvent::OBSERVE_STOP, name); 
}

const list<VexEvent> *VexData::getEvents() const
{
	return &events;
}

void VexData::addEvent(double mjd, VexEvent::EventType eventType, const string &name)
{
	events.push_back(VexEvent(mjd, eventType, name));
	events.sort();
}

void VexData::addEvent(double mjd, VexEvent::EventType eventType, const string &name, const string &scan)
{
	events.push_back(VexEvent(mjd, eventType, name, scan));
	events.sort();
}

ostream& operator << (ostream& os, const VexInterval& x)
{
	int p = os.precision();
	os.precision(12);
	os << "mjd(" << x.mjdStart << "," << x.mjdStop << ")";
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
		os << "  " << iter->first << " range=" << iter->second << endl;
	}

	os << "  setup=" << x.setupName << endl;

	return os;
}

ostream& operator << (ostream& os, const VexClock& x)
{
	os << "Clock(" << x.mjdStart << ": " << x.offset << ", " << x.rate << ", " << x.offset_epoch << ")";

	return os;
}

ostream& operator << (ostream& os, const VexAntenna& x)
{
	os << "Antenna " << x.name <<
		"\n   x=" << x.x << "  dx/dt=" << x.dx <<
		"\n   y=" << x.y << "  dy/dt=" << x.dy <<
		"\n   z=" << x.z << "  dz/dt=" << x.dz <<
		"\n   posEpoch=" << x.posEpoch <<
		"\n   axisType=" << x.axisType <<
		"\n   axisOffset=" << x.axisOffset << endl;

	for(vector<VexVSN>::const_iterator it = x.vsns.begin(); it != x.vsns.end(); it++)
	{
		os << "   " << *it << endl;
	}

	for(vector<VexClock>::const_iterator it = x.clocks.begin(); it != x.clocks.end(); it++)
	{
		os << "   " << *it << endl;
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
	for(unsigned int i = 0; i < x.ifs.size(); i++)
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
	for(unsigned int i = 0; i < x.subbands.size(); i++)
	{
		os << "  subband=" << x.subbands[i] << endl;
	}
	for(it = x.formats.begin(); it != x.formats.end(); ++it)
	{
		os << "  format[" << it->first << "] =" << it->second << endl;
	}
	
	return os;
}

ostream& operator << (ostream& os, const VexEOP& x)
{
	os << "EOP(" << x.mjd << ", " << x.tai_utc << ", " << x.ut1_utc << ", " << x.xPole << ", " << x.yPole << ")";

	return os;
}


ostream& operator << (ostream& os, const VexVSN& x)
{
	os << "VSN(" << x.name << ", " << x.mjdStart << ", " << x.mjdStop << ")";

	return os;
}

ostream& operator << (ostream& os, const VexJob& x)
{
	vector<string>::const_iterator s;
	map<string,string>::const_iterator v;
	int p = os.precision();
	os.precision(12);
	os << "Job " << x.jobSeries << x.jobId << endl;
	os << "  " << x.mjdStart << " - " << x.mjdStop << endl;
	os << "  duty cycle = " << x.dutyCycle << endl;
	os << "  scans =";
	for(s = x.scans.begin(); s != x.scans.end(); s++)
	{
		os << " " << *s;
	}
	os << endl;
	for(v = x.vsns.begin(); v != x.vsns.end(); v++)
	{
		os << "  " << "VSN[" << v->first << "] = " << v->second << endl;
	}

	os.precision(p);

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

ostream& operator << (ostream& os, const VexJobFlag& x)
{
	int p = os.precision();

	os.precision(12);

	os << x.mjdStart << " " << x.mjdStop << " " << x.antId;

	os.precision(p);

	return os;
}

ostream& operator << (ostream& os, const VexData& x)
{
	os << "Vex:" << endl;

	int n = x.nSource();
	os << n << " sources:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << *x.getSource(i);
	}

	n = x.nScan();
	os << n << " scans:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << *x.getScan(i);
	}

	n = x.nAntenna();
	os << n << " antennas:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << *x.getAntenna(i);
	}

	n = x.nMode();
	os << n << " modes:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << *x.getMode(i);
	}

	n = x.nEOP();
	os << n << " eops:" << endl;
	for(int i = 0; i < n; i++)
	{
		os << "   " << *x.getEOP(i) << endl;
	}

	const list<VexEvent> *events = x.getEvents();
	list<VexEvent>::const_iterator iter;
	os << "Events:" << endl;
	for(iter = events->begin(); iter != events->end(); iter++)
	{
		os << "   " << *iter << endl;
	}

	return os;
}
