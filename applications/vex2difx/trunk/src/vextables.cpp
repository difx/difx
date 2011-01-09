/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken & Adam Deller               *
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
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cmath>
#include "vextables.h"

const double RAD2ASEC=180.0*3600.0/M_PI;

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
	"Leap Second",
	"Manual Break",
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

// returns a negative number if no overlap
double VexInterval::overlap(const VexInterval &v) const
{
	return min(mjdStop, v.mjdStop) - max(mjdStart, v.mjdStart);
}

void VexInterval::logicalAnd(double start, double stop)
{
	if(mjdStart < start)
	{
		mjdStart = start;
	}
	if(mjdStop > stop)
	{
		mjdStop = stop;
	}
}

void VexInterval::logicalAnd(const VexInterval &v)
{
	if(mjdStart < v.mjdStart)
	{
		mjdStart = v.mjdStart;
	}
	if(mjdStop > v.mjdStop)
	{
		mjdStop = v.mjdStop;
	}
}

void VexInterval::logicalOr(double start, double stop)
{
	if(mjdStart > start)
	{
		mjdStart = start;
	}
	if(mjdStop < stop)
	{
		mjdStop = stop;
	}
}

void VexInterval::logicalOr(const VexInterval &v)
{
	if(mjdStart > v.mjdStart)
	{
		mjdStart = v.mjdStart;
	}
	if(mjdStop < v.mjdStop)
	{
		mjdStop = v.mjdStop;
	}
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
		if(it->pol == 'R')
		{
			R = true;
		}
		else if(it->pol == 'L')
		{
			L = true;
		}
		else if(it->pol == 'X')
		{
			X = true;
		}
		else if(it->pol == 'Y')
		{
			Y = true;
		}
		else
		{
			cerr << "Error: VexMode::getPols: subband with illegal polarization (" << it->pol << ") encountered." << endl;
			
			exit(0);
		}
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
	unsigned int nBit;
	map<string,VexSetup>::const_iterator it;

	nBit = setups.begin()->second.format.nBit;

	for(it = setups.begin(); it != setups.end(); it++)
	{
		if(it->second.format.nBit != nBit)
		{
			break;
		}
	}

	return nBit;
}

const VexSetup* VexMode::getSetup(const string &antName) const
{
	map<string,VexSetup>::const_iterator it;

	it = setups.find(antName);
	if(it == setups.end())
	{
		cerr << "Error: VexMode::getSetup: antName=" << antName << " not found." << endl;
		
		exit(0);
	}

	return &it->second;
}

const VexFormat* VexMode::getFormat(const string &antName) const
{
	map<string,VexSetup>::const_iterator it;

	it = setups.find(antName);
	if(it == setups.end())
	{
		cerr << "Error: VexMode::getFormat: antName=" << antName << " not found." << endl;

		exit(0);
	}


	return &it->second.format;
}

double VexIF::getLowerEdgeFreq() const
{
	double bandCenter = ifSSLO;

	// Calculate the center of the 500-1000 MHz IF range;
	if(ifSideBand == 'L')
	{
		bandCenter -= 750.0e6;
	}
	else
	{
		bandCenter += 750.0e6;
	}

	return bandCenter - 500.0e6;
}

string VexIF::VLBABandName() const
{
	double bandCenter = ifSSLO;

	// Calculate the center of the 500-1000 MHz IF range;
	if(ifSideBand == 'L')
	{
		bandCenter -= 750.0e6;
	}
	else
	{
		bandCenter += 750.0e6;
	}

	if(bandCenter < 1.0e9)
	{
		return "90cm";
	}
	else if(bandCenter < 2.0e9)
	{
		return "20cm";
	}
	else if(bandCenter < 3.0e9)
	{
		return "13cm";
	}
	else if(bandCenter < 6.0e9)
	{
		return "6cm";
	}
	else if(bandCenter < 9.5e9)
	{
		return "4cm";
	}
	else if(bandCenter < 17.0e9)
	{
		return "2cm";
	}
	else if(bandCenter < 25.0e9)
	{
		return "1cm";
	}
	else if(bandCenter < 40.5e9)
	{
		return "9mm";
	}
	else if(bandCenter < 60.0e9)
	{
		return "7mm";
	}
	else if(bandCenter < 100.0e9)
	{
		return "3mm";
	}

	return "None";
}

bool operator == (VexSubband &s1, VexSubband &s2)
{
	if(s1.pol       != s2.pol      ||
	   s1.freq      != s2.freq     ||
	   s1.sideBand  != s2.sideBand ||
	   s1.bandwidth != s2.bandwidth)
	{
		return false;
	}
	else
	{
		return true;
	}
}

bool VexSource::hasSourceName(const string &name) const
{
	vector<string>::const_iterator n;

	for(n = sourceNames.begin(); n != sourceNames.end(); n++)
	{
		if(*n == name)
		{
			return true;
		}
	}

	return false;
}

int VexData::sanityCheck()
{
        int nWarn = 0;

        if(eops.size() < 5)
        {
                cerr << "Warning: Fewer than 5 EOPs specified" << endl;
                nWarn++;
        }

        for(vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); it++)
        {
                if(it->clocks.size() == 0)
                {
                        cerr << "Warning: no clock values for antenna " << it->name << " ." << endl;
                        nWarn++;
                }
        }

        for(vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); it++)
        {
		if(it->dataSource == DataSourceFile && it->basebandFiles.size() == 0)
		{
			cerr << "Warning: file based correlation desired but no files provided for antenna " << it->name << " ." << endl;
			nWarn++;
		}
		if(it->dataSource == DataSourceModule && it->basebandFiles.size() == 0)
		{
			cerr << "Warning: module based correlation desired but no media specified for antenna " << it->name << " ." << endl;
			nWarn++;
		}
		if(it->dataSource == DataSourceNone)
		{
			cerr << "Warning: data source is NONE for antenna " << it->name << " ." << endl;
			nWarn++;
		}
        }

        return nWarn;
}

VexSource *VexData::newSource()
{
	sources.push_back(VexSource());

	return &sources.back();
}

// get the clock epoch as a MJD value (with fractional component), negative 
// means not found.  Also fills in the first two coeffs, returned in seconds
double VexAntenna::getVexClocks(double mjd, double *coeffs) const
{
	vector<VexClock>::const_iterator it;
	double epoch = -1.0;

	for(it = clocks.begin(); it != clocks.end(); it++)
	{
		if(it->mjdStart <= mjd)
		{
			epoch = it->offset_epoch;
			coeffs[0] = it->offset;
			coeffs[1] = it->rate;
		}
	}

	return epoch;
}

const VexSource *VexData::getSource(unsigned int num) const
{
	if(num >= nSource())
	{
		return 0;
	}

	return &sources[num];
}

int VexData::getSourceIdByDefName(const string &defName) const
{
	vector<VexSource>::const_iterator it;
	int i = 0;

	for(it = sources.begin(); it != sources.end(); it++)
	{
		if(it->defName == defName)
		{
			return i;
		}
		i++;
	}

	return -1;
}

const VexSource *VexData::getSourceByDefName(const string &defName) const
{
	for(unsigned int i = 0; i < nSource(); i++)
	{
		if(sources[i].defName == defName)
			return &sources[i];
	}

	return 0;
}

const VexSource *VexData::getSourceBySourceName(const string &name) const
{
	for(unsigned int i = 0; i < nSource(); i++)
	{
		for(unsigned int j = 0; j < sources[i].sourceNames.size(); j++)
		if(sources[i].sourceNames[j] == name)
			return &sources[i];
	}

	return 0;
}

VexScan *VexData::newScan()
{
	scans.push_back(VexScan());
	return &scans.back();
}

void VexJob::assignVSNs(const VexData &V)
{
	list<string> antennas;
	vector<string>::const_iterator s;
	map<string,VexInterval>::const_iterator a;
	list<string>::const_iterator i;

	for(s = scans.begin(); s != scans.end(); s++)
	{
		const VexScan* S = V.getScanByDefName(*s);
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
		if(V.getAntenna(*i)->dataSource != DataSourceModule)
		{
			vsns[*i] = "None";
		}
		else
		{
			const string &vsn = V.getVSN(*i, *this);
			if(vsn != "None")
			{
				vsns[*i] = vsn;
			}
		}
	}
}

string VexJob::getVSN(const string &antName) const
{
	map<string,string>::const_iterator a;

	for(a = vsns.begin(); a != vsns.end(); a++)
	{
		if(a->first == antName)
		{
			return a->second;
		}
	}

	return string("None");
}

double VexJob::calcOps(const VexData *V, int fftSize, bool doPolar) const
{
	double ops = 0.0;
	int nAnt, nPol, nSubband;
	double sampRate, seconds;
	const VexMode *M;
	char pols[8];
	double opsPerSample;
	vector<string>::const_iterator si;

	for(si = scans.begin(); si != scans.end(); si++)
	{
		const VexScan *S = V->getScanByDefName(*si);
		M = V->getModeByDefName(S->modeDefName);
		if(!M)
		{
			return 0.0;
		}
		
		sampRate = M->sampRate;
		nPol = M->getPols(pols);
		if(nPol > 1 && doPolar)
		{
			nPol = 2;
		}
		else
		{
			nPol = 1;
		}

		seconds = S->duration_seconds();
		nAnt = S->stations.size();
		nSubband = M->subbands.size();

		// Estimate number of operations based on VLBA Sensitivity Upgrade Memo 16
		// Note: currently this does not consider oversampling
		// Note: this assumes all polarizations are matched
		opsPerSample = 16.0 + 5.0*log(fftSize)/log(2.0) + 2.5*nAnt*nPol;
		ops += opsPerSample*seconds*sampRate*nSubband*nAnt;

	}

	return ops;
}

double VexJob::calcSize(const VexData *V) const
{
	double size = 0.0;
	int nScan = scans.size();

	for(int i = 0; i < nScan; i++)
	{
		size += V->getScanByDefName(scans[i])->size;
	}

	return size;
}

bool VexJobGroup::hasScan(const string &scanName) const
{
	return find(scans.begin(), scans.end(), scanName) != scans.end();
}

void VexJobGroup::genEvents(const list<VexEvent> &eventList)
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

bool VexJob::hasScan(const string &scanName) const
{
        vector<string>::const_iterator it;

        for(it = scans.begin(); it != scans.end(); it++)
        {
                if(*it == scanName)
                {
                        return true;
                }
        }

        return false;
}

int VexJob::generateFlagFile(const VexData &V, const char *fileName, unsigned int invalidMask) const
{
	vector<VexJobFlag> flags;
	map<string,int> antIds;
	map<string,string>::const_iterator a;
	list<VexEvent>::const_iterator e;
	map<string,VexInterval>::const_iterator sa;
	unsigned int nAnt;
	ofstream of;
	const list<VexEvent> &eventList = *V.getEvents();

	for(nAnt = 0, a = vsns.begin(); a != vsns.end(); nAnt++, a++)
	{
		antIds[a->first] = nAnt;
	}

	// Assume all flags from the start.  
	vector<unsigned int> flagMask(nAnt, 
		VexJobFlag::JOB_FLAG_RECORD | 
		VexJobFlag::JOB_FLAG_POINT | 
		VexJobFlag::JOB_FLAG_TIME | 
		VexJobFlag::JOB_FLAG_SCAN);
	vector<double> flagStart(nAnt, mjdStart);

	// Except -- if not a Mark5 Module case, don't assume RECORD flag is on
	for(a = vsns.begin(); a != vsns.end(); a++)
	{
		const VexAntenna *ant = V.getAntenna(a->first);

		if(!ant)
		{
			cerr << "Developer error: generateFlagFile: antenna " <<
				a->first << " not found in antenna table." << endl;
			exit(0);
		}

		if(ant->basebandFiles.size() > 0)
		{
			// Aha -- not module based so unflag JOB_FLAG_RECORD
			flagMask[antIds[a->first]] &= ~VexJobFlag::JOB_FLAG_RECORD;
		}
	}

	// Then go through each event, adjusting current flag state.  
	for(e = eventList.begin(); e != eventList.end(); e++)
	{
		if(e->eventType == VexEvent::RECORD_START)
		{
			if(antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] &= ~VexJobFlag::JOB_FLAG_RECORD;
			}
		}
		else if(e->eventType == VexEvent::RECORD_STOP)
		{
			if(antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] |= VexJobFlag::JOB_FLAG_RECORD;
			}
		}
		else if(e->eventType == VexEvent::SCAN_START)
		{
			if(hasScan(e->scan))
			{
				const VexScan *scan = V.getScanByDefName(e->scan);

				if(!scan)
				{
					cerr << "Developer error: generateFlagFile: SCAN_START, scan=0" << endl;
					exit(0);
				}
				for(sa = scan->stations.begin(); sa != scan->stations.end(); sa++)
				{
					if(antIds.count(sa->first) == 0)
					{
						continue;
					}
					flagMask[antIds[sa->first]] &= ~VexJobFlag::JOB_FLAG_SCAN;
				}
			}
		}
		else if(e->eventType == VexEvent::SCAN_STOP)
		{
			if(hasScan(e->scan))
			{
				const VexScan *scan = V.getScanByDefName(e->scan);

				if(!scan)
				{
					cerr << "Developer error! generateFlagFile: SCAN_STOP, scan=0" << endl;
					exit(0);
				}
				for(sa = scan->stations.begin(); sa != scan->stations.end(); sa++)
				{
					if(antIds.count(sa->first) == 0)
					{
						continue;
					}
					flagMask[antIds[sa->first]] |= VexJobFlag::JOB_FLAG_SCAN;
				}
			}
		}
		else if(e->eventType == VexEvent::ANT_SCAN_START)
		{
			if(hasScan(e->scan) && antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] &= ~VexJobFlag::JOB_FLAG_POINT;
			}
		}
		else if(e->eventType == VexEvent::ANT_SCAN_STOP)
		{
			if(hasScan(e->scan) && antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] |= VexJobFlag::JOB_FLAG_POINT;
			}
		}
		else if(e->eventType == VexEvent::JOB_START)
		{
			if(fabs(e->mjd - mjdStart) < 0.5/86400.0)
			{
				for(a = vsns.begin(); a != vsns.end(); a++)
				{
					flagMask[antIds[a->first]] &= ~VexJobFlag::JOB_FLAG_TIME;
				}
			}
		}
		else if(e->eventType == VexEvent::JOB_STOP)
		{
			if(fabs(e->mjd - mjdStart) < 0.5/86400.0)
			{
				for(a = vsns.begin(); a != vsns.end(); a++)
				{
					flagMask[antIds[a->first]] |= VexJobFlag::JOB_FLAG_TIME;
				}
			}
		}

		for(unsigned int antId = 0; antId < nAnt; antId++)
		{
			if( (flagMask[antId] & invalidMask) == 0)
			{
				if(flagStart[antId] > 0)
				{
					if(e->mjd - flagStart[antId] > 0.5/86400.0)
					{
						VexJobFlag f(flagStart[antId], e->mjd, antId);
						// only add flag if it overlaps in time with this job
						if(overlap(f))
						{
							flags.push_back(f);
						}
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

	// At end of loop see if any flag->unflag (or vice-versa) occurs.
	for(unsigned int antId = 0; antId < nAnt; antId++)
	{
		if( (flagMask[antId] & invalidMask) != 0)
		{
			if(mjdStop - flagStart[antId] > 0.5/86400.0)
			{
				VexJobFlag f(flagStart[antId], mjdStop, antId);
				// only add flag if it overlaps in time with this job
				if(overlap(f))
				{
					flags.push_back(f);
				}
			}
		}
	}

	// write data to file
	of.open(fileName);
	of << flags.size() << endl;
	for(unsigned int f = 0; f < flags.size(); f++)
	{
		of << "  " << flags[f] << endl;
	}
	of.close();

	return flags.size();
}

void VexJobGroup::createJobs(vector<VexJob> &jobs, VexInterval &jobTimeRange, const VexData *V, double maxLength, double maxSize) const
{
	list<VexEvent>::const_iterator s, e;
	jobs.push_back(VexJob());
	VexJob *J = &jobs.back();
	double totalTime, scanTime = 0.0;
	double size = 0.0;
	string id("");

	// note these are backwards now -- will set these to minimum range covering scans
	J->setTimeRange(jobTimeRange.mjdStop, jobTimeRange.mjdStart);

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
				cerr << "Programming error: createJobs: id != e->name  (" << id << " != " << e->name << ")" << endl;
				cerr << "Contact developer" << endl;
				exit(0);
			}
			VexInterval scanTimeRange(s->mjd, e->mjd);
			scanTimeRange.logicalAnd(jobTimeRange);
			if(scanTimeRange.duration() > 0.0)
			{
				J->scans.push_back(e->name);
				J->logicalOr(scanTimeRange);
				scanTime += scanTimeRange.duration();

				// Work in progress: calculate correlated size of scan
				size += V->getScanByDefName(id)->size;

				/* start a new job at scan boundary if maxLength exceeded */
				if(J->duration() > maxLength || size > maxSize)
				{
					totalTime = J->duration();
					J->dutyCycle = scanTime / totalTime;
					J->dataSize = size;
					jobs.push_back(VexJob());
					J = &jobs.back();
					scanTime = 0.0;
					size = 0.0;
					J->setTimeRange(jobTimeRange.mjdStop, jobTimeRange.mjdStart);
				}
			}
		}
	}

	totalTime = J->duration();
	
	if(totalTime <= 0.0)
	{
		jobs.pop_back();
	}
	else
	{
		J->dutyCycle = scanTime / totalTime;
		J->dataSize = size;
	}
}

const VexScan *VexData::getScan(unsigned int num) const
{
	if(num >= nScan())
	{
		return 0;
	}

	return &scans[num];
}

const VexScan *VexData::getScanByDefName(const string &defName) const
{
	for(unsigned int i = 0; i < nScan(); i++)
	{
		if(scans[i].defName == defName)
			return &scans[i];
	}

	return 0;
}

void VexData::setScanSize(unsigned int num, double size)
{
	if(num >= nScan())
	{
		return;
	}
	
	scans[num].size = size;
}

void VexData::getScanList(list<string> &scanList) const
{
	vector<VexScan>::const_iterator it;

	for(it = scans.begin(); it != scans.end(); it++)
	{
		scanList.push_back(it->defName);
	}
}

int VexEOP::setkv(const string &key, const string &value)
{
	stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "tai_utc")
	{
		ss >> tai_utc;
	}
	else if(key == "ut1_utc")
	{
		ss >> ut1_utc;
	}
	else if(key == "xPole")
	{
		ss >> xPole;
		xPole /= RAD2ASEC;
	}
	else if(key == "yPole")
	{
		ss >> yPole;
		yPole /= RAD2ASEC;
	}
	else
	{
		cerr << "Warning: EOP: Unknown parameter '" << key << "'." << endl;
		nWarn++;
	}

	return nWarn;
}

VexAntenna *VexData::newAntenna()
{
	antennas.push_back(VexAntenna());
	return &antennas.back();
}

const VexAntenna *VexData::getAntenna(unsigned int num) const
{
	if(num >= nAntenna())
	{
		return 0;
	}

	return &antennas[num];
}

const VexAntenna *VexData::getAntenna(const string &name) const
{
	for(unsigned int i = 0; i < nAntenna(); i++)
	{
		if(antennas[i].name == name)
			return &antennas[i];
	}

	return 0;
}

int VexSetup::phaseCalIntervalMHz() const
{
	map<string,VexIF>::const_iterator it;
	int p;
	int pc = 0;

	for(it = ifs.begin(); it != ifs.end(); it++)
	{
		p = it->second.phaseCalIntervalMHz;
		if(p > 0 && (p < pc || pc == 0))
		{
			pc = p;
		}
	}

	return pc;
}

const VexIF *VexSetup::getIF(const string &ifname) const
{
	map<string,VexIF>::const_iterator it;

	for(it = ifs.begin(); it != ifs.end(); it++)
	{
		if(it->second.name == ifname)
		{
			return &it->second;
		}
	}

	return 0;
}

bool operator ==(const VexChannel &c1, const VexChannel &c2)
{
	if( (c1.recordChan   != c2.recordChan)   ||
	    (c1.subbandId    != c2.subbandId)    ||
	    (c1.ifname       != c2.ifname)       ||
	    (c1.bbcFreq      != c2.bbcFreq)      ||
	    (c1.bbcSideBand  != c2.bbcSideBand)  ||
	    (c1.tones.size() != c2.tones.size()) )
	{
		return false;
	}
	for(unsigned int i = 0; i < c1.tones.size(); i++)
	{
		if(c1.tones[i] != c2.tones[i])
		{
			return false;
		}
	}

	return true;
}

bool operator ==(const VexFormat &f1, const VexFormat &f2)
{
	if( (f1.format      != f2.format)      ||
	    (f1.nBit        != f2.nBit)        ||
	    (f1.nRecordChan != f2.nRecordChan) )
	{
		return false;
	}

	for(unsigned int r = 0; r < f1.nRecordChan; r++)
	{
		if( !(f1.channels[r] == f2.channels[r]) )
		{
			return false;
		}
	}

	return true;
}

VexMode *VexData::newMode()
{
	modes.push_back(VexMode());
	return &modes.back();
}

const VexMode *VexData::getMode(unsigned int num) const
{
	if(num >= nMode())
	{
		return 0;
	}

	return &modes[num];
}

const VexMode *VexData::getModeByDefName(const string &defName) const
{
	for(unsigned int i = 0; i < nMode(); i++)
	{
		if(modes[i].defName == defName)
		{
			return &modes[i];
		}
	}

	return 0;
}

int VexData::getModeIdByDefName(const string &defName) const
{
	vector<VexMode>::const_iterator it;
	int i = 0;

	for(it = modes.begin(); it != modes.end(); it++)
	{
		if(it->defName == defName)
		{
			return i;
		}
		i++;
	}

	return -1;
}


VexEOP *VexData::newEOP()
{
	eops.push_back(VexEOP());
	return &eops.back();
}

const VexEOP *VexData::getEOP(unsigned int num) const
{
	if(num > nEOP())
	{
		return 0;
	}

	return &eops[num];
}

bool VexData::usesAntenna(const string &antennaName) const
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

bool VexData::usesMode(const string &modeDefName) const
{
	int n = nScan();

	for(int i = 0; i < n; i++)
	{
		if(getScan(i)->modeDefName == modeDefName)
		{
			return true;
		}
	}

	return false;
}

unsigned int VexData::nVSN(const string &antName) const
{
	const VexAntenna *A;

	A = getAntenna(antName);
	if(!A)
	{
		return 0;
	}
	else
	{
		return A->basebandFiles.size();
	}
}

void VexData::addVSN(const string &antName, const string &vsn, const VexInterval &timeRange)
{
	int n = nAntenna();

	for(int i = 0; i < n; i++)
	{
		if(antennas[i].name == antName)
		{
			antennas[i].basebandFiles.push_back(VexBasebandFile(vsn, timeRange));
			antennas[i].dataSource = DataSourceModule;
		}
	}
}

string VexData::getVSN(const string &antName, const VexInterval &timeRange) const
{
	const VexAntenna *A;
	vector<VexBasebandFile>::const_iterator v;
	double best = 0.0;
	string bestVSN("None");

	A = getAntenna(antName);
	if(!A)
	{
		return bestVSN;
	}

	if(A->dataSource != DataSourceModule)
	{
		return bestVSN;
	}

	for(v = A->basebandFiles.begin(); v != A->basebandFiles.end(); v++)
	{
		double timeOverlap = timeRange.overlap(*v);
		if(timeOverlap > best)
		{
			best = timeOverlap;
			bestVSN = v->filename;
		}
	}

	return bestVSN;
}

void VexData::setExper(const string &name, const VexInterval &experTimeRange)
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

	exper.name = name;
	exper.setTimeRange(experTimeRange);
	if(exper.mjdStart < 10000)
	{
		exper.mjdStart = a;
	}
	if(exper.mjdStop < 10000)
	{
		exper.mjdStop = b;
	}

	addEvent(exper.mjdStart, VexEvent::OBSERVE_START, name); 
	addEvent(exper.mjdStop, VexEvent::OBSERVE_STOP, name); 
}

const list<VexEvent> *VexData::getEvents() const
{
	return &events;
}

void VexData::addEvent(double mjd, VexEvent::EventType eventType, const string &name)
{
	events.push_back(VexEvent(mjd, eventType, name));
}

void VexData::addEvent(double mjd, VexEvent::EventType eventType, const string &name, const string &scan)
{
	events.push_back(VexEvent(mjd, eventType, name, scan));
}

void VexData::sortEvents()
{
	events.sort();
}

void VexData::findLeapSeconds()
{
        int n = eops.size();

        if(n < 2)
        {
                return;
        }

        for(int i = 1; i < n; i++)
        {
                if(eops[i-1].tai_utc != eops[i].tai_utc)
                {
                        addEvent(eops[i].mjd, VexEvent::LEAP_SECOND, "Leap second");
                        cout << "Leap second detected at day " << eops[i].mjd << endl;
                }
        }
}

void VexData::addBreaks(const vector<double> &breaks)
{
        int n = breaks.size();

        if(n < 1)
        {
                return;
        }

        for(int i = 0; i < n; i++)
        {
                double mjd = breaks[i];

                if(exper.contains(mjd))
                {
                        addEvent(mjd, VexEvent::MANUAL_BREAK, "");
                }
        }
}

ostream& operator << (ostream &os, const VexInterval &x)
{
	int p = os.precision();

	os.precision(12);
	os << "mjd(" << x.mjdStart << "," << x.mjdStop << ")";
	os.precision(p);

	return os;
}

ostream& operator << (ostream &os, const VexSource &x)
{
	int n = x.sourceNames.size();

	os << "Source " << x.defName << endl;
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

ostream& operator << (ostream &os, const VexScan &x)
{
	os << "Scan " << x.defName << 
		"\n  timeRange=" << (const VexInterval&)x <<
		"\n  mode=" << x.modeDefName <<
		"\n  source=" << x.sourceDefName << 
		"\n  size=" << x.size << " bytes \n";

	for(map<string,VexInterval>::const_iterator iter = x.stations.begin(); iter != x.stations.end(); iter++)
	{
		os << "  " << iter->first << " range=" << iter->second << endl;
	}

	for(map<string,bool>::const_iterator iter = x.recordEnable.begin(); iter != x.recordEnable.end(); iter++)
	{
		os << "  " << iter->first << " enable=" << iter->second << endl;
	}

	os << "  setup=" << x.corrSetupName << endl;

	return os;
}

ostream& operator << (ostream &os, const VexClock &x)
{
	os << "Clock(" << x.mjdStart << ": " << x.offset << ", " << x.rate << ", " << x.offset_epoch << ")";

	return os;
}

ostream& operator << (ostream &os, const VexAntenna &x)
{
	os << "Antenna " << x.name <<
		"\n  x=" << x.x << "  dx/dt=" << x.dx <<
		"\n  y=" << x.y << "  dy/dt=" << x.dy <<
		"\n  z=" << x.z << "  dz/dt=" << x.dz <<
		"\n  posEpoch=" << x.posEpoch <<
		"\n  axisType=" << x.axisType <<
		"\n  axisOffset=" << x.axisOffset << endl;

	os << "  dataSource=" << dataSourceNames[x.dataSource] << endl;

	for(vector<VexBasebandFile>::const_iterator it = x.basebandFiles.begin(); it != x.basebandFiles.end(); it++)
	{
		os << "  " << *it << endl;
	}

	for(vector<VexClock>::const_iterator it = x.clocks.begin(); it != x.clocks.end(); it++)
	{
		os << "  " << *it << endl;
	}

	return os;
}

ostream& operator << (ostream &os, const VexSubband &x)
{
	os << "[" << x.freq << " Hz, " << x.bandwidth << " Hz, sb=" << x.sideBand << ", pol=" << x.pol << "]";
	
	return os;
}

ostream& operator << (ostream &os, const VexChannel &x)
{
	vector<int>::const_iterator v;
	
	os << "[IF=" << x.ifname << " s=" << x.subbandId << " -> r=" << x.recordChan << " tones=";
	for(v = x.tones.begin(); v != x.tones.end(); v++)
	{
		if(v != x.tones.begin())
		{
			os << ",";
		}
		os << *v;
	}
	os << "]";

	return os;
}

ostream& operator << (ostream &os, const VexIF &x)
{
	os << "[name=" << x.name << ", SSLO=" << x.ifSSLO << ", sb=" << x.ifSideBand << ", pol=" << x.pol << ", phaseCalInterval=" << x.phaseCalIntervalMHz << " MHz]";

	return os;
}

ostream& operator << (ostream &os, const VexFormat &x)
{
	vector<VexChannel>::const_iterator it;
	
	os << "[format=" << x.format << ", nBit=" << x.nBit << ", nChan=" << x.nRecordChan;
	for(it = x.channels.begin(); it != x.channels.end(); it++)
	{
		os << ", " << *it;
	}
	os << "]";

	return os;
}

ostream& operator << (ostream &os, const VexSetup &x)
{
	map<string,VexIF>::const_iterator it;
	
	os << "    Format = " << x.format << endl;
	for(it = x.ifs.begin(); it != x.ifs.end(); it++)
	{
		os << "    IF: " << it->first << " " << it->second << endl;
	}

	return os;
}

ostream& operator << (ostream &os, const VexMode &x)
{
	map<string,VexSetup>::const_iterator it;

	os << "Mode " << x.defName << endl;
	for(unsigned int i = 0; i < x.subbands.size(); i++)
	{
		os << "  Subband[" << i << "]=" << x.subbands[i] << endl;
	}
	for(it = x.setups.begin(); it != x.setups.end(); ++it)
	{
		os << "  Setup[" << it->first << "]" << endl;
		os << it->second;
	}
	
	return os;
}

ostream& operator << (ostream &os, const VexEOP &x)
{
	os << "EOP(" << x.mjd << ", " << x.tai_utc << ", " << x.ut1_utc << ", " << (x.xPole*RAD2ASEC) << ", " << (x.yPole*RAD2ASEC) << ")";

	return os;
}


ostream& operator << (ostream &os, const VexBasebandFile &x)
{
	os << "Baseband(" << x.filename << ", " << (const VexInterval&)x << ")";

	return os;
}

ostream& operator << (ostream &os, const VexJob &x)
{
	vector<string>::const_iterator s;
	map<string,string>::const_iterator v;
	int p = os.precision();
	
	os.precision(12);
	os << "Job " << x.jobSeries << "_" << x.jobId << endl;
	os << "  " << (const VexInterval&)x << endl;
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
	os << "  size = " << x.dataSize << " bytes" << endl;

	os.precision(p);

	return os;
}

ostream& operator << (ostream &os, const VexJobGroup &x)
{
	int p = os.precision();
	
	os.precision(12);
	os << "Group: scans " << x.scans.front() << " - " << x.scans.back() << " = " << (const VexInterval &)x << endl;
	os.precision(p);
	
	return os;
}

ostream& operator << (ostream &os, const VexEvent &x)
{
	int d, s;

	d = static_cast<int>(x.mjd);
	s = static_cast<int>((x.mjd - d)*86400.0 + 0.5);

	os << "mjd=" << d << " sec=" << s << " : " << VexEvent::eventName[x.eventType] << " " << x.name;

	return os;
}

ostream& operator << (ostream &os, const VexJobFlag &x)
{
	int p = os.precision();

	os.precision(12);

	os << x.mjdStart << " " << x.mjdStop << " " << x.antId;

	os.precision(p);

	return os;
}

ostream& operator << (ostream &os, const VexData &x)
{
	int n = x.nSource();

	os << "Vex:" << endl;
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
