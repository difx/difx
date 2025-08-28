/***************************************************************************
 *   Copyright (C) 2009-2022 by Walter Brisken & Adam Deller               *
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
 * $Id: vex_data.cpp 10425 2022-04-12 19:56:47Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/vex2difx/vexdatamodel/vex_data.cpp $
 * $LastChangedRevision: 10425 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2022-04-13 03:56:47 +0800 (三, 2022-04-13) $
 *
 *==========================================================================*/

#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <list>
#include <regex.h>
#include "vex_data.h"
#include "vex_utility.h"


int VexData::sanityCheck()
{
	int nWarn = 0;

	if(eops.size() < 5)
	{
		std::cerr << "Warning: Fewer than 5 EOPs specified" << std::endl;
		++nWarn;
	}

	return nWarn;
}

VexSource *VexData::newSource()
{
	sources.push_back(VexSource());

	return &sources.back();
}

VexSource *VexData::newSource(const std::string &name, double ra, double dec)
{
	sources.push_back(VexSource(name, ra, dec));

	return &sources.back();
}

const VexSource *VexData::getSource(unsigned int num) const
{
	if(num >= nSource())
	{
		return 0;
	}

	return &sources[num];
}

int VexData::getSourceIdByDefName(const std::string &defName) const
{
	for(std::vector<VexSource>::const_iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(it->defName == defName)
		{
			return it - sources.begin();
		}
	}

	return -1;
}

const VexSource *VexData::getSourceByDefName(const std::string &defName) const
{
	for(std::vector<VexSource>::const_iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(it->defName == defName)
		{
			return &(*it);
		}
	}

	return 0;
}

const VexSource *VexData::getSourceBySourceName(const std::string &name) const
{
	for(std::vector<VexSource>::const_iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(find(it->sourceNames.begin(), it->sourceNames.end(), name) != it->sourceNames.end())
		{
			return &(*it);
		}
	}

	return 0;
}

void VexData::setSourceCalCode(const std::string &name, char calCode)
{
	for(std::vector<VexSource>::iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(it->defName == name)
		{
			it->calCode = calCode;
		}
	}
}

void VexData::setSourceEphemerisFile(const std::string &name, const std::string &ephemFile, const std::string &ephemObject)
{
	for(std::vector<VexSource>::iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(it->defName == name)
		{
			it->setBSP(ephemFile.c_str(), ephemObject.c_str());
		}
	}
}

// (X, Y, Z in meters)
void VexData::setSourceITRFCoordinates(const std::string &name, double X, double Y, double Z)
{
	for(std::vector<VexSource>::iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(it->defName == name)
		{
			it->setFixed(X, Y, Z);
		}
	}
}

// (ra, dec in radians)
void VexData::setSourceCoordinates(const std::string &name, double ra, double dec)
{
	for(std::vector<VexSource>::iterator it = sources.begin(); it != sources.end(); ++it)
	{
		if(it->defName == name)
		{
			it->type = VexSource::Star;
			it->ra = ra;
			it->dec = dec;
		}
	}
}



VexScan *VexData::newScan()
{
	scans.push_back(VexScan());

	return &scans.back();
}


const VexScan *VexData::getScan(unsigned int num) const
{
	if(num >= nScan())
	{
		return 0;
	}

	return &scans[num];
}

const VexScan *VexData::getScanByDefName(const std::string &defName) const
{
	for(std::vector<VexScan>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		if(it->defName == defName)
		{
			return &(*it);
		}
	}

	return 0;
}

const VexScan *VexData::getScanByAntennaTime(const std::string &antName, double mjd) const
{
	for(std::vector<VexScan>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		const Interval *interval = it->getAntennaInterval(antName);
		if(interval)
		{
			if(interval->contains(mjd))
			{
				return &(*it);
			}
		}
	}

	return 0;
}

static Interval adjustTimeRange(std::map<std::string, double> &antStart, std::map<std::string, double> &antStop, unsigned int minSubarraySize)
{
	std::list<double> start;
	std::list<double> stop;
	double mjdStart, mjdStop;

	if(minSubarraySize < 1)
	{
		std::cerr << "Developer error: adjustTimeRange: minSubarraySize = " << minSubarraySize << " is < 1" << std::endl;

		exit(EXIT_FAILURE);
	}

	if(antStart.size() != antStop.size())
	{
		std::cerr << "Developer error: adjustTimeRange: size mismatch" << std::endl;

		exit(EXIT_FAILURE);
	}

	if(antStart.size() < minSubarraySize)
	{
		// Return an acausal interval
		return Interval(1, 0);
	}

	for(std::map<std::string, double>::iterator it = antStart.begin(); it != antStart.end(); ++it)
	{
		start.push_back(it->second);
	}
	start.sort();
	// Now the start times are sorted chronologically

	for(std::map<std::string, double>::iterator it = antStop.begin(); it != antStop.end(); ++it)
	{
		stop.push_back(it->second);
	}
	stop.sort();
	// Now stop times are sorted chronologically

	// Pick off times where min subarray condition is met
	// If these are in the wrong order (i.e., no such interval exists)
	// Then these will form an acausal interval which will be caught by
	// the caller.
	for(unsigned int i = 0; i < minSubarraySize-1; ++i)
	{
		start.pop_front();
		stop.pop_back();
	}
	mjdStart = start.front();
	mjdStop = stop.back();

	// Adjust start times where needed
	for(std::map<std::string, double>::iterator it = antStart.begin(); it != antStart.end(); ++it)
	{
		if(it->second < mjdStart)
		{
			it->second = mjdStart;
		}
	}

	for(std::map<std::string, double>::iterator it = antStop.begin(); it != antStop.end(); ++it)
	{
		if(it->second > mjdStop)
		{
			it->second = mjdStop;
		}
	}

	return Interval(mjdStart, mjdStop);
}

// this removes scans out of time range or with fewer than minSubarraySize antennas
// the final time ranges (both of scans and antennas within) are truncated to the time window when the subarray size condition is met.
void VexData::reduceScans(int minSubarraySize, const Interval &timerange)
{
	std::list<std::string> antsToRemove;
	std::map<std::string, double> antStart, antStop;

// FIXME: maybe print some statistics such as number of scans dropped due to minsubarraysize and timerange

	for(std::vector<VexScan>::iterator it = scans.begin(); it != scans.end(); )
	{
		antStart.clear();
		antStop.clear();
		antsToRemove.clear();

		for(std::map<std::string,Interval>::iterator sit = it->stations.begin(); sit != it->stations.end(); ++sit)
		{
			if(sit->second.overlap(timerange) <= 0.0)
			{
				antsToRemove.push_back(sit->first);
			}
			else
			{
				sit->second.logicalAnd(timerange);
				antStart[sit->first] = sit->second.mjdStart;
				antStop[sit->first]  = sit->second.mjdStop;
			}
		}
		for(std::list<std::string>::const_iterator ait = antsToRemove.begin(); ait != antsToRemove.end(); ++ait)
		{
			it->stations.erase(*ait);
		}

		Interval antennaTimeRange = adjustTimeRange(antStart, antStop, minSubarraySize);

		if(!antennaTimeRange.isCausal() || it->overlap(antennaTimeRange) <= 0.05/86400.0)
		{
			it = scans.erase(it);
			continue;
		}

		for(std::map<std::string,Interval>::iterator sit = it->stations.begin(); sit != it->stations.end(); ++sit)
		{
			sit->second.logicalAnd(antennaTimeRange);
		}

		it->logicalAnd(antennaTimeRange);
		++it;
	}
}

void VexData::setScanSize(unsigned int num, double size)
{
	if(num >= nScan())
	{
		return;
	}
	
	scans[num].size = size;
}

void VexData::getScanList(std::list<std::string> &scanList) const
{
	for(std::vector<VexScan>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		scanList.push_back(it->defName);
	}
}

unsigned int VexData::nAntennasWithRecordedData(const VexScan &scan) const
{
	unsigned int nAnt = 0;

	for(std::map<std::string,Interval>::const_iterator it = scan.stations.begin(); it != scan.stations.end(); ++it)
	{
		if(hasData(it->first, scan) && scan.getRecordEnable(it->first))
		{
			++nAnt;
		}
	}

	return nAnt;
}

bool VexData::removeScan(const std::string name)
{
	int removed = false;

	for(std::vector<VexScan>::iterator it = scans.begin(); it != scans.end(); )
	{
		if(it->defName == name)
		{
			it = scans.erase(it);
			removed = true;
		}
		else
		{
			++it;
		}
	}

	return removed;
}

void VexData::deletePhaseCenters(unsigned int num)
{
	if(num >= nScan())
	{
		std::cerr << "Developer error: VexData::deletePhaseCentres() : num=" << num << " nScan=" << nScan() << std::endl;
	}

	scans[num].phaseCenters.clear();
}

void VexData::addPhaseCenter(unsigned int num, const std::string &name)
{
	if(num >= nScan())
	{
		std::cerr << "Developer error: VexData::addPhaseCentre() : num=" << num << " nScan=" << nScan() << std::endl;
	}

	scans[num].phaseCenters.push_back(name);
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

const VexAntenna *VexData::getAntenna(const std::string &name) const
{
	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == name)
		{
			return &(*it);
		}
	}

	return 0;
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

const VexMode *VexData::getModeByDefName(const std::string &defName) const
{
	for(std::vector<VexMode>::const_iterator it = modes.begin(); it != modes.end(); ++it)
	{
		if(it->defName == defName)
		{
			return &(*it);
		}
	}

	return 0;
}

void VexData::generateRecordChans()
{
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); ++it)
	{
		it->generateRecordChans();
	}
}

unsigned int VexData::nRecordChan(const VexMode &mode, const std::string &antName) const
{
	unsigned int nRecChan = 0;

	std::map<std::string,VexSetup>::const_iterator it = mode.setups.find(antName);
	if(it != mode.setups.end())
	{
		nRecChan = it->second.nRecordChan();
	}
	else
	{
		std::cerr << "Warning: Ant " << antName << " not found in mode " << mode.defName << std::endl;
	}

	return nRecChan;
}

int VexData::getAntennaIdByName(const std::string &antName) const
{
	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			return it - antennas.begin();
		}
	}

	return -1;
}

int VexData::getAntennaIdByDefName(const std::string &antName) const
{
	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->defName == antName)
		{
			return it - antennas.begin();
		}
	}

	return -1;
}

// returns < 0 if number of channels varies with mode
int VexData::getNumAntennaRecChans(const std::string &antName) const
{
	int nRecChan = 0;
	
	for(std::vector<VexMode>::const_iterator it = modes.begin(); it != modes.end(); ++it)
	{
		int n;

		n = nRecordChan(*it, antName);
		if(n > 0)
		{
			if(nRecChan == 0)
			{
				nRecChan = n;
			}
			if(nRecChan != n)
			{
				return -1;	// two scans with different numbers of record chans found.
			}
		}
	}

	return nRecChan;
}

// returns false if antenna was not there.  Otherwise true
// this function essentially wipes out any record of this antenna being part of observing
bool VexData::removeAntenna(const std::string name)
{
	bool rv = false;

	// remove VexAntenna
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); )
	{
		if(it->name == name)
		{
			it = antennas.erase(it);
			rv = true;
		}
		else
		{
			++it;
		}
	}

	// remove antenna from scans
	for(std::vector<VexScan>::iterator it = scans.begin(); it != scans.end(); )
	{
		it->stations.erase(name);
		it->recordEnable.erase(name);

		if(it->stations.empty())
		{
			it = scans.erase(it);
		}
		else
		{
			++it;
		}
	}

	// remove antenna from modes
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); )
	{
		it->setups.erase(name);

		if(it->setups.empty())
		{
			it = modes.erase(it);
		}
		else
		{
			++it;
		}
	}

	return rv;
}


int VexData::getModeIdByDefName(const std::string &defName) const
{
	for(std::vector<VexMode>::const_iterator it = modes.begin(); it != modes.end(); ++it)
	{
		if(it->defName == defName)
		{
			return it - modes.begin();
		}
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

void VexData::addEOP(const VexEOP &e)
{
	VexEOP *E;

	E = newEOP();
	*E = e;
}


VexExtension *VexData::newExtension()
{
	extensions.push_back(VexExtension());

	return &extensions.back();
}

const VexExtension *VexData::getExtension(unsigned int num) const
{
	if(num > nExtension())
	{
		return 0;
	}

	return &extensions[num];
}

void VexData::addExtension(const VexExtension &e)
{
	VexExtension *E;

	E = newExtension();
	*E = e;
}


bool VexData::usesAntenna(const std::string &antennaName) const
{
	unsigned int n = nAntenna();

	for(unsigned int i = 0; i < n; ++i)
	{
		if(getAntenna(i)->name == antennaName)
		{
			return true;
		}
	}

	return false;
}

bool VexData::usesMode(const std::string &modeDefName) const
{
	unsigned int n = nScan();

	for(unsigned int i = 0; i < n; ++i)
	{
		if(getScan(i)->modeDefName == modeDefName)
		{
			return true;
		}
	}

	return false;
}

std::vector<VexBasebandData> *VexData::getVSNs(const std::string &antName)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			return &(it->vsns);
		}
	}

	return 0;
}

void VexData::addVSN(const std::string &antName, int drive, const std::string &vsn, const Interval &timeRange)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->vsns.push_back(VexBasebandData(vsn, drive, -1, timeRange));
		}
	}
}
void VexData::addFile(const std::string &antName, int drive, const std::string &filename, const Interval &timeRange)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->files.push_back(VexBasebandData(filename, drive, -1, timeRange));
		}
	}
}


// removes all baseband data for a given antenna/datastream.  If datastreamId < 0, removes from all datastreams
void VexData::removeBasebandData(const std::string &antName, int datastreamId)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->removeBasebandData(datastreamId);
		}
	}
}

void VexData::setExper(const std::string &name, const Interval &experTimeRange)
{
	exper.name = name;
	exper.segment = "";
	exper.setTimeRange(experTimeRange);
}

void VexData::setExper(const std::string &name, const std::string &segment, const Interval &experTimeRange)
{
	exper.name = name;
	exper.segment = segment;
	exper.setTimeRange(experTimeRange);
}

void VexData::addLeapSecondEvents(std::list<Event> &events) const
{
	int n = eops.size();

	if(n < 2)
	{
		return;
	}

	for(int i = 1; i < n; ++i)
	{
		if(eops[i-1].tai_utc != eops[i].tai_utc)
		{
			addEvent(events, eops[i].mjd, Event::LEAP_SECOND, "Leap second");
			std::cout << "Leap second detected at day " << eops[i].mjd << std::endl;
		}
	}
}

void VexData::swapPolarization(const std::string &antName)
{
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); ++it)
	{
		it->swapPolarization(antName);
	}
}

void VexData::setPhaseCalInterval(const std::string &antName, float phaseCalIntervalMHz)
{
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); ++it)
	{
		it->setPhaseCalInterval(antName, phaseCalIntervalMHz);
	}
}

void VexData::selectTones(const std::string &antName, enum ToneSelection selection, double guardBandMHz)
{
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); ++it)
	{
		it->selectTones(antName, selection, guardBandMHz);
	}
}

void VexData::setClock(const std::string &antName, const VexClock &clock)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->clocks.clear();
			it->clocks.push_back(clock);
		}
	}
}

// deltaClock in sec and deltaClockRate in sec/sec
void VexData::adjustClock(const std::string &antName, double deltaClock, double deltaClockRate)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			for(std::vector<VexClock>::iterator c = it->clocks.begin(); c != it->clocks.end(); ++c)
			{
				c->offset += deltaClock;	// [sec]
				c->rate += deltaClockRate;	// [sec/sec]
			}
		}
	}
}

void VexData::setAntennaDifxName(const std::string &name, const std::string &difxName)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == name)
		{
			it->difxName = difxName;
		}
	}
}

void VexData::setTcalFrequency(const std::string &antName, int tcalFrequency)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->tcalFrequency = tcalFrequency;
		}
	}
}

void VexData::setAntennaPosition(const std::string &antName, double X, double Y, double Z)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->x = X;
			it->y = Y;
			it->z = Z;
			it->dx = 0.0;
			it->dy = 0.0;
			it->dz = 0.0;
		}
	}
}

void VexData::setAntennaAxisOffset(const std::string &antName, double axisOffset)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->axisOffset = axisOffset;
		}
	}
}

void VexData::setAntennaPolConvert(const std::string &antName, bool doConvert)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->polConvert = doConvert;
		}
	}
}

void VexData::addExperEvents(std::list<Event> &events) const
{
	addEvent(events, exper.mjdStart, Event::OBSERVE_START, exper.name); 
	addEvent(events, exper.mjdStop, Event::OBSERVE_STOP, exper.name); 
}

void VexData::addClockEvents(std::list<Event> &events) const
{
	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		for(std::vector<VexClock>::const_iterator cit = it->clocks.begin(); cit != it->clocks.end(); ++cit)
		{
			addEvent(events, cit->mjdStart, Event::CLOCK_BREAK, it->defName);
		}
	}
}

void VexData::addScanEvents(std::list<Event> &events) const
{
	for(std::vector<VexScan>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		addEvent(events, it->mjdStart, Event::SCAN_START, it->defName, it->defName);
		addEvent(events, it->mjdStop,  Event::SCAN_STOP,  it->defName, it->defName);
		for(std::map<std::string,Interval>::const_iterator sit = it->stations.begin(); sit != it->stations.end(); ++sit)
		{
			addEvent(events, std::max(sit->second.mjdStart, it->mjdStart), Event::ANT_SCAN_START, sit->first, it->defName);
			addEvent(events, std::min(sit->second.mjdStop,  it->mjdStop),  Event::ANT_SCAN_STOP,  sit->first, it->defName);
		}
	}

}

void VexData::addVSNEvents(std::list<Event> &events) const
{
	for(unsigned int antId = 0; antId < antennas.size(); ++antId)
	{
		const VexAntenna *A = getAntenna(antId);

		for(std::vector<VexBasebandData>::const_iterator vit = A->vsns.begin(); vit != A->vsns.end(); ++vit)
		{
			if(vit->streamId >= 0)
			{
				enum DataSource ds = getDataSource(antId, vit->streamId);
				if(ds == DataSourceModule || ds == DataSourceMark6)
				{
					addEvent(events, vit->mjdStart, Event::RECORD_START, A->defName);
					addEvent(events, vit->mjdStop,  Event::RECORD_STOP,  A->defName);
				}
			}
		}
	}
}

void VexData::addBreakEvents(std::list<Event> &events, const std::vector<double> &breaks) const
{
	for(std::vector<double>::const_iterator t = breaks.begin(); t != breaks.end(); ++t)
	{
		if(exper.contains(*t))
		{
			addEvent(events, *t, Event::MANUAL_BREAK, "");
		}
	}
}

void VexData::generateEvents(std::list<Event> &events) const
{
	events.clear();

	addExperEvents(events);
	addClockEvents(events);
	addScanEvents(events);
	addVSNEvents(events);
	addLeapSecondEvents(events);

	events.sort();
}

void VexData::setDifxTsys(unsigned int antId, unsigned int streamId, double tSys)
{
	std::vector<VexMode>::iterator mit;

	if(antId >= antennas.size())
	{
		std::cerr << "Developer error: VexData::setDifxTsys: antId = " << antId << " where size of antennas[] is " << antennas.size() << std::endl;
		
		exit(EXIT_FAILURE);
	}

	for(mit = modes.begin(); mit != modes.end(); ++mit)
	{
		std::map<std::string,VexSetup>::iterator it = mit->setups.find(antennas[antId].name);

		if(it == mit->setups.end())
		{
			continue;
		}
		if(streamId >= it->second.streams.size())
		{
			std::cerr << "Developer error: VexData::setDifxTsys: antId = " << antId << " streamId = " << streamId << " where number of streams is " << it->second.streams.size() << std::endl;

			exit(EXIT_FAILURE);
		}

		it->second.streams[streamId].difxTsys = tSys;
	}
}

void VexData::setNoDatastream(unsigned int antId, unsigned int streamId)
{
	setDataSource(antId, streamId, DataSourceNone);
}

void VexData::setFiles(unsigned int antId, unsigned int streamId, const std::vector<VexBasebandData> &files)
{
	antennas[antId].removeBasebandData(streamId);
	for(std::vector<VexBasebandData>::const_iterator it = files.begin(); it != files.end(); ++it)
	{
		antennas[antId].files.push_back(VexBasebandData(it->filename, -1, streamId, *it));
	}
	setDataSource(antId, streamId, DataSourceFile);
}

void VexData::setMark6Files(unsigned int antId, unsigned int streamId, const std::vector<VexBasebandData> &files)
{
	// We do not need to remove baseband data for mark6, the loaded vsns are needed for determining media change.
	// When .input files are generated DataSourceMark6 will be detected and will use files data.
	for(std::vector<VexBasebandData>::const_iterator it = files.begin(); it != files.end(); ++it)
	{
		antennas[antId].files.push_back(VexBasebandData(it->filename, -1, streamId, *it));
	}
	setDataSource(antId, streamId, DataSourceMark6);
}

// Module == Mark5
void VexData::setModule(unsigned int antId, unsigned int streamId, const std::string &vsn)
{
	antennas[antId].removeBasebandData(streamId);
	antennas[antId].vsns.push_back(VexBasebandData(vsn, -1, streamId));
	setDataSource(antId, streamId, DataSourceModule);
}

void VexData::setNetworkParameters(unsigned int antId, unsigned int streamId, const std::string &networkPort, int windowSize)
{
	if(antennas[antId].ports.size() <= streamId)
	{
		antennas[antId].ports.resize(streamId+1);
	}
	antennas[antId].ports[streamId].networkPort = networkPort;
	antennas[antId].ports[streamId].windowSize = windowSize;
	setDataSource(antId, streamId, DataSourceNetwork);
}

void VexData::setFake(unsigned int antId, unsigned int streamId)
{
	setDataSource(antId, streamId, DataSourceFake);
}

void VexData::setSampling(const std::string &antName, unsigned int streamId, enum SamplingType dataSampling)
{
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); ++it)
	{
		it->setSampling(antName, streamId, dataSampling);
	}
}

void VexData::setCanonicalVDIF(const std::string &modeName, const std::string &antName)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			int tStart = 0;

			for(std::vector<VexStream>::iterator sit = it->second.streams.begin(); sit != it->second.streams.end(); ++sit)
			{
				if(sit->format == VexStream::FormatVDIF && sit->nThread() != sit->nRecordChan)
				{
					sit->singleThread = false;
					sit->threads.clear();
					for(unsigned int c = 0; c < sit->nRecordChan; ++c)
					{
						sit->threads.push_back(VexThread(c + tStart));
					}
				}
				tStart += sit->nRecordChan;
			}
		}
		else
		{
			std::cerr << "Developer error: setCanonicalVDIF being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: setCanonicalVDIF being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

void VexData::cloneStreams(const std::string &modeName, const std::string &antName, int copies)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			int nrc;

			if(it->second.streams.size() == 1 && it->second.streams[0].nRecordChan % copies == 0)
			{
				nrc = it->second.streams[0].nRecordChan/copies;
			}
			else
			{
				nrc = it->second.streams[0].nRecordChan;
			}
			it->second.streams.resize(copies);
			it->second.streams[0].nRecordChan = nrc;
			if(copies > 1)
			{
				for(int c = 1; c < copies; ++c)
				{
					it->second.streams[c] = it->second.streams[0];
					it->second.streams[c].nRecordChan = nrc;
				}
			}
		}
		else
		{
			std::cerr << "Developer error: cloneStreams being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: cloneStreams being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

void VexData::removeStreamsWithNoDataSource()
{
	std::vector<VexMode>::iterator mit;

	for(mit = modes.begin(); mit != modes.end(); ++mit)
	{
		std::map<std::string,VexSetup>::iterator sit;

		for(sit = mit->setups.begin(); sit != mit->setups.end(); ++sit)
		{
			int n;

			n = 0;
			std::vector<VexStream>::iterator it = sit->second.streams.begin();
			while(it != sit->second.streams.end())
			{
				if(it->dataSource == DataSourceNone)
				{
					++n;
					it = sit->second.streams.erase(it);
				}
				else
				{
					++it;
				}
			}

			if(n > 0)
			{
				std::cout << "Note: removed " << n << " datastreams from antenna " << sit->first << " because of no data source." << std::endl;
			}
		}
	}
}

void VexData::setDataSource(unsigned int antId, unsigned int streamId, enum DataSource dataSource)
{
	std::vector<VexMode>::iterator mit;

	if(antId >= antennas.size())
	{
		std::cerr << "Developer error: VexData::setDataSource: antId = " << antId << " where size of antennas[] is " << antennas.size() << std::endl;
		
		exit(EXIT_FAILURE);
	}

	for(mit = modes.begin(); mit != modes.end(); ++mit)
	{
		std::map<std::string,VexSetup>::iterator it = mit->setups.find(antennas[antId].name);

		if(it == mit->setups.end())
		{
			continue;
		}
		if(streamId >= it->second.streams.size())
		{
			std::cerr << "Developer error: VexData::setDataSource: antId = " << antId << " streamId = " << streamId << " where number of streams is " << it->second.streams.size() << std::endl;

			exit(EXIT_FAILURE);
		}

		it->second.streams[streamId].dataSource = dataSource;
	}
}

enum DataSource VexData::getDataSource(unsigned int antId, unsigned int streamId) const
{
	std::vector<VexMode>::const_iterator mit;

	if(antId >= antennas.size())
	{
		std::cerr << "Developer error: VexData::getDataSource: antId = " << antId << " where size of antennas[] is " << antennas.size() << std::endl;
		
		exit(EXIT_FAILURE);
	}

	for(mit = modes.begin(); mit != modes.end(); ++mit)
	{
		std::map<std::string,VexSetup>::const_iterator it = mit->setups.find(antennas[antId].name);

		if(it == mit->setups.end())
		{
			continue;
		}
		if(streamId >= it->second.streams.size())
		{
			std::cerr << "Developer error: VexData::getDataSource: antId = " << antId << " streamId = " << streamId << " where number of streams is " << it->second.streams.size() << std::endl;

			exit(EXIT_FAILURE);
		}

		if(it->second.streams[streamId].dataSource != DataSourceUnspecified)
		{
			return it->second.streams[streamId].dataSource;
		}

	}

	return DataSourceUnspecified;
}

enum DataSource VexData::getDataSource(const std::string &antName, unsigned int streamId) const
{
	std::vector<VexMode>::const_iterator mit;

	for(mit = modes.begin(); mit != modes.end(); ++mit)
	{
		std::map<std::string,VexSetup>::const_iterator it = mit->setups.find(antName);

		if(it == mit->setups.end())
		{
			continue;
		}
		if(streamId >= it->second.streams.size())
		{
			std::cerr << "Developer error: VexData::getDataSource: antName = " << antName << " streamId = " << streamId << " where number of streams is " << it->second.streams.size() << std::endl;

			exit(EXIT_FAILURE);
		}

		if(it->second.streams[streamId].dataSource != DataSourceUnspecified)
		{
			return it->second.streams[streamId].dataSource;
		}

	}

	std::cerr << "Developer error: VexData::getDataSource: antName = " << antName << " is not contained in the data model" << std::endl;
		
	exit(EXIT_FAILURE);
}

VexStream::DataFormat VexData::getFormat(const std::string &modeName, const std::string &antName, int dsId) const
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		const VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::const_iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			return it->second.streams[dsId].format;
		}
		else
		{
			std::cerr << "Developer error: getFormat being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: getFormat being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

bool VexData::setFormat(const std::string &modeName, const std::string &antName, int dsId, const std::string &formatName)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			return it->second.streams[dsId].parseFormatString(formatName);
		}
		else
		{
			std::cerr << "Developer error: setFormat being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: setFormat being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

void VexData::setStreamBands(const std::string &modeName, const std::string &antName, int dsId, int nBand, int startBand)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			if(nBand > 0)
			{
				it->second.streams[dsId].nRecordChan = nBand;
			}
			// FIXME: handle startBand / bandmap
		}
		else
		{
			std::cerr << "Developer error: setStreamBands being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: setStreamBands being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

void VexData::setStreamFrameSize(const std::string &modeName, const std::string &antName, int dsId, int frameSize)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			if(frameSize > 0)
			{
				it->second.streams[dsId].VDIFFrameSize = frameSize;
			}
			// FIXME: handle startBand / bandmap
		}
		else
		{
			std::cerr << "Developer error: setStreamFrameSize being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: setStreamFrameSize being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

void VexData::setStreamThreadsAbsent(const std::string &modeName, const std::string &antName, int dsId, const std::set<int> &threadsAbsent)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			it->second.streams[dsId].threadsAbsent = threadsAbsent;
		}
		else
		{
			std::cerr << "Developer error: setStreamThreadsAbsent being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: setStreamThreadsAbsent being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

void VexData::setStreamThreadsIgnore(const std::string &modeName, const std::string &antName, int dsId, const std::set<int> &threadsIgnore)
{
	int modeId = getModeIdByDefName(modeName);

	if(modeId >= 0)
	{
		VexMode &M = modes[modeId];
		std::map<std::string,VexSetup>::iterator it = M.setups.find(antName);
		if(it != M.setups.end())
		{
			it->second.streams[dsId].threadsIgnore = threadsIgnore;
		}
		else
		{
			std::cerr << "Developer error: setStreamThreadsIgnore being called with antName = " << antName << " which is not found in mode " << modeName << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Developer error: setStreamThreadsIgnore being called with modeName = " << modeName << " which returned modeId " << modeId << std::endl;

		exit(EXIT_FAILURE);
	}
}

double VexData::getEarliestScanStart() const
{
	double start = 1000000.0;

	for(std::vector<VexScan>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		if(it->mjdStart < start)
		{
			start = it->mjdStart;
		}
	}

	if(start > 999999)
	{
		start = -1000000.0;
	}

	return start;
}

double VexData::getLatestScanStop() const
{
	double stop = -1000000.0;

	for(std::vector<VexScan>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		if(it->mjdStop > stop)
		{
			stop = it->mjdStop;
		}
	}

	if(stop < -999999)
	{
		stop = 1000000.0;
	}

	return stop;
}

bool VexData::hasData(const std::string &antName, const VexScan &scan) const
{
	bool hd = false;
	const VexAntenna *A = getAntenna(antName);
	if(!A)
	{
		std::cerr << "Developer error: VexData::hasData: cannot find antenna " << antName << std::endl;

		exit(EXIT_FAILURE);
	}
	const VexMode *M = getModeByDefName(scan.modeDefName);
	if(!M)
	{
		std::cerr << "Developer error: VexData::hasData:: cannot find mode " << scan.modeDefName << std::endl;

		exit(EXIT_FAILURE);
	}
	std::map<std::string,VexSetup>::const_iterator s = M->setups.find(antName);
	if(s != M->setups.end())
	{
		for(unsigned int d = 0; d < s->second.streams.size(); ++d)
		{
			switch(s->second.streams[d].dataSource)
			{
			case DataSourceFile:
			case DataSourceMark6:
				for(std::vector<VexBasebandData>::const_iterator it = A->files.begin(); it != A->files.end(); ++it)
				{
					if(it->overlap(scan) > 0.0)
					{
						hd = true;
						break;
					}
				}
				break;
			case DataSourceModule:
				for(std::vector<VexBasebandData>::const_iterator it = A->vsns.begin(); it != A->vsns.end(); ++it)
				{
					if(it->overlap(scan) > 0.0)
					{
						hd = true;
						break;
					}
				}
				break;
			case DataSourceNetwork:
			case DataSourceFake:
				hd = true;
				break;
			default:
				break;
			}

			if(hd)
			{
				break;
			}
		}
	}

	return hd;
}

int VexData::getPolarizations() const
{
	int rv = 0;

	for(std::vector<VexMode>::const_iterator it = modes.begin(); it != modes.end(); ++it)
	{
		rv |= it->getPolarizations();
	}

	return rv;
}

int VexData::getConvertedPolarizations() const
{
	int rv = 0;
	std::list<std::string> antsToConvert;

	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->polConvert)
		{
			antsToConvert.push_back(it->name);
		}
	}

	for(std::vector<VexMode>::const_iterator it = modes.begin(); it != modes.end(); ++it)
	{
		rv |= it->getConvertedPolarizations(antsToConvert);
	}

	return rv;
}

bool VexData::isSX() const
{
	bool hasSX = false;
	bool hasOther = false;

	for(unsigned int modeNum = 0; modeNum < nMode(); ++modeNum)
	{
		const VexMode *mode = getMode(modeNum);

		for(std::map<std::string,VexSetup>::const_iterator ssi = mode->setups.begin(); ssi != mode->setups.end(); ++ssi)
		{
			const VexSetup &setup = ssi->second;
			bool hasS, hasX, hasElse;

			hasS = hasX = hasElse = false;

			for(std::map<std::string,VexIF>::const_iterator it = setup.ifs.begin(); it != setup.ifs.end(); ++it)
			{
				const VexIF &vif = it->second;
				double averageTune;	// (Hz)
				
				averageTune = setup.averageTuningForIF(vif.ifLink);

				if(averageTune > 2.0e9 && averageTune < 3.9e9)
				{
					hasS = true;
				}
				else if(averageTune > 7.9e9 && averageTune < 11.9e9)
				{
					hasX = true;
				}
				else
				{
					hasElse = true;
				}
			}

			if(hasX && hasS && !hasElse)
			{
				hasSX = true;
			}
			else
			{
				hasOther = true;
			}
		}
	}

	return hasSX & (!hasOther);
}

void VexData::setVersion(const std::string &ver)
{
	version = atof(ver.c_str());
}

void VexData::setVersion(double ver)
{
	version = ver;
}

double VexData::getVersion() const
{
	return version;
}


std::ostream& operator << (std::ostream &os, const VexData &x)
{
	size_t n;

	os << "Vex " << x.getVersion() << ":" << std::endl;
	os << *x.getExper() << std::endl;

	n = x.nSource();
	os << n << " sources:" << std::endl;
	for(size_t i = 0; i < n; ++i)
	{
		os << *x.getSource(i);
	}

	n = x.nScan();
	os << n << " scans:" << std::endl;
	for(size_t i = 0; i < n; ++i)
	{
		os << *x.getScan(i);
	}

	n = x.nAntenna();
	os << n << " antennas:" << std::endl;
	for(size_t i = 0; i < n; ++i)
	{
		os << *x.getAntenna(i);
	}

	n = x.nMode();
	os << n << " modes:" << std::endl;
	for(size_t i = 0; i < n; ++i)
	{
		os << *x.getMode(i);
	}

	n = x.nEOP();
	os << n << " eops:" << std::endl;
	for(size_t i = 0; i < n; ++i)
	{
		os << "  " << *x.getEOP(i) << std::endl;
	}

	n = x.nExtension();
	os << n << " extensions:" << std::endl;
	for(size_t i = 0; i < n; ++i)
	{
		os << "  " << *x.getExtension(i) << std::endl;
	}

	return os;
}
