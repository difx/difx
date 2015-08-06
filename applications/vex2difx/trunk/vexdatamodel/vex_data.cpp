/***************************************************************************
 *   Copyright (C) 2009-2015 by Walter Brisken & Adam Deller               *
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
#include <regex.h>
#include "vex_data.h"
#include "util.h"


int VexData::sanityCheck()
{
	int nWarn = 0;

	if(eops.size() < 5)
	{
		std::cerr << "Warning: Fewer than 5 EOPs specified" << std::endl;
		++nWarn;
	}

	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->clocks.empty())
		{
			std::cerr << "Warning: no clock values for antenna " << it->name << " ." << std::endl;
			++nWarn;
		}
	}

	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
#if 0
FIXME: this functionality needs to be put somewhere else
		if(it->dataSource == DataSourceFile && it->basebandFiles.empty())
		{
			std::cerr << "Warning: file based correlation desired but no files provided for antenna " << it->name << " ." << std::endl;
			++nWarn;
		}
		if(it->dataSource == DataSourceModule && it->basebandFiles.empty())
		{
			std::cerr << "Warning: module based correlation desired but no media specified for antenna " << it->name << " ." << std::endl;
			++nWarn;
		}
		if(it->dataSource == DataSourceNone)
		{
			std::cerr << "Warning: data source is NONE for antenna " << it->name << " ." << std::endl;
			++nWarn;
		}
#endif
	}

	return nWarn;
}

VexSource *VexData::newSource()
{
	sources.push_back(VexSource());

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

		if(!antennaTimeRange.isCausal() || it->overlap(antennaTimeRange) <= 0.5/86400.0)
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
		const VexAntenna *A = getAntenna(it->first);

if(A == 0)
{
std::cerr << "VexData::nAntennasWithRecordedData : getAntenna( " << it->first << " ) returned null" << std::endl;
continue;
}

		if(A->hasData(scan) && scan.getRecordEnable(it->first))
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

void VexData::addVSN(const std::string &antName, unsigned int datastreamId, const std::string &vsn, const Interval &timeRange)
{
	for(std::vector<VexAntenna>::iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->name == antName)
		{
			it->vsns.push_back(VexBasebandData(vsn, datastreamId, timeRange));
			it->dataSource = DataSourceModule;
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

void VexData::setPhaseCalInterval(const std::string &antName, int phaseCalIntervalMHz)
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
	for(std::vector<VexAntenna>::const_iterator it = antennas.begin(); it != antennas.end(); ++it)
	{
		if(it->dataSource == DataSourceModule)
		{
			for(std::vector<VexBasebandData>::const_iterator vit = it->vsns.begin(); vit != it->vsns.end(); ++vit)
			{
				addEvent(events, vit->mjdStart, Event::RECORD_START, it->defName);
				addEvent(events, vit->mjdStop,  Event::RECORD_STOP,  it->defName);
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

void VexData::setFiles(unsigned int antId, unsigned int streamId, const std::vector<VexBasebandData> &files)
{
	antennas[antId].removeBasebandData(streamId);
	for(std::vector<VexBasebandData>::const_iterator it = files.begin(); it != files.end(); ++it)
	{
		antennas[antId].files.push_back(VexBasebandData(it->filename, streamId, *it));
	}
	antennas[antId].dataSource = DataSourceFile;
}

void VexData::setModule(unsigned int antId, unsigned int streamId, const std::string &vsn)
{
	antennas[antId].removeBasebandData(streamId);
	antennas[antId].vsns.push_back(VexBasebandData(vsn, streamId));
	antennas[antId].dataSource = DataSourceModule;
}

void VexData::setNetworkParameters(unsigned int antId, unsigned int streamId, const std::string &networkPort, int windowSize)
{
	if(antennas[antId].ports.size() <= streamId)
	{
		antennas[antId].ports.resize(streamId+1);
	}
	antennas[antId].ports[streamId].networkPort = networkPort;
	antennas[antId].ports[streamId].windowSize = windowSize;
	antennas[antId].dataSource = DataSourceNetwork;
}

void VexData::setFake(unsigned int antId)
{
	antennas[antId].dataSource = DataSourceFake;
}

void VexData::setSampling(const std::string &antName, unsigned int streamId, enum SamplingType dataSampling)
{
	for(std::vector<VexMode>::iterator it = modes.begin(); it != modes.end(); ++it)
	{
		setSampling(antName, streamId, dataSampling);
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
			for(std::vector<VexStream>::iterator sit = it->second.streams.begin(); sit != it->second.streams.end(); ++sit)
			{
				if(sit->format == VexStream::FormatVDIF && sit->nThread == 0)
				{
					sit->nBit = 2;
					sit->nThread = sit->nRecordChan;
					sit->threads.clear();
					for(unsigned int c = 0; c < sit->nRecordChan; ++c)
					{
						sit->threads.push_back(c);
					}
				}
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

std::ostream& operator << (std::ostream &os, const VexData &x)
{
	int n = x.nSource();

	os << "Vex:" << std::endl;
	os << n << " sources:" << std::endl;
	for(int i = 0; i < n; ++i)
	{
		os << *x.getSource(i);
	}

	n = x.nScan();
	os << n << " scans:" << std::endl;
	for(int i = 0; i < n; ++i)
	{
		os << *x.getScan(i);
	}

	n = x.nAntenna();
	os << n << " antennas:" << std::endl;
	for(int i = 0; i < n; ++i)
	{
		os << *x.getAntenna(i);
	}

	n = x.nMode();
	os << n << " modes:" << std::endl;
	for(int i = 0; i < n; ++i)
	{
		os << *x.getMode(i);
	}

	n = x.nEOP();
	os << n << " eops:" << std::endl;
	for(int i = 0; i < n; ++i)
	{
		os << "   " << *x.getEOP(i) << std::endl;
	}

	return os;
}
