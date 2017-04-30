/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdlib>
#include <sstream>
#include <map>
#include <algorithm>
#include "jobgroup.h"
#include "makejobs.h"
#include "mediachange.h"

// A is assumed to be the first scan in time order
static bool areScansCompatible(const VexScan *A, const VexScan *B, const CorrParams *P)
{
	if(((B->mjdStart < A->mjdStop) && (fabs(B->mjdStart-A->mjdStop) > 1.0e-8)) ||
	   (B->mjdStart > A->mjdStop + P->maxGap))
	{
		return false;
	}
	if(A->overlap(*B) >= -0.1/86400.0)
	{
		return false;
	}
	if(P->singleScan)
	{
		return false;
	}
	if(P->singleSetup && A->modeDefName != B->modeDefName)
	{
		return false;
	}
	
	return true;
}


// Divides scans into different groups where each group contains scans that can be correlated at the same time.
// This does not pay attention to media or clock breaks
static void genJobGroups(std::vector<JobGroup> &JGs, const VexData *V, const CorrParams *P, const std::list<Event> &events, int verbose)
{
	unsigned int nNoRecordScan = 0;
	std::list<std::string> scans;
	V->getScanList(scans);

	while(!scans.empty())
	{
		const VexScan *scan = V->getScanByDefName(scans.front());
		unsigned int nRecordedAnt = V->nAntennasWithRecordedData(*scan);

		if(nRecordedAnt < P->minSubarraySize)
		{
			if(verbose > 2)
			{
				std::cout << "Skipping scan " << scans.front() << " because it has no recorded data." << std::endl;
			}
			
			scans.pop_front();
			++nNoRecordScan;

			continue;
		}
		JGs.push_back(JobGroup());
		JobGroup &JG = JGs.back();
		JG.scans.push_back(scans.front());
		JG.setTimeRange(*scan);
		scans.pop_front();

		const VexScan *scan1 = V->getScanByDefName(JG.scans.back());
		const std::string &corrSetupName1 = P->findSetup(scan1->defName, scan1->sourceDefName, scan1->modeDefName);
		const CorrSetup *corrSetup1 = P->getCorrSetup(corrSetupName1);

		for(std::list<std::string>::iterator it = scans.begin(); it != scans.end();)
		{
			const VexScan *scan2 = V->getScanByDefName(*it);
			const std::string &corrSetupName2 = P->findSetup(scan2->defName, scan2->sourceDefName, scan2->modeDefName);
			const CorrSetup *corrSetup2 = P->getCorrSetup(corrSetupName2);

			// Skip any scans that don't overlap with .v2d mjdStart and mjdStop
			if(P->overlap(*scan2) <= 0.0)
			{
				++it;
				continue;
			}

#warning "FIXME: verify modes are compatible"
			if(areCorrSetupsCompatible(corrSetup1, corrSetup2, P) &&
			   areScansCompatible(scan1, scan2, P))
			{
				JG.logicalOr(*scan2);	// expand jobGroup time to include this scan
				JG.scans.push_back(*it);
				it = scans.erase(it);
				scan1 = scan2;
				corrSetup1 = corrSetup2;
			}
			else
			{	
				++it;
			}
		}
	}

	if(verbose && nNoRecordScan > 0)
	{
		std::cout << nNoRecordScan << " scans dropped because they recorded no baseband data." << std::endl;
	}

	for(std::vector<JobGroup>::iterator jg = JGs.begin(); jg != JGs.end(); ++jg)
	{
		jg->genEvents(events);
		jg->logicalAnd(*P);		// possibly shrink job group to requested range
	}
}

static void genJobs(std::vector<Job> &Js, const JobGroup &JG, const VexData *V, const CorrParams *P, int verbose)
{
	std::map<std::string,double> recordStop;
	std::map<double,int> usage;
	std::map<double,int> clockBreaks;
	int nClockBreaks = 0;
	std::list<MediaChange> changes;
	std::list<double> times;
	std::list<double> breaks;
	double mjdLast = -1.0;
	double mjdBest = 0.0;
	double start;
	int nAnt;
	int nLoop = 0;
	Interval scanRange;

	// first initialize recordStop and usage
	for(std::list<Event>::const_iterator e = JG.events.begin(); e != JG.events.end(); ++e)
	{
		if(e->eventType == Event::RECORD_START)
		{
			recordStop[e->name] = -1.0;
		}
		if(e->eventType == Event::SCAN_START && (scanRange.mjdStart < 1.0 || e->mjd < scanRange.mjdStart))
		{
			scanRange.mjdStart = e->mjd;
		}
		if(e->eventType == Event::SCAN_STOP && (scanRange.mjdStop < 1.0 || e->mjd > scanRange.mjdStop))
		{
			scanRange.mjdStop = e->mjd;
		}

		usage[e->mjd] = 0;
		clockBreaks[e->mjd] = 0;
	}
	nAnt = recordStop.size();

	scanRange.logicalAnd(*P);	// Shrink time range to v2d start / stop interval


	// populate changes, times, and usage
	for(std::list<Event>::const_iterator e = JG.events.begin(); e != JG.events.end(); ++e)
	{
		if(mjdLast > 0.0 && e->mjd > mjdLast)
		{
			usage[e->mjd] = usage[mjdLast];
			mjdLast = e->mjd;
			if(JG.containsAbsolutely(e->mjd))
			{
				times.push_back(e->mjd);
			}
		}
		else if(mjdLast < 0.0)
		{
			usage[e->mjd] = 0;
			mjdLast = e->mjd;
			if(JG.containsAbsolutely(e->mjd))
			{
				times.push_back(e->mjd);
			}
		}

		if(e->eventType == Event::RECORD_START)
		{
			if(recordStop[e->name] > 0.0)
			{
				if(JG.containsAbsolutely(recordStop[e->name]) &&
				   JG.containsAbsolutely(e->mjd) &&
				   scanRange.containsAbsolutely(e->mjd))
				{
					changes.push_back(MediaChange(e->name, recordStop[e->name], e->mjd));
					if(verbose > 0)
					{
						std::cout << "Media change: " << e->name << " " << (Interval)(changes.back()) << std::endl;
					}
				}
			}
		}
		else if(e->eventType == Event::RECORD_STOP)
		{
			recordStop[e->name] = e->mjd;
		}
		else if(e->eventType == Event::ANT_SCAN_START)
		{
			++usage[e->mjd];
		}
		else if(e->eventType == Event::ANT_SCAN_STOP)
		{
			--usage[e->mjd];
		}
		else if(e->eventType == Event::CLOCK_BREAK ||
			e->eventType == Event::LEAP_SECOND ||
			e->eventType == Event::ANTENNA_START ||
			e->eventType == Event::ANTENNA_STOP ||
			e->eventType == Event::MANUAL_BREAK)
		{
			if(JG.containsAbsolutely(e->mjd))
			{
				++clockBreaks[e->mjd];
				++nClockBreaks;
			}
		}
	}

	// now go through and set breakpoints
	while(!changes.empty() || nClockBreaks > 0)
	{
		int scoreBest;
		int nEvent = JG.events.size();

		++nLoop;
		if(nLoop > nEvent+3) // There is clearly a problem converging!
		{
			std::cerr << "Developer error: jobs not converging after " << nLoop << " tries.\n" << std::endl;

			std::cerr << "Events:" << std::endl;
			std::list<Event>::const_iterator iter;
			for(iter = JG.events.begin(); iter != JG.events.end(); ++iter)
			{
				std::cerr << "   " << *iter << std::endl;
			}

			std::cerr << "nClockBreaks = " << nClockBreaks << std::endl;

			std::cerr << "Media Changes remaining were:" << std::endl;
			std::list<MediaChange>::const_iterator it;
			for(it = changes.begin(); it != changes.end(); ++it)
			{
				std::cerr << "   " << *it << std::endl;
			}

			exit(EXIT_FAILURE);
		}

		// look for break with highest score
		// Try as hard as possible to minimize number of breaks
		scoreBest = -1;
		for(std::list<double>::const_iterator t = times.begin(); t != times.end(); ++t)
		{
			int score = nGap(changes, *t) * (nAnt-usage[*t]+1) + 100*clockBreaks[*t];
			if(score > scoreBest)
			{
				scoreBest = score;
				mjdBest = *t;
			}
		}

		breaks.push_back(mjdBest);
		nClockBreaks -= clockBreaks[mjdBest];
		clockBreaks[mjdBest] = 0;

		// find modules that change in the new gap
		for(std::list<MediaChange>::iterator c = changes.begin(); c != changes.end();)
		{
			if(c->mjdStart <= mjdBest && c->mjdStop >= mjdBest)
			{
				c = changes.erase(c);
			}
			else
			{
				++c;
			}
		}
	}
	breaks.sort();

	// Add a break at end so num breaks = num jobs
	breaks.push_back(JG.mjdStop);

	// form jobs
	start = JG.mjdStart;
	for(std::list<double>::const_iterator t = breaks.begin(); t != breaks.end(); ++t)
	{
		Interval jobTimeRange(start, *t);
		if(jobTimeRange.duration() > P->minLength)
		{
			JG.createJobs(Js, jobTimeRange, V, P->maxLength, P->maxSize);
		}
		else
		{
			std::cerr << "Warning: skipping short job of " << (jobTimeRange.duration()*86400.0) << " seconds duration." << std::endl;
		}
		start = *t;
	}
}

void makeJobs(std::vector<Job>& J, const VexData *V, const CorrParams *P, std::list<Event> &events, std::list<std::pair<int,std::string> > &removedAntennas, int verbose)
{
	std::vector<JobGroup> JG;

	// Do splitting of jobs
	genJobGroups(JG, V, P, events, verbose);

	if(verbose > 0)
	{
		std::cout << JG.size() << " job groups created:" << std::endl;
		for(std::vector<JobGroup>::const_iterator jg = JG.begin(); jg != JG.end(); ++jg)
		{
			std::cout << "  " << *jg;
		}
	}

	for(std::vector<JobGroup>::const_iterator jg = JG.begin(); jg != JG.end(); ++jg)
	{
		genJobs(J, *jg, V, P, verbose);
	}

	// Finalize all the new job structures
	int jobId = P->startSeries;
	for(std::vector<Job>::iterator j = J.begin(); j != J.end(); ++j)
	{
		std::ostringstream name;
		j->jobSeries = P->jobSeries;
		j->jobId = jobId;

		// note: this is an internal name only, not the job prefix that 
		// becomes part of the filenames
		name << j->jobSeries << "_" << j->jobId;

		addEvent(events, j->mjdStart, Event::JOB_START, name.str());
		addEvent(events, j->mjdStop,  Event::JOB_STOP,  name.str());

		// finds antennas that are active during at least a subset of the jobs scans and have media
		j->assignAntennas(*V, removedAntennas, P->sortAntennas);

		if(!P->sortAntennas)
		{
			if(P->antennaList.empty())
			{
				std::cerr << "Note: antennas will be sorted alphabetically even though sortAntennas=False was provided.  To prevent this, an explicit antenna list needs to be provided." << std::endl;
			}
			else
			{
				std::vector<std::string> orig;

				orig = j->jobAntennas;
				j->jobAntennas.clear();

				for(std::list<std::string>::const_iterator it = P->antennaList.begin(); it != P->antennaList.end(); ++it)
				{
					if(std::find(orig.begin(), orig.end(), *it) != orig.end())
					{
						j->jobAntennas.push_back(*it);
					}
				}
			}
		}
		
		// If fewer than minSubarray antennas remain, then mark the job as bad and exclude writing it later.
		if(j->jobAntennas.size() < P->minSubarraySize)
		{
			j->jobSeries = "-";	// Flag to not actually produce this job
		}
		++jobId;
	}
	events.sort();
}
