/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken & Adam Deller               *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstring>
#include <fstream>
#include <vexdatamodel.h>
#include <algorithm>
#include "job.h"
#include "jobflag.h"

void Job::assignAntennas(const VexData &V, std::list<std::pair<int,std::string> > &removedAntennas, bool sortAntennas)
{
	jobAntennas.clear();

	for(std::vector<std::string>::const_iterator s = scans.begin(); s != scans.end(); ++s)
	{
		const VexScan* S = V.getScanByDefName(*s);
		for(std::map<std::string,Interval>::const_iterator a = S->stations.begin(); a != S->stations.end(); ++a)
		{
			if(find(jobAntennas.begin(), jobAntennas.end(), a->first) == jobAntennas.end())
			{
				if(V.hasData(a->first, *S))	// a->first is antenna name
				{
					jobAntennas.push_back(a->first);
				}
				else
				{
					removedAntennas.push_back(std::pair<int,std::string>(jobId, a->first));
				}
			}
		}
	}
	if(sortAntennas)
	{
		sort(jobAntennas.begin(), jobAntennas.end());
	}
}

double Job::calcOps(const VexData *V, int fftSize, bool doPolar) const
{
	double ops = 0.0;
	int nAnt, nPol, nSubband;
	double sampRate;	/* [samples per second] */
	double seconds;
	const VexMode *M;
	char pols[8];
	double opsPerSample;

	for(std::vector<std::string>::const_iterator si = scans.begin(); si != scans.end(); ++si)
	{
		const VexScan *S = V->getScanByDefName(*si);
		M = V->getModeByDefName(S->modeDefName);
		if(!M)
		{
			return 0.0;
		}
		
		sampRate = M->getAverageSampleRate();
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
		// Note: this assumes all polarizations are matched
		opsPerSample = 16.0 + 5.0*intlog2(fftSize) + 2.5*nAnt*nPol;
		ops += opsPerSample*seconds*sampRate*nSubband*nAnt;
	}

	return ops;
}

double Job::calcSize(const VexData *V) const
{
	double size = 0.0;

	for(std::vector<std::string>::const_iterator it = scans.begin(); it != scans.end(); ++it)
	{
		size += V->getScanByDefName(*it)->size;
	}

	return size;
}

bool Job::hasScan(const std::string &scanName) const
{
	// if find returns .end(), then it was not found
	return find(scans.begin(), scans.end(), scanName) != scans.end();
}

struct iless
{
	bool operator()(const std::string& a, const std::string& b) const 
	{
		return strcasecmp(a.c_str(), b.c_str()) < 0;
	}
};

int Job::generateFlagFile(const VexData &V, const std::list<Event> events, const char *fileName, unsigned int invalidMask) const
{
	std::vector<JobFlag> flags;
	std::map<std::string,unsigned int,iless> antIds;
	unsigned int nAnt = 0;
	std::ofstream of;

	for(std::vector<std::string>::const_iterator a = jobAntennas.begin(); a != jobAntennas.end(); ++a)
	{
		antIds[*a] = nAnt;
		++nAnt;
	}

	// Assume all flags from the start.  
	std::vector<unsigned int> flagMask(nAnt, 
		JobFlag::JOB_FLAG_RECORD | 
		JobFlag::JOB_FLAG_POINT | 
		JobFlag::JOB_FLAG_TIME | 
		JobFlag::JOB_FLAG_SCAN);
	std::vector<double> flagStart(nAnt, mjdStart);

	// Except if not a Mark5 Module case, don't assume RECORD flag is on
	for(std::vector<std::string>::const_iterator a = jobAntennas.begin(); a != jobAntennas.end(); ++a)
	{
		if(V.getDataSource(*a, 0) != DataSourceModule) // FIXME: This line assumes all datastreams behave the same.  solution: need datastream-based flags
		{
			// Aha! not module based so unflag JOB_FLAG_RECORD
			flagMask[antIds[*a]] &= ~JobFlag::JOB_FLAG_RECORD;
		}
	}

	// Then go through each event, adjusting current flag state.  
	for(std::list<Event>::const_iterator e = events.begin(); e != events.end(); ++e)
	{
		if(e->eventType == Event::RECORD_START)
		{
			if(antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] &= ~JobFlag::JOB_FLAG_RECORD;
			}
		}
		else if(e->eventType == Event::RECORD_STOP)
		{
			if(antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] |= JobFlag::JOB_FLAG_RECORD;
			}
		}
		else if(e->eventType == Event::SCAN_START)
		{
			if(hasScan(e->scan))
			{
				const VexScan *scan = V.getScanByDefName(e->scan);

				if(!scan)
				{
					std::cerr << "Developer error: generateFlagFile: SCAN_START, scan=0" << std::endl;

					exit(EXIT_FAILURE);
				}
				for(std::map<std::string,Interval>::const_iterator sa = scan->stations.begin(); sa != scan->stations.end(); ++sa)
				{
					if(antIds.count(sa->first) == 0)
					{
						continue;
					}
					flagMask[antIds[sa->first]] &= ~JobFlag::JOB_FLAG_SCAN;
				}
			}
		}
		else if(e->eventType == Event::SCAN_STOP)
		{
			if(hasScan(e->scan))
			{
				const VexScan *scan = V.getScanByDefName(e->scan);

				if(!scan)
				{
					std::cerr << "Developer error! generateFlagFile: SCAN_STOP, scan=0" << std::endl;

					exit(EXIT_FAILURE);
				}
				for(std::map<std::string,Interval>::const_iterator sa = scan->stations.begin(); sa != scan->stations.end(); ++sa)
				{
					if(antIds.count(sa->first) == 0)
					{
						continue;
					}
					flagMask[antIds[sa->first]] |= JobFlag::JOB_FLAG_SCAN;
				}
			}
		}
		else if(e->eventType == Event::ANT_SCAN_START)
		{
			if(hasScan(e->scan) && antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] &= ~JobFlag::JOB_FLAG_POINT;
			}
		}
		else if(e->eventType == Event::ANT_SCAN_STOP)
		{
			if(hasScan(e->scan) && antIds.count(e->name) > 0)
			{
				flagMask[antIds[e->name]] |= JobFlag::JOB_FLAG_POINT;
			}
		}
		else if(e->eventType == Event::JOB_START)
		{
			if(fabs(e->mjd - mjdStart) < 0.5/86400.0)
			{
				for(unsigned int antId = 0; antId < nAnt; ++antId)
				{
					flagMask[antId] &= ~JobFlag::JOB_FLAG_TIME;
				}
			}
		}
		else if(e->eventType == Event::JOB_STOP)
		{
			if(fabs(e->mjd - mjdStart) < 0.5/86400.0)
			{
				for(unsigned int antId = 0; antId < nAnt; ++antId)
				{
					flagMask[antId] |= JobFlag::JOB_FLAG_TIME;
				}
			}
		}

		for(unsigned int antId = 0; antId < nAnt; ++antId)
		{
			if( (flagMask[antId] & invalidMask) == 0)
			{
				if(flagStart[antId] > 0)
				{
					if(e->mjd - flagStart[antId] > 0.5/86400.0)
					{
						JobFlag f(flagStart[antId], e->mjd, antId);
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
	for(unsigned int antId = 0; antId < nAnt; ++antId)
	{
		if( (flagMask[antId] & invalidMask) != 0)
		{
			if(mjdStop - flagStart[antId] > 0.5/86400.0)
			{
				JobFlag f(flagStart[antId], mjdStop, antId);
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
	of << flags.size() << std::endl;
	for(std::vector<JobFlag>::const_iterator it = flags.begin(); it != flags.end(); ++it)
	{
		of << "  " << *it << std::endl;
	}
	of.close();

	return flags.size();
}

std::ostream& operator << (std::ostream &os, const Job &x)
{
	int p = os.precision();
	
	os.precision(12);
	os << "Job " << x.jobSeries << "_" << x.jobId << std::endl;
	os << "  " << (const Interval&)x << std::endl;
	os << "  duty cycle = " << x.dutyCycle << std::endl;
	os << "  scans =";
	for(std::vector<std::string>::const_iterator s = x.scans.begin(); s != x.scans.end(); ++s)
	{
		os << " " << *s;
	}
	os << std::endl;
	os << "  Antenna list:";
	for(std::vector<std::string>::const_iterator a = x.jobAntennas.begin(); a != x.jobAntennas.end(); ++a)
	{
		os << " " << *a;
	}
	os << std::endl;
	os << "  size = " << x.dataSize << " bytes" << std::endl;

	os.precision(p);

	return os;
}
