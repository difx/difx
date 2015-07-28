#include <cstdlib>
#include <algorithm>
#include "jobgroup.h"

bool JobGroup::hasScan(const std::string &scanName) const
{
	return find(scans.begin(), scans.end(), scanName) != scans.end();
}

void JobGroup::genEvents(const std::list<Event> &eventList)
{
	for(std::list<Event>::const_iterator it = eventList.begin(); it != eventList.end(); ++it)
	{
		if(it->eventType == Event::SCAN_START ||
		   it->eventType == Event::SCAN_STOP ||
		   it->eventType == Event::ANT_SCAN_START ||
		   it->eventType == Event::ANT_SCAN_STOP)
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
}

void JobGroup::createJobs(std::vector<Job> &jobs, Interval &jobTimeRange, const VexData *V, double maxLength, double maxSize) const
{
	std::list<Event>::const_iterator s, e;
	jobs.push_back(Job());
	Job *J = &jobs.back();
	double totalTime, scanTime = 0.0;
	double size = 0.0;
	std::string id("");

	// note these are backwards now; will set these to minimum range covering scans
	J->setTimeRange(jobTimeRange.mjdStop, jobTimeRange.mjdStart);

	for(e = events.begin(); e != events.end(); ++e)
	{
		if(e->eventType == Event::SCAN_START)
		{
			s = e;
			id = e->name;
		}
		if(e->eventType == Event::SCAN_STOP)
		{
			if(id != e->name)
			{
				std::cerr << "Developer error: createJobs: id != e->name  (" << id << " != " << e->name << ")" << std::endl;

				exit(EXIT_FAILURE);
			}
			Interval scanTimeRange(s->mjd, e->mjd);
			scanTimeRange.logicalAnd(jobTimeRange);
			if(scanTimeRange.duration() > 0.0)
			{
				const VexScan *scan = V->getScanByDefName(id);
				if(!scan)
				{
					std::cerr << "Developer error: createJobs: getScanByDefName() returned numm for id = " << id << std::endl;
					
					exit(EXIT_FAILURE);
				}
				J->modeName = scan->modeDefName;
				J->scans.push_back(e->name);
				J->logicalOr(scanTimeRange);
				scanTime += scanTimeRange.duration();

				// Work in progress: calculate correlated size of scan
				size += scan->size;

				/* start a new job at scan boundary if maxLength exceeded */
				if(J->duration() > maxLength || size > maxSize)
				{
					totalTime = J->duration();
					J->dutyCycle = scanTime / totalTime;
					J->dataSize = size;
					jobs.push_back(Job());
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

std::ostream& operator << (std::ostream &os, const JobGroup &x)
{
	int p = os.precision();
	
	os.precision(12);
	os << "Group: scans " << x.scans.front() << " - " << x.scans.back() << " = " << (const Interval &)x << std::endl;
	os.precision(p);
	
	return os;
}
