#ifndef __JOBGROUP_H__
#define __JOBGROUP_H__

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include "job.h"
#include "event.h"
#include "vex_data.h"

class JobGroup : public Interval
{
public:
	std::vector<std::string> scans;
	std::list<Event> events;

	bool hasScan(const std::string &scanName) const;
	void genEvents(const std::list<Event> &eventList);
	void createJobs(std::vector<Job> &jobs, Interval &jobTimeRange, const VexData *V, double maxLength, double maxSize) const;
};

std::ostream& operator << (std::ostream &os, const JobGroup &x);

#endif
