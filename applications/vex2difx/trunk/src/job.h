#ifndef __JOB_H__
#define __JOB_H__

#include <iostream>
#include <vector>
#include <string>
#include "interval.h"
#include "vex_data.h"
#include "event.h"

class Job : public Interval
{
public:
	Job() : Interval(0.0, 1000000.0), jobSeries("Bogus"), jobId(-1), dutyCycle(1.0), dataSize(0.0) {}

	void assignAntennas(const VexData &V, std::list<std::pair<int,std::string> > &removedAntennas);
	bool hasScan(const std::string &scanName) const;
	int generateFlagFile(const VexData &V, const std::list<Event> events, const char *fileName, unsigned int invalidMask=0xFFFFFFFF) const;

	// return the approximate number of Operations required to compute this scan
	double calcOps(const VexData *V, int fftSize, bool doPolar) const;
	double calcSize(const VexData *V) const;

	std::string jobSeries;
	int jobId;
	std::string modeName;
	std::vector<std::string> scans;
	std::vector<std::string> jobAntennas;	// vector of antennas used in this job
	double dutyCycle;		// fraction of job spent in scans
	double dataSize;		// [bytes] estimate of data output size
};

std::ostream& operator << (std::ostream &os, const Job &x);

#endif
