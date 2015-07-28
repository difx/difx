#ifndef __JOBFLAG_H__
#define __JOBFLAG_H__

#include <iostream>
#include "interval.h"

class JobFlag : public Interval
{
public:
	static const unsigned int JOB_FLAG_RECORD = 1 << 0;
	static const unsigned int JOB_FLAG_POINT  = 1 << 1;
	static const unsigned int JOB_FLAG_TIME   = 1 << 2;
	static const unsigned int JOB_FLAG_SCAN   = 1 << 3;
	JobFlag() : antId(-1) {}
	JobFlag(double start, double stop, int ant) : Interval(start, stop), antId(ant) {}

	int antId;
};

std::ostream& operator << (std::ostream &os, const JobFlag &x);

#endif
