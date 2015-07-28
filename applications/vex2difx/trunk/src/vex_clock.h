
#ifndef __VEX_CLOCK_H__
#define __VEX_CLOCK_H__

#include <iostream>

class VexClock
{
public:
	VexClock() : mjdStart(0.0), offset(0.0), rate(0.0), offset_epoch(50000.0) {}
	void flipSign() 
	{ 
		offset = -offset;
		rate = -rate;
	}

	double mjdStart;	// [mjd]
	double offset;		// [sec]
	double rate;		// [sec/sec]
	double offset_epoch;	// [mjd]
};

std::ostream& operator << (std::ostream &os, const VexClock &x);

#endif
