#ifndef __VEX_EXPER_H__
#define __VEX_EXPER_H__

#include <iostream>
#include "interval.h"

class VexExper : public Interval
{
public:
	VexExper() : Interval(0.0, 1000000.0) {}

	std::string name;
};

std::ostream& operator << (std::ostream &os, const VexExper &x);

#endif
