#ifndef __VEX_SOURCE_H__
#define __VEX_SOURCE_H__

#include <iostream>
#include <string>
#include <vector>

class VexSource
{
public:
	VexSource() : ra(0.0), dec(0.0), calCode(' ') {}
	bool hasSourceName(const std::string &name) const;

	std::string defName;			// in the "def ... ;" line in Vex
	
	std::vector<std::string> sourceNames;	// from source_name statements
	double ra;		// (rad)
	double dec;		// (rad)
	char calCode;

	static const unsigned int MAX_SRCNAME_LENGTH = 12;
};

std::ostream& operator << (std::ostream &os, const VexSource &x);

#endif
