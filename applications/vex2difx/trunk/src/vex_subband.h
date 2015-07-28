#ifndef __VEX_SUBBAND_H__
#define __VEX_SUBBAND_H__

#include <iostream>

class VexSubband		// Mode-specific frequency details for all antennas
{
public:
	VexSubband(double f=0.0, double b=0.0, char s=' ', char p=' ') : freq(f), bandwidth(b), sideBand(s), pol(p) {}

	double freq;		// (Hz)
	double bandwidth;	// (Hz)
	char sideBand;		// net side band of channel (U or L)
	char pol;		// R or L
};

bool operator == (const VexSubband &s1, const VexSubband &s2);

std::ostream& operator << (std::ostream &os, const VexSubband &x);

#endif
