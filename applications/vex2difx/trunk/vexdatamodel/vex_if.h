#ifndef __VEX_IF_H__
#define __VEX_IF_H__

#include <iostream>
#include <string>

class VexIF
{
public:
	VexIF() : ifSSLO(0.0), ifSideBand(' '), pol(' '), phaseCalIntervalMHz(0) {}
	std::string bandName() const;
	std::string VLBABandName() const;
	double getLowerEdgeFreq() const;

	std::string name;
	double ifSSLO;		// [Hz] SSLO of the IF
	char ifSideBand;	// U or L
	char pol;		// R or L
	int phaseCalIntervalMHz;// MHz, typically 1 or 5 (or 0 if none)

	// special values needed for VLBA, extracted from comment line
	std::string comment;
};

std::ostream& operator << (std::ostream &os, const VexIF &x);

#endif
