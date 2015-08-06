#ifndef __VEX_EOP_H__
#define __VEX_EOP_H__

#include <iostream>
#include <string>

extern const double RAD2ASEC;

class VexEOP
{
public:
	VexEOP() : mjd(0), tai_utc(0), ut1_utc(0.0), xPole(0.0), yPole(0.0) {}

	double mjd;		// days
	double tai_utc;		// seconds
	double ut1_utc;		// seconds
	double xPole, yPole;	// radian

	int setkv(const std::string &key, const std::string &value);
};

std::ostream& operator << (std::ostream &os, const VexEOP &x);

#endif
