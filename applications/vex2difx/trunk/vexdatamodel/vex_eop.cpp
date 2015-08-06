#include <sstream>
#include <cmath>
#include "vex_eop.h"

const double RAD2ASEC=180.0*3600.0/M_PI;

int VexEOP::setkv(const std::string &key, const std::string &value)
{
	std::stringstream ss;
	int nWarn = 0;

	ss << value;

	if(key == "tai_utc")
	{
		ss >> tai_utc;
	}
	else if(key == "ut1_utc")
	{
		ss >> ut1_utc;
	}
	else if(key == "xPole")
	{
		ss >> xPole;
		xPole /= RAD2ASEC;
	}
	else if(key == "yPole")
	{
		ss >> yPole;
		yPole /= RAD2ASEC;
	}
	else
	{
		std::cerr << "Warning: EOP: Unknown parameter '" << key << "'." << std::endl;
		++nWarn;
	}

	return nWarn;
}

std::ostream& operator << (std::ostream &os, const VexEOP &x)
{
	os << "EOP(" << x.mjd << ", " << x.tai_utc << ", " << x.ut1_utc << ", " << (x.xPole*RAD2ASEC) << ", " << (x.yPole*RAD2ASEC) << ")";

	return os;
}
