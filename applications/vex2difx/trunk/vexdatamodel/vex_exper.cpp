#include "vex_exper.h"

std::ostream& operator << (std::ostream &os, const VexExper &x)
{
	int p = os.precision();

	os.precision(12);
	os << "Experiment " << x.name << " mjd(" << x.mjdStart << "," << x.mjdStop << ")";
	os.precision(p);

	return os;
}

