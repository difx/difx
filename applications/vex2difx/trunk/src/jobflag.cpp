#include "jobflag.h"

std::ostream& operator << (std::ostream &os, const JobFlag &x)
{
	int p = os.precision();

	os.precision(12);

	os << x.mjdStart << " " << x.mjdStop << " " << x.antId;

	os.precision(p);

	return os;
}
