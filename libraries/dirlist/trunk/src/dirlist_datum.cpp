#include "dirlist_datum.h"

std::ostream& operator << (std::ostream &os, const DirListDatum &x)
{
	os << x.getName() << " " << x.getMjdStart() << " " << x.getMjdStop();

	return os;
}
