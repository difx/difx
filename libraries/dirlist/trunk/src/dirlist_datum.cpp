#include "dirlist_datum.h"

void DirListDatum::print(std::ostream &os, bool doEOL) const
{
	int p = os.precision();
	os.precision(12);
	os << getName() << getName() << " " << getMjdStart() << " " << getSecStart() << " " << getDuration();
	os.precision(p);
	if(doEOL)
	{
		os << std::endl;
	}
}

void DirListDatum::printComment(std::ostream &os, bool doEOL) const
{
	os << " # " << comment;
	if(doEOL)
	{
		os << std::endl;
	}
}

std::ostream& operator << (std::ostream &os, const DirListDatum &x)
{
	os << x.getName() << " " << x.getMjdStart() << " " << x.getSecStart() << " " << x.getDuration();

	return os;
}
