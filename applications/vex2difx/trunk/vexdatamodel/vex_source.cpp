#include <algorithm>
#include "vex_source.h"

bool VexSource::hasSourceName(const std::string &name) const
{
	// if result of find is .end(), then it is not in the list
	return find(sourceNames.begin(), sourceNames.end(), name) != sourceNames.end();
}

std::ostream& operator << (std::ostream &os, const VexSource &x)
{
	os << "Source " << x.defName << std::endl;
	for(std::vector<std::string>::const_iterator it = x.sourceNames.begin(); it != x.sourceNames.end(); ++it)
	{
		os << "  name=" << *it << std::endl;
	}
	os << "  ra=" << x.ra <<
		"\n  dec=" << x.dec << std::endl;

	return os;
}
