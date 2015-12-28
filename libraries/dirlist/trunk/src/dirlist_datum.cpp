#include <cstdio>
#include <cstdlib>
#include <ostream>
#include <string>
#include <vector>
#include "dirlist_exception.h"
#include "dirlist_datum.h"

// Parses a line from, e.g., output of vsum
bool DirListDatum::setFromOldFileListString(const char *str)
{
	const int MaxFileNameLength = 256;
	char scanName[MaxFileNameLength];
	double mjd1, mjd2;
	int n, p;

	n = sscanf(str, "%s%lf%lf%n", scanName, &mjd1, &mjd2, &p);
	if(n != 3 || mjd2 <= mjd1 || mjd1 < 10000.0 || mjd1 > 1000000.0)
	{
		return false;
	}

	setName(scanName);
	setMjdStart(static_cast<int>(mjd1));
	setSecStart(86400.0*(mjd1 - static_cast<int>(mjd1)));
	setDuration(86400.0*(mjd2 - mjd1));
	setComment(str+p);

	return true;
}

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

bool DirListDatum::setFromTokens(const std::vector<std::string> &tokens)
{
	if(tokens.size() < 4)
	{
		throw DirListException("DirListDatum::setFromTokens(): wrong number of tokens.  Should be 4.  Was ", tokens.size());
	}
	setName(tokens[0]);
	setMjdStart(atoi(tokens[1].c_str()));
	setSecStart(atof(tokens[2].c_str()));
	setDuration(atof(tokens[3].c_str()));

	return true;
}

std::ostream& operator << (std::ostream &os, const DirListDatum &x)
{
	os << x.getName() << " " << x.getMjdStart() << " " << x.getSecStart() << " " << x.getDuration();

	return os;
}
