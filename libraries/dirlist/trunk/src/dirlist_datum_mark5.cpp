#include <cstdio>
#include <cstdlib>
#include <ostream>
#include "dirlist_exception.h"
#include "dirlist_datum_mark5.h"

#define MODULE_LEGACY_SCAN_LENGTH	64

// Parses a line from a .dir file
bool DirListDatumMark5::setFromOldString(const char *str)
{
	char scanName[MODULE_LEGACY_SCAN_LENGTH];
	int n;
	int p;
	
	n = sscanf(str, "%Ld%Ld%d%d%d%d%lf%d%d%d%d%63s%n",
		&start, &length, &mjdStart, &intSec, &frameNumInSecond, &framesPerSecond,
		&duration, &frameBytes, &frameOffset, &tracks, &format, scanName, &p);
	
	if(n != 12 || frameBytes <= 0)
	{
		return false;
	}

	setName(scanName);
	setSecStart(intSec + static_cast<double>(frameNumInSecond)/static_cast<double>(framesPerSecond));
	setComment(str+p);

	return true;
}

void DirListDatumMark5::print(std::ostream &os, bool doEOL) const
{
	DirListDatum::print(os, false);
	os << " " << getStart() << " " << getLength() << " " << getIntSec() << " " << getFrameNumInSecond() << " " << getFramesPerSecond() << " " << getFrameBytes() << " " << getFrameOffset() << " "   << getTracks() << " " << getFormat();
	if(doEOL)
	{
		os << std::endl;
	}
}

bool DirListDatumMark5::setFromTokens(const std::vector<std::string> &tokens)
{
	if(tokens.size() < 13)
	{
		std::stringstream msg;

		msg << "DirListDatumMark5::setFromTokens(): too few tokens provided (13 needed); num provided was " << tokens.size();

		throw DirListException(msg.str(), DirListException::TypeParseError);
	}
	DirListDatum::setFromTokens(tokens);
	start = atoll(tokens[4].c_str());
	length = atoll(tokens[5].c_str());
	intSec = atoi(tokens[6].c_str());
	frameNumInSecond = atoi(tokens[7].c_str());
	framesPerSecond = atoi(tokens[8].c_str());
	frameBytes = atoi(tokens[9].c_str());
	frameOffset = atoi(tokens[10].c_str());
	tracks = atoi(tokens[11].c_str());
	format = atoi(tokens[12].c_str());

	return true;
}

std::ostream& operator << (std::ostream &os, const DirListDatumMark5 &x)
{
	os << dynamic_cast<const DirListDatum &>(x) << " " << x.getStart() << " " << x.getLength() << " " << x.getIntSec() << " " << x.getFrameNumInSecond() << " " << x.getFramesPerSecond() << " " << x.getFrameBytes() << " " << x.getFrameOffset() << " " << x.getTracks() << " " << x.getFormat();

	return os;
}
