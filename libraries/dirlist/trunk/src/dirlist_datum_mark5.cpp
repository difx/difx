#include <cstdio>
#include <ostream>
#include "dirlist_datum_mark5.h"

#define MODULE_LEGACY_SCAN_LENGTH	64

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

std::ostream& operator << (std::ostream &os, const DirListDatumMark5 &x)
{
	os << dynamic_cast<const DirListDatum &>(x) << " " << x.getStart() << " " << x.getLength() << " " << x.getIntSec() << " " << x.getFrameNumInSecond() << " " << x.getFramesPerSecond() << " " << x.getFrameBytes() << " " << x.getFrameOffset() << " " << x.getTracks() << " " << x.getFormat();

	return os;
}
