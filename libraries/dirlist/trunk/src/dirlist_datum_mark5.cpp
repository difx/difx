#include <cstdio>
#include <ostream>
#include "dirlist_datum_mark5.h"

#define MODULE_LEGACY_SCAN_LENGTH	64

bool DirListDatumMark5::setFromString(const char *str)
{
	char scanName[MODULE_LEGACY_SCAN_LENGTH];
	int n;
	double mjdStart;
	
	n = sscanf(str, "%Ld%Ld%d%d%d%d%lf%d%d%d%d%63s",
		&start, &length, &mjd, &sec, &framenuminsecond, &framespersecond,
		&duration, &framebytes, &frameoffset, &tracks, &format, scanName);
	
	if(n != 12 || framebytes <= 0)
	{
		return false;
	}

	mjdStart = mjd + (sec + (double)framenuminsecond/(double)framespersecond)/86400.0;

	setName(scanName);
	setMjdStart(mjdStart);
	setMjdStop(mjdStart + duration/86400.0);

	return true;
}

std::ostream& operator << (std::ostream &os, const DirListDatumMark5 &x)
{
	os << dynamic_cast<const DirListDatum &>(x) << " " << x.getStart() << " " << x.getLength() << " " << x.getDuration() << " " << x.getMjd() << " " << x.getSec() << " " << x.getFramenuminsecond() << " " << x.getFramespersecond() << " " << x.getFramebytes() << " " << x.getFrameoffset() << " " << x.getTracks() << " " << x.getFormat();

	return os;
}
