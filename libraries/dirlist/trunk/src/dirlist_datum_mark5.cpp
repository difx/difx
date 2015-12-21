#include "dirlist_datum_mark5.h"

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
	os << DirListDatum::x << " " << start << " " << length << " " << duration << " " mjd << " " << sec << " " << framenuminsecond << " " << framespersecond << " " framebytes << " " << frameoffset << " " << tracks << " " << format;

	return os;
}
