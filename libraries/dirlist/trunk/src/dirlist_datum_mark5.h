#ifndef __DIRLIST_DATUM_MARK5_H__
#define __DIRLIST_DATUM_MARK5_H__

#include <set>
#include <ostream>
#include "dirlist.h"

class DirListDatumMark5 : public DirListDatum
{
public:
	bool setFromString(const char *str);
private:
	long long start;
	long long length;
	double duration;	/* scan duration in seconds */
	int mjd, sec;		/* timestamp of first frame */
	int framenuminsecond;	/* frame number since last 1 second tick */
	int framespersecond;	/* number of frames per second (always integer) */
	int framebytes;		/* length of entire frame in bytes */
	int frameoffset;	/* bytes to start of first frame */
	int tracks;
	int format;
	std::set<int> startThreads;	// set of threads present at beginning of the stream
	std::set<int> endThreads;	// set of threads present at end of the stream
};

std::ostream& operator << (std::ostream &os, const DirListDatumMark5 &x);

#endif
