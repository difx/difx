#ifndef __DIRLIST_DATUM_MARK5_H__
#define __DIRLIST_DATUM_MARK5_H__

#include <set>
#include <ostream>
#include "dirlist_datum.h"

class DirListDatumMark5 : public DirListDatum
{
public:
	virtual ~DirListDatumMark5() { }
	bool setFromOldString(const char *str);
	long long getStart() const { return start; }
	virtual long long getLength() const { return length; }
	int getIntSec() const { return intSec; }
	int getFrameNumInSecond() const { return frameNumInSecond; }
	int getFramesPerSecond() const { return framesPerSecond; }
	int getFrameBytes() const { return frameBytes; }
	int getFrameOffset() const { return frameOffset; }
	virtual int getTracks() const { return tracks; }
	int getFormat() const { return format; }
	virtual void print(std::ostream &os, bool doEOL = true) const;
	virtual long long getStartPointer() const { return start + frameOffset; }
	virtual bool setFromTokens(const std::vector<std::string> &tokens);

private:
	long long start;	/* start pointer on disk */
	long long length;	/* number of bytes */
	int intSec;		/* integer second portion of timestamp of first frame */
	int frameNumInSecond;	/* frame number since last 1 second tick */
	int framesPerSecond;	/* number of frames per second (always integer) */
	int frameBytes;		/* length of entire frame in bytes */
	int frameOffset;	/* bytes to start of first frame */
	int tracks;
	int format;
	std::set<int> startThreads;	// set of threads present at beginning of the stream
	std::set<int> endThreads;	// set of threads present at end of the stream
};

std::ostream& operator << (std::ostream &os, const DirListDatumMark5 &x);

#endif
