/*
 * FrameFilter.h
 *
 *  Created on: Jun 9, 2011
 *      Author: jtrier
 */

#ifndef FRAMEFILTER_H_
#define FRAMEFILTER_H_

//#include "xcTypes.h"
#include "CStreamFilter.h"

class CFrameFilter : public CStreamFilter
{
public:
	CFrameFilter();
	CFrameFilter(UINT64 startFrame, UINT64 endFrame);
	virtual ~CFrameFilter();
	/**
	 * Print the filter data to stdout.
	 */
	void printFilter();
    void setFilter();
	void setFilter(UINT64 startFrame, UINT64 endFrame);
	FilterReturn shouldBeFiltered(X3cPacket &pkt);
	FilterReturn shouldBeFiltered(UINT64 frameNumber);
	bool beforeStart(UINT64 frameNumber);
	bool afterEnd(UINT64 frameNumber);

	UINT64 getStartFrame() {
		return startFrame;
	}
	UINT64 getEndFrame() {
		return endFrame;
	}

private:
	UINT64 startFrame;
	UINT64 endFrame;
protected:

};

#endif /* FRAMEFILTER_H_ */
