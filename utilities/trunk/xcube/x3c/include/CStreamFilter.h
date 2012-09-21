/*
 * FrameFilter.h
 *
 *  Created on:
 *      Author: jtrier
 */

#ifndef CSTREAMFILTER_H_
#define CSTREAMFILTER_H_

#include "xcTypes.h"
#include "X3cPacket.h"

class CStreamFilter {
public:

//	CStreamFilter();
//	virtual ~CStreamFilter();

    enum FilterReturn
    {
        PktFilter = 0,
        PktAllow,
        PktEOF
    };

    /**
	 * Print the filter data to stdout.
	 */
    virtual void printFilter() = 0;
	virtual void setFilter() = 0;
//	virtual bool shouldBeFiltered(UINT64 frameNumber);
	virtual CStreamFilter::FilterReturn shouldBeFiltered(X3cPacket &pkt) = 0;



private:
protected:

};

#endif /* CSTREAMFILTER_H_ */
