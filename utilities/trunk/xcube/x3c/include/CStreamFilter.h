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

class CStreamFilter
{
public:
    virtual ~CStreamFilter()
    {}

    enum FilterReturn
    {
        PktFilter = 0,
        PktAllow,
        PktEOF
    };

	virtual CStreamFilter::FilterReturn shouldBeFiltered(X3cPacket &pkt) = 0;
};

#endif /* CSTREAMFILTER_H_ */
