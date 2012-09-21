/*
 * CTimeFilter.h
 *
 *  Created on: Jul 18, 2011
 *      Author: jtrier
 */

#ifndef TIMEFILTER_H_
#define TIMEFILTER_H_

//#include "xcTypes.h"
#include "CStreamFilter.h"

class CTimeFilter: public CStreamFilter
{
public:
    CTimeFilter();
    ~CTimeFilter();
    /**
     * Print the filter data to stdout.
     */
    void printFilter();

    void setFilter();

    /**
     * Filter the payloads by time.  Filtering is base on the
     * the X3C stream packet time, but only deal with seconds
     * <p>
     * This filter limits the payload output by only allowing packets between
     * the start / end values.
     * <p>
     * If this is used it must be set before open or read are called.
     *
     * @param startTime - the time to start getting the packets
     * @param startFormat - the format used to interpret the startTime (defined by strptime)
     * @param relativeStart - is the given time relative to the start of the payload
     * @param endTime - the time to end getting the packets
     * @param endFormat - the format used to interpret the endTime (defined by strptime)
     * @param relativeEnd - is the given time relative to the start of the payload
     */

    void setFilter(
            const char* startTime, const char* startFormat, bool relativeStart,
            const char* endTime,   const char* endFormat,   bool relativeEnd);

    /**
     * Sets the base time, for relative times and checking the date on non-relative times
     * <p>
     * This should only be set by the first packet of a stream, and is currently only
     * used the the GenericPacketParser.open() function.
     * <p>
     * Do not use this outside of the GenericPacketParser.open() function.
     *
     * @param baseTime - the time to the stream started.
     */
    void setBaseTime(UINT32 baseTime);

    /**
     * Check to see if the given time is outside the start and end times.
     * <p>
     *
     * @return  - false if the packet time is greater than startTime and less than endTime,
     * 				otherwise true
     */
    CStreamFilter::FilterReturn shouldBeFiltered(UINT32 frameTime);

    CStreamFilter::FilterReturn shouldBeFiltered(X3cPacket &pkt);

    /**
     * Check to see if the given time is before the start time.
     * <p>
     *
     * @return  - false if the packet time is greater than startTime,
     * 				otherwise true
     */
    bool beforeStart(UINT32 frameTime);
    /**
     * Check to see if the given time is efter the end time.
     * <p>
     *
     * @return  - false if the packet time is less than endTime,
     * 				otherwise true
     */
    bool afterEnd(UINT32 frameTime);

//	time_t getStartTime() {
//		return startTime;
//	}
//	time_t getEndTime() {
//		return endTime;
//	}
//	UINT32 getBaseTime() {
//		return baseTime;
//	}
    /**
     * Check to make sure the the parameters actually return a time.
     * <p>
     *
     * @return  - true if the strings given create a time, false otherwise
     */
    static bool checkTime(const char* time = NULL, const char* fmt = NULL);
    /**
     * Parse the strings to get out a number of seconds since 1970.
     * <p>
     *
     * @return - the number of seconds since 1970, 0 if invalid values given.
     */
    static time_t parseTime(const char* time = NULL, const char* fmt = NULL);

    void setStartTime(UINT32 sec, UINT32 nsec);
    void setStartTime(struct x3c_timeval &tm);
    void setEndTime(UINT32 sec, UINT32 nsec);
    void setEndTime(struct x3c_timeval &tm);

    UINT64 getStartTime();
    UINT64 getEndTime();

private:
    time_t startTime;
    char* startFmt;
    bool relativeStart;
    time_t endTime;
    char* endFmt;
    bool relativeEnd;
    UINT32 baseTime;
    static time_t setDate(time_t from, time_t to);

    struct x3c_timeval x3cStartTime;
    struct x3c_timeval x3cEndTime;
};

#endif /* TIMEFILTER_H_ */
