/*
 * CTimeFilter.h
 *
 *  Created on: Jul 18, 2011
 *      Author: jtrier
 */

#ifndef TIMEFILTER_H_
#define TIMEFILTER_H_

#include <string>
#include "CStreamFilter.h"

class CTimeFilter: public CStreamFilter
{
public:
    /**
     * No Arg constructor
     */
    explicit CTimeFilter(bool isRelativeTime = false);

    /**
     * Construct from calendar times
     */
    CTimeFilter(UINT64 startTime, UINT64 endTime,
                bool isRelativeTime = false);

    /**
     * Construct from string representations of start and end times
     */
    CTimeFilter(std::string const& startTime, std::string const& startFormat, 
                std::string const& endTime, std::string const& endFormat,
                bool isRelativeTime = false);

    /**
     * Destructor
     */
    ~CTimeFilter();

    /**
     * Set the start time for this filter
     *
     * @param time    A string representation of the start time
     * @param format  The format string
     */
    void setStartTime(std::string const& time, std::string const& format);

    /**
     * Set the end time for this filter
     *
     * @param time    A string representation of the end time
     * @param format  The format string
     */
    void setEndTime(std::string const& time, std::string const& format);

    /**
     * Set the start time for this filter
     *
     * @param ts    an instance of struct timespec representing the start time
     *              in UNIX time.
     */
    void setStartTime(timespec const& ts);

    /**
     *
     */
    void setStartTime(UINT32 sec, UINT32 nsec);

    /**
     *
     */
    void setStartTime(struct x3c_timeval &tm);

    /**
     *
     */
    void setEndTime(UINT32 sec, UINT32 nsec);

    /**
     *
     */
    void setEndTime(struct x3c_timeval &tm);


    /**
     * Set the end time for this filter
     *
     * @param ts    an instance of struct timespec representing the end time
     *              in UNIX time.
     */
    void setEndTime(timespec const& ts);

    /**
     *
     */
    void setRelativeTime(bool isRelativeTime);

    /**
     *
     */
    bool isRelativeTime() const;

    /**
     * Check to see if the given time is outside the start and end times.
     * <p>
     *
     * @return  - false if the packet time is greater than startTime and less than endTime,
     * 				otherwise true
     */
    CStreamFilter::FilterReturn shouldBeFiltered(UINT32 frameTime);


    /**
     *
     */
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

    /**
     *
     */
    UINT64 getStartTime() const;

    /**
     *
     */
    UINT64 getEndTime() const;

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

    static time_t setDate(time_t from, time_t to);

private:
    time_t               mStartTime;
    std::string          mStartFmt;
    time_t               mEndTime;
    std::string          mEndFmt;
    bool                 mIsRelativeTime;
    UINT32               mBaseTime;
    x3c_timeval          mX3cStartTime;
    x3c_timeval          mX3cEndTime;

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

    // CGenerticPacketParser needs access to setBaseTime()
    friend class CGenericPacketParser;
};

inline void CTimeFilter::setRelativeTime(bool isRelativeTime)
{
    mIsRelativeTime = isRelativeTime;
}

inline bool CTimeFilter::isRelativeTime() const
{
    return mIsRelativeTime;
}

inline void CTimeFilter::setStartTime(UINT32 sec, UINT32 nsec)
{
    mX3cStartTime.tv_sec = sec;
    mX3cStartTime.tv_nsec = nsec;

    mStartTime = sec;
}


inline void CTimeFilter::setStartTime(struct x3c_timeval &tm)
{
    mX3cStartTime.tv_sec = tm.tv_sec;
    mX3cStartTime.tv_nsec = tm.tv_nsec;

    mStartTime = tm.tv_sec;
}


inline void CTimeFilter::setEndTime(UINT32 sec, UINT32 nsec)
{
    mX3cEndTime.tv_sec = sec;
    mX3cEndTime.tv_nsec = nsec;

    mEndTime = sec;
}


inline void CTimeFilter::setEndTime(struct x3c_timeval &tm)
{
    mX3cEndTime.tv_sec = tm.tv_sec;
    mX3cEndTime.tv_nsec = tm.tv_nsec;

    mEndTime = tm.tv_sec;
}


inline UINT64 CTimeFilter::getStartTime() const
{
    return (((UINT64)mX3cStartTime.tv_sec) << 32) | (UINT64)mX3cStartTime.tv_nsec;
}


inline UINT64 CTimeFilter::getEndTime() const
{
    return (((UINT64)mX3cEndTime.tv_sec) << 32) | (UINT64)mX3cEndTime.tv_nsec;
}

#endif /* TIMEFILTER_H_ */
