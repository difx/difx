#ifndef __VEX_EVENT_H__
#define __VEX_EVENT_H__

#include <iostream>
#include <vector>
#include <list>
#include <string>
#include "interval.h"

class Event
{
public:
	enum EventType	// NOTE! keep Event::eventName up to date
	{
		NO_EVENT,
		ANTENNA_STOP,
		ANT_SCAN_STOP,
		SCAN_STOP,
		JOB_STOP,
		OBSERVE_STOP,
		RECORD_STOP,
		CLOCK_BREAK,
		LEAP_SECOND,
		MANUAL_BREAK,
		RECORD_START,
		OBSERVE_START,
		JOB_START,
		SCAN_START,
		ANT_SCAN_START,
		ANTENNA_START
	};

	static const char eventName[][20];

	double mjd;
	enum EventType eventType;
	std::string name;
	std::string scan;

	Event() : mjd(0.0), eventType(NO_EVENT), name("") {}
	Event(double m, enum EventType e, const std::string &a) : mjd(m), eventType(e), name(a), scan("") {}
	Event(double m, enum EventType e, const std::string &a, const std::string &b) : mjd(m), eventType(e), name(a), scan(b) {}
};

void addEvent(std::list<Event> &events, double mjd, Event::EventType eventType, const std::string &name);

void addEvent(std::list<Event> &events, double mjd, Event::EventType eventType, const std::string &name, const std::string &scan);

double getAntennaStartMJD(const std::list<Event> &events, const std::string &name);

double getAntennaStopMJD(const std::list<Event> &events, const std::string &name);

void getEventListTimeRange(const std::list<Event> &events, Interval &eventTimeRange);

void printEventList(const std::list<Event> &events);

bool operator < (const Event &a, const Event &b);

std::ostream& operator << (std::ostream &os, const Event &x);

#endif
