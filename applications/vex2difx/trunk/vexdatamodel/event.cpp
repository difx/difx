#include "event.h"

// Note: the ordering here is crucial!
const char Event::eventName[][20] =
{
	"None",
	"Ant Stop",
	"Ant Off-source",
	"Scan Stop",
	"Job Stop",
	"Observe Stop",
	"Record Stop",
	"Clock Break",
	"Leap Second",
	"Manual Break",
	"Record Start",
	"Observe Start",
	"Job Start",
	"Scan Start",
	"Ant On-source",
	"Ant Start"
};

bool operator<(const Event &a, const Event &b)
{
	if(a.mjd < b.mjd - 0.000001)
	{
		return true;
	}
	else if(a.mjd > b.mjd + 0.000001)
	{
		return false;
	}
	if(a.eventType < b.eventType)
	{
		return true;
	}
	else if(a.eventType > b.eventType)
	{
		return false;
	}

	return a.name < b.name;
}

void addEvent(std::list<Event> &events, double mjd, Event::EventType eventType, const std::string &name)
{
	events.push_back(Event(mjd, eventType, name));
}

void addEvent(std::list<Event> &events, double mjd, Event::EventType eventType, const std::string &name, const std::string &scan)
{
	events.push_back(Event(mjd, eventType, name, scan));
}

double getAntennaStartMJD(const std::list<Event> &events, const std::string &name)
{
	if(events.empty())
	{
		return 0.0;
	}

	double earliest = 1e9;

	for(std::list<Event>::const_iterator e = events.begin(); e != events.end(); ++e)
	{
		if(e->mjd < earliest)
		{
			earliest = e->mjd;
		}
		if(e->eventType == Event::ANTENNA_START && e->name == name)
		{
			return e->mjd;
		}
	}

	// if antenna is not represented, return something 1 day earlier than the first event just to be inclusive
	return earliest - 1.0;
}

double getAntennaStopMJD(const std::list<Event> &events, const std::string &name)
{
	if(events.empty())
	{
		return 0;
	}

	double latest = -1e9;

	for(std::list<Event>::const_iterator e = events.begin(); e != events.end(); ++e)
	{
		if(e->eventType == Event::ANTENNA_STOP && e->name == name)
		{
			return e->mjd;
		}
	}

	// if antenna is not represented, return something 1 day later than the last event just to be inclusive
	return latest + 1.0;
}

void getEventListTimeRange(const std::list<Event> &events, Interval &eventTimeRange)
{
	eventTimeRange.mjdStart = 1.0e7;
	eventTimeRange.mjdStop = 0.0;

	for(std::list<Event>::const_iterator it = events.begin(); it != events.end(); ++it)
	{
		if(it->mjd < eventTimeRange.mjdStart && it->eventType != Event::CLOCK_BREAK)
		{
			eventTimeRange.mjdStart = it->mjd;
		}
		if(it->mjd > eventTimeRange.mjdStop && it->eventType != Event::CLOCK_BREAK)
		{
			eventTimeRange.mjdStop = it->mjd;
		}
	}
}

void printEventList(const std::list<Event> &events)
{
	std::cout << "Event list:" << std::endl;
	for(std::list<Event>::const_iterator it = events.begin(); it != events.end(); ++it)
	{
		std::cout << "  " << *it << std::endl;
	}
}

std::ostream& operator << (std::ostream &os, const Event &x)
{
	int d, s;

	d = static_cast<int>(x.mjd);
	s = static_cast<int>((x.mjd - d)*86400.0 + 0.5);

	os << "mjd=" << d << " sec=" << s << " : " << Event::eventName[x.eventType] << " " << x.name;

	return os;
}
