/***************************************************************************
 *   Copyright (C) 2015-2016 by Walter Brisken & Adam Deller               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/multidatastream_refactor/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

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
