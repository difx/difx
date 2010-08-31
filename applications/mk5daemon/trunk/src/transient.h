/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __TRANSIENT_H__
#define __TRANSIENT_H__

#include <string>
#include <list>
#include <pthread.h>
#include <difxmessage.h>

using namespace std;

class Event
{
public:
	Event(double start, double stop, double pri) : 
		startMJD(start), stopMJD(stop), priority(pri) {}
	double startMJD, stopMJD, priority;

	friend bool operator< (Event &t1, Event &t2);
};

class EventQueue
{
public:
	EventQueue(string id) : jobId(id), destDir("nowhere"), user("nobody"), maxSize(5) {}
	string jobId;
	string destDir;
	string user;
	unsigned int maxSize;
	list<Event> events;
	list<string> units;

	void addMark5Unit(const char *unit);
	void addEvent(const DifxMessageTransient *dt);
	void setUser(const char *u);
	int copy(double maxDuration);
	void print() const;
};

class EventManager
{
public:
	list<EventQueue> queues;
	pthread_mutex_t lock;

	EventManager();
	~EventManager();
	EventQueue *startJob(const char *jobId);
	void stopJob(const char *jobId, double maxDuration);
	bool addEvent(const DifxMessageTransient *dt);
	void print();
};

#endif
