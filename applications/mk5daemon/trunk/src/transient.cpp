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

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include "transient.h"

bool operator< (Event &t1, Event &t2)
{
	return t1.priority < t2.priority;
}

void EventQueue::addEvent(const DifxMessageTransient *dt)
{
	events.push_back(Event(dt->startMJD, dt->stopMJD, dt->priority));
	while(events.size() > maxSize)
	{
		// Need to delete an element -- delete the lowest priority one
		events.sort();
		events.pop_front();
	}
	destDir = dt->destDir;
}

int EventQueue::copy(double maxDuration)
{
	const int CommandSize = 512;
	list<Event>::const_iterator e;
	list<string>::const_iterator m;
	char command[CommandSize];
	int v;
	int nCopy = 0;
	time_t tStart;

	if(events.empty())
	{
		return 0;
	}

	/* put the events in decreasing priority and generate the copy command */
	events.sort();
	events.reverse();

	tStart = time(0);

	// FIXME -- this should be parallelized across units!
	for(e = events.begin(); e != events.end(); e++)
	{
		for(m = units.begin(); m != units.end(); m++)
		{
			snprintf(command, CommandSize, 
				"su - %s -c 'ssh -x %s \"mk5cp Active %14.8f_%14.8f %s\"'",
				user.c_str(),
				m->c_str(),
				e->startMJD, e->stopMJD, destDir.c_str());
			printf("Executing %s\n", command);

			v = system(command);
		}
		nCopy++;
		if(time(0)-tStart > maxDuration)
		{
			break;
		}
	}

	return nCopy;
}

void EventQueue::addMark5Unit(const char *unit)
{
	units.push_back(string(unit));
}

void EventQueue::setUser(const char *u)
{
	user = u;
}

void EventQueue::print() const
{
	list<Event>::const_iterator e;
	list<string>::const_iterator u;

	cout << "  Job [" << jobId << "]:" << endl;

	cout << "    user: " << user << endl;

	cout << "    units:";
	for(u = units.begin(); u != units.end(); u++)
	{
		cout << " " << *u;
	}
	cout << endl;

	for(e = events.begin(); e != events.end(); e++)
	{
		cout << "    " << "pri=" << e->priority << " start=" << e->startMJD << " stop=" << e->stopMJD << endl;
	}
}

EventManager::EventManager()
{
	pthread_mutex_init(&lock, 0);
}

EventManager::~EventManager()
{
	pthread_mutex_destroy(&lock);
}

EventQueue *EventManager::startJob(const char *jobId)
{
	list<EventQueue>::iterator q;
	bool existed=false;

	pthread_mutex_lock(&lock);

	for(q = queues.begin(); q != queues.end(); q++)
	{
		if(q->jobId == jobId)
		{
			queues.erase(q);
			existed = true;
		}
	}

	if(!existed)
	{
		queues.push_back(EventQueue(jobId));
	}

	pthread_mutex_unlock(&lock);

	return &queues.back();
}

void EventManager::stopJob(const char *jobId, double maxDuration)
{
	list<EventQueue>::iterator q;

	pthread_mutex_lock(&lock);

	for(q = queues.begin(); q != queues.end(); q++)
	{
		if(q->jobId == jobId)
		{
			q->copy(maxDuration);
			queues.erase(q);
			break;
		}
	}

	pthread_mutex_unlock(&lock);
}

bool EventManager::addEvent(const DifxMessageTransient *dt)
{
	list<EventQueue>::iterator q;
	bool queued = false;

	pthread_mutex_lock(&lock);

	for(q = queues.begin(); q != queues.end(); q++)
	{
		if(q->jobId == dt->jobId)
		{
			q->addEvent(dt);
			queued = true;
			break;
		}
	}

	pthread_mutex_unlock(&lock);

	return queued;
}

void EventManager::print()
{
	list<EventQueue>::const_iterator q;
	
	pthread_mutex_lock(&lock);
	
	cout << "Transient queue [" << queues.size() << " jobs running]" << endl;
	
	for(q = queues.begin(); q != queues.end(); q++)
	{
		q->print();
	}

	pthread_mutex_unlock(&lock);
}
