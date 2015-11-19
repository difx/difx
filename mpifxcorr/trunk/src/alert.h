/***************************************************************************
 *   Copyright (C) 2007-2015 by Walter Brisken and Adam Deller             *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate: 2008-09-10 10:13:06 -0400 (Wed, 10 Sep 2008) $
//
//============================================================================

#ifndef __ALERT_H__
#define __ALERT_H__

#include <string>
#include <iostream>
#include <sstream>
#include <pthread.h>

using namespace std;

class Alert
{
private:
	int alertLevel;
	stringstream alertString;
	pthread_mutex_t lock;
	int last;	// 0 for startl, 1 for endl;
public:
	Alert(int level = 4) : alertLevel(level)
	{
		last = 1;
		pthread_mutex_init(&lock, 0);
	}
	
	~Alert()
	{
		// flush unsent message at destruction time
		if(alertString.str().length() > 0)
		{
			sendAlert();
		}
	}

        void setAlertLevel(int level)
        {
                alertLevel = level;
        }
	
	// operator << to take any type that can be << to a stringstream 
	// and pass it into the internal stringstream
	template<class T> Alert& operator<<(T data)
	{
		alertString << data;
		return *this;
	}
	
	// so "manipulator" objects (e.g. endl) can be used
	Alert& operator<<(Alert& (*f)(Alert &))
	{
		return f(*this);
	}

        void precision(int p)
        {
                alertString.precision(p);
        }

        int precision()
        {
                return alertString.precision();
        }

	// the function endl calls
	Alert& sendAlert();

	friend Alert& endl(Alert &x);
	friend Alert& startl(Alert &x);
};

inline Alert& endl(Alert &x)
{
	if(x.last == 1)
	{
		cerr << "Developer error: two endls in a row.  string was : " << x.alertString.str() << endl;
	}
        x.sendAlert();
	if(x.last != 1)
	{
		x.last = 1;
		pthread_mutex_unlock(&x.lock);
	}
	return x;
}

inline Alert& startl(Alert &x)
{
	pthread_mutex_lock(&x.lock);
	x.last = 0;

	return x;
}

extern Alert cfatal;
extern Alert csevere;
extern Alert cerror;
extern Alert cwarn;
extern Alert cinfo;
extern Alert cverbose;
extern Alert cdebug;

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
