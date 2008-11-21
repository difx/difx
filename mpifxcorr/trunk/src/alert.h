/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken and Adam Deller                  *
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

using namespace std;

class Alert
{
private:
	stringstream alertString;
	int alertLevel;
public:
	Alert(int level = 4) : alertLevel(level)
	{
	}
	
	~Alert()
	{
		// flush unsent message at destruction time
		if(alertString.str().length() > 0)
		{
			sendAlert();
		}
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

	// the function endl calls
	Alert& sendAlert();
};

inline Alert& endl(Alert &x)
{
	return x.sendAlert();
}

extern Alert cfatal;
extern Alert csevere;
extern Alert cerror;
extern Alert cwarn;
extern Alert cinfo;
extern Alert cverbose;
extern Alert cdebug;

#endif
