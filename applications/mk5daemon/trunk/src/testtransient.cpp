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

#include <cstring>
#include "transient.h"

 int main()
 {
	EventManager E;
	DifxMessageTransient dt;

	E.print();

	dt.startMJD = 45000.0;
	dt.stopMJD = 55000.0;
	dt.priority = 4.0;
	strcpy(dt.destDir, "/dev/null");

	EventQueue *Q = E.startJob("jobXX");
	E.print();
	EventQueue *R = E.startJob("jobYY");

	Q->addMark5Unit("mk5-1");
	Q->addMark5Unit("mk5-2");
	Q->setUser("Homer");
	R->addMark5Unit("mk5-3");
	R->addMark5Unit("mk5-4");
	R->setUser("Marge");

	for(int g = 0; g < 10; g++)
	{
		dt.priority = g;
		strcpy(dt.jobId, "jobXX");
		E.addEvent(&dt);
		dt.priority = 10-g;
		strcpy(dt.jobId, "jobYY");
		E.addEvent(&dt);
		dt.priority = g;
		strcpy(dt.jobId, "jobZZ");
		E.addEvent(&dt);
		E.print();
	}

	E.print();

	E.stopJob("jobXX", 500);

	E.print();
	E.stopJob("jobYY", 5000);
	E.print();

 	return 0;
 }
