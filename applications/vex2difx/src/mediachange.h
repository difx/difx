/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
 * $Id: mediachange.h 6686 2015-06-02 17:12:30Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision: 6686 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2015-06-03 01:12:30 +0800 (三, 2015-06-03) $
 *
 *==========================================================================*/

#ifndef __MEDIACHANGE_H__
#define __MEDIACHANGE_H__

#include <ostream>
#include <string>
#include <list>
#include "interval.h"

class MediaChange : public Interval
{
public:
	MediaChange(std::string A, double start, double stop) : Interval(start, stop), ant(A) {}

	std::string ant;
};

std::ostream& operator << (std::ostream& os, const MediaChange& x);

int nGap(const std::list<MediaChange> &m, double mjd);

#endif
