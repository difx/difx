/***************************************************************************
 *   Copyright (C) 2016-2017 by Walter Brisken                             *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: utils.cpp 10035 2021-06-24 10:10:29Z JanWagner $
// $HeadURL: $
// $LastChangedRevision: 10035 $
// $Author: JanWagner $
// $LastChangedDate: 2021-06-24 18:10:29 +0800 (四, 2021-06-24) $
//
//============================================================================

#include <cstdio>
#include <string.h>

char *fgetsNoCR(char *line, int MaxLineLength, FILE *in)
{
	char *v;
	unsigned int i;

	memset(line, 0, MaxLineLength);

	v = fgets(line, MaxLineLength, in);
	if(!v)
	{
		return v;
	}

	// strip CR/LF chars from end
	for(i = 0; line[i]; ++i)
	{
		if(line[i] < ' ')
		{
			line[i] = 0;

			break;
		}
	}

	return v;
}
