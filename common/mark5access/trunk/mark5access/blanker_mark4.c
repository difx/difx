/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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

#include "mark5access/mark5_stream.h"

/* Same as generic mark5 blanker, but also make sure start and end of each 
 * frame is blanked.
 */

int blanker_mark4(struct mark5_stream *ms)
{
	int r, s, e, z, delta;

	r = blanker_mark5(ms);

	s = 96*ms->framebytes/20000;
	e = 19936*ms->framebytes/20000;
	z = e >> ms->log2blankzonesize;

	delta = s - ms->blankzonestartvalid[0];
	if(delta > 0)
	{
		r += delta;
	}
	delta = ms->blankzoneendvalid[z] - e;
	if(delta > 0)
	{
		if(delta > ms->framebytes)
		{
			delta = 64*ms->framebytes/20000;
		}
		r += delta;
	}

	ms->blankzonestartvalid[0] = s;
	ms->blankzoneendvalid[z] = e; 

	return r;
}
