/***************************************************************************
 *   Copyright (C) 2011 by Walter Brisken                                  *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "difxio/parsedifx.h"
#include "difxio/difx_write.h"

int main()
{
	FILE *out;
	DifxStringArray sa;

	DifxStringArrayinit(&sa);	/* needed because the array started out uninitialized */

	out = fopen("wwh.txt", "w");

	DifxStringArrayprint(&sa);

	DifxStringArrayadd(&sa, "Walter Brisken", 6);
	DifxStringArrayadd(&sa, "was", 6);
	DifxStringArrayadd(&sa, "here", 6);

	writeDifxLineStringArray(out, "STATEMENT", &sa);

	DifxStringArrayprint(&sa);

	DifxStringArrayclear(&sa);

	DifxStringArrayaddlist(&sa, "Was,Walter,here?");

	writeDifxLineStringArray(out, "QUESTION", &sa);

	DifxStringArrayprint(&sa);

	DifxStringArrayclear(&sa);

	DifxStringArrayaddlist(&sa, "There,once,was,a,man,from,Nantucket.,Or,was,there?,I,think,that,story,sounds,crazy,apocryphal.");

	DifxStringArrayprint(&sa);

	writeDifxLineStringArray(out, "STORY", &sa);

	DifxStringArrayclear(&sa);

	writeDifxLine(out, "ONE", "one");
	writeDifxLineDouble(out, "PI", "%e", M_PI);

	fclose(out);

	return EXIT_SUCCESS;
}
