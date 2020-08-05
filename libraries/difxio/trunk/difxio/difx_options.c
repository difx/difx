/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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
// $Id: difx_polyco.c 7037 2015-10-15 01:49:46Z JanWagner $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/difx_polyco.c $
// $LastChangedRevision: 7037 $
// $Author: JanWagner $
// $LastChangedDate: 2015-10-15 03:49:46 +0200 (Thu, 15 Oct 2015) $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_options.h"

DifxioOptions difxioOptions = {
	.verbosity = 0,
	.tryLocalDir = 0
};

int difxioSetOption(int option_id, const void* value)
{
	if(value == NULL)
	{
		return -1;
	}

	switch(option_id)
	{
		case DIFXIO_OPT_VERBOSITY:
			difxioOptions.verbosity = *((int*)value);
			return 0;

		case DIFXIO_OPT_LOCALDIR:
			difxioOptions.tryLocalDir = *((int*)value);
			return 0;

		default:
			break;
	}

	return -1;
}

int difxioGetOption(int option_id, void* value)
{
	if(value == NULL)
	{
		return -1;
	}

	switch(option_id)
	{
		case DIFXIO_OPT_VERBOSITY:
			*((int*)value) = difxioOptions.verbosity;
			return 0;

		case DIFXIO_OPT_LOCALDIR:
			*((int*)value) = difxioOptions.tryLocalDir;
			return 0;

		default:
			return -1;
	}

	return -1;
}
