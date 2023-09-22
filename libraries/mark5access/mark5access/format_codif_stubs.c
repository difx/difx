/***************************************************************************
 *   Copyright (C) 2009-2017 by Walter Brisken, Adam Deller, Chris Phillips*
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
// $Id: format_codif_stubs.c 9433 2020-02-20 01:45:17Z ChrisPhillips $
// $HeadURL: $
// $LastChangedRevision: 9433 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2020-02-20 09:45:17 +0800 (四, 2020-02-20) $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>

#include "mark5access/mark5_stream.h"

int get_codif_threads(const unsigned char *data, size_t length, int dataframesize)
{
	fprintf(m5stderr, "mark5_stream: Error - Not compiled with CODIF!! Quitting\n");

	exit(EXIT_FAILURE);
}

int find_codif_frame(const unsigned char *data, int length, size_t *offset, int *framesize, int *headersize)
{
	fprintf(m5stderr, "mark5_stream: Error - Not compiled with CODIF!! Quitting\n");

	exit(EXIT_FAILURE);
}
