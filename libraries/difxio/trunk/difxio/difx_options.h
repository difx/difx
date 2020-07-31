/***************************************************************************
 *   Copyright (C) 2007-2018 by Walter Brisken, Adam Deller & Helge Rottmann *
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
// $Id: difx_input.h 9563 2020-06-22 15:26:01Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/difx_input.h $
// $LastChangedRevision: 9563 $
// $Author: WalterBrisken $
// $LastChangedDate: 2020-06-22 17:26:01 +0200 (Mon, 22 Jun 2020) $
//
//============================================================================

#ifndef __DIFX_OPTIONS_H__
#define __DIFX_OPTIONS_H__

#ifdef __cplusplus
extern "C" {
#endif

/* structure containing options that affect difxio behavior during general parsing */
typedef struct
{
	int verbosity;
	int tryLocalDir; /* if 1, then , then files *.calc, *.im, and *.difx are sought in the same directory as *.input file */
} DifxioOptions;

#define DIFXIO_OPT_VERBOSITY    0
#define DIFXIO_OPT_LOCALDIR     1

int difxioSetOption(int option_id, const void* value);
int difxioGetOption(int option_id);

extern DifxioOptions difxioOptions; // instantiated in difx_options.c

#ifdef __cplusplus
}
#endif

#endif
