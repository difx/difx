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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: util.h 4923 2012-10-02 10:03:10Z JohnMorgan $
// $HeadURL: https://svn.atnf.csiro.au/difx/applications/difx2fits/trunk/src/fitsTS.c $
// $LastChangedRevision: 4923 $
// $Author: JohnMorgan $
// $LastChangedDate: 2012-10-02 18:03:10 +0800 (二, 2012-10-02) $
//
//============================================================================

#ifndef __DIFX2FITS_UTIL_H__
#define __DIFX2FITS_UTIL_H__

#include <glob.h>

int glob2(const char *label, const char *pattern, int flags, int errfunc(const char *epath, int eerrno), glob_t *pglob);

#endif
