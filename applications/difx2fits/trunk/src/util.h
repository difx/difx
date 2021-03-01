/***************************************************************************
 *   Copyright (C) 2011-2013 by Walter Brisken                             *
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
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/applications/difx2fits/trunk/src/fitsTS.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __DIFX2FITS_UTIL_H__
#define __DIFX2FITS_UTIL_H__

#include <glob.h>

/* in the functions below, label is used in error messages to mention something
 * about the context of the glob when failures occur
 */

int glob2(const char *label, const char *pattern, int flags, int errfunc(const char *epath, int eerrno), glob_t *pglob);

int globcase(const char *label, const char *match, char *fileName);

int glob2_sort_pcal_files( glob_t *pglob);

int sortjobpcal ( const DifxInput *D, int antennaId, int *jobxref );

#endif
