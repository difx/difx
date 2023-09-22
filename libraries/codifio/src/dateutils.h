/***************************************************************************
 *   Copyright (C) 2009-2015 by Adam Deller / Walter Brisken               *
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
// $Id: dateutils.h 6957 2015-08-31 15:34:32Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision: 6957 $
// $Author: WalterBrisken $
// $LastChangedDate: 2015-09-01 01:34:32 +1000 (Tue, 01 Sep 2015) $
//
//============================================================================

#ifndef __DATEUTILS_H__
#define __DATEUTILS_H__

#ifdef __cplusplus
extern "C" {
#endif

#define UNIXZERO_MJD 40587

void mjd2ymd(int mjd, int *year, int *month, int *day);

int ymd2doy(int yr, int mo, int day);

int ymd2mjd(int yr, int mo, int day);

#ifdef __cplusplus
}
#endif

#endif
