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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/parsevis.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __RADIOASTRON_H__
#define __RADIOASTRON_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
	double Delta_t;		/* the difference in time between the spacecraft and TT time frames, in seconds,
				   such that the spacecraft clock reads TT + \Delta t seconds at TT MJD time
				   mjd.fracDay.  T_{SC} = TT + \Delta t (s) */
	double dtdtau;		/* The rate of \Delta (s/s) */
} RadioastronTimeFrameOffset;

typedef struct
{
	double X[3];		/* unit vector for X axis (usually up or North)*/
	double Y[3];		/* unit vector for Y axis */
	double Z[3];		/* unit vector for Z axis (toward source) */
} RadioastronAxisVectors;

#ifdef __cplusplus
}
#endif

#endif
