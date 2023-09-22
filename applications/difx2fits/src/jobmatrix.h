/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken                             *
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
 * $Id: jobmatrix.h 8605 2018-12-05 14:28:45Z HelgeRottmann $
 * $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/difx2fits/src/jobmatrix.h $
 * $LastChangedRevision: 8605 $
 * $Author: HelgeRottmann $
 * $LastChangedDate: 2018-12-05 22:28:45 +0800 (三, 2018-12-05) $
 *
 *==========================================================================*/

#ifndef __JOBMATRIX_H__
#define __JOBMATRIX_H__

#include "difxio/difx_input.h"
#include "fitsUV.h"

struct _JobMatrix;

typedef struct _JobMatrix JobMatrix;

JobMatrix *newJobMatrix(const DifxInput *D, const char *filebase, double deltaT);

void writeJobMatrix(const JobMatrix *jm, int passNum);

void deleteJobMatrix(JobMatrix *jm);

int feedJobMatrix(JobMatrix *jm, const struct UVrow *data, int jobId);

#endif
