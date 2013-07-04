/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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
 * $Id: mk5daemon.h 3555 2011-07-24 00:36:06Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/mk5daemon.h $
 * $LastChangedRevision: 3555 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2011-07-23 18:36:06 -0600 (Sat, 23 Jul 2011) $
 *
 *==========================================================================*/

#ifndef __SMART_H__
#define __SMART_H__

#include <xlrapi.h>

#define N_SMART_DRIVES			8
#define SMART_TEMP_STRING_LENGTH	40

#ifdef XLR_MAX_SMARTVALUES
#define HAS_SMART
#else
#define XLR_MAX_SMARTVALUES 1
#endif

typedef struct
{
	char model[XLR_MAX_DRIVENAME+1];
	char serial[XLR_MAX_DRIVESERIAL+1];
	char rev[XLR_MAX_DRIVEREV+1];
	int smartCapable;
	long long capacity;	/* in bytes */
} DriveInformation;

typedef struct
{
	double mjd;
	char vsn[10];
#ifdef HAS_SMART
	S_SMARTVALUES smartXLR[N_SMART_DRIVES][XLR_MAX_SMARTVALUES];
#endif
	int id[N_SMART_DRIVES][XLR_MAX_SMARTVALUES];
	long long value[N_SMART_DRIVES][XLR_MAX_SMARTVALUES];
	int nValue[N_SMART_DRIVES];

	DriveInformation drive[N_SMART_DRIVES];
} Mk5Smart;

typedef struct
{
	int id;
	int critical;
	char desc[32];
} SmartDescription;

extern const SmartDescription smartDescriptions[];

const char *getSmartDescription(int smartId);

int isSmartCritical(int smartId);


#endif
