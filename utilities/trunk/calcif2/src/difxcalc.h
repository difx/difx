/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & Adam Deller               *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __DIFX_CALC_H__
#define __DIFX_CALC_H__

#include <difxio.h>
#include "CALCServer.h"

#define MAX_MODEL_OVERSAMP 5

typedef struct
{
	int order;
	int oversamp;
	int increment;	/* seconds */
	double delta;	/* (rad) step size for dtau/dl and dtau/dm */
	char calcServer[64];
	int calcProgram;
	int calcVersion;
	int allowNegDelay;
	struct getCALC_arg request;
	enum AberCorr aberCorr;
	CLIENT *clnt;
} CalcParams;

int difxCalcInit(const DifxInput *D, CalcParams *p);
int difxCalc(DifxInput *D, CalcParams *p, int verbose);

#endif
