/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#ifndef __DIFX_CALCULATOR_H__
#define __DIFX_CALCULATOR_H__

#include "difx_input.h"

typedef struct
{
	int nAntenna;
	int nBaseline;
} DifxCalculatorConfig;

typedef struct
{
	int nConfig;
	double speedUp;
	DifxCalculatorConfig *config;
} DifxCalculator;

DifxCalculator *newDifxCalculator();

void deleteDifxCalculator(DifxCalculator *C);

DifxCalculatorConfig *newDifxCalculatorConfigArray(int n);

void deleteDifxCalculatorConfigArray(DifxCalculatorConfig *c, int n);

int populateDifxCalculator(DifxCalculator *C, const DifxInput *D);

void printDifxCalculator(const DifxCalculator *C);

#endif
