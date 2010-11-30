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
	/* fundamental parameters */
	int nAntenna;
	int nBaseline;
	double nBand;
	double bandwidth;		/* Hz */
	double nPol;
	double nPolPerBand;
	double nBit;
	int nChan;
	int nPhaseCenter;
	double specAvg;			/* averaged over baselines.  Weird: yes. */
	int dataBufferFactor;
	int nDataSegment;
	double tSubint;			/* seconds */
	double tGuard;			/* seconds */
	int arrayStride;
	int xmacStride;
	double tInt;			/* seconds */

	/* network / disk usage */
	double blocksPerSend;
	double basebandMessageSize;	/* bytes */
	double basebandReadSize;	/* bytes */
	double recDataRate;		/* bps */
	double datastreamOutputRate;	/* bps */
	double coreInputRatio;
	double coreInputRate;		/* bps */
	double coreOutputRatio;
	double coreOutputRate;		/* bps */
	double managerInputRate;	/* bps */
	double diskDataRate;		/* bytes/sec */
	double datasetSize;		/* bytes */

	/* memory usage */
	double datastreamBufferSize;	/* bytes */
	double visibilitySize;		/* bytes */
	double modeSize;		/* bytes */
	double coreSize;		/* bytes */
	double managerSize;		/* bytes */

	/* times */
	double subintsPerInt;
	double datastreamBufferDur;	/* seconds */
	double datastreamReadDur;	/* seconds */
	double managerSlack;		/* seconds */
	double coreBufferDur;		/* seconds */

} DifxCalculatorConfig;

typedef struct
{
	/* fundamental parameters */
	int nConfig;
	double speedUp;
	int nCore;
	double nThread;
	int visibilityLength;
	double tObs;	/* seconds */
	int hasThreadsFile;

	/* derived parameters */

	DifxCalculatorConfig *config;
} DifxCalculator;

DifxCalculator *newDifxCalculator();

void deleteDifxCalculator(DifxCalculator *C);

DifxCalculatorConfig *newDifxCalculatorConfigArray(int n);

void deleteDifxCalculatorConfigArray(DifxCalculatorConfig *c, int n);

int populateDifxCalculator(DifxCalculator *C, const DifxInput *D);

void printDifxCalculatorConfig(const DifxCalculatorConfig *c);

void printDifxCalculator(const DifxCalculator *C);

#endif
