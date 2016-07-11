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
// $Id: difx_tcal.h 5164 2013-02-14 21:05:23Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/parsevis.h $
// $LastChangedRevision: 5164 $
// $Author: WalterBrisken $
// $LastChangedDate: 2013-02-14 14:05:23 -0700 (Thu, 14 Feb 2013) $
//
//============================================================================

/* TODO:
 * o Offer more different interpolation types
 * o newDifxTcalFromEnviron() which looks at new env var DIFX_TCAL
 *    which could have values:  VLBA:/home/jansky3/vlbaops/TCAL
 *                              DIFX:/users/difx/tcals
 *                              CONSTANT:2.12,1.98
 *    or others as they are added to this code.
 */


#ifndef __DIFX_TCAL_H__
#define __DIFX_TCAL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#define MAX_DIFX_TCAL_ANTENNA_LENGTH	8
#define MAX_DIFX_TCAL_RECEIVER_LENGTH	8
#define MAX_DIFX_TCAL_TYPE_LENGTH	16
#define DIFX_TCAL_FILENAME_LENGTH	256

/* Note! Keep this in sync with difxTcalTypeString in difx_tcal.c */
enum DifxTcalType
{
	DifxTcalNone = 0,		/* the uninitialized state */
	DifxTcalTypeConstant,
	DifxTcalTypeVLBA,		/* separate files per band.antenna in path[] */
	DifxTcalTypeDIFX,		/* one file pointed to by path[] */
	NumDifxTcalTypes		/* this line must end the enum */
};

extern const char difxTcalTypeString[][MAX_DIFX_TCAL_TYPE_LENGTH];

/* Note! Keep this in sync with difxTcalInterpolationString in difx_tcal.c */
enum DifxTcalInterpolation
{
	DifxTcalInterpolationNearest = 0,
	DifxTcalInterpolationLinear,
	NumDifxTcalInterpolations	/* this line must end the enum */
};

extern const char difxTcalInterpolationString[][MAX_DIFX_TCAL_TYPE_LENGTH];

typedef struct
{
	float freq;		/* MHz */
	float tcal[2];		/* K */
	char pol[2];
} DifxTcalValue;

typedef struct
{
	char antenna[MAX_DIFX_TCAL_ANTENNA_LENGTH];	/* null matches all antennas, but is overridden by an exact match */
	char receiver[MAX_DIFX_TCAL_RECEIVER_LENGTH];	/* e.g., "6cm" (or null) */
	double mjdStart;				/* start of validity of this data (or 0) */
	int serial;					/* serial number of receiver (or -1) */
	enum DifxTcalInterpolation interpolation;

	DifxTcalValue *value;
	int nValue;
	
	/* internal use only */
	int _nValueAlloc;
	int _freqRangeExceeded;
} DifxTcalGroup;

typedef struct
{
	int verbose;
	enum DifxTcalType type;
	char path[DIFX_TCAL_FILENAME_LENGTH];	/* type-dependent value; could be filename or directory */

	DifxTcalGroup *group;	/* one group holds all values for a antenna/receiver/epoch triplet */
	int nGroup;
	int _nGroupAlloc;	/* internal use only */
} DifxTcal;


/* These first functions apply to all possible Tcal formats */
/* Once set-up, these should be the only functions needed to get values */

DifxTcal *newDifxTcal();

void deleteDifxTcal(DifxTcal *dt);

int getDifxTcalGroupIndex(const DifxTcal *dt, double mjd, const char *antenna, const char *receiver);

int addDifxTcalGroup(DifxTcal *dt);

float getDifxTcal(DifxTcal *dt, double mjd, const char *antenna, const char *receiver, char pol, float freq);

int addDifxTcalValue(DifxTcalGroup* group, float freq, float tcal1, char pol1, float tcal2, char pol2);

int setDifxTcalInterpolationType(DifxTcalGroup *group, enum DifxTcalInterpolation interpolation);

void printDifxTcal(const DifxTcal *dt);

void summarizeDifxTcal(const DifxTcal *dt);

void fprintDifxTcal(FILE *out, const DifxTcal *dt);

void fsummarizeDifxTcal(FILE *out, const DifxTcal *dt);


/* below here are site-specific functions */

/* 1. For a constant Tcal value over all frquencies */

int setDifxTcalConstant(DifxTcal *dt, float tcal1, char pol1, float tcal2, char pol2);


/* 2. For VLBA: */

int setDifxTcalVLBA(DifxTcal *dt, const char *tcalPath);

const char *defaultVLBAReceiver(float freq);	/* freq in MHz */

const char *defaultVLBAReceiverStrict(float freq);	/* freq in MHz */


/* 3. For DIFX format file: */

int setDifxTcalDIFX(DifxTcal *dt, const char *tcalFile);


#ifdef __cplusplus
}
#endif


#endif
