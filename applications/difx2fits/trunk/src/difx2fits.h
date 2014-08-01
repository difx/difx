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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: difx2fits.h 1520 2009-09-23 23:34:38Z AdamDeller $
// $HeadURL: $
// $LastChangedRevision: 1520 $
// $Author: AdamDeller $
// $LastChangedDate: 2009-09-23 17:34:38 -0600 (Wed, 23 Sep 2009) $
//
//============================================================================
#ifndef __DIFX2FITS_H__
#define __DIFX2FITS_H__

#include <difxio/difx_input.h>
#include "fits.h"

#define array_MAX_BANDS		32
#define array_MAX_TONES		1024
#define array_MAX_STATES	4
#define MAX_INPUT_FILES		4096

/* This sets leap secons if for some reason they were not otherwise conveyed */
#define DEFAULT_IAT_UTC 33

struct CommandLineOptions
{
	char *fitsFile;
	char *baseFile[MAX_INPUT_FILES];
	int nBaseFile;
	int writemodel;
	int pretend;
	double scale;
	int verbose;
	int sniffAllPhaseCentres;
	int sniffAllBins;
	int alwaysWriteAutocorr;
        int skipExtraAutocorrs;
	/* some overrides */
	int specAvg;
	int doalldifx;
	float nOutChan;
	float startChan;
	int keepOrder;
	int dontCombine;
	int overrideVersion;
	int dontIncludeVisibilities;
	double sniffTime;
	int pulsarBin;
	int phaseCentre;
	double DifxTsysAvgSeconds;
	double DifxPcalAvgSeconds;
	double jobMatrixDeltaT; /* seconds */
	char *primaryBand;	/* for VLITE */
	char *historyFile;	/* if set, dump contents to FITS history */
};

const DifxInput *DifxInput2FitsHeader(const DifxInput *D,
	struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsAG(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsSU(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsAN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsFR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsML(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, int phaseCentre);

const DifxInput *DifxInput2FitsCT(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsMC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, int phaseCentre);

const DifxInput *DifxInput2FitsUV(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, 
	struct CommandLineOptions *opts);

const DifxInput *dummy_DifxInput2FitsUV(const DifxInput *D,
        struct fits_keywords *p_fits_keys,
	struct fitsPrivate *out, struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsFL(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsTS(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, int phaseCentre, double DifxTcalAvgSeconds);

const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, int phaseCentre, double DifxTcalAvgSeconds, int verbose);

const DifxInput *DifxInput2FitsWR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsSO(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGD(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGM(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	struct CommandLineOptions *opts);

#endif
