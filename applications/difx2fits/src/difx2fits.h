/***************************************************************************
 *   Copyright (C) 2008-2024 by Walter Brisken                             *
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
#ifndef __DIFX2FITS_H__
#define __DIFX2FITS_H__

#include <difxio.h>
#include "fits.h"
#include "snifferoptions.h"

#define array_MAX_BANDS		32
#define array_MAX_TONES		1024
#define array_MAX_STATES	4
#define MAX_INPUT_FILES		4096

/* This sets leap secons if for some reason they were not otherwise conveyed */
#define DEFAULT_IAT_UTC 33

enum AllPcalTonesMode
{
	AllPcalTonesOff = 0,	/* put in PH table that which corresponds to job files */
	AllPcalTonesOn,		/* put all pulse cal tones in PH table */
	AllPcalTonesBoth 	/* create two PH tables, one according to job file and the other with all tones */
};

struct CommandLineOptions
{
	char *fitsFile;
	char *baseFile[MAX_INPUT_FILES];
	char *applyBandpassFile;
	char *includeSourceList;
	char *applyDelayCalFile;
	int nBaseFile;
	int writemodel;
	int pretend;
	double scale;
	int verbose;
	int sniffAllPhaseCentres;
	int sniffAllBins;
	int alwaysWriteAutocorr;
	int profileMode;
	int skipExtraAutocorrs;
	int maxJobsToMerge;	/* <=0 means no limit */
	/* some overrides */
	int specAvg;
	int doalldifx;
	float nOutChan;
	float startChan;
	int keepOrder;
	int dontCombine;
	int overrideVersion;
	int dontIncludeVisibilities;
	int pulsarBin;
	int phaseCentre;
	double DifxTsysAvgSeconds;
	double DifxPcalAvgSeconds;
	double jobMatrixDeltaT; /* seconds */
	char *primaryBand;	/* for VLITE */
	char *historyFile;	/* if set, dump contents to FITS history */
	int antpol;		/* if 1, then polarization is determined by antenna */        
	int polxy2hv;		/* if 1, then polarization X/Y is transformed to H/V */
	int localdir;		/* if 1, then *.calc, *.im, and *.difx are sought in the same directory as *.input files */
	enum AllPcalTonesMode allpcaltones;
	int relabelCircular;	/* if != 0, then relabel all polarizations as R/L regardless of their actual values */
	int doVanVleck;		/* if != 0, then correct for Van Vleck (quantization correction) */
	DifxMergeOptions mergeOptions;
	DifxDataFilterOptions filterOptions;
	SnifferOptions snifferOptions;
};

const DifxInput *DifxInput2FitsHeader(const DifxInput *D,
	struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsAG(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsSU(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsAN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsFR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsML(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsCT(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsMC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsUV(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts, int passNum);

const DifxInput *DifxInput2FitsFL(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsTS(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts, enum AllPcalTonesMode allpcaltones);

const DifxInput *DifxInput2FitsWR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsSO(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGD(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGM(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, const struct CommandLineOptions *opts);

#endif
