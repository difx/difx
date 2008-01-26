/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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

#ifndef __DIFX_INPUT_H__
#define __DIFX_INPUT_H__

#define DIFX_SESSION_LEN	4

/* Straight from DiFX frequency table */
typedef struct
{
	double freq;		/* (MHz) */
	double bw;		/* (MHz) */
	char sideband;		/* U or L */
} DifxFreq;

/* To become a FITS IF */
typedef struct
{
	double freq;		/* (MHz) */
	double bw;		/* (MHz) */
	char sideband;		/* U or L */
	int nPol;
	char pol[2];		/* polarization codes : L R X or Y. */
} DifxIF;

/* From DiFX config table, with additional derived information */
typedef struct
{
	double tInt;		/* integration time (sec) */
	int nChan;
	char name[32];
	int postFFringe;	/* 0 or 1 */
	int quadDelayInterp;	/* 0 or 1 */
	int pulsarBinning;	/* 0 or 1 */
	int nPol;		/* number of pols in datastreams (1 or 2) */
	char pol[2];		/* the polarizations */
	int doPolar;		/* >0 if cross hands to be correlated */
	int nIF;		/* number of FITS IFs to create */
	DifxIF *IF;		/* FITS IF definitions */
	int quantBits;		/* 1 or 2 */
	int nRecChan;		/* number of recorded channels */
	int freqId;		/* 0-based number -- uniq IF[] index */
	int *indexDS;		/* 0-based; [antId] data stream table index */
				/* -1 terminated */
	int *indexBL;		/* baseline table indicies for this config */
	int *freqId2IF;		/* map from freq table index to IF */
	
} DifxConfig;

typedef struct
{
	int antId;
	char dataFormat[32];
	int quantBits;		/* quantization bits */
	int nFreq;		/* num freqs from this datastream */
	int nRecChan;		/* number of base band channels recorded */
	int *nPol;		/* [freq] */
	int *freqId;		/* [freq] index to DifxFreq table */
	double *clockOffset;	/* (us) [freq] */
	
	int *RCfreqId;		/* [recChan] index to DifxFreq table */
	char *RCpolName;	/* [recChan] Polarization name (R, L, X or Y) */
} DifxDSEntry;

typedef struct
{
	int dsA, dsB;		/* indices to datastream table */
	int nFreq;
	int *nPolProd;		/* [freq] */
	int **recChanA;		/* [freq][productIndex] */
	int **recChanB;		/* [freq][productIndex] */
} DifxBaseline;

typedef struct
{
	char name[32];
	double delay;		/* (us) at start of job */
	double rate;		/* (us/s) */
	char mount[8];		/* azel, ... */
	double offset[3];	/* axis offset, (m) */
	double X, Y, Z;		/* telescope position, (m) */
	double dX, dY, dZ;	/* telescope position derivative, (m/s) */
	char vsn[12];		/* vsn for module */
} DifxAntenna;

typedef struct
{
	double ra, dec;		/* radians */
	char name[32];		/* source name */
	char calcode[4];	/* usually only 1 char long */
	int qual;		/* source qualifier */
	int configId;		/* to determine freqId */
} DifxSource;

typedef struct
{
	double u, v, w;		/* baseline (m) */
	double t;		/* delay (us) */
	double dt;		/* rate (us/s) */
	double a;		/* atmosphere delay (us) */
	double da;		/* atmosphere rate (us/s) */
} DifxModel;

typedef struct
{
	double mjdStart;	/* (day) */
	double mjdEnd;		/* (day) */
	double ra, dec;		/* (radians) */
	char name[32];		/* name of source */
	char calcode[4];	/* usually only 1 digit */
	int qual;		/* source qualifier */
	int sourceId;		/* 0, 1, ... nScan-1 */
	int configId;		/* 0, 1, ... nConfig-1 */
	int nPoint;		/* number of points modeled for scan */
	int nAntenna;
	DifxModel **model;	/* indexed by [ant][inc] */
				/* NOTE : inc can be [-1 .. nPoint+1] ! */
} DifxScan;

typedef struct
{
	int mjd;		/* (day) */
	int tai_utc;		/* (sec) */
	double ut1_utc;		/* (sec) */
	double xPole, yPole;	/* (arcsec) */
} DifxEOP;

typedef struct
{
	double mjd1, mjd2;	/* (day) */
	int antId;		/* antenna number */
} DifxAntennaFlag;

typedef struct
{
	double jobStart;	/* cjobgen job start time (mjd) */
	double jobStop;		/* cjobgen job start time (mjd) */
	double mjdStart;	/* subjob start time (mjd) */
	double duration;	/* subjob observe duration (sec) */
	double modelInc;	/* model (delay, uvw) interval */
	int activeDatastreams;
	int activeBaselines;
	int jobId;		/* correlator job number */
	int subjobId;		/* difx specific sub-job id */
	int subarrayId;		/* sub array number of the specified sub-job */
	char obsCode[8];	/* project name */
	char obsSession[8];	/* project session (e.g., A, B, C1) */
	char taperFunction[8];	/* usually "UNIFORM" */
	double refFreq;		/* some sort of reference frequency, (MHz) */
	int specAvg;		/* number of channels to average */
	int nOutChan;		/* number of channels to write to FITS */
	char calcServer[32];	/* name of calc server */

	int nIF;		/* maximum num IF across configs */
	int nPol;		/* maximum num pol across configs */
	int doPolar;		/* 0 if not, 1 if so */
	int nPolar;		/* nPol*(doPolar+1) */
				/* 1 for single pol obs */
				/* 2 for dual pol, parallel hands only */
				/* 4 for full pol */
	double chanBW;		/* MHz common channel bandwidth. 0 if differ */
	int quantBits;		/* 0 if if different in configs; or 1 or 2 */
	char polPair[4];	/* "  " if different in configs */
	
	int nAntenna, nConfig, nFreq, nScan, nSource, nEOP, nFlag;
	int nDSEntry, nBaseline;
	DifxConfig	*config;
	DifxFreq	*freq;
	DifxAntenna	*antenna;
	DifxScan	*scan;
	DifxSource	*source;
	DifxEOP		*eop;
	DifxAntennaFlag *flag;
	DifxDSEntry	*dsentry;
	DifxBaseline    *baseline;
} DifxInput;


DifxInput *newDifxInput();
void deleteDifxInput(DifxInput *D);
void printDifxInput(const DifxInput *D);
DifxInput *loadDifxInput(const char *fileprefix);
int DifxInputGetSourceId(const DifxInput *D, double mjd);
int DifxInputGetAntennaId(const DifxInput *D, const char *antName);

#endif
