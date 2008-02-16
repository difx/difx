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
	char sideband;		/* U or L -- net sideband */
} DifxFreq;

/* To become a FITS IF */
typedef struct
{
	double freq;		/* (MHz) */
	double bw;		/* (MHz) */
	char sideband;		/* U or L -- net sideband */
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
	int pulsarId;		/* -1 if not pulsar */
	int nPol;		/* number of pols in datastreams (1 or 2) */
	char pol[2];		/* the polarizations */
	int doPolar;		/* >0 if cross hands to be correlated */
	int quantBits;		/* 1 or 2 */
	int nRecChan;		/* number of recorded channels */
	int *datastreamId;	/* 0-based; [antennaId] datastream table indx */
				/* -1 terminated */
	int *baselineId;	/* baseline table indicies for this config */
				/* -1 terminated */
	
	int nIF;		/* number of FITS IFs to create */
	DifxIF *IF;		/* FITS IF definitions */
	int freqId;		/* 0-based number -- uniq FITS IF[] index */
	int *freqId2IF;		/* map from freq table index to IF */
	int ***baselineFreq2IF;	/* [a1][a2][freqNum] -> IF */
	
} DifxConfig;

typedef struct
{
	int antennaId;
	char dataFormat[32];
	int quantBits;		/* quantization bits */
	int nFreq;		/* num freqs from this datastream */
	int nRecChan;		/* number of base band channels recorded */
	int *nPol;		/* [freq] */
	int *freqId;		/* [freq] index to DifxFreq table */
	double *clockOffset;	/* (us) [freq] */
	
	int *RCfreqId;		/* [recChan] index to DifxFreq table */
	char *RCpolName;	/* [recChan] Polarization name (R, L, X or Y) */
} DifxDatastream;

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
	char name[32];		/* null terminated */
	double delay;		/* (us) at start of job */
	double rate;		/* (us/s) */
	char mount[8];		/* azel, ... */
	double offset[3];	/* axis offset, (m) */
	double X, Y, Z;		/* telescope position, (m) */
	double dX, dY, dZ;	/* telescope position derivative, (m/s) */
	char vsn[12];		/* vsn for module */
	int spacecraftId;	/* -1 if not a spacecraft */
} DifxAntenna;

typedef struct
{
	double ra, dec;		/* radians */
	char name[32];		/* source name */
	char calCode[4];	/* usually only 1 char long */
	int qual;		/* source qualifier */
	int configId;		/* to determine freqId */
	int spacecraftId;	/* -1 if not spacecraft */
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
	char calCode[4];	/* usually only 1 digit */
	int qual;		/* source qualifier */
	int sourceId;		/* 0, 1, ... nScan-1 */
	int configId;		/* 0, 1, ... nConfig-1 */
	int nPoint;		/* number of points modeled for scan */
	int nAntenna;
	DifxModel **model;	/* indexed by [ant][point] */
				/* NOTE : point is over [-1 .. nPoint+1] ! */
				/* NOTE : model[ant] can be zero -> no data */
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
	int mjd;
	double fracDay;
	long double X, Y, Z;	/* (m) */
	long double dX, dY, dZ;	/* (m/sec) */
} sixVector;

typedef struct
{
	char name[32];		/* name of spacecraft */
	int nPoints;		/* number of entries in ephemeris */
	sixVector *pos;		/* array of positions and velocities */
} DifxSpacecraft;

typedef struct
{
	double mjd1, mjd2;	/* (day) */
	int antennaId;		/* antenna number */
} DifxAntennaFlag;

typedef struct
{
	int nBin;		/* number of pulsar bins */
} DifxPulsar;

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
	int nDatastream, nBaseline, nSpacecraft, nPulsar;
	DifxConfig	*config;
	DifxFreq	*freq;
	DifxAntenna	*antenna;
	DifxScan	*scan;		/* assumed in time order */
	DifxSource	*source;
	DifxEOP		*eop;		/* assumed one per day, optional */
	DifxAntennaFlag *flag;		/* optional table */
	DifxDatastream	*datastream;
	DifxBaseline    *baseline;
	DifxSpacecraft	*spacecraft;	/* optional table */
	DifxPulsar	*pulsar;	/* optional table */
} DifxInput;

/* DifxFreq functions */
DifxFreq *newDifxFreqArray(int nFreq);
void deleteDifxFreqArray(DifxFreq *df);
void printDifxFreq(const DifxFreq *df);
int isSameDifxFreq(const DifxFreq *df1, const DifxFreq *df2);
void copyDifxFreq(DifxFreq *dest, const DifxFreq *src);
DifxFreq *mergeDifxFreqArrays(const DifxFreq *df1, int ndf1,
	const DifxFreq *df2, int ndf2, int *freqIdRemap);

/* DifxAntenna functions */
DifxAntenna *newDifxAntennaArray(int nAntenna);
void deleteDifxAntennaArray(DifxAntenna *da);
void printDifxAntenna(const DifxAntenna *da);
int isSameDifxAntenna(const DifxAntenna *da1, const DifxAntenna *da2);
void copyDifxAntenna(DifxAntenna *dest, const DifxAntenna *src);
DifxAntenna *mergeDifxAntennaArrays(const DifxAntenna *da1, int nda1,
	const DifxAntenna *da2, int nda2, int *antennaIdRemap);

/* DifxBaseline functions */
DifxBaseline *newDifxBaselineArray(int nBaseline);
void deleteDifxBaselineArray(DifxBaseline *db, int nBaseline);
void printDifxBaseline(const DifxBaseline *db);
int isSameDifxBaseline(const DifxBaseline *db1, const DifxBaseline *db2);
void copyDifxBaseline(DifxBaseline *dest, const DifxBaseline *src);
DifxBaseline *mergeDifxBaselineArrays(const DifxBaseline *db1, int ndb1,
	const DifxBaseline *db2, int ndb2, int *baselineIdRemap);

/* DifxDatastream functions */
DifxDatastream *newDifxDatastreamArray(int nDatastream);
void deleteDifxDatastreamArray(DifxDatastream *ds, int nDatastream);
void printDifxDatastream(const DifxDatastream *ds);
int isSameDifxDatastream(const DifxDatastream *dd1, const DifxDatastream *dd2,
	const int *freqIdRemap, const int *antennaIdRemap);
void copyDifxDatastream(DifxDatastream *dest, const DifxDatastream *src,
	const int *freqIdRemap, const int *antennaIdRemap);
DifxDatastream *mergeDifxDatastreamArrays(const DifxDatastream *dd1, int ndd1,
	const DifxDatastream *dd2, int ndd2, int *datastreamIdRemap,
	const int *freqIdRemap, const int *antennaIdRemap);

/* DifxConfig functions */
DifxConfig *newDifxConfigArray(int nConfig);
void deleteDifxConfigArray(DifxConfig *dc);
void printDifxConfig(const DifxConfig *dc);
int DifxConfigGetPolId(const DifxConfig *dc, char polName);
int DifxConfigRecChan2IFPol(const DifxInput *D, int configId,
	int antennaId, int recChan, int *bandId, int *polId);
int isSameDifxConfig(const DifxConfig *dc1, const DifxConfig *dc2,
	const int *baselineIdRemap, const int *datastreamIdRemap,
	const int *pulsarIdRemap);
void copyDifxConfig(DifxConfig *dest, const DifxConfig *src,
	const int *baselineIdRemap, const int *datastreamIdRemap,
	const int *pulsarIdRemap);
DifxConfig *mergeDifxConfigArrays(const DifxConfig *dc1, int ndc1,
	const DifxConfig *dc2, int ndc2, int *configIdRemap,
	const int *baselineIdRemap, const int *datastreamIdRemap,
	const int *pulsarIdRemap);

/* DifxModel functions */
DifxModel **newDifxModelArray(int nAntenna, int nPoint);
DifxModel *dupDifxModelColumn(const DifxModel *src, int nPoint);
void deleteDifxModelArray(DifxModel **dm, int nAntenna);
void printDifxModel(const DifxModel *dm);

/* DifxScan functions */
DifxScan *newDifxScanArray(int nScan);
void deleteDifxScanArray(DifxScan *ds, int nScan);
void printDifxScan(const DifxScan *ds);
void copyDifxScan(DifxScan *dest, const DifxScan *src,
	const int *antennaIdRemap, const int *configIdRemap);
DifxScan *mergeDifxScanArrays(const DifxScan *ds1, int nds1,
	const DifxScan *ds2, int nds2,
	const int *antennaIdRemap, const int *configIdRemap);

/* DifxEOP functions */
DifxEOP *newDifxEOPArray(int nEOP);
void deleteDifxEOPArray(DifxEOP *de);
void printDifxEOP(const DifxEOP *de);
void copyDifxEOP(DifxEOP *dest, const DifxEOP *src);
DifxEOP *mergeDifxEOPArrays(const DifxEOP *de1, int nde1,
	const DifxEOP *de2, int nde2, int *nde);

/* DifxSpacecraft functions */
DifxSpacecraft *newDifxSpacecraftArray(int nSpacecraft);
void deleteDifxSpacecraft(DifxSpacecraft *ds, int nSpacecraft);
void printDifxSpacecraft(const DifxSpacecraft *ds);

/* DifxSource functions */
DifxSource *newDifxSourceArray(int nSource);
void deleteDifxSourceArray(DifxSource *ds);
void printDifxSource(const DifxSource *ds);

/* DifxIF functions */
DifxIF *newDifxIFArray(int nIF);
void deleteDifxIFArray(DifxIF *di);
void printDifxIF(const DifxIF *di);
void deleteBaselineFreq2IF(int ***map);
void printBaselineFreq2IF(int ***map, int nAnt, int nChan);
int makeBaselineFreq2IF(DifxInput *D, int configId);

/* DifxAntennaFlag functions */
DifxAntennaFlag *newDifxAntennaFlagArray(int nFlag);
void deleteDifxAntennaFlagArray(DifxAntennaFlag *df);
void copyDifxAntennaFlag(DifxAntennaFlag *dest, const DifxAntennaFlag *src,
	const int *antennaIdRemap);
DifxAntennaFlag *mergeDifxAntennaFlagArrays(const DifxAntennaFlag *df1, 
	int ndf1, const DifxAntennaFlag *df2, int ndf2, 
	const int *antennaIdRemap);

/* DifxInput functions */
DifxInput *newDifxInput();
void deleteDifxInput(DifxInput *D);
void printDifxInput(const DifxInput *D);
DifxInput *loadDifxInput(const char *fileprefix);
DifxInput *updateDifxInput(DifxInput *D);
DifxInput *mergeDifxInputs(const DifxInput *D1, const DifxInput *D2);
int DifxInputGetSourceId(const DifxInput *D, double mjd);
int DifxInputGetAntennaId(const DifxInput *D, const char *antName);

#endif
