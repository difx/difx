/***************************************************************************
 *   Copyright (C) 2007-2013 by Walter Brisken & Adam Deller               *
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
// $Id: difx_input.h 5102 2013-01-07 23:20:21Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/difx_input.h $
// $LastChangedRevision: 5102 $
// $Author: WalterBrisken $
// $LastChangedDate: 2013-01-07 16:20:21 -0700 (Mon, 07 Jan 2013) $
//
//============================================================================

/*      #ifndef __DIFX_INPUT_H__               */
#define __DIFX_INPUT_H__

#include <stdio.h>
#include "parsedifx.h"

#define MAX_MODEL_ORDER			5
#define MAX_PHS_CENTRES			1000
#define MAX_ABER_CORR_STRING_LENGTH	16
#define MAX_DATA_SOURCE_NAME_LENGTH	16
#define MAX_ANTENNA_MOUNT_NAME_LENGTH	8
#define MAX_SAMPLING_NAME_LENGTH	16
#define MAX_TONE_SELECTION_STRING_LENGTH 12

#define DIFXIO_FILENAME_LENGTH		256
#define DIFXIO_NAME_LENGTH		32
#define DIFXIO_CALCODE_LENGTH		4
#define DIFXIO_VERSION_LENGTH		64
#define DIFXIO_HOSTNAME_LENGTH		32
#define DIFXIO_OBSCODE_LENGTH		8
#define DIFXIO_SESSION_LENGTH		8
#define DIFXIO_TAPER_LENGTH		8
#define DIFXIO_SHELF_LENGTH		8

#define DIFXIO_POL_R			0x01
#define DIFXIO_POL_L			0x02
#define DIFXIO_POL_X			0x10
#define DIFXIO_POL_Y			0x20
#define DIFXIO_POL_ERROR		0x100
#define DIFXIO_POL_RL			(DIFXIO_POL_R | DIFXIO_POL_L)
#define DIFXIO_POL_XY			(DIFXIO_POL_X | DIFXIO_POL_Y)

#ifdef __cplusplus
extern "C" {
#endif



/* Notes about antenna numbering
 *
 * antennaId will typically refer to the index to the DifxInput array called
 * antenna[].  In general, this list will not be in the same order as the
 * antennas as listed in .input file TELESCOPE tables due to both sorting
 * and combining of multiple jobs.
 *
 * Some arrays take as indicies the original antenna index as found in .input
 * files.  These are:  DifxConfig.baselineFreq2IF[][][] and
 * DifxConfig.ant2dsId[][]
 */

/* keep this current with aberCorrStrings in difx_job.c */
enum AberCorr
{
	AberCorrUncorrected = 0,
	AberCorrApproximate,
	AberCorrExact,
	AberCorrNoAtmos,
	NumAberCorrOptions	/* must remain as last entry */
};

extern const char aberCorrStrings[][MAX_ABER_CORR_STRING_LENGTH];


/* keep this current with datastreamTypeNames in difx_datastream.c */
enum DataSource
{
	DataSourceNone = 0,
	DataSourceModule,
	DataSourceFile,
	DataSourceNetwork,
	DataSourceFake,
	NumDataSources		/* must remain as last entry */
};

extern const char dataSourceNames[][MAX_DATA_SOURCE_NAME_LENGTH];

/* keep this current with samplingTypeNames in difx_datastream.c */
enum SamplingType
{
	SamplingReal = 0,
	SamplingComplex,	/* "standard" complex sampling: separate quanization of real and imag */
	NumSamplingTypes	/* must remain as last entry */
};

extern const char samplingTypeNames[][MAX_SAMPLING_NAME_LENGTH];

/* keep this current with antennaMountTypeNames in difx_antenna.c */
/* Note that the numbering scheme is based on the FITS-IDI defs, but with XYNS added at end */
/* See AIPS memo 114 for the list of mount types */
enum AntennaMountType
{
	AntennaMountAltAz = 0,
	AntennaMountEquatorial = 1,
	AntennaMountOrbiting = 2,	/* note: uncertain calc support */
	AntennaMountXYEW = 3,		/* Hobart is the prime example */
	AntennaMountNasmythR = 4,	/* note: in calcserver, falls back to azel as is appropriate */
	AntennaMountNasmythL = 5,	/* note: in calcserver, falls back to azel as is appropriate */
	AntennaMountXYNS = 6,		/* note: no FITS-IDI/AIPS support */
	AntennaMountOther = 7,		/* set to this if different from the others */
	NumAntennaMounts		/* must remain as last entry */
};

enum OutputFormatType
{
	OutputFormatDIFX = 0,
	OutputFormatASCII = 1,
	NumOutputFormat			/* must remain as last entry */
};

extern const char antennaMountTypeNames[][MAX_ANTENNA_MOUNT_NAME_LENGTH];

/* keep this current with toneSelectionNames[] in difx_input.c */
enum ToneSelection
{
	ToneSelectionVex = 0,	// trust the vex file	[default]
	ToneSelectionNone,	// Don't pass any tones along
	ToneSelectionEnds,	// send along two tones at edges of the band
	ToneSelectionAll,	// send along all tones
	ToneSelectionSmart,	// like Ends, but try to stay toneGuard MHz away from band edges
	ToneSelectionMost,	// all except those within toneGuard
	ToneSelectionUnknown,	// an error condition

	NumToneSelections	// needs to be at end of list
};

extern const char toneSelectionNames[][MAX_TONE_SELECTION_STRING_LENGTH];


/* Straight from DiFX frequency table */
typedef struct
{
	double freq;		/* (MHz) */
	double bw;		/* (MHz) */
	char sideband;		/* U or L -- net sideband */
        int nChan;
	int specAvg;            /* This is averaging within mpifxcorr  */
	int overSamp;
	int decimation;
	int nTone;		/* Number of pulse cal tones */
	int *tone;		/* Array of tone indices */
} DifxFreq;

/* To become a FITS IF */
typedef struct
{
	double freq;		/* (MHz) */
	double bw;		/* (MHz) */
	char sideband;		/* U or L -- net sideband */
	int nPol;		/* 1 or 2 */
	char pol[2];		/* polarization codes (one per nPol) : L R X or Y. */
} DifxIF;

typedef struct
{
	char fileName[DIFXIO_FILENAME_LENGTH];	/* filename containing polyco data */
	double dm;		/* pc/cm^3 */
	double refFreq;		/* MHz */
	double mjd;		/* center time for first polynomial */
	int nCoef;		/* number of coefficients per polynomial */
	int nBlk;		/* number of minutes spanned by each */
	double p0;		/* reference phase */
	double f0;		/* reference spin frequency */
	double *coef;
} DifxPolyco;

typedef struct
{
	char fileName[DIFXIO_FILENAME_LENGTH];	/* pulsar config filename */
	int nPolyco;		/* number of polyco structures in file */
	DifxPolyco *polyco;	/* individual polyco file contents */
	int nBin;		/* number of pulsar bins */
	double *binEnd;		/* [bin] end phase [0.0, 1.0) of bin */
	double *binWeight;	/* [bin] weight to apply to bin */
	int scrunch;		/* 1 = yes, 0 = no */
} DifxPulsar;

typedef struct
{
	char fileName[DIFXIO_FILENAME_LENGTH];	/* Phased array config filename */
	/* FIXME: next two parameters should become enums */
	char outputType[DIFXIO_NAME_LENGTH];	/* FILTERBANK or TIMESERIES */
	char outputFormat[DIFXIO_NAME_LENGTH];	/* DIFX or VDIF */
	double accTime;		/* Accumulation time in ns for phased array output */
	/* FIXME: below should be part of an enum */
	int complexOutput;	/* 1=true (complex output), 0=false (real output) */
	int quantBits;		/* Bits to re-quantise to */
} DifxPhasedArray;

/* From DiFX config table, with additional derived information */
typedef struct
{
	char name[DIFXIO_NAME_LENGTH];  /* name for configuration */
	double tInt;		/* integration time (sec) */
	int subintNS;		/* Length of a subint in nanoseconds */
	int guardNS;		/* "Guard" nanoseconds appended to the end of a send */
	int fringeRotOrder;	/* 0, 1 or 2 */
	int strideLength;	/* Must be integer divisor of number of channels */
	int xmacLength;         /* Must be integer divisor of number of channels */
	int numBufferedFFTs;    /* The number of FFTs to do in a row before XMAC'ing */
	int pulsarId;		/* -1 if not pulsar */
	int phasedArrayId;	/* -1 if not phased array mode */
	int nPol;		/* number of pols in datastreams (1 or 2) */
	char pol[2];		/* the polarizations */
	int polMask;		/* bit field using DIFX_POL_x from above */
	int doPolar;		/* >0 if cross hands to be correlated */
	int doAutoCorr;		/* >0 if autocorrelations are to be written to disk */
	int quantBits;		/* 1 or 2 */
	int nAntenna;
	int nDatastream;	/* number of datastreams attached */
	int nBaseline;		/* number of baselines */
	int *datastreamId;	/* 0-based; datastream table indx */
				/* -1 terminated [ds # < nDatastream]  */
	int *baselineId;	/* baseline table indicies for this config */
				/* -1 terminated [bl # < nBaseline] */
	
	/* FIXME: Really these don't belong in here.  Someday (DiFX-3?) move these out. */
	int nIF;		/* number of FITS IFs to create */
	DifxIF *IF;		/* FITS IF definitions */
	int fitsFreqId;		/* 0-based number -- unique FITS IF[] index NOT AN INDEX TO DifxFreq[]! */
	int *freqId2IF;		/* map from freq table [0 to nFreq] index to IF [0 to nIF-1] or -1 */
				/* a value of -1 indicates this Freq is not used */
	int *freqIdUsed;	/* Is the Freq table index used by this config? */

	int *ant2dsId;		/* map from .input file antenna# to internal
				 * DifxDatastream Id. [0..nAntenna-1]
				 * this should be used only in conjunction
				 * with .difx/ antenna numbers! */
} DifxConfig;

typedef struct
{
	DifxStringArray sourceName;	/* (DiFX) name(s) of source, optional */
	DifxStringArray scanId;		/* Scan identifier(s) from vex file, optional */
	char calCode[DIFXIO_CALCODE_LENGTH];  /* calCode, optional */
	int qual;		/* Source qualifier, optional */
	double mjdStart;	/* start time, optional */
	double mjdStop; 	/* stop time, optional */
	char configName[DIFXIO_NAME_LENGTH];  /* Name of the configuration to which 
				   this rule is applied */	
} DifxRule;

typedef struct
{
	int antennaId;		/* index to D->antenna */
	float tSys;		/* 0.0 for VLBA DiFX */
	char dataFormat[DIFXIO_NAME_LENGTH];   /* e.g., VLBA, MKIV, ... */

	enum SamplingType dataSampling; /* REAL or COMPLEX */
	int nFile;              /* number of files */
        char **file;            /* list of files to correlate (if not VSN) */
	int networkPort;        /* eVLBI port for this datastream */
	int windowSize;         /* eVLBI TCP window size */
	int quantBits;		/* quantization bits */
	int dataFrameSize;	/* (bytes) size of formatted data frame */
	enum DataSource dataSource;	/* MODULE, FILE, NET, other? */

	int phaseCalIntervalMHz;/* 0 if no phase cal extraction, otherwise extract every tone */
	int tcalFrequency;	/* 0 if no switched power extraction to be done.  =80 for VLBA */
	int nRecTone;     /* number of pcal tones in the *recorded* baseband*/
	int *recToneFreq; /* Frequency of each pcal tone in the *recorded* baseband in MHz */
	int *recToneOut;  /* bool Recorded pcal written out?*/

	double *clockOffset;	/* (us) [freq] */
	double *freqOffset;	/* Freq offsets for each frequency in Hz */
	
	int nRecFreq;		/* number of freqs recorded in this datastream */
	int nRecBand;		/* number of base band channels recorded */
	int *nRecPol;		/* [recfreq] */
	int *recFreqId;		/* [recfreq] index to DifxFreq table */
	int *recBandFreqId;	/* [recband] index to recFreqId[] */
	char *recBandPolName;	/* [recband] Polarization name (R, L, X or Y) */

        int nZoomFreq;		/* number of "zoom" freqs (within recorded freqs) for this datastream */
	int nZoomBand;		/* number of zoom subbands */
	int *nZoomPol;		/* [zoomfreq] */
	int *zoomFreqId;	/* [zoomfreq] index to DifxFreq table */
	int *zoomBandFreqId;	/* [zoomband] index to zoomfreqId[] */
	char *zoomBandPolName;	/* [zoomband] Polarization name (R, L, X or Y) */
} DifxDatastream;

typedef struct
{
	int dsA, dsB;		/* indices to datastream table */
	int nFreq;
	int *nPolProd;		/* [freq] */

	/* note: band in excess of nRecBand are assumed to be zoom bands */
	int **bandA;		/* [freq][productIndex] */
	int **bandB;		/* [freq][productIndex] */
} DifxBaseline;

typedef struct
{
	char name[DIFXIO_NAME_LENGTH];		/* null terminated */
	int origId;		/* antennaId before a sort */
	double clockrefmjd;	/* Reference time for clock polynomial */
	int clockorder;		/* Polynomial order of the clock model */
	double clockcoeff[MAX_MODEL_ORDER+1];	/* clock polynomial coefficients (us, us/s, us/s^2... */
	enum AntennaMountType mount;
	double offset[3];	/* axis offset, (m) */
	double X, Y, Z;		/* telescope position, (m) */
	double dX, dY, dZ;	/* telescope position derivative, (m/s) */
	int spacecraftId;	/* -1 if not a spacecraft */
	char shelf[DIFXIO_SHELF_LENGTH];  /* shelf location of module; really this should not be here! */
} DifxAntenna;

typedef struct
{
	double ra, dec;		/* radians */
	char name[DIFXIO_NAME_LENGTH];		/* source name */
	char calCode[DIFXIO_CALCODE_LENGTH];	/* usually only 1 char long */
	int qual;		/* source qualifier */
	int spacecraftId;	/* -1 if not spacecraft */
	int numFitsSourceIds;	/* Should be equal to the number of configs */
				/* FITS source IDs are filled in in deriveFitsSourceIds */
	int *fitsSourceIds;	/* 0-based FITS source id */
	double pmRA;		/* arcsec/year */
	double pmDec; 		/* arcsec/year */
	double parallax;	/* arcsec */
	double pmEpoch;		/* MJD */
} DifxSource;

typedef struct
{
	int mjd;		/* day of start of polynomial validity */
	int sec;		/* time (sec) of start of validity */
	int order;		/* order of polynomial -> order+1 terms! */
	int validDuration;	/* (seconds), from mjd, sec */
	double delay[MAX_MODEL_ORDER+1];	/* (us/sec^n); n=[0, order] */
	double dry[MAX_MODEL_ORDER+1];		/* (us/sec^n) */
	double wet[MAX_MODEL_ORDER+1];		/* (us/sec^n) */
	double az[MAX_MODEL_ORDER+1];		/* azimuth (deg) */
	double elcorr[MAX_MODEL_ORDER+1];	/* el (corrected for refraction; i.e., the one used for pointing) (deg) */
	double elgeom[MAX_MODEL_ORDER+1];	/* el (uncorrected for refraction) (deg) */
	double parangle[MAX_MODEL_ORDER+1];	/* parallactic angle (deg) */
	double u[MAX_MODEL_ORDER+1];		/* (m/sec^n) */
	double v[MAX_MODEL_ORDER+1];		/* (m/sec^n) */
	double w[MAX_MODEL_ORDER+1];		/* (m/sec^n) */
} DifxPolyModel;

typedef struct
{
	double mjdStart;			/* (day) */
	double mjdEnd;				/* (day) */
	int startSeconds;			/* Since model reference (top of calc file) */
	int durSeconds; 			/* Duration of the scan */
        char identifier[DIFXIO_NAME_LENGTH];	/* Usually a zero-based number */
	char obsModeName[DIFXIO_NAME_LENGTH];	/* Identifying the "mode" of observation */
	int maxNSBetweenUVShifts;		/* Maximum interval until data must be shifted/averaged */
	int maxNSBetweenACAvg;			/* Maximum interval until autocorrelations are sent/averaged */
        int pointingCentreSrc;  		/* index to source array */
        int nPhaseCentres;      		/* Number of correlation centres */
        int phsCentreSrcs[MAX_PHS_CENTRES]; 	/* indices to source array */
	int orgjobPhsCentreSrcs[MAX_PHS_CENTRES];/* indices to the source array from the original (pre-merged) job */
	int jobId;				/* 0, 1, ... nJob-1 */
        int configId;           		/* to determine freqId */
	int nAntenna;
	int nPoly;
	DifxPolyModel ***im;	/* indexed by [ant][src][poly] */
				/* ant is index of antenna in .input file */
				/*   src ranges over [0...nPhaseCentres] */
				/*   poly ranges over [0 .. nPoly-1] */
				/* NOTE : im[ant] can be zero -> no data */
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
	char name[DIFXIO_NAME_LENGTH];	/* name of spacecraft */
	int nPoint;		/* number of entries in ephemeris */
	sixVector *pos;		/* array of positions and velocities */
} DifxSpacecraft;

typedef struct
{
	double mjd1, mjd2;	/* (day) */
	int antennaId;		/* antenna number (index to D->antenna) */
} DifxAntennaFlag;


/* DifxJob contains information relevant for a particular job.
 * In some cases, multiple jobs will be concatennated and some
 * information about the individual jobs will be needed.
 * In particular, the DifxAntennaFlag contains flags that are
 * generated by .input/.calc generating programs (such as vex2difx)
 * which are job dependent and are to be applied when building the
 * output FITS files.
 */

typedef struct
{
	char difxVersion[DIFXIO_VERSION_LENGTH];  /* Name of difx version in .calc file */
	char difxLabel[DIFXIO_VERSION_LENGTH];    /* Name of difx label in .calc file */
	double jobStart;	/* cjobgen job start time (mjd) */
	double jobStop;		/* cjobgen job start time (mjd) */
	double mjdStart;	/* subjob start time (mjd) */
	double duration;	/* subjob observe duration (sec) */
	int jobId;		/* correlator job number */
	int subjobId;		/* difx specific sub-job id */
	int subarrayId;		/* sub array number of the specified sub-job */
	char obsCode[DIFXIO_OBSCODE_LENGTH];	 /* project name */
	char obsSession[DIFXIO_SESSION_LENGTH];	 /* project session (e.g., A, B, C1) */

	/* FIXME; taperFunction should become an enum */
	char taperFunction[DIFXIO_TAPER_LENGTH]; /* usually "UNIFORM" */
	char calcServer[DIFXIO_HOSTNAME_LENGTH]; /* name of calc server */
	int calcVersion;	/* version number of calc server */
	int calcProgram;	/* RPC program id of calc server */
	int activeDatastreams;
	int activeBaselines;
	int polyOrder;		/* polynomial model order */
	int polyInterval;	/* (sec) length of valid polynomial */
	enum AberCorr aberCorr;	/* level of correction for aberration */
	double dutyCycle;	/* fraction of time in scans */

	int nFlag;
	DifxAntennaFlag *flag;  /* flags to be applied at FITS building time */

	/* Filenames */
	char vexFile[DIFXIO_FILENAME_LENGTH];
	char inputFile[DIFXIO_FILENAME_LENGTH];
	char calcFile[DIFXIO_FILENAME_LENGTH];
	char imFile[DIFXIO_FILENAME_LENGTH];
	char flagFile[DIFXIO_FILENAME_LENGTH];
	char threadsFile[DIFXIO_FILENAME_LENGTH];
	char outputFile[DIFXIO_FILENAME_LENGTH];

	/* Remappings.  These are null arrays unless some renumbering from original values occurred */
	int *jobIdRemap;	/* confusingly, not the same jobId as that in this structure, but rather index to DifxJob */
	int *freqIdRemap;
	int *antennaIdRemap;
	int *datastreamIdRemap;
	int *baselineIdRemap;
	int *pulsarIdRemap;
	int *configIdRemap;
	int *sourceIdRemap;
	int *spacecraftIdRemap;
} DifxJob;

typedef struct
{
	int fracSecondStartTime;/* allow writing of fractional second start time? */
	double mjdStart;	/* start of combined dataset */
	double mjdStop;		/* end of combined dataset */
	double refFreq;		/* some sort of reference frequency, (MHz) */
	int startChan;		/* first (unaveraged) channel to write, only set for difx2fits */
	int specAvg;		/* number of channels to average post corr. */
	int nInChan;		/* number of correlated channels, only set for difx2fits */
	int nOutChan;		/* number of channels to write to FITS, only set for difx2fits */
				/* Statchan, nInChan and nOutChan are all set haphazardly, and
				   will certainly have odd things if not all freqs are the same */
	int visBufferLength;	/* number of visibility buffers in mpifxcorr */

	int nIF;		/* maximum num IF across configs */
	int nPol;		/* maximum num pol across configs */
	int doPolar;		/* 0 if not, 1 if so */
	int nPolar;		/* nPol*(doPolar+1) */
				/* 1 for single pol obs */
				/* 2 for dual pol, parallel hands only */
				/* 4 for full pol */
	double chanBW;		/* MHz common channel bandwidth. 0 if differ */
	int quantBits;		/* 0 if different in configs; or 1 or 2 */
	char polPair[4];	/* "  " if different in configs */
	int dataBufferFactor;
        int nDataSegments;
        enum OutputFormatType outputFormat;

	int nCore;		/* from the .threads file, or zero if no file */
	int *nThread;		/* [coreId]: how many threads to use on each core */

	int nAntenna, nConfig, nRule, nFreq, nScan, nSource, nEOP, nFlag;
	int nDatastream, nBaseline, nSpacecraft, nPulsar, nPhasedArray, nJob;
	DifxJob		*job;
	DifxConfig	*config;
	DifxRule        *rule;
	DifxFreq	*freq;
	DifxAntenna	*antenna;
	DifxScan	*scan;		/* assumed in time order */
	DifxSource	*source;
	DifxEOP		*eop;		/* assumed one per day, optional */
	DifxDatastream	*datastream;
	DifxBaseline    *baseline;
	DifxSpacecraft	*spacecraft;	/* optional table */
	DifxPulsar	*pulsar;	/* optional table */
	DifxPhasedArray	*phasedarray;	/* optional table */
} DifxInput;

/* DifxJob functions */
DifxJob *newDifxJobArray(int nJob);
void deleteDifxJobArray(DifxJob *dj, int nJob);
void printDifxJob(const DifxJob *dj);
void fprintDifxJob(FILE *fp, const DifxJob *dj);
void copyDifxJob(DifxJob *dest, const DifxJob *src, int *antennaIdRemap);
void generateDifxJobFileBase(DifxJob *dj, char *fileBase);
DifxJob *mergeDifxJobArrays(const DifxJob *dj1, int ndj1,
	const DifxJob *dj2, int ndj2, int *jobIdRemap, 
	int *antennaIdRemap, int *ndj);

/* DifxFreq functions */
DifxFreq *newDifxFreqArray(int nFreq);
void DifxFreqAllocTones(DifxFreq *df, int nTone);
void deleteDifxFreqInternals(DifxFreq *df);
void deleteDifxFreqArray(DifxFreq *df, int nFreq);
void printDifxFreq(const DifxFreq *df);
void fprintDifxFreq(FILE *fp, const DifxFreq *df);
int isSameDifxFreqToneSet(const DifxFreq *df1, const DifxFreq *df2);
int isSameDifxFreq(const DifxFreq *df1, const DifxFreq *df2);
int isDifxIFInsideDifxFreq(const DifxIF *di, const DifxFreq *df);
void copyDifxFreq(DifxFreq *dest, const DifxFreq *src);
int simplifyDifxFreqs(DifxInput *D);
DifxFreq *mergeDifxFreqArrays(const DifxFreq *df1, int ndf1,
	const DifxFreq *df2, int ndf2, int *freqIdRemap, int *ndf);
int writeDifxFreqArray(FILE *out, int nFreq, const DifxFreq *df);

/* DifxAntenna functions */
enum AntennaMountType stringToMountType(const char *str);
DifxAntenna *newDifxAntennaArray(int nAntenna);
void deleteDifxAntennaArray(DifxAntenna *da, int nAntenna);
void printDifxAntenna(const DifxAntenna *da);
void fprintDifxAntenna(FILE *fp, const DifxAntenna *da);
void fprintDifxAntennaSummary(FILE *fp, const DifxAntenna *da);
int isSameDifxAntenna(const DifxAntenna *da1, const DifxAntenna *da2);
int isSameDifxAntennaClock(const DifxAntenna *da1, const DifxAntenna *da2);
int getDifxAntennaShiftedClock(const DifxAntenna *da, double dt, int outputClockSize, double *clockOut);
void copyDifxAntenna(DifxAntenna *dest, const DifxAntenna *src);
DifxAntenna *mergeDifxAntennaArrays(const DifxAntenna *da1, int nda1,
	const DifxAntenna *da2, int nda2, int *antennaIdRemap,
	int *nda);
int writeDifxAntennaArray(FILE *out, int nAntenna, const DifxAntenna *da,
        int doMount, int doOffset, int doCoords, int doClock, int doShelf);

/* DifxDatastream functions */
enum DataSource stringToDataSource(const char *str);
enum SamplingType stringToSamplingType(const char *str);
DifxDatastream *newDifxDatastreamArray(int nDatastream);
void DifxDatastreamAllocFiles(DifxDatastream *ds, int nFile);
void DifxDatastreamAllocFreqs(DifxDatastream *dd, int nReqFreq);
void DifxDatastreamAllocBands(DifxDatastream *dd, int nRecBand);
void DifxDatastreamAllocZoomFreqs(DifxDatastream *dd, int nZoomFreq);
void DifxDatastreamAllocZoomBands(DifxDatastream *dd, int nZoomBand);
void DifxDatastreamAllocPhasecalTones(DifxDatastream *dd, int nTones);
void DifxDatastreamCalculatePhasecalTones(DifxDatastream *dd, const DifxFreq *df);
int DifxDatastreamGetPhasecalTones(double *toneFreq, const DifxDatastream *dd, const DifxFreq *df, int maxCount);
void deleteDifxDatastreamInternals(DifxDatastream *dd);
void deleteDifxDatastreamArray(DifxDatastream *dd, int nDatastream);
void fprintDifxDatastream(FILE *fp, const DifxDatastream *dd);
void printDifxDatastream(const DifxDatastream *dd);
int isSameDifxDatastream(const DifxDatastream *dd1, const DifxDatastream *dd2,
	const int *freqIdRemap, const int *antennaIdRemap);
void copyDifxDatastream(DifxDatastream *dest, const DifxDatastream *src,
	const int *freqIdRemap, const int *antennaIdRemap);
void moveDifxDatastream(DifxDatastream *dest, DifxDatastream *src);
int simplifyDifxDatastreams(DifxInput *D);
DifxDatastream *mergeDifxDatastreamArrays(const DifxDatastream *dd1, int ndd1,
	const DifxDatastream *dd2, int ndd2, int *datastreamIdRemap,
	const int *freqIdRemap, const int *antennaIdRemap, int *ndd);
int writeDifxDatastream(FILE *out, const DifxDatastream *dd);
int DifxDatastreamGetRecBands(DifxDatastream *dd, int freqId, char *pols, int *recBands);
int DifxDatastreamGetZoomBands(DifxDatastream *dd, int freqId, char *pols, int *zoomBands);

/* DifxBaseline functions */
DifxBaseline *newDifxBaselineArray(int nBaseline);
void DifxBaselineAllocFreqs(DifxBaseline *b, int nFreq);
void DifxBaselineAllocPolProds(DifxBaseline *b, int freq, int nPol);
void deleteDifxBaselineInternals(DifxBaseline *db);
void deleteDifxBaselineArray(DifxBaseline *db, int nBaseline);
void fprintDifxBaseline(FILE *fp, const DifxBaseline *db);
void printDifxBaseline(const DifxBaseline *db);
int isSameDifxBaseline(const DifxBaseline *db1, const DifxBaseline *db2,
	const int *datastreamIdRemap);
void copyDifxBaseline(DifxBaseline *dest, const DifxBaseline *src,
	const int *datastreamIdRemap);
void moveDifxBaseline(DifxBaseline *dest, DifxBaseline *src);
int simplifyDifxBaselines(DifxInput *D);
DifxBaseline *mergeDifxBaselineArrays(const DifxBaseline *db1, int ndb1,
	const DifxBaseline *db2, int ndb2, int *baselineIdRemap,
	const int *datastreamIdRemap, int *ndb);
int writeDifxBaselineArray(FILE *out, int nBaseline, const DifxBaseline *db);

/* DifxPolyco functions */
DifxPolyco *newDifxPolycoArray(int nPolyco);
void deleteDifxPolycoInternals(DifxPolyco *dp);
void deleteDifxPolycoArray(DifxPolyco *dp, int nPolyco);
void printDifxPolycoArray(const DifxPolyco *dp, int nPolyco);
void fprintDifxPolycoArray(FILE *fp, const DifxPolyco *dp, int nPolyco);
void copyDifxPolyco(DifxPolyco *dest, const DifxPolyco *src);
DifxPolyco *dupDifxPolycoArray(const DifxPolyco *src, int nPolyco);
int loadPulsarPolycoFile(DifxPolyco **dpArray, int *nPoly, const char *filename);
int loadPulsarConfigFile(DifxInput *D, const char *fileName);
int DifxPolycoArrayGetMaxPolyOrder(const DifxPolyco *dp, int nPolyco);

/* DifxPhasedArray functions */
DifxPhasedArray *newDifxPhasedarrayArray(int nPhasedArray);
DifxPhasedArray *growDifxPhasedarrayArray(DifxPhasedArray *dpa, int origSize);
void deleteDifxPhasedarrayArray(DifxPhasedArray *dpa, int nPhasedArray);
void fprintDifxPhasedArray(FILE *fp, const DifxPhasedArray *dpa);
void printDifxPhasedArray(const DifxPhasedArray *dpa);
int isSameDifxPhasedArray(const DifxPhasedArray *dpa1, 
	const DifxPhasedArray *dpa2);
DifxPhasedArray *dupDifxPhasedarrayArray(const DifxPhasedArray *src, 
	int nPhasedArray);
DifxPhasedArray *mergeDifxPhasedarrayArrays(const DifxPhasedArray *dpa1, 
	int ndpa1, const DifxPhasedArray *dpa2, int ndpa2, 
	int *phasedArrayIdRemap, int *ndpa);

/* DifxPulsar functions */
DifxPulsar *newDifxPulsarArray(int nPulsar);
DifxPulsar *growDifxPulsarArray(DifxPulsar *dp, int origSize);
void deleteDifxPulsarInternals(DifxPulsar *dp);
void deleteDifxPulsarArray(DifxPulsar *dp, int nPulsar);
void fprintDifxPulsar(FILE *fp, const DifxPulsar *dp);
void printDifxPulsar(const DifxPulsar *dp);
int isSameDifxPulsar(const DifxPulsar *dp1, const DifxPulsar *dp2);
DifxPulsar *dupDifxPulsarArray(const DifxPulsar *src, int nPulsar);
DifxPulsar *mergeDifxPulsarArrays(const DifxPulsar *dp1, int ndp1,
	const DifxPulsar *dp2, int ndp2, int *pulsarIdRemap, int *ndp);
int DifxPulsarArrayGetMaxPolyOrder(const DifxPulsar *dp, int nPulsar);

/* DifxConfig functions */
DifxConfig *newDifxConfigArray(int nConfig);
void DifxConfigAllocDatastreamIds(DifxConfig *dc, int nDatastream, int start);
void DifxConfigAllocBaselineIds(DifxConfig *dc, int nBaseline, int start);
void deleteDifxConfigInternals(DifxConfig *dc);
void deleteDifxConfigArray(DifxConfig *dc, int nConfig);
void fprintDifxConfig(FILE *fp, const DifxConfig *dc);
void printDifxConfig(const DifxConfig *dc);
void fprintDifxConfigSummary(FILE *fp, const DifxConfig *dc);
void printDifxConfigSummary(const DifxConfig *dc);
int isSameDifxConfig(const DifxConfig *dc1, const DifxConfig *dc2);
void copyDifxConfig(DifxConfig *dest, const DifxConfig *src,
	const int *baselineIdRemap, const int *datastreamIdRemap,
	const int *pulsarIdRemap);
void moveDifxConfig(DifxConfig *dest, DifxConfig *src);
int simplifyDifxConfigs(DifxInput *D);
DifxConfig *mergeDifxConfigArrays(const DifxConfig *dc1, int ndc1,
	const DifxConfig *dc2, int ndc2, int *configIdRemap,
	const int *baselineIdRemap, const int *datastreamIdRemap,
	const int *pulsarIdRemap, int *ndc);
int DifxConfigCalculateDoPolar(DifxConfig *dc, DifxBaseline *db);
int DifxConfigGetPolId(const DifxConfig *dc, char polName);
int DifxConfigRecBand2FreqPol(const DifxInput *D, int configId,
	int antennaId, int recBand, int *freqId, int *polId);
int writeDifxConfigArray(FILE *out, int nConfig, const DifxConfig *dc, const DifxPulsar *pulsar,
	const DifxPhasedArray *phasedarray);

/* DifxRule functions */
DifxRule *newDifxRuleArray(int nRule);
void deleteDifxRuleArray(DifxRule *dr);
void fprintDifxRule(FILE *fp, const DifxRule *dc);
void printDifxRule(const DifxRule *dr);
int writeDifxRuleArray(FILE *out, const DifxInput *D);
void copyDifxRule(DifxRule * dest, DifxRule * src);
int simplifyDifxRules(DifxInput *D);
int ruleAppliesToScanSource(const DifxRule * dr, const DifxScan * ds, const DifxSource * src);

/* DifxPolyModel functions */
DifxPolyModel ***newDifxPolyModelArray(int nAntenna, int nSrcs, int nPoly);
DifxPolyModel *dupDifxPolyModelColumn(const DifxPolyModel *src, int nPoly);
void deleteDifxPolyModelArray(DifxPolyModel ***dpm, int nAntenna, int nSrcs);
void printDifxPolyModel(const DifxPolyModel *dpm);
void fprintDifxPolyModel(FILE *fp, const DifxPolyModel *dpm);

/* DifxScan functions */
DifxScan *newDifxScanArray(int nScan);
void deleteDifxScanInternals(DifxScan *ds);
void deleteDifxScanArray(DifxScan *ds, int nScan);
void fprintDifxScan(FILE *fp, const DifxScan *ds);
void printDifxScan(const DifxScan *ds);
void fprintDifxScanSummary(FILE *fp, const DifxScan *ds);
void printDifxScanSummary(const DifxScan *ds);
void copyDifxScan(DifxScan *dest, const DifxScan *src,
	const int *sourceIdRemap, const int *jobIdRemap, 
	const int *configIdRemap, const int *antennaIdRemap);
DifxScan *mergeDifxScanArrays(const DifxScan *ds1, int nds1,
	const DifxScan *ds2, int nds2, const int *sourceIdRemap, 
	const int *jobIdRemap, const int *configIdRemap, 
	const int *antennaIdRemap, int *nds);
int getDifxScanIMIndex(const DifxScan *ds, double mjd, double iat, double *dt);
int writeDifxScan(FILE *out, const DifxScan *ds, int scanId, 
	const DifxConfig *dc);
int writeDifxScanArray(FILE *out, int nScan, const DifxScan *ds, 
	const DifxConfig *dc);
int padDifxScans(DifxInput *D);

/* DifxEOP functions */
DifxEOP *newDifxEOPArray(int nEOP);
void deleteDifxEOPArray(DifxEOP *de);
void printDifxEOP(const DifxEOP *de);
void fprintDifxEOP(FILE *fp, const DifxEOP *de);
void printDifxEOPSummary(const DifxEOP *de);
void fprintDifxEOPSummary(FILE *fp, const DifxEOP *de);
void copyDifxEOP(DifxEOP *dest, const DifxEOP *src);
DifxEOP *mergeDifxEOPArrays(const DifxEOP *de1, int nde1, const DifxEOP *de2, int nde2, int *nde);
int writeDifxEOPArray(FILE *out, int nEOP, const DifxEOP *de);

/* DifxSpacecraft functions */
DifxSpacecraft *newDifxSpacecraftArray(int nSpacecraft);
DifxSpacecraft *dupDifxSpacecraftArray(const DifxSpacecraft *src, int n);
void deleteDifxSpacecraftInternals(DifxSpacecraft *ds);
void deleteDifxSpacecraftArray(DifxSpacecraft *ds, int nSpacecraft);
void printDifxSpacecraft(const DifxSpacecraft *ds);
void fprintDifxSpacecraft(FILE *fp, const DifxSpacecraft *ds);
int computeDifxSpacecraftEphemeris(DifxSpacecraft *ds, double mjd0, double deltat, int nPoint, const char *name, const char *naifFile, const char *ephemFile, double ephemStellarAber, double ephemClockError);
DifxSpacecraft *mergeDifxSpacecraft(const DifxSpacecraft *ds1, int nds1, const DifxSpacecraft *ds2, int nds2, int *spacecraftIdRemap, int *nds);
int evaluateDifxSpacecraft(const DifxSpacecraft *sc, int mjd, double fracMjd, sixVector *interpolatedPosition);
int writeDifxSpacecraftArray(FILE *out, int nSpacecraft, DifxSpacecraft *ds);

/* DifxSource functions */
DifxSource *newDifxSourceArray(int nSource);
void deleteDifxSourceArray(DifxSource *ds, int nSource);
void printDifxSource(const DifxSource *ds);
void fprintDifxSource(FILE *fp, const DifxSource *ds);
void printDifxSourceSummary(const DifxSource *ds);
void fprintDifxSourceSummary(FILE *fp, const DifxSource *ds);
int writeDifxSourceArray(FILE *out, int nSource, const DifxSource *ds, int doCalcode, int doQual, int doSpacecraftID);
int isSameDifxSource(const DifxSource *ds1, const DifxSource *ds2);
void copyDifxSource(DifxSource *dest, const DifxSource *src);
DifxSource *mergeDifxSourceArrays(const DifxSource *ds1, int nds1, const DifxSource *ds2, int nds2, int *sourceIdRemap, int *nds);

/* DifxIF functions */
DifxIF *newDifxIFArray(int nIF);
void deleteDifxIFArray(DifxIF *di);
void printDifxIF(const DifxIF *di);
void fprintDifxIF(FILE *fp, const DifxIF *di);
void printDifxIFSummary(const DifxIF *di);
void fprintDifxIFSummary(FILE *fp, const DifxIF *di);
int isSameDifxIF(const DifxIF *di1, const DifxIF *di2);

/* DifxAntennaFlag functions */
DifxAntennaFlag *newDifxAntennaFlagArray(int nFlag);
void deleteDifxAntennaFlagArray(DifxAntennaFlag *df);
void printDifxAntennaFlagArray(const DifxAntennaFlag *df, int nf);
void fprintDifxAntennaFlagArray(FILE *fp, const DifxAntennaFlag *df, int nf);
void copyDifxAntennaFlag(DifxAntennaFlag *dest, const DifxAntennaFlag *src, const int *antennaIdRemap);
DifxAntennaFlag *mergeDifxAntennaFlagArrays(const DifxAntennaFlag *df1, int ndf1, const DifxAntennaFlag *df2, int ndf2, const int *antennaIdRemap, int *ndf);

/* DifxInput functions */
enum ToneSelection stringToToneSelection(const char *str);
DifxInput *newDifxInput();
void deleteDifxInput(DifxInput *D);
void printDifxInput(const DifxInput *D);
void fprintDifxInput(FILE *fp, const DifxInput *D);
void printDifxInputSummary(const DifxInput *D);
void fprintDifxInputSummary(FILE *fp, const DifxInput *D);
void DifxConfigMapAntennas(DifxConfig *dc, const DifxDatastream *ds);
DifxInput *loadDifxInput(const char *filePrefix);
DifxInput *loadDifxCalc(const char *filePrefix);
//DifxInput *deriveSourceTable(DifxInput *D);
DifxInput *allocateSourceTable(DifxInput *D, int length);
DifxInput *updateDifxInput(DifxInput *D);
int areDifxInputsMergable(const DifxInput *D1, const DifxInput *D2);
int areDifxInputsCompatible(const DifxInput *D1, const DifxInput *D2);
DifxInput *mergeDifxInputs(const DifxInput *D1, const DifxInput *D2, int verbose);
int isAntennaFlagged(const DifxJob *J, double mjd, int antennaId);
int DifxInputGetPointingSourceIdByJobId(const DifxInput *D, double mjd, int jobId);
int DifxInputGetPointingSourceIdByAntennaId(const DifxInput *D, double mjd, int antennaId);
int DifxInputGetScanIdByJobId(const DifxInput *D, double mjd, int jobId);
int DifxInputGetScanIdByAntennaId(const DifxInput *D, double mjd, int antennaId);
int DifxInputGetAntennaId(const DifxInput *D, const char *antennaName);
int DifxInputGetDatastreamIdsByAntennaId(int *dsIds, const DifxInput *D, int antennaId, int maxCount);
int DifxInputGetOriginalDatastreamIdsByAntennaIdJobId(int *dsIds, const DifxInput *D, int antennaId, int jobId, int maxCount);
int DifxInputGetMaxTones(const DifxInput *D);
int DifxInputGetMaxPhaseCentres(const DifxInput *D);
int DifxInputGetFreqIdByBaselineFreq(const DifxInput *D, int baselineId, int baselineFreq);
int DifxInputGetDatastreamId(const DifxInput *D, int jobId, int antId);
int DifxInputSortAntennas(DifxInput *D, int verbose);
int DifxInputSimFXCORR(DifxInput *D);
int DifxInputGetPointingCentreSource(const DifxInput *D, int sourceId);
void DifxInputAllocThreads(DifxInput *D, int nCore);
void DifxInputSetThreads(DifxInput *D, int nThread);
int DifxInputLoadThreads(DifxInput *D);
int DifxInputWriteThreads(const DifxInput *D);
int polMaskValue(char polName);

/* Writing functions */
int writeDifxIM(const DifxInput *D);
int writeDifxCalc(const DifxInput *D);
int writeDifxInput(const DifxInput *D);
int writeDifxPulsarFiles(const DifxInput *D);

/* Remap functions */
void fprintRemap(FILE *out, const char *name, const int *Remap);
void printRemap(const char *name, const int *Remap);
int *newRemap(int nItem);
void deleteRemap(int *Remap);
int *dupRemap(const int *Remap);
int sizeofRemap(const int *Remap);
int reverseRemap(const int *Remap, int y);	/* find index corresponding to y */

#ifdef __cplusplus
}
#endif

/*              #endif               */
