#ifndef __CONFIGURATION_H__
#define __CONFIGURATION_H__

#include <difxio/difx_input.h>

#define MAX_FEATURES			100
#define MAX_ANTENNA_PARAMETERS		25
#define MAX_FEATURE_TYPE_STRING_LENGTH	32

/* this file contains info supplied by an externa configuration file */

/* keep up to date with const char FeatureTypeStrings[][] in configuration.c */
enum FeatureType
{
	Feature_Gaussian = 0,	/* <center freq> <FWHM> <peak FD> */
	Feature_Sinc,		/* <center freq> <null-to-null width> <peak FD> */
	Feature_Triangle,	/* <center freq> <null-to-null width> <peak FD> */
	Feature_Box,		/* <center freq> <width> <peak FD> */
	Feature_Tone,		/* <freq> <flux> */

	NumFeatureType	/* list terminator */
};

extern const char FeatureTypeStrings[][MAX_FEATURE_TYPE_STRING_LENGTH];

typedef struct
{
	char name[DIFXIO_NAME_LENGTH];
	double droppedPacketRate;	/* between 0 and 1 */
	double invalidPacketRate;	/* between 0 and 1 */
	double clockOffset;		/* [us] */
	double SEFD;			/* [Jy] */
	int switchedPowerFreq;		/* [Hz] */
	double switchedPowerFrac;	/* fraction of Rx noise; typically 0.02 */
	int pulseCalInterval;		/* [MHz] */
	double pulseCalFrac;		/* total pulse cal power as fraction of system power */
	double pulseCalDelay;		/* [us] */
	/* FIXME: add filter parameters, cross polarization, ... */
} AntennaParameters;

typedef struct
{
	double freq;			/* [MHz] center freq */
	double width;			/* [MHz] full width, somewhat feature dependent */
	union
	{
		double fluxDensity;	/* [Jy] peak flux density (for non-tones) */
		double flux;		/* [Jy.Hz] total tone source flux (for tones) */
	};
	enum FeatureType type;
} Feature;

typedef struct
{
/* source spectrum */
	/* broad-band component */
	double fluxDensity;	/* [Jy] broadband flux density */
	double specIndex;	/* spectral index  S ~ \nu^\alpha */
	double specIndexFreq;	/* [MHz] reference frequency for spectral index */

	/* spectral features */
	Feature features[MAX_FEATURES];
	int nFeature;

	/* antenna-specific parameters */
	AntennaParameters antParms[MAX_ANTENNA_PARAMETERS];
	int nAntParm;

/* other settable parameters */

} Configuration;


AntennaParameters *getAntennaParameters(Configuration *config, const char *antName);

const AntennaParameters *getAntennaParametersConst(const Configuration *config, const char *antName);

Configuration *loadConfigration(const char *filename);

Configuration *newConfiguration();

void deleteConfiguration(Configuration *config);

void printConfiguration(const Configuration *config);

#endif
