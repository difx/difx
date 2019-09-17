#ifndef __ANTENNA_DB_H__
#define __ANTENNA_DB_H__

#include <stdio.h>

#define ANTENNA_DB_NAME_LENGTH		16
#define ANTENNA_DB_MOUNT_LENGTH		8

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
	char name[ANTENNA_DB_NAME_LENGTH];
	char ivsName[ANTENNA_DB_NAME_LENGTH];		/* Name as used in IVS situations */
	double x, y, z;					/* ITRF coords [m]; note: not precise enough for correlation! */
	double diameter;				/* [m] */
	double arrayExtent;				/* [km] ; radius of array from the given xyz.  0.0 for single dishes */
	char mountType[ANTENNA_DB_MOUNT_LENGTH];	/* leave blank for AzEl */
} AntennaDBEntry;

/* recognized mount types: "" -> "AZEL", "EQUA", "SPACE", "XYEW", "NASR", "NASL", "XYNS" */


void fprintAntennaDBEntry(FILE *out, const AntennaDBEntry *ae);

const AntennaDBEntry *antennaDBGetByIndex(unsigned int index);

const AntennaDBEntry *antennaDBGetByXYZ(double x, double y, double z);

const AntennaDBEntry *antennaDBGetByName(const char *name);

void ecef2lla(double *lat, double *lon, double *alt, double x, double y, double z);

#ifdef __cplusplus
}
#endif


#endif
