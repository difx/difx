#ifndef __ANTENNA_DB_H__
#define __ANTENNA_DB_H__

#include <stdio.h>

#define ANTENNA_DB_NAME_LENGTH	16

typedef struct
{
	char name[ANTENNA_DB_NAME_LENGTH];
	double x, y, z;				/* ITRF coords [m] */
	double diameter;			/* [m] */
} AntennaDBEntry;

void fprintAntennaDBEntry(FILE *out, const AntennaDBEntry *ae);

const AntennaDBEntry *antennaDBGetByIndex(unsigned int index);

const AntennaDBEntry *antennaDBGetByXYZ(double x, double y, double z);

const AntennaDBEntry *antennaDBGetByName(const char *name);

void ecef2lla(double *lat, double *lon, double *alt, double x, double y, double z);

#endif
