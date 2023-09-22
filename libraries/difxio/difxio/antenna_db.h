#ifndef __ANTENNA_DB_H__
#define __ANTENNA_DB_H__

#include <stdio.h>
#include <stdint.h>
#include "difx_input.h"

#define ANTENNA_DB_NAME_LENGTH		16
#define ANTENNA_DB_MOUNT_LENGTH		8

#ifdef __cplusplus
extern "C" {
#endif

/* Antennas can be members of one or more of the following groups */

#define VLBI_GROUP_HSA		0x0001		
#define VLBI_GROUP_VLBA		0x0002		/* All VLBA also in HSA */
#define VLBI_GROUP_EVN		0x0004
#define VLBI_GROUP_LBA		0x0008
#define VLBI_GROUP_EAVN		0x0010
#define VLBI_GROUP_KVN		0x0020		/* All KVN inside EAVN */
#define VLBI_GROUP_CVN		0x0040		/* All CVN inside EAVN */
#define VLBI_GROUP_VERA		0x0080		/* All VERA inside EAVN */
#define VLBI_GROUP_VLA		0x0100
#define VLBI_GROUP_SX		0x0200
#define VLBI_GROUP_VGOS		0x0400
#define VLBI_GROUP_GMVA		0x0800
#define VLBI_GROUP_EHT		0x1000
#define VLBI_GROUP_DSN		0x2000


#define VLBI_GROUP_ARRAY	0x200000	/* Phased array */
#define VLBI_GROUP_DEFUNCT	0x400000	/* Antenna that is no longer operational */

typedef struct
{
	char name[ANTENNA_DB_NAME_LENGTH];
	char ivsName[ANTENNA_DB_NAME_LENGTH];		/* Name as used in IVS situations */
	double x, y, z;					/* ITRF coords [m]; note: not precise enough for correlation! */
	double diameter;				/* [m] */
	double arrayExtent;				/* [km] ; radius of array from the given xyz.  0.0 for single dishes */
	char mountType[ANTENNA_DB_MOUNT_LENGTH];	/* leave blank for AzEl */
	uint32_t group;					/* bitmask -- see bitfield description above */
} AntennaDBEntry;

/* recognized mount types: "" -> "AZEL", "EQUA", "SPACE", "XYEW", "NASR", "NASL", "XYNS" */


void fprintAntennaDBEntry(FILE *out, const AntennaDBEntry *ae);

const AntennaDBEntry *antennaDBGetByIndex(unsigned int index);

const AntennaDBEntry *antennaDBGetByXYZ(double x, double y, double z);

const AntennaDBEntry *antennaDBGetByName(const char *name);

const AntennaDBEntry *antennaDBGetByIVSName(const char *ivsName);

void ecef2lla(double *lat, double *lon, double *alt, double x, double y, double z);

int isAntennaInGroup(double x, double y, double z, uint32_t group);

int isDifxAntennaInGroup(const DifxAntenna *da, uint32_t group);

#ifdef __cplusplus
}
#endif


#endif
