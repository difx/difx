#ifndef T110_VERSION
#define T110_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_110_v0 type_110


typedef struct
    {
    int			r_cell[33];		/* Right cell data */
    int			l_cell[33];		/* Left cell data */
    } raw_data;

struct type_110 
    {
    char		record_id[3];		/* Standard 3-digit id */
    char		version_no[2];		/* Standard 2-digit version # */
    char		unused1;		/* Reserved space */
    short		nblocks;		/* Number of raw_data entries */
    char		unused2[2];		/* Padding for alignment */
    char		baseline[2];		/* Standard baseline id */
    short		filenum;		/* Fileset file number */
    char		rootcode[6];		/* Root suffix */
    int			index;			/* Index number for type 101 rec. */
    int			ap;			/* Acc period number */
    int			flag;			/* Up to 32 correlation flags */
    int			status;			/* Up to 32 status bits */
    float		bitshift;		/* bitshift rate (bits/sec) */
    float		fbit;			/* Fractional bitshift at AP center */
    raw_data		data[1];		/* Raw data dump by corr. block */
    };

#endif
