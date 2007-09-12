#ifndef __PARSE_VIS_H__
#define __PARSE_VIS_H__

#include <stdio.h>
#include <complex.h>
#include "difxio/parsedifx.h"

typedef struct
{
	FILE *infile;			/* file pointer */
	DifxParameters *params;		/* structure containing text params */
	int nchan;			/* number of channels to expect */
	int visnum;			/* counter of number of vis */
	float complex *visdata;		/* pointer to nchan complex values */
} DifxVisRecord;

/* open difx file and return pointer to vis structure */
DifxVisRecord *newDifxVisRecord(const char *filename, int nchan);

/* close difx file and delete any allocated resources */
void deleteDifxVisRecord(DifxVisRecord *vis);

/* load next vis record  return -1 if eof */
int DifxVisRecordgetnext(DifxVisRecord *vis);

/* find next occurance of given parameters */
int DifxVisRecordfindnext(DifxVisRecord *vis, int baseline, int freqid,
	const char *pol);


#endif
