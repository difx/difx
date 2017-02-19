/***************************************************************************
 *   Copyright (C) 2017 by Walter Brisken                                  *
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
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/parsevis.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <glob.h>
#include "difx_input.h"
#include "parsevis.h"

const char program[] = "avgDiFX";
const char author[] = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "2015 Nov 08";

void usage(const char *pgm)
{
	fprintf(stderr, "\n%s %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to average visibility data from two difx filesets\n\n");
	fprintf(stderr, "Usage: <Difx Fileset 1> <Difx Fileset 2> <Output Difx Fileset>\n\n");
	fprintf(stderr, "A file set is specified either by its .input file, or by the\n");
	fprintf(stderr, "portion of the .input file before \".input\".\n\n");
	fprintf(stderr, "The first two filesets must exist, have identical parameters,\n");
	fprintf(stderr, "and should overlap in time.  The files from the first of these\n");
	fprintf(stderr, "will be duplicated to form the scaffold for the output fileset\n\n");
}

typedef struct
{
	unsigned int sync;
	unsigned int version;
	int baselineId;
	int mjd;
	double sec;
	int configId;
	int sourceId;
	int freqId;
	char pol[4];
	int binId;
	double weight;
	double uvw[3];		/* [m] */
} BinaryHeader;

int readBinaryHeader(BinaryHeader *H, FILE *in)
{
	int v;

	H->pol[2] = 0;
	v = fread(&H->sync, sizeof(int), 1, in);
	if(v != 1)
	{
		return -1;
	}
	if(H->sync != VISRECORD_SYNC_WORD_DIFX2)
	{
		return -2;
	}
	v = fread(&H->version, sizeof(int), 1, in);
	if(v != 1)
	{
		return -3;
	}
	if(H->version != 1)
	{
		return -4;
	}
	v  = fread(&H->baselineId, sizeof(int), 1, in);
	v += fread(&H->mjd, sizeof(int), 1, in);
	v += fread(&H->sec, sizeof(double), 1, in);
	v += fread(&H->configId, sizeof(int), 1, in);
	v += fread(&H->sourceId, sizeof(int), 1, in);
	v += fread(&H->freqId, sizeof(int), 1, in);
	v += fread(H->pol, 1, 2, in);
	v += fread(&H->binId, sizeof(int), 1, in);
	v += fread(&H->weight, sizeof(double), 1, in);
	v += fread(H->uvw, sizeof(double), 3, in);
	if(v != 13)
	{
		return -4;
	}

	return 0;
}

int writeBinaryHeader(const BinaryHeader *H, FILE *out)
{
	int v;

	v  = fwrite(&H->sync, sizeof(int), 1, out);
	v += fwrite(&H->version, sizeof(int), 1, out);
	v += fwrite(&H->baselineId, sizeof(int), 1, out);
	v += fwrite(&H->mjd, sizeof(int), 1, out);
	v += fwrite(&H->sec, sizeof(double), 1, out);
	v += fwrite(&H->configId, sizeof(int), 1, out);
	v += fwrite(&H->sourceId, sizeof(int), 1, out);
	v += fwrite(&H->freqId, sizeof(int), 1, out);
	v += fwrite(H->pol, 1, 2, out);
	v += fwrite(&H->binId, sizeof(int), 1, out);
	v += fwrite(&H->weight, sizeof(double), 1, out);
	v += fwrite(H->uvw, sizeof(double), 3, out);
	if(v != 15)
	{
		return -1;
	}

	return 0;
}

static int polval(char pol)
{
	switch(pol)
	{
	case 'R': return 1;
	case 'L': return 2;
	case 'X': return 3;
	case 'Y': return 4;
	default:  return 0;
	}
}

static inline void copyBinaryHeader(BinaryHeader *dest, const BinaryHeader *src)
{
	memcpy(dest, src, sizeof(BinaryHeader));
}

/* < 0 means H1 < H2, 0 means H1 = H2, > 0 means H1 > H2 */
int compareBinaryHeaders(const BinaryHeader *H1, const BinaryHeader *H2)
{
	double dt;
	int d;

	dt = (H1->mjd - H2->mjd)*86400 + (H1->sec - H2->sec);
	if(dt < 0.0)
	{
		return -1;
	}
	if(dt > 0.0)
	{
		return 1;
	}
	/* autocorrs come after cross corrs */
	d = 0;
	if(H2->baselineId % 257 != 0)
	{
		++d;
	}
	if(H1->baselineId % 257 != 0)
	{
		--d;
	}
	if(d < 0)
	{
		return -2;
	}
	if(d > 0)
	{
		return 2;
	}
	d = H1->baselineId - H2->baselineId;
	if(d < 0)
	{
		return -3;
	}
	if(d > 0)
	{
		return 3;
	}
	d = H1->freqId - H2->freqId;
	if(d < 0)
	{
		return -4;
	}
	if(d > 0)
	{
		return 4;
	}
	d = (polval(H1->pol[0]) - polval(H2->pol[0]))*10 + (polval(H1->pol[1]) - polval(H2->pol[1]));
	if(H1->baselineId % 257 == 0)
	{
		if(H1->pol[0] == H1->pol[1])
		{
			d -= 100;
		}
		if(H2->pol[0] == H2->pol[1])
		{
			d += 100;
		}
	}
	if(d < 0)
	{
		return -5;
	}
	if(d > 0)
	{
		return 5;
	}

	return 0;
}

void printBinaryHeaderLong(const BinaryHeader *H)
{
	printf("DiFX Binary Header\n");
	printf("  sync = 0x%8x\n", H->sync);
	printf("  baselineId = %d\n", H->baselineId);
	printf("  mjd = %d\n", H->mjd);
	printf("  sec = %8.2f\n", H->sec);
	printf("  configId = %d\n", H->configId);
	printf("  sourceId = %d\n", H->sourceId);
	printf("  freqId = %d\n", H->freqId);
	printf("  pol = %s\n", H->pol);
	printf("  binId = %d\n", H->binId);
	printf("  weight = %5.3f\n", H->weight);
	printf("  U = %-6.4e\n", H->uvw[0]);
	printf("  V = %-6.4e\n", H->uvw[1]);
	printf("  W = %-6.4e\n", H->uvw[2]);
}

void printBinaryHeaderShort(const BinaryHeader *H)
{
	printf("%5d %5d %8.2f %2d %2d %2d %s %2d %5.3f (%-6.4e, %-6.4e, %-6.4e)\n", H->baselineId, H->mjd, H->sec, H->configId, H->sourceId, H->freqId, H->pol, H->binId, H->weight, H->uvw[0], H->uvw[1], H->uvw[2]);
}

typedef struct
{
	FILE *in;
	DifxInput *D;
	char outputFile[DIFXIO_FILENAME_LENGTH];
	BinaryHeader curHeader, lastHeader;
	int n;	/* headers read to date */
	int nChan;
	float *data;
} AverageInput;

void deleteAverageInput(AverageInput *A)
{
	if(A)
	{
		if(A->D)
		{
			deleteDifxInput(A->D);
		}
		if(A->in)
		{
			fclose(A->in);
		}
		if(A->data)
		{
			free(A->data);
		}
		free(A);
	}
}

AverageInput *openAverageInput(const char *filename)
{
	AverageInput *A;
	glob_t G;
	char pattern[DIFXIO_FILENAME_LENGTH];

	A = (AverageInput *)calloc(1, sizeof(AverageInput));

	A->D = loadDifxInput(filename);
	if(!A->D)
	{
		fprintf(stderr, "Cannot open DiFX fileset %s for read\n", filename);

		deleteAverageInput(A);

		return 0;
	}
	A->D = updateDifxInput(A->D, 0);
	if(!A->D)
	{
		fprintf(stderr, "Update failed for DiFX fileset %s.  Quitting\n", filename);

		deleteAverageInput(A);
		
		return 0;
	}

	A->nChan = A->D->freq->nChan;
	A->data = (float *)malloc(A->nChan*2*sizeof(float));

	snprintf(pattern, DIFXIO_FILENAME_LENGTH, "%s/DIFX*", A->D->job->outputFile);
	glob(pattern, 0, 0, &G);

	if(G.gl_pathc != 1)
	{
		fprintf(stderr, "Need exactly one DIFX* file in %s; found %d\n", A->D->job->outputFile, (int)(G.gl_pathc));

		globfree(&G);
		deleteAverageInput(A);

		return 0;
	}

	snprintf(A->outputFile, DIFXIO_FILENAME_LENGTH, "%s", G.gl_pathv[0]);
	globfree(&G);

printf("Opening %s\n", A->outputFile);
	A->in = fopen(A->outputFile, "r");
	if(!A->in)
	{
		fprintf(stderr, "Cannot open DiFX data file %s for read\n", A->D->job->outputFile);

		deleteAverageInput(A);

		return 0;
	}

	return A;
}

int readAverageInput(AverageInput *A)
{
	int rv;

	copyBinaryHeader(&A->lastHeader, &A->curHeader);
	rv = readBinaryHeader(&A->curHeader, A->in);
	if(rv != 0)
	{
		if(feof(A->in))
		{
			return -1;
		}
		else
		{
			fprintf(stderr, "Error reading header %d: %d\n", A->n, rv);
	
			return -2;
		}
	}
	rv = fread(A->data, 2*sizeof(float), A->nChan, A->in);
	if(rv != A->nChan)
	{
		fprintf(stderr, "Cannot read data array %d: %d/%d complex values read\n", A->n, rv, A->nChan);
	
		return -3;
	}

	if(A->n != 0)
	{
		rv = compareBinaryHeaders(&A->lastHeader, &A->curHeader);
		if(rv >= 0)
		{
			printf("Weird: last two headers were in the wrong order; cmp=%d:\n", rv);
			printf("   %8d : ", A->n);
			printBinaryHeaderShort(&A->lastHeader);
			printf("   %8d : ", A->n+1);
			printBinaryHeaderShort(&A->curHeader);

			return -4;
		}
	}

	++A->n;

	return 0;
}

/* copies, removing .input if need be */
void arg2fileset(char *dest, const char *src, int length)
{
	int l;

	snprintf(dest, length, "%s", src);

	l = strlen(dest);
	if(l > 6)
	{
		if(strcmp(dest+l-6, ".input") == 0)
		{
			dest[l-6] = 0;
		}
	}
}

int main(int argc, char **argv)
{
	char path[DIFXIO_FILENAME_LENGTH];
	char outputFilename[DIFXIO_FILENAME_LENGTH];
	char cmd[2*DIFXIO_FILENAME_LENGTH];
	FILE *out;
	int rv, c, i;
	const char *p;
	AverageInput *A1, *A2;
	int n0=0, n1=0, n2=0;
	char inputFileset1[DIFXIO_FILENAME_LENGTH] = "";
	char inputFileset2[DIFXIO_FILENAME_LENGTH] = "";
	char outputFileset[DIFXIO_FILENAME_LENGTH] = "";

	if(argc != 4)
	{
		usage(argv[0]);

		return EXIT_SUCCESS;
	}

	arg2fileset(inputFileset1, argv[1], DIFXIO_FILENAME_LENGTH);
	arg2fileset(inputFileset2, argv[2], DIFXIO_FILENAME_LENGTH);
	arg2fileset(outputFileset, argv[3], DIFXIO_FILENAME_LENGTH);

	A1 = openAverageInput(inputFileset1);
	if(!A1)
	{
		return EXIT_FAILURE;
	}

	A2 = openAverageInput(inputFileset2);
	if(!A2)
	{
		deleteAverageInput(A1);

		return EXIT_FAILURE;
	}

	if(!areDifxInputsCompatible(A1->D, A2->D, 0))
	{
		fprintf(stderr, "Input files are not compatible\n");

		deleteAverageInput(A1);
		deleteAverageInput(A2);

		return EXIT_FAILURE;
	}

	rv = readAverageInput(A1);
	if(rv != 0)
	{
		fprintf(stderr, "Error reading first header1: %d\n", rv);

		deleteAverageInput(A1);
		deleteAverageInput(A2);
	
		return EXIT_FAILURE;
	}

	rv = readAverageInput(A2);
	if(rv != 0)
	{
		fprintf(stderr, "Error reading first header2: %d\n", rv);

		deleteAverageInput(A1);
		deleteAverageInput(A2);
	
		return EXIT_FAILURE;
	}

	printf("First file, first ");
	printBinaryHeaderLong(&A1->curHeader);
	printf("Second file, first ");
	printBinaryHeaderLong(&A2->curHeader);

	if(outputFileset[0] == '/')
	{
		path[0] = 0;
	}
	else
	{
		getcwd(path, DIFXIO_FILENAME_LENGTH);
	}

	p = 0;
	for(i = 0; A1->outputFile[i]; ++i)
	{
		if(A1->outputFile[i] == '/')
		{
			p = A1->outputFile + i + 1;
		}
	}

	if(p == 0)
	{
		fprintf(stderr, "WEIRD! p = 0\n");

		return EXIT_FAILURE;
	}

	snprintf(outputFilename, DIFXIO_FILENAME_LENGTH, "%s/%s.difx/%s", path, outputFileset, p); 

	snprintf(A1->D->job->inputFile, DIFXIO_FILENAME_LENGTH, "%s/%s.input", path, outputFileset);
	snprintf(A1->D->job->calcFile, DIFXIO_FILENAME_LENGTH, "%s/%s.calc", path, outputFileset);
	snprintf(A1->D->job->threadsFile, DIFXIO_FILENAME_LENGTH, "%s/%s.threads", path, outputFileset);
	snprintf(A1->D->job->imFile, DIFXIO_FILENAME_LENGTH, "%s/%s.im", path, outputFileset);
	snprintf(A1->D->job->outputFile, DIFXIO_FILENAME_LENGTH, "%s/%s.difx", path, outputFileset);

	writeDifxCalc(A1->D);
	writeDifxInput(A1->D);
	writeDifxIM(A1->D);

	rv = mkdir(A1->D->job->outputFile, 0700);
	if(rv != 0)
	{
		fprintf(stderr, "Error: cannot make directory: %s\n", A1->D->job->outputFile);

		deleteAverageInput(A1);
		deleteAverageInput(A2);

		return EXIT_FAILURE;
	}

	snprintf(cmd, 2*DIFXIO_FILENAME_LENGTH, "cp -f %s.difx/PCAL* %s.difx", inputFileset1, outputFileset);
	system(cmd);

	out = fopen(outputFilename, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", outputFileset);

		deleteAverageInput(A1);
		deleteAverageInput(A2);

		return EXIT_FAILURE;
	}

	for(;;)
	{
		c = compareBinaryHeaders(&A2->curHeader, &A2->curHeader);

		if(c == 0)
		{
			rv = writeBinaryHeader(&A1->curHeader, out);
			if(rv != 0)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			
			for(i = 0; i < 2*A1->nChan; ++i)
			{
				A1->data[i] = 0.5*(A1->data[i] + A2->data[i]);
			}
			rv = fwrite(A1->data, 2*sizeof(float), A1->nChan, out);
			if(rv != A1->nChan)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			++n0;
		}
		else if(c <= 0)
		{
			rv = writeBinaryHeader(&A1->curHeader, out);
			if(rv != 0)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			
			rv = fwrite(A1->data, 2*sizeof(float), A1->nChan, out);
			if(rv != A1->nChan)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			++n1;
		}
		else
		{
			rv = writeBinaryHeader(&A2->curHeader, out);
			if(rv != 0)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			
			rv = fwrite(A2->data, 2*sizeof(float), A2->nChan, out);
			if(rv != A2->nChan)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			++n2;
		}

		if(c <= 0)
		{
			rv = readAverageInput(A1);
			if(rv < 0)
			{
				break;
			}
		}

		if(c >= 0)
		{
			rv = readAverageInput(A2);
			if(rv < 0)
			{
				break;
			}
		}
	}

	printf("%d records averaged from both files\n", n0);
	printf("%d records copied from first file\n", n1);
	printf("%d records copied from second file\n", n2);

	deleteAverageInput(A1);
	deleteAverageInput(A2);
	fclose(out);

	return 0;
}
