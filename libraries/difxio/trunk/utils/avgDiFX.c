#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parsevis.h"

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

int main(int argc, char **argv)
{
	FILE *in1, *in2, *out;
	BinaryHeader cH1, lH1;
	BinaryHeader cH2, lH2;
	int n1 = 0;
	int n2 = 0;
	int nChan = 320;
	int rv;
	int c;
	float *data1;
	float *data2;

	if(argc < 4)
	{
		fprintf(stderr, "Seek help\n");

		return EXIT_SUCCESS;
	}
	in1 = fopen(argv[1], "r");
	if(!in1)
	{
		fprintf(stderr, "Cannot open %s for read\n", argv[1]);

		return EXIT_FAILURE;
	}

	in2 = fopen(argv[2], "r");
	if(!in2)
	{
		fprintf(stderr, "Cannot open %s for read\n", argv[2]);

		return EXIT_FAILURE;
	}

	out = fopen(argv[3], "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", argv[3]);

		return EXIT_FAILURE;
	}

	data1 = (float *)malloc(nChan*2*sizeof(float));
	data2 = (float *)malloc(nChan*2*sizeof(float));

	rv = readBinaryHeader(&cH1, in1);
	if(rv != 0)
	{
		fprintf(stderr, "Error reading first header1: %d\n", rv);
	
		return EXIT_FAILURE;
	}
	rv = fread(data1, 2*sizeof(float), nChan, in1);
	if(rv != nChan)
	{
		fprintf(stderr, "Cannot read first data1 array: %d/%d complex values read\n", rv, nChan);
	
		return EXIT_FAILURE;
	}

	rv = readBinaryHeader(&cH2, in2);
	if(rv != 0)
	{
		fprintf(stderr, "Error reading first header1: %d\n", rv);
	
		return EXIT_FAILURE;
	}
	rv = fread(data2, 2*sizeof(float), nChan, in2);
	if(rv != nChan)
	{
		fprintf(stderr, "Cannot read first data2 array: %d/%d complex values read\n", rv, nChan);
	
		return EXIT_FAILURE;
	}
	printf("First file, first ");
	printBinaryHeaderLong(&cH1);
	printf("Second file, first ");
	printBinaryHeaderLong(&cH2);
	printf("1 : %8d : ", n1);
	printBinaryHeaderShort(&cH1);
	printf("2 : %8d : ", n2);
	printBinaryHeaderShort(&cH2);

	++n1;
	++n2;

	for(;;)
	{
		c = compareBinaryHeaders(&cH1, &cH2);

		if(c == 0)
		{
			int i;
			
			rv = writeBinaryHeader(&cH1, out);
			if(rv != 0)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			
			for(i = 0; i < 2*nChan; ++i)
			{
				data1[i] = 0.5*(data1[i] + data2[i]);
			}
			rv = fwrite(data1, 2*sizeof(float), nChan, out);
			if(rv != nChan)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
		}
		else if(c <= 0)
		{
			rv = writeBinaryHeader(&cH1, out);
			if(rv != 0)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			
			rv = fwrite(data1, 2*sizeof(float), nChan, out);
			if(rv != nChan)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
		}
		else
		{
			rv = writeBinaryHeader(&cH2, out);
			if(rv != 0)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
			
			rv = fwrite(data2, 2*sizeof(float), nChan, out);
			if(rv != nChan)
			{
				fprintf(stderr, "Problem writing.  Disk full?\n");

				break;
			}
		}

		if(c <= 0)
		{
			copyBinaryHeader(&lH1, &cH1);
			rv = readBinaryHeader(&cH1, in1);
			if(rv == -1)
			{
				break;
			}
			if(rv != 0)
			{
				fprintf(stderr, "Error reading header1 num %d: %d\n", n1, rv);
				break;
			}
			rv = fread(data1, 2*sizeof(float), nChan, in1);
			if(rv != nChan)
			{
				fprintf(stderr, "Cannot read data1 array %d: %d/%d complex values read\n", n1, rv, nChan);
				break;
			}

			//printf("1 : %8d : ", n1);
			//printBinaryHeaderShort(&cH1);

			rv = compareBinaryHeaders(&lH1, &cH1);
			if(rv >= 0)
			{
				printf("Weird: last two header1s were in the wrong order; cmp=%d:\n", rv);
				printf("  1 : %8d : ", n1-1);
				printBinaryHeaderShort(&lH1);
				printf("  1 : %8d : ", n1);
				printBinaryHeaderShort(&cH1);

				break;
			}

			++n1;
		}

		if(c >= 0)
		{
			copyBinaryHeader(&lH2, &cH2);
			rv = readBinaryHeader(&cH2, in2);
			if(rv == -1)
			{
				break;
			}
			if(rv != 0)
			{
				fprintf(stderr, "Error reading header2 num %d: %d\n", n2, rv);
				break;
			}
			rv = fread(data2, 2*sizeof(float), nChan, in2);
			if(rv != nChan)
			{
				fprintf(stderr, "Cannot read data2 array %d: %d/%d complex values read\n", n2, rv, nChan);
				break;
			}

			//printf("2 : %8d : ", n2);
			//printBinaryHeaderShort(&cH2);

			rv = compareBinaryHeaders(&lH2, &cH2);
			if(rv >= 0)
			{
				printf("Weird: last two header2s were in the wrong order; cmp=%d:\n", rv);
				printf("  2 : %8d : ", n2-1);
				printBinaryHeaderShort(&lH2);
				printf("  2 : %8d : ", n2);
				printBinaryHeaderShort(&cH2);

				break;
			}

			++n2;
		}
	}

	free(data1);
	free(data2);
	fclose(in1);
	fclose(in2);
	fclose(out);

	return 0;
}
