/***************************************************************************
 *   Copyright (C) 2023 by Walter Brisken                                  *
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <inttypes.h>
#include <math.h>
#include <vdifio.h>

const char program[] = "vdifMeanRMS";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20231222";

#define OPTIMAL_2BIT_HIGH   3.3359
#define OPTIMAL_2BIT_HIGH2 (3.3359*3.3359)


volatile int die = 0;

typedef struct
{
	unsigned int threadId;
	unsigned int nBit;
	unsigned int dataBytes;	/* data bytes per VDIF frame */
	unsigned int seconds;
	double sum;
	double sum2;
	double n;
} StatsRegister;

StatsRegister *newStatsRegister(int threadId, int nBit, int dataBytes)
{
	StatsRegister *reg;

	reg = (StatsRegister *)calloc(1, sizeof(StatsRegister));
	reg->threadId = threadId;
	reg->nBit = nBit;
	reg->dataBytes = dataBytes;

	return reg;
}

void deleteStatsRegister(StatsRegister *reg)
{
	if(reg)
	{
		free(reg);
	}
}

StatsRegister **newStatsRegisters()
{
	return (StatsRegister **)calloc(VDIF_MAX_THREAD_ID+1, sizeof(StatsRegister *));
}

void deleteStatsRegisters(StatsRegister **regs)
{
	if(regs)
	{
		unsigned int t;

		for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
		{
			if(regs[t])
			{
				deleteStatsRegister(regs[t]);
			}
		}
		free(regs);
	}
}

void fprintStatsRegister(FILE *out, const StatsRegister *reg, unsigned int inc)
{
	double mean, rms;

	mean = reg->sum/reg->n;
	rms = sqrt(reg->sum2/reg->n - mean*mean);

	fprintf(out, "%u %u %f %f\n", reg->threadId, reg->seconds, mean/inc, rms/inc);
}

void resetStatsRegister(StatsRegister *reg)
{
	reg->sum = 0.0;
	reg->sum2 = 0.0;
	reg->n = 0.0;
}

void feedStatsRegister(StatsRegister *reg, const unsigned char *data)
{
	unsigned int i;

	switch(reg->nBit)
	{
	case 1:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			int j;
			unsigned char d;

			d = data[i];
			for(d = data[i], j = 0; j < 4; ++j, d >>= 1)
			{
				reg->sum += (d & 0x01) ? 1 : -1;
			}
			reg->sum2 += 8;
		}
		reg->n += reg->dataBytes*8;
		break;
	case 2:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			int j;
			unsigned char d;

			for(d = data[i], j = 0; j < 4; ++j, d >>= 2)
			{
				switch(d & 0x03)
				{
				case 0:
					reg->sum -= OPTIMAL_2BIT_HIGH;
					reg->sum2 += OPTIMAL_2BIT_HIGH2;
					break;
				case 1:
					reg->sum -= 1;
					reg->sum2 += 1;
					break;
				case 2:
					reg->sum += 1;
					reg->sum2 += 1;
					break;
				case 3:
					reg->sum += OPTIMAL_2BIT_HIGH;
					reg->sum2 += OPTIMAL_2BIT_HIGH2;
					break;
				}
			}
		}
		reg->n += reg->dataBytes*4;
		break;
	case 4:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			unsigned char d;
			double x;

			d = data[i];
			x = (d & 0x0F) - 7.5;
			reg->sum += x;
			reg->sum2 += x*x;
			d >>= 4;
			x = (d & 0x0F) - 7.5;
			reg->sum += x;
			reg->sum2 += x*x;
		}
		reg->n += reg->dataBytes*2;
		break;
	case 8:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			double x;
			x = data[i] - 127.5;
			reg->sum += x;
			reg->sum2 += x*x;
		}
		reg->n += reg->dataBytes;
		break;
	case 16:
		for(i = 0; i < reg->dataBytes; i+=2)
		{
			double x;

			x = *(const uint16_t *)(data+i) - 32767.5;
			reg->sum += x;
			reg->sum2 += x*x;
		}
		reg->n += reg->dataBytes/2;
		break;
	case 32:
		for(i = 0; i < reg->dataBytes; i+=4)
		{
			double x;

			x = *(const uint32_t *)(data+i) - 2147483647.5;
			reg->sum += x;
			reg->sum2 += x*x;
		}
		reg->n += reg->dataBytes/4;
		break;
	default:
		fprintf(stderr, "Error: no decoder for %d bits\n", reg->nBit);

		exit(EXIT_FAILURE);
	}
}

void sigtermHandler(int i)
{
	fprintf(stderr, "%s: terminating due to TERM signal.\n", program);
	fprintf(stderr, "Partial results will be produced.\n");

	die = 1;
}

void sigintHandler(int i)
{
	fprintf(stderr, "%s: terminating due to INT signal.\n", program);
	fprintf(stderr, "Partial results will be produced.\n");

	die = 1;
}

void setSignals()
{
	struct sigaction sigTerm;
	struct sigaction sigInt;

	sigTerm.sa_handler = sigtermHandler;
	sigemptyset(&sigTerm.sa_mask);
	sigTerm.sa_flags = 0;
	sigaction(SIGTERM, &sigTerm, 0);

	sigInt.sa_handler  = sigintHandler;
	sigemptyset(&sigInt.sa_mask);
	sigInt.sa_flags = 0;
	sigaction(SIGINT, &sigInt, 0);
}

void usage()
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "Usage: %s [options] <VDIF file> [ <output file> [ <threadId> ] ]\n\n", program);
	fprintf(stderr, "This program calculates mean and RMS for VDIF files on a 1 second basis.\n\n");
	fprintf(stderr, "Options can include:\n");
	fprintf(stderr, "  -h  or  --help      print this help and quit\n");
	fprintf(stderr, "  -2                  scale by 1/2\n");
	fprintf(stderr, "  -4                  scale by 1/4\n");
	fprintf(stderr, "  -8                  scale by 1/8\n");
	fprintf(stderr, "Note: a hyphen (-) can be used to indicate stdin for <VDIF file>\n");
	fprintf(stderr, "and/or to indicate stdout for <output file>.\n");
	fprintf(stderr, "If no thread Id is provided, all threads will be processed.\n");
}

int main(int argc, char **argv)
{
	const char *inFileName = 0;
	const char *outFileName = 0;
	StatsRegister **regs;
	vdif_header vh;
	unsigned char *data = 0;
	int dataSize = 0;
	FILE *in = 0, *out = 0;
	uint64_t frameCount = 0;
	int t;
	unsigned int a;
	unsigned int inc = 1;
	int printThread = -1;

	if(argc < 2)
	{
		usage();

		return EXIT_SUCCESS;
	}

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 || strcmp(argv[a], "--help") == 0)
			{
				usage();

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "-2") == 0)
			{
				inc = 2;
			}
			else if(strcmp(argv[a], "-4") == 0)
			{
				inc = 4;
			}
			else if(strcmp(argv[a], "-8") == 0)
			{
				inc = 8;
			}
			else if(strcmp(argv[a], "-") == 0)
			{
				if(!inFileName)
				{
					inFileName = "<stdin>";
					in = stdin;
				}
				else if(!outFileName)
				{
					outFileName = "<stdout>";
					out = stdout;
				}
				else
				{
					fprintf(stderr, "Extraneous '-' parameter -- input and output already configured.\n");

					return EXIT_FAILURE;
				}

			}
			else
			{
				fprintf(stderr, "Error: unknown option: %s\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
		else if(inFileName == 0)
		{
			inFileName = argv[a];
			in = fopen(inFileName, "r");
			if(!in)
			{
				fprintf(stderr, "Error: cannot open %s for read\n", inFileName);

				return EXIT_FAILURE;
			}
		}
		else if(outFileName == 0)
		{
			outFileName = argv[a];
			out = fopen(outFileName, "w");
			if(!out)
			{
				fprintf(stderr, "Error: cannot open %s for write\n", outFileName);

				return EXIT_FAILURE;
			}
		}
		else if(printThread < 0)
		{
			printThread = atoi(argv[a]);
		}
		else
		{
			fprintf(stderr, "Extraneous command line parameter: %s\n", argv[a]);

			return EXIT_FAILURE;
		}
	}

	if(!in)
	{
		fprintf(stderr, "Error: incomplete command line.\n");

		return EXIT_FAILURE;
	}

	if(!out)
	{
		out = stdout;
	}
	fprintf(out, "# %s ver %s\n", program, version);
	fprintf(out, "# Input file %s\n", inFileName);

	regs = newStatsRegisters();

	setSignals();

	while(!die)
	{
		size_t rv;
		int payloadSize;

		rv = fread(&vh, sizeof(vdif_header), 1, in);
		if(rv != 1)
		{
			break;
		}
		if(vh.legacymode)
		{
			fprintf(stderr, "Error: legacy header detected on frame %" PRId64 "\n", frameCount);

			break;
		}
		payloadSize = getVDIFFrameBytes(&vh) - VDIF_HEADER_BYTES;
		if(payloadSize < 8 || payloadSize > 1<<20)
		{
			fprintf(stderr, "Error: unreasonable payload size: %d.  Corrupt file?\n", payloadSize);

			return EXIT_FAILURE;
		}
		if(dataSize == 0)
		{
			free(data);
			data = (unsigned char*)malloc(payloadSize);
			dataSize = payloadSize;
		}
		else if(payloadSize != dataSize)
		{
			fprintf(stderr, "Changing payload size!  %d != %d.  Corrupt file?\n", payloadSize, dataSize);

			return EXIT_FAILURE;
		}

		if(regs[vh.threadid] == 0)
		{
			regs[vh.threadid] = newStatsRegister(vh.threadid, getVDIFBitsPerSample(&vh), payloadSize);
		}
		else
		{
			if(regs[vh.threadid]->nBit != getVDIFBitsPerSample(&vh))
			{
				fprintf(stderr, "Weird: threadId %d nBit changed from %d to %d.  Stopping.\n", vh.threadid, regs[vh.threadid]->nBit, getVDIFBitsPerSample(&vh));
				
				break;
			}
			if(regs[vh.threadid]->dataBytes != payloadSize)
			{
				fprintf(stderr, "Weird: threadId %d payload size changed from %d to %d.  Stopping.\n", vh.threadid, regs[vh.threadid]->dataBytes, payloadSize);
			}
		}
	
		rv = fread(data, payloadSize, 1, in);
		if(rv != 1)
		{
			fprintf(stderr, "Weird: data frame %" PRId64 " is incomplete.\n", frameCount);
			
			break;
		}

		if(regs[vh.threadid]->seconds == 0)
		{
			regs[vh.threadid]->seconds = vh.seconds;
		}
		else if(regs[vh.threadid]->seconds != vh.seconds)
		{
			if(printThread < 0 || vh.threadid == printThread)
			{
				fprintStatsRegister(out, regs[vh.threadid], inc);
			}
			resetStatsRegister(regs[vh.threadid]);
			regs[vh.threadid]->seconds = vh.seconds;
		}

		feedStatsRegister(regs[vh.threadid], data);

		++frameCount;
	}

	fprintf(stderr, "Frames read: %" PRId64 "\n", frameCount);

	for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
	{
		if(printThread >= 0 && t != printThread)
		{
			continue;
		}
		if(regs[t])
		{
			fprintStatsRegister(out, regs[t], inc);
		}
	}

	if(data)
	{
		free(data);
	}
	deleteStatsRegisters(regs);
	if(in != stdin)
	{
		fclose(in);
	}
	if(out != stdout)
	{
		fclose(out);
	}

	return EXIT_SUCCESS;
}
