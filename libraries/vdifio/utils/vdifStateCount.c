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

const char program[] = "vdifStateCount";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20231025";

volatile int die = 0;

typedef struct
{
	int threadId;
	int nBit;
	int dataBytes;		/* data bytes per VDIF frame */
	int64_t *counts;	/* Indexed by VDIF native representation */
} StateCountRegister;

enum StateCountPrintLevel
{
	StateCountPrintAll = 0,
	StateCountPrintCompact,
	StateCountPrintNonzero,

	StateCountPrintError	/* list terminator */
};

StateCountRegister *newStateCountRegister(int threadId, int nBit, int dataBytes)
{
	StateCountRegister *reg;

	reg = (StateCountRegister *)calloc(1, sizeof(StateCountRegister));
	reg->threadId = threadId;
	reg->nBit = nBit;
	reg->dataBytes = dataBytes;
	reg->counts = (int64_t *)calloc(1<<nBit, sizeof(int64_t));

	return reg;
}

void deleteStateCountRegister(StateCountRegister *reg)
{
	if(reg)
	{
		if(reg->counts)
		{
			free(reg->counts);
		}
		free(reg);
	}
}

StateCountRegister **newStateCountRegisters()
{
	return (StateCountRegister **)calloc(VDIF_MAX_THREAD_ID+1, sizeof(StateCountRegister *));
}

void deleteStateCountRegisters(StateCountRegister **regs)
{
	if(regs)
	{
		int t;

		for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
		{
			if(regs[t])
			{
				deleteStateCountRegister(regs[t]);
			}
		}
		free(regs);
	}
}

void fprintStateCountRegister(FILE *out, const StateCountRegister *reg, enum StateCountPrintLevel pl)
{
	int a=-1, b=-1;	/* first and last index with non-zero counts */
	int i;
	int N;
	double offset;

	N = 1 << reg->nBit;
	offset = N/2 - 0.5;

	for(i = 0; i < N; ++i)
	{
		if(reg->counts[i] > 0)
		{
			a = i;
			break;
		}
	}
	for(i = N-1; i >= 0; --i)
	{
		if(reg->counts[i] > 0)
		{
			b = i;
			break;
		}
	}

	if(a < 0 || b < 0)
	{
		fprintf(out, "# No values for threadId = %d\n", reg->threadId);
	}
	else
	{
		double sum = 0.0;
		double x = 0.0;
		double xx = 0.0;
		double mean, rms;
		
		for(i = a; i <= b; ++i)
		{
			sum += reg->counts[i];
			x += (i-offset)*reg->counts[i];
			xx += (i-offset)*(i-offset)*reg->counts[i];
		}
		mean = x/sum;
		rms = sqrt(xx/sum - mean*mean);

		fprintf(out, "# Thread %d  nBit %d  dataBytes %d\n", reg->threadId, reg->nBit, reg->dataBytes);
		fprintf(out, "# Mean %f\n", mean);
		fprintf(out, "# RMS %f\n", rms);
		switch(pl)
		{
		case StateCountPrintAll:
			for(i = 0; i < N; ++i)
			{
				fprintf(out, "%d  %3.1f %" PRId64 " %f\n", reg->threadId, i-offset, reg->counts[i], reg->counts[i]/sum);
			}
			break;
		case StateCountPrintCompact:
			for(i = a; i <= b; ++i)
			{
				fprintf(out, "%d  %3.1f %" PRId64 " %f\n", reg->threadId, i-offset, reg->counts[i], reg->counts[i]/sum);
			}
			break;
		case StateCountPrintNonzero:
			for(i = a; i <= b; ++i)
			{
				if(reg->counts[i] > 0)
				{
					fprintf(out, "%d  %3.1f %" PRId64 " %f\n", reg->threadId, i-offset, reg->counts[i], reg->counts[i]/sum);
				}
			}
			break;
		default:
			fprintf(stdout, "Error: Print mode.\n");
			break;
		}
	}
}

void countStates(StateCountRegister *reg, const unsigned char *data)
{
	int i;

	switch(reg->nBit)
	{
	case 1:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			unsigned char d;

			d = data[i];
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
			d >>= 1;
			++reg->counts[d & 0x01];
		}
		break;
	case 2:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			unsigned char d;

			d = data[i];
			++reg->counts[d & 0x03];
			d >>= 2;
			++reg->counts[d & 0x03];
			d >>= 2;
			++reg->counts[d & 0x03];
			d >>= 2;
			++reg->counts[d & 0x03];
		}
		break;
	case 4:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			unsigned char d;

			d = data[i];
			++reg->counts[d & 0x0F];
			d >>= 4;
			++reg->counts[d & 0x0F];
		}
		break;
	case 8:
		for(i = 0; i < reg->dataBytes; ++i)
		{
			++reg->counts[data[i]];
		}
		break;
	case 16:
		for(i = 0; i < reg->dataBytes; i+=2)
		{
			const uint16_t *d;

			d = (const uint16_t *)(data + i);
			++reg->counts[*d];
		}
		break;
	case 32:
		for(i = 0; i < reg->dataBytes; i+=4)
		{
			const uint32_t *d;

			d = (const uint32_t *)(data + i);
			++reg->counts[*d];
		}
		break;
	default:
		fprintf(stderr, "Error: no state counter for %d bits\n", reg->nBit);

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
	fprintf(stderr, "Usage: %s <VDIF file> [ <output file> ]\n\n", program);
	fprintf(stderr, "This program generates a histogram of state counts for VDIF files.\n\n");
}

int main(int argc, char **argv)
{
	const char *inFileName;
	const char *outFileName = 0;
	StateCountRegister **regs;
	vdif_header vh;
	unsigned char *data;
	int dataSize;
	enum StateCountPrintLevel pl = StateCountPrintCompact;
	FILE *in, *out;
	uint64_t frameCount = 0;
	int t;

	if(argc < 2)
	{
		usage();

		exit(0);
	}

	inFileName = argv[1];
	in = fopen(inFileName, "r");
	if(!in)
	{
		fprintf(stderr, "Error: cannot open %s for read\n", inFileName);

		return EXIT_FAILURE;
	}

	if(argc > 2)
	{
		outFileName = argv[2];
		out = fopen(outFileName, "w");
		if(!out)
		{
			fprintf(stderr, "Error: cannot open %s for write\n", outFileName);

			return EXIT_FAILURE;
		}
	}
	else
	{
		out = stdout;
	}

	regs = newStateCountRegisters();
	dataSize = 8000;
	data = (unsigned char *)malloc(dataSize);

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
		if(payloadSize > dataSize)
		{
			free(data);
			data = (unsigned char*)malloc(payloadSize);
			dataSize = payloadSize;
		}

		if(regs[vh.threadid] == 0)
		{
			regs[vh.threadid] = newStateCountRegister(vh.threadid, getVDIFBitsPerSample(&vh), payloadSize);
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

		countStates(regs[vh.threadid], data);

		++frameCount;
	}

	printf("File: %s\n", inFileName);
	printf("Frames read: %" PRId64 "\n", frameCount);

	for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
	{
		if(regs[t])
		{
			fprintStateCountRegister(out, regs[t], pl);
		}
	}

	free(data);
	deleteStateCountRegisters(regs);
	fclose(in);
	if(outFileName)
	{
		fclose(out);
	}

	return EXIT_SUCCESS;
}
