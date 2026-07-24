/***************************************************************************
 *   Copyright (C) 2023-2024 by Walter Brisken                             *
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
const char version[] = "0.3";
const char verdate[] = "20240227";

volatile int die = 0;

typedef struct
{
	unsigned int threadId;
	unsigned int nBit;
	unsigned int dataBytes;	/* data bytes per VDIF frame */
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
		unsigned int t;

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

void fprintStateCountRegister(FILE *out, const StateCountRegister *reg, enum StateCountPrintLevel pl, unsigned int inc, unsigned int eomask)
{
	int a=-1, b=-1;	/* first and last index with non-zero counts */
	unsigned int i;
	unsigned int N;
	double offset;

	N = 1 << reg->nBit;
	offset = N/2 - inc/2.0;

	for(i = 0; i < N; i += inc)
	{
		if(reg->counts[i] > 0)
		{
			a = i;
			break;
		}
	}
	for(i = N-inc; i >= 0; i -= inc)
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
		
		for(i = a; i <= b; i += inc)
		{
			sum += reg->counts[i];
			x += (i-offset)*reg->counts[i];
			xx += (i-offset)*(i-offset)*reg->counts[i];
		}
		mean = x/sum;
		rms = sqrt(xx/sum - mean*mean);

		fprintf(out, "# Thread %u  nBit %u  dataBytes %u\n", reg->threadId, reg->nBit, reg->dataBytes);
		fprintf(out, "# Mean %f\n", mean/inc);
		fprintf(out, "# RMS %f\n", rms/inc);
		switch(pl)
		{
		case StateCountPrintAll:
			for(i = 0; i < N; i += inc)
			{
				if(eomask & (1 << ((i/inc) % 2)))
				{
					fprintf(out, "%u  %3.1f %" PRId64 " %f\n", reg->threadId, (i-offset)/inc, reg->counts[i], reg->counts[i]/sum);
				}
			}
			break;
		case StateCountPrintCompact:
			for(i = a; i <= b; i += inc)
			{
				if(eomask & (1 << ((i/inc) % 2)))
				{
					fprintf(out, "%u  %3.1f %" PRId64 " %f\n", reg->threadId, (i-offset)/inc, reg->counts[i], reg->counts[i]/sum);
				}
			}
			break;
		case StateCountPrintNonzero:
			for(i = a; i <= b; i += inc)
			{
				if((eomask & (1 << ((i/inc) % 2))) && reg->counts[i] > 0)
				{
					fprintf(out, "%u  %3.1f %" PRId64 " %f\n", reg->threadId, (i-offset)/inc, reg->counts[i], reg->counts[i]/sum);
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
	unsigned int i;

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
	fprintf(stderr, "Usage: %s [options] <VDIF file> [ <output file> [ <threadId> ] ]\n\n", program);
	fprintf(stderr, "This program generates a histogram of state counts for VDIF files.\n\n");
	fprintf(stderr, "Options can include:\n");
	fprintf(stderr, "  -h  or  --help      print this help and quit\n");
	fprintf(stderr, "  -c  or  --compact   print compact version of histogram [default]\n");
	fprintf(stderr, "  -n  or  --nonzero   print only non-zero elements of histogram\n");
	fprintf(stderr, "  -a  or  --all       print entire histogram\n");
	fprintf(stderr, "  -e  or  --even      print only even bins\n");
	fprintf(stderr, "  -o  or  --odd       print only odd bins\n");
	fprintf(stderr, "  -1                  consider all bins [default]\n");
	fprintf(stderr, "  -2                  only consider every 2nd bin\n");
	fprintf(stderr, "  -4                  only consider every 4th bin\n");
	fprintf(stderr, "  -8                  only consider every 8th bin\n");
	fprintf(stderr, "Note: a hyphen (-) can be used to indicate stdin for <VDIF file>\n");
	fprintf(stderr, "and/or to indicate stdout for <output file>.\n");
	fprintf(stderr, "If no thread Id is provided, all threads will be processed.\n");
}

int main(int argc, char **argv)
{
	const char *inFileName = 0;
	const char *outFileName = 0;
	StateCountRegister **regs;
	vdif_header vh;
	unsigned char *data = 0;
	int dataSize = 0;
	enum StateCountPrintLevel pl = StateCountPrintCompact;
	FILE *in = 0, *out = 0;
	uint64_t frameCount = 0;
	int t;
	unsigned int a;
	unsigned int inc = 1;
	int printThread = -1;
	unsigned int eomask = 0x03;

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
			else if(strcmp(argv[a], "-c") == 0 || strcmp(argv[a], "--compact") == 0)
			{
				pl = StateCountPrintCompact;
			}
			else if(strcmp(argv[a], "-n") == 0 || strcmp(argv[a], "--nonzero") == 0)
			{
				pl = StateCountPrintNonzero;
			}
			else if(strcmp(argv[a], "-a") == 0 || strcmp(argv[a], "--all") == 0)
			{
				pl = StateCountPrintAll;
			}
			else if(strcmp(argv[a], "-e") == 0 || strcmp(argv[a], "--even") == 0)
			{
				eomask = 0x01;
			}
			else if(strcmp(argv[a], "-o") == 0 || strcmp(argv[a], "--odd") == 0)
			{
				eomask = 0x02;
			}
			else if(strcmp(argv[a], "-1") == 0)
			{
				inc = 1;
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

	regs = newStateCountRegisters();

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
			break;
		}

		countStates(regs[vh.threadid], data);

		++frameCount;
	}

	fprintf(stderr, "Frames read: %" PRId64 "\n", frameCount);

	fprintf(out, "# %s ver %s\n", program, version);
	fprintf(out, "# Input file %s\n", inFileName);
	for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
	{
		if(printThread >= 0 && t != printThread)
		{
			continue;
		}
		if(regs[t])
		{
			fprintStateCountRegister(out, regs[t], pl, inc, eomask);
		}
	}

	if(data)
	{
		free(data);
	}
	deleteStateCountRegisters(regs);
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
