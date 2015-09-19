/***************************************************************************
 *   Copyright (C) 2015 Walter Brisken                                     *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glob.h>
#include <regex.h>
#include <inttypes.h>
#include "vdifmark6.h"

/* returns pointer to start of non-whitespace text, or 0 if no content is found */
static char *trimLine(char *line)
{
	int i;

	if(line[0] == 0)
	{
		return 0;
	}

	/* remove comment or carriage return */
	for(i = 0; line[i]; ++i)
	{
		if(line[i] == '#' || line[i] == '\n')
		{
			line[i] = 0;
			break;
		}
	}

	/* remove trailing whitespace */
	for(--i; i > 0; --i)
	{
		if(line[i] > ' ')
		{
			break;
		}
		line[i] = 0;
	}

	if(i <= 0)
	{
		/* nothing useful here */

		return 0;
	}

	/* find first non-trivial character */
	for(i = 0; line[i]; ++i)
	{
		if(line[i] > ' ')
		{
			return line + i;
		}
	}
	
	/* nothing good found */
	return 0;
}

/* greatest common divisor, from wikipedia */
static unsigned int gcd(unsigned int u, unsigned int v)
{
  // simple cases (termination)
  if (u == v)
    return u;
  if (u == 0)
    return v;
  if (v == 0)
    return u;
 
  // look for factors of 2
  if ((~u) & 1) // u is even
  {
    if (v & 1) // v is odd
    {
      return gcd(u >> 1, v);
    }
    else // both u and v are even
    {
      return gcd(u >> 1, v >> 1) << 1;
    }
  }
  if ((~v) & 1) // u is odd, v is even
  {  
    return gcd(u, v >> 1);
  }
  // reduce larger argument
  if (u > v)
  {  
    return gcd((u - v) >> 1, v);
  }
  return gcd((v - u) >> 1, u);
}

struct vdif_mark6_mux *configurevdifmark6mux(const char *templateFilename, const char *fileParameter, int inputFramesPerSecond)
{
	const int MaxLineLength = 1024;
	const int MaxFilenameLength = 512;
	char line[MaxLineLength];
	struct vdif_mark6_mux *vm;
	FILE *in;
	regex_t paramMatch;
	regex_t streamMatch;
	regex_t slotMatch;
	regmatch_t subexpressions[4];
	int lineNum = 0;
	char mountPoint[MaxFilenameLength];
	int chansPerThread = 1;
	int interleaveFactor = 1;
	char slotUsed[MAX_VDIF_MUX_SLOTS];
	int v;

	memset(slotUsed, 0, MAX_VDIF_MUX_SLOTS);

	mountPoint[0] = 0;
	vm = (struct vdif_mark6_mux *)calloc(1, sizeof(struct vdif_mark6_mux));
	if(!vm)
	{
		fprintf(stderr, "Error: configurevdifmark6mux: cannot allocate %d bytes for vdif_mark6_mux structure\n", (int)sizeof(struct vdif_mark6_mux));
		return 0;
	}
	vm->inputFramesPerSecond = inputFramesPerSecond;

	in = fopen(templateFilename, "r");
	if(!in)
	{
		fprintf(stderr, "Error: configurevdifmark6mux: cannot open %s\n", templateFilename);
		deletevdifmark6mux(vm);

		return 0;
	}
	
	v = regcomp(&paramMatch, "^(\\S+)\\s*=\\s*(\\S+)$", REG_EXTENDED);
	if(v != 0)
	{
		fprintf(stderr, "Developer error: configurevdifmark6mux: cannot compile regex for paramMatch\n");
		deletevdifmark6mux(vm);

		return 0;
	}
	v = regcomp(&streamMatch, "^stream([0-9]+)\\s*=\\s*(\\S+)$", REG_EXTENDED);
	if(v != 0)
	{
		fprintf(stderr, "Developer error: configurevdifmark6mux: cannot compile regex for streamMatch\n");
		deletevdifmark6mux(vm);

		return 0;
	}
	v = regcomp(&slotMatch, "^slot([0-9]+)\\s*=\\s*([0-9]+)\\s*,\\s*([0-9]+)$", REG_EXTENDED);
	if(v != 0)
	{
		fprintf(stderr, "Developer error: configurevdifmark6mux: cannot compile regex for slotMatch\n");
		deletevdifmark6mux(vm);

		return 0;
	}

	for(;;)
	{
		char *rv;
		char *txt;

		rv = fgets(line, MaxLineLength, in);
		if(rv == 0)
		{
			break;
		}
		++lineNum;
		txt = trimLine(line);
		if(txt == 0)
		{
			// a content-free line
			continue;
		}
		if(regexec(&streamMatch, txt, 3, subexpressions, 0) == 0)
		{
			int n, v, t;
			glob_t globOut;
			char globPattern[MaxFilenameLength];
			int streamId;
			const char *streamData;

			txt[subexpressions[1].rm_eo] = 0;	/* terminate end of stream number subexpression */
			txt[subexpressions[2].rm_eo] = 0;	/* terminate end of stream data subexpression */
			struct vdif_mark6_mux_stream *stream;

			streamId = atoi(txt + subexpressions[1].rm_so);
			if(streamId < 0 || streamId > 1023)
			{
				fprintf(stderr, "Error: configurevdifmark6mux: streamId must be between 0 and 1023, inclusive.  %d was given on line %d\n", streamId, lineNum);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
			streamData = txt + subexpressions[2].rm_so;

			if(streamId >= vm->nStream)
			{
				vm->streams = realloc(vm->streams, (streamId+1)*sizeof(struct vdif_mark6_mux_stream));
				memset(vm->streams + vm->nStream, 0, (streamId + 1- vm->nStream)*sizeof(struct vdif_mark6_mux_stream));

				vm->nStream = streamId + 1;
			}
			stream = vm->streams + streamId;
	
			for(t = 0; t <= VDIF_MAX_THREAD_ID; ++t)
			{
				stream->slotIndex[t] = -1;
			}

			n = snprintf(globPattern, MaxFilenameLength, "%s/%s/%s", mountPoint, streamData, fileParameter);
			if(n >= MaxFilenameLength)
			{
				fprintf(stderr, "Developer error: configurevdifmark6mux: glob pattern length %d exceeds buffer size %d for pattern %s/%s/%s\n", n, MaxFilenameLength-1, mountPoint, streamData, fileParameter);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
	
			v = glob(globPattern, 0, 0, &globOut);
			if(v != 0)
			{
				fprintf(stderr, "Error: configurevdifmark6mux: globbing on %s returned %d\n", globPattern, v);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}

			if(globOut.gl_pathc <= 0)
			{
				fprintf(stderr, "Error: no files found for stream%d\n", streamId);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}

			stream->m6d = openMark6(globOut.gl_pathc, globOut.gl_pathv);

			globfree(&globOut);
		}
		else if(regexec(&slotMatch, txt, 4, subexpressions, 0) == 0)
		{
			int slotId, streamId, threadId;

			txt[subexpressions[1].rm_eo] = 0;	/* terminate end of slot number subexpression */
			txt[subexpressions[2].rm_eo] = 0;	/* terminate end of stream number subexpression */
			txt[subexpressions[3].rm_eo] = 0;	/* terminate end of thread number subexpression */

			slotId = atoi(txt + subexpressions[1].rm_so);
			streamId = atoi(txt + subexpressions[2].rm_so);
			threadId = atoi(txt + subexpressions[3].rm_so);

			if(slotId < 0 || slotId >= MAX_VDIF_MUX_SLOTS)
			{
				fprintf(stderr, "Error: configurevdifmark6mux: slot %d not supported.  The maximum number of slots is %d.  Note slot numbers start at 0.\n", slotId, MAX_VDIF_MUX_SLOTS);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
			if(streamId < 0 || streamId >= vm->nStream || vm->streams[streamId].m6d == 0)
			{
				fprintf(stderr, "Error: configurevdifmark6mux: line %d: slot %d references stream %d which is not (yet) defined.\n", lineNum, slotId, streamId);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
			if(threadId < 0 || threadId > VDIF_MAX_THREAD_ID)
			{
				fprintf(stderr, "Error: configurevdifmark6mux: line %d: slot %d references thread %d which is outside the allowed range of 0 to %d, inclusive.\n", lineNum, slotId, threadId, VDIF_MAX_THREAD_ID);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
			if(vm->streams[streamId].slotIndex[threadId] >= 0)
			{
				fprintf(stderr, "Error: configurevdifmark6mux: line %d: slot %d is redefining output for stream %d thread %d\n", lineNum, slotId, streamId, threadId);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
			if(slotUsed[slotId])
			{
				fprintf(stderr, "Error: configurevdifmark6mux: line %d: slot %d is being redefined.  This is not allowed.\n", lineNum, slotId);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}

			vm->streams[streamId].slotIndex[threadId] = slotId;
			vm->goodMask |= (1 << slotId);
			++slotUsed[slotId];
			++vm->nSlot;
		}
		else if(regexec(&paramMatch, txt, 3, subexpressions, 0) == 0)
		{
			const char *param;
			const char *value;
			int n;

			txt[subexpressions[1].rm_eo] = 0;	/* terminate end of param subexpression */
			txt[subexpressions[2].rm_eo] = 0;	/* terminate end of value subexpression */

			param = txt + subexpressions[1].rm_so;
			value = txt + subexpressions[2].rm_so;

			if(strcmp(param, "mountPoint") == 0)
			{
				n = snprintf(mountPoint, MaxFilenameLength, "%s", value);
				if(n >= MaxFilenameLength)
				{
					fprintf(stderr, "Error: configurevdifmark6mux: line %d: provided mountPoint is longer than allowed (%d > %d).\n", lineNum, n, MaxFilenameLength-1);
					deletevdifmark6mux(vm);
					vm = 0;

					break;
				}	
			}
			else if(strcmp(param, "frameSize") == 0)
			{
				vm->inputFrameSize = atoi(value);
				if(vm->inputFrameSize < VDIF_HEADER_BYTES+4 || vm->inputFrameSize % 4 != 0)
				{
					fprintf(stderr, "Error: configurevdifmark6mux: line %d: provided frameSize must be >=%d and a multiple of 4\n", lineNum, VDIF_HEADER_BYTES + 4);
					deletevdifmark6mux(vm);
					vm = 0;

					break;
				}
				vm->inputDataSize = vm->inputFrameSize - VDIF_HEADER_BYTES;
			}
			else if(strcmp(param, "bitsPerSample") == 0)
			{
				vm->bitsPerSample = atoi(value);
				if(vm->bitsPerSample <= 0 || vm->bitsPerSample > 16)
				{
					fprintf(stderr, "Error: configurevdifmark6mux: line %d: provided bitsPerSample must be > 0 and <= 16\n", lineNum);
					deletevdifmark6mux(vm);
					vm = 0;

					break;
				}
			}
			else if(strcmp(param, "chansPerThread") == 0)
			{
				chansPerThread = atoi(value);
				if(chansPerThread != 1 && chansPerThread != 2 && chansPerThread != 4 && chansPerThread != 8 && chansPerThread != 16 && chansPerThread != 32)
				{
					fprintf(stderr, "Error: configurevdifmark6mux: line %d: provided chansPerThread must be 1, 2, 4, 8, 16 or 32\n", lineNum);
					deletevdifmark6mux(vm);
					vm = 0;

					break;
				}
			}
			else if(strcmp(param, "interleaveFactor") == 0)
			{
				interleaveFactor = atoi(value);
				if(interleaveFactor != 1 && interleaveFactor != 2 && interleaveFactor != 4)
				{
					fprintf(stderr, "Error: configurevdifmark6mux: line %d: provided interleaveFactor must be 1, 2 or 4\n", lineNum);
					deletevdifmark6mux(vm);
					vm = 0;

					break;
				}
			}
			else
			{
				fprintf(stderr, "Error: configurevdifmark6mux: line %d: unknown parameter provided: %s\n", lineNum, param);
				deletevdifmark6mux(vm);
				vm = 0;

				break;
			}
		}
		else
		{
			fprintf(stderr, "Error: configurevdifmark6mux: unparsable line # %d in file %s:\n", lineNum, templateFilename);
			fprintf(stderr, "---> '%s'\n", line);
			deletevdifmark6mux(vm);
			vm = 0;

			break;
		}

	}

	regfree(&slotMatch);
	regfree(&streamMatch);
	regfree(&paramMatch);

	fclose(in);

	if(vm)
	{
		if(vm->inputFrameSize < 0)
		{
			fprintf(stderr, "Error: configurevdifmark6mux: requred parameter 'frameSize' not provided in %s\n", templateFilename);
			deletevdifmark6mux(vm);

			return 0;
		}
		else if(vm->bitsPerSample < 0)
		{
			fprintf(stderr, "Error: configurevdifmark6mux: requred parameter 'bitsPerSample' not provided in %s\n", templateFilename);
			deletevdifmark6mux(vm);
			
			return 0;
		}
		else if(vm->nSlot % interleaveFactor != 0)
		{
			fprintf(stderr, "Error: configurevdifmark6mux: 'interleaveFactor' (provided as %d) must divide evenly into number of slots (%d)\n", interleaveFactor, vm->nSlot);
			deletevdifmark6mux(vm);

			return 0;
		}
	}

	if(vm)
	{
		/* things seemed OK... */

		vm->nOutputChan = 1;
		if(vm->nSlot > 1)
		{
			/* round nOutputChan up to next power of 2 */
			for(vm->nOutputChan = 1; vm->nOutputChan < vm->nSlot; vm->nOutputChan *= 2) { }
		}
		vm->bitsPerSlot = chansPerThread*vm->bitsPerSample;
		vm->nOutputChan = chansPerThread*vm->nSlot/interleaveFactor;
		vm->outputDataSize = vm->inputDataSize*vm->nSlot;
		vm->outputFrameSize = vm->outputDataSize + VDIF_HEADER_BYTES;
		vm->frameGranularity = inputFramesPerSecond/gcd(inputFramesPerSecond, 1000000000);
		vm->nSort = 10;
		vm->nGap = 10;
	}

	return vm;
}

void deletevdifmark6mux(struct vdif_mark6_mux *vm)
{
	if(vm)
	{
		int i;
	
		if(vm->streams)
		{
			for(i = 0; i < vm->nStream; ++i)
			{
				if(vm->streams[i].m6d)
				{
					closeMark6(vm->streams[i].m6d);
					vm->streams[i].m6d = 0;
				}
			}
			free(vm->streams);
		}

		free(vm);
	}
}

void printvdifmark6mux(const struct vdif_mark6_mux *vm)
{
	if(vm)
	{
		int s;

		printf("vdif_mark6_mux:\n");
		printf("  inputFrameSize = %d\n", vm->inputFrameSize);
		printf("  inputDataSize = %d\n", vm->inputDataSize);
		printf("  outputFrameSize = %d\n", vm->outputFrameSize);
		printf("  outputDataSize = %d\n", vm->outputDataSize);
		printf("  inputFramesPerSecond = %d\n", vm->inputFramesPerSecond);
		printf("  frameGranularity = %d\n", vm->frameGranularity);
		printf("  bitsPerSample = %d\n", vm->bitsPerSample);
		printf("  bitsPerSlot = %d\n", vm->bitsPerSlot);
		printf("  nSort = %d\n", vm->nSort);
		printf("  nGap = %d\n", vm->nGap);
		printf("  nSlot = %d\n", vm->nSlot);
		printf("  nOutputChan = %d\n", vm->nOutputChan);
		printf("  goodMask = 0x%" PRIx64 "\n", vm->goodMask);
		printf("  flags = 0x%02x\n", vm->flags);
		printf("  nStream = %d\n", vm->nStream);
		for(s = 0; s < vm->nStream; ++s)
		{
			printf("    stream%d:\n", s);
			if(vm->streams[s].m6d)
			{
				int i;

				printf("      nFile = %d:\n", vm->streams[s].m6d->nFile);
				for(i = 0; i < vm->streams[s].m6d->nFile; ++i)
				{
					printf("        %s\n", vm->streams[s].m6d->mk6Files[i].fileName);
				}
				printf("      thread to slot map:\n");
				for(i = 0; i <= VDIF_MAX_THREAD_ID; ++i)
				{
					if(vm->streams[s].slotIndex[i] >= 0)
					{
						printf("        %d -> %d\n", i, vm->streams[s].slotIndex[i]);
					}
				}
			}
			else
			{	
				printf("      Unused\n");
			}
		}		
	}
	else
	{
		printf("vdif_mark6_mux is NULL\n");
	}
}

int vdifmark6mux(unsigned char *dest, int destSize, const struct vdif_mark6_mux *vmm, int64_t startOutputFrameNumber, struct vdif_mux_statistics *stats)
{
	int srcSize;
	int nThread;
	int nSlot;	/* nThread rounded up to power of 2 */
	struct vdif_mux *vm;
	int s, v;
	const vdif_header *vh;

	vm = &(vmm->vm);

	/* compute list of threads based on slotIndex in the vdif_mark6_mux_streams */


	srcSize = nSlot * destSize * vm->inputFrameSize;
	if(srcSize > vmm->scratchSize)
	{
		vmm->scratchSize = srcSize;
		vmm->scratch = realloc(vmm->scratch, srcSize);
	}

	/* initialize the scratch, ensuring all packets are invalid */
	memset(vmm->scratch, 1, srcSize);

	/* identify startOutputFrameNumber if not specified */
	if(startOutputFrameNumber < 0)
	{
		for(s = 0; s < vmm->nStream; ++s)
		{
			int frameNumber;

			vh = (const vdif_header *)(vmm->streams[s].m6d->mk6Files[vmm->streams[s].currentFileNum]
			frameNumber = (int64_t)(getVDIFFrameEpochSecOffset(vh)) * vm->inputFramesPerSecond + getVDIFFrameNumber(vh);
		}
	}

	/* loop over streams, copying frames to scratch as appropriate */
	for(s = 0; s < vmm->nStream; ++s)
	{
	}

	v = vdifmux(dest, destSize, vmm->scratch, srcSize, vm, startOutputFrameNumber, stats);

	return v;
}

struct vdif_mark6_mux_statistics *newvdifmark6muxstatistics(const struct vdif_mark6_mux *vm)
{
	struct vdif_mark6_mux_statistics *stats;

	stats = (struct vdif_mark6_mux_statistics *)calloc(1, sizeof(struct vdif_mark6_mux_statistics));

/* FIXME */

	return stats;
}

void deletevdifmark6muxstatistics(struct vdif_mark6_mux_statistics *stats)
{
}

void printvdifmark6muxstatistics(const struct vdif_mark6_mux_statistics *stats)
{
}

void resetvdifmark6muxstatistics(struct vdif_mark6_mux_statistics *stats)
{
}
