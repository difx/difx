/***************************************************************************
 *   Copyright (C) 2013-2025 by Walter Brisken, Mark Wainright             *
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
#include <unistd.h>
#include <vdifio.h>
#include "config.h"

#ifdef HAVE_MARK6SG
#include <mark6sg/mark6gather.h>
#include "mark6gather_vdif.h"
#endif

const char program[] = "vsum";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>, Mark Wainright <mwainrig@nrao.edu>";
const char version[] = "0.9";
const char verdate[] = "20250212";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to summarize the contents of VDIF data files\n\n");
	printf("Usage: %s [<options>] <file1> [<file2> [ ... ] ]\n\n", pgm);
	printf("  <fileX> is the name of a VDIF data file\n\n");
	printf("  <options> can include:\n");
	printf("    -h or --help       Print this usage information and quit\n");
	printf("    -s or --shortsum   Print a short summary, also usable for input to vex2difx\n");
	printf("    -e or --exhaustive Generate some statistics requiring full read of file\n");
	printf("    -t or --timeoffset Add time offset to filetimes\n");
#ifdef HAVE_MARK6SG
	printf("    -6 or --mark6      Operate directly on Mark6 module data\n");
	printf("    --allmark6         Operate directly on all Mark6 scans found on mounted modules\n");
	printf("    --mark6slot <slot> Operate directly on all Mark6 scans found on module in <slot>\n");
#endif
	printf("\n");
}

static void summarizeFileExhaustive(const char *fileName, int isMark6)
{
	struct vdif_file_summary sum;
	int r;

	if(isMark6)
	{
		fprintf(stdout, "Exhaustive reporting is not available for Mark6 formatted data: %s\n", fileName);

		return;
	}

	r = summarizevdiffile(&sum, fileName, 0);
	if(r < 0)
	{
		fprintf(stderr, "File %s VDIF summary failed with return value %d\n\n", fileName, r);
	}
	else
	{
		FILE *in;
		vdif_header *vh;
		long long int nInvalid, nValid;
		long long int threadHisto[2][1024];	/* first index: 0 = valid, 1 = invalid; second index thread Id */
		long long int threadOutOfSequence[1024];
		int lastFrame[1024];
		int highestFrame;
		int inferredFrameRate;
		long long int missingFrames, totalFrames;
		int t, f;

		highestFrame = -1;
		nInvalid = nValid = 0;
		for(t = 0; t < 1024; ++t)
		{
			threadHisto[0][t] = threadHisto[1][t] = 0;
			threadOutOfSequence[t] = 0;
			lastFrame[t] = -1;
		}

		vh = (vdif_header *)malloc(sum.frameSize);
		in = fopen(fileName, "r");

		while(fread(vh, sum.frameSize, 1, in) > 0)
		{
			t = vh->threadid;
			f = vh->frame;

			if(f > highestFrame)
			{
				highestFrame = f;
			}
			if(vh->invalid)
			{
				++nInvalid;
				++threadHisto[1][t];
			}
			else
			{
				++nValid;
				++threadHisto[0][t];
			}
			if(lastFrame[t] >= 0)
			{
				if(f != 0 && f != lastFrame[t] + 1)
				{
					++threadOutOfSequence[t];
				}
			}
			lastFrame[t] = f;
		}
		
		fclose(in);
		free(vh);

		if(sum.framesPerSecond > 0)
		{
			inferredFrameRate = sum.framesPerSecond;
		}
		else
		{
			inferredFrameRate = highestFrame;
		}
		totalFrames = (sum.endSecond - sum.startSecond)*(long long)inferredFrameRate + (sum.endFrame - sum.startFrame + 1);
		missingFrames = totalFrames - sum.fileSize/sum.frameSize;
		
		printf("Extended summary of ");
		printvdiffilesummary(&sum);
		printf("  highest frame number = %d\n", highestFrame);
		printf("  inferred number of missing frames = %Ld\n", missingFrames);
		printf("  number of valid frames = %Ld\n", nValid);
		for(t = 0; t < 1024; ++t)
		{
			if(threadHisto[0][t] > 0) printf("    number in thread %4d = %Ld\n", t, threadHisto[0][t]);
		}
		printf("  number of invalid frames = %Ld\n", nInvalid);
		for(t = 0; t < 1024; ++t)
		{
			if(threadHisto[1][t] > 0) printf("    number in thread %4d = %Ld\n", t, threadHisto[1][t]);
		}
		printf("  number of out of sequence frames for\n");
		for(t = 0; t < 1024; ++t)
		{
			if(threadHisto[0][t] > 0 || threadHisto[1][t] > 0)
			{
				printf("    thread %4d = %Ld\n", t, threadOutOfSequence[t]);
			}
		}
	}
}

/* NOTE: toff is never used in this function... */
static void summarizeFile(const char *fileName, int shortSum, int isMark6, int toff)
{
	struct vdif_file_summary sum;
	int r;

#ifdef HAVE_MARK6SG
	if(isMark6)
	{
		r = summarizevdifmark6(&sum, fileName, 0);
	}
	else
#endif
	{
		r = summarizevdiffile(&sum, fileName, 0);
	}

	if(r < 0)
	{
		fprintf(stderr, "File %s VDIF summary failed with return value %d\n\n", fileName, r);
	}
	else if(shortSum)
	{
		const int MaxFilenameLength = 512;
		double mjd1, mjd2;
		char fullFileName[MaxFilenameLength];

		mjd1 = vdiffilesummarygetstartmjd(&sum) + (sum.startSecond % 86400)/86400.0;
		mjd2 = mjd1 + (sum.endSecond - sum.startSecond + 1)/86400.0;

		if(fileName[0] != '/' && isMark6 == 0)
		{
			char path[MaxFilenameLength];
			if(getcwd(path, MaxFilenameLength) == NULL)
			{
				/* CJP Maybe wrong behaviour or warning needed */
				snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
			}
			else
			{
				snprintf(fullFileName, MaxFilenameLength, "%s/%s", path, fileName);
			}
		}
		else
		{
			snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
		}
		printf("%s %14.8f %14.8f\n", fullFileName, mjd1, mjd2);
	}
	else
	{
		printvdiffilesummary(&sum);
	}
}

#ifdef HAVE_MARK6SG
static void processAllMark6Scans(int shortSum)
{
	char **fileList;
	int n;

	n = getMark6FileList(&fileList);

	if(n == 0)
	{
		printf("No Mark6 files found in %s\n", getMark6Root());
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			summarizeFile(fileList[i], shortSum, 1, 0);
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}
}

static void processMark6ScansSlot(int slot, int shortSum)
{
	char **fileList;
	int n;

	n = getMark6SlotFileList(slot, &fileList);

	if(n == 0)
	{
		printf("No Mark6 files found in /mnt/disks/%d/*/data\n", slot);
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			summarizeFile(fileList[i], shortSum, 1, 0);
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}
}
#endif

int main(int argc, char **argv)
{
	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}
	else
	{
		int a, slot;
		int exhaustive = 0;
		int shortSum = 0;
		int isMark6 = 0;
		int toff = 0;

		for(a = 1; a < argc; ++a)
		{
			if(strcmp(argv[a], "-s") == 0 ||
			   strcmp(argv[a], "--shortsum") == 0)
			{
				shortSum = 1;
			}
			else if(strcmp(argv[a], "-e") == 0 ||
			   strcmp(argv[a], "--exhaustive") == 0)
			{
				exhaustive = 1;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "-t") == 0 ||
			   strcmp(argv[a], "--timeoffset") == 0)
			{
			       a++;
			       toff = atoi(argv[a]);
			}
#ifdef HAVE_MARK6SG
			else if(strcmp(argv[a], "-6") == 0 ||
			   strcmp(argv[a], "--mark6") == 0)
			{
				isMark6 = 1;
			}
			else if(strcmp(argv[a], "--allmark6") == 0)
			{
				processAllMark6Scans(shortSum);
				
				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "--mark6slot") == 0)
			{
                                ++a;
                                slot = atoi(argv[a]);
                                if(slot < 1 || slot > 4)
                                {
                                        exit(EXIT_FAILURE);
                                }
                                
				processMark6ScansSlot(slot, shortSum);
				
				exit(EXIT_SUCCESS);
			}
#endif
			else if(exhaustive)
			{
				summarizeFileExhaustive(argv[a], isMark6);
			}
			else
			{
				summarizeFile(argv[a], shortSum, isMark6, toff);
			}
		}
	}

	return 0;
}
