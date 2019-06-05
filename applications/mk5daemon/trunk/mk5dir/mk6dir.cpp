/***************************************************************************
 *   Copyright (C) 2018 by Mark Wainright                                  *
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
#include <mark5access/mark6gather_mark5b.h>
#include <vdifio.h>
#include <mark6sg/mark6gather.h>
#include <mark6gather_vdif.h>
#include <difxmessage.h>
#include "config.h"

const char program[] = "mk6dir";
const char author[]  = "Mark Wainright <mwainrig@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20190604";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A program to list the files on a mark6 module in a form\n");
	printf("that can be used as a vex2difx filelist.\n");
	printf("This program is run locally on a mark6 unit, and the\n");
	printf("slot number or VSN for the module to be listed must be provided.\n");
	printf("This program can also change the state of a mark6 module\n");
	printf("to \'cataloged\' (VLBA use).\n\n");
	printf("Usage: %s [<options>] <slot number | VSN>\n\n", pgm);
	printf("  <options> can include:\n");
	printf("    -h or --help       print this usage information and quit\n");
	printf("    -v or --verbose    send more output to the screen\n");
	printf("    -c or --catalog    change module state to \'cataloged\' (VLBA use)\n");
	printf("    -m or --message    output mark6 activity difxmessage while running\n");
	printf("\nThis program This program will write the filelist to the directory defined by\n");
	printf("environment variable MARK5_DIR_PATH.\n");
	printf("If this variable is not defined this program will not function.\n");
	printf("\nThe filelist file will have the name <VSN>.filelist where <VSN>\n");
	printf("is the module serial number.\n");
	printf("\nExamples:\n");
	printf("\n  mk6dir -v 1         # generate filelist for module in slot 1\n");
	printf("\n  mk6dir -v LBO%%0001  # generate filelist for module with VSN LBO%%0001\n");
	printf("\n  mk6dir -m -c 2      # generate filelist for module in slot 2, change state to \"cataloged\",\n");
	printf("                        and output mark6 activity difxmessage\n\n");
}

void summarizeFile(const char *fileName, char *vsn, char activityMsg, char verbose, int fileidx, int numfiles, FILE *summaryFile)
{
	struct vdif_file_summary sum;
	struct mark5b_file_summary sum5b;
	int r;
	const int MaxFilenameLength = 512;
	double mjd1, mjd2;
	char fullFileName[MaxFilenameLength];

	r = summarizemark5bmark6(&sum5b, fileName);

	if(r == 0)
	{
		mark5bfilesummaryfixmjdtoday(&sum5b);
		mjd1 = mark5bfilesummarygetstartmjd(&sum5b) + (sum5b.startSecond % 86400)/86400.0;
		if(sum5b.endSecond < sum5b.startSecond)
		{
			sum5b.endSecond += 86400;
		}
		mjd2 = mjd1 + (sum5b.endSecond - sum5b.startSecond + 1)/86400.0;

		snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
		if(verbose)
		{
			printf("File %s is mark5b\n", fileName);
			printf("%s %14.8f %14.8f - %4d of %d\n", fullFileName, mjd1, mjd2, fileidx + 1, numfiles);
		}
		if(activityMsg)
		{
			DifxMessageMark6Activity m6activity;
			memset(&m6activity, 0, sizeof(m6activity));

			m6activity.state = MARK6_STATE_GETDIR;
			strcpy(m6activity.activeVsn, vsn);
			strcpy(m6activity.scanName, fullFileName);
			// send percent complete in last 3 digits and file index in preceding digits
			m6activity.position = (((fileidx + 1) * 100) / numfiles) + 1000 * (fileidx + 1);
			m6activity.dataMJD = mjd1;

			difxMessageSendMark6Activity(&m6activity);
		}
		fprintf(summaryFile, "%s %14.8f %14.8f %Ld\n", fullFileName, mjd1, mjd2, sum5b.fileSize);
		return;
	}

	r = summarizevdifmark6(&sum, fileName, 0);

	if(r < 0)
	{
		fprintf(stderr, "File %s VDIF summary failed with return value %d\n\n", fileName, r);
	}
	else
	{
		mjd1 = vdiffilesummarygetstartmjd(&sum) + (sum.startSecond % 86400)/86400.0;
		mjd2 = mjd1 + (sum.endSecond - sum.startSecond + 1)/86400.0;

		snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
		if(verbose)
		{
			printf("File %s is vdif\n", fileName);
			printf("%s %14.8f %14.8f - %4d of %d\n", fullFileName, mjd1, mjd2, fileidx + 1, numfiles);
		}
		if(activityMsg)
		{
			DifxMessageMark6Activity m6activity;
			memset(&m6activity, 0, sizeof(m6activity));

			m6activity.state = MARK6_STATE_GETDIR;
			strcpy(m6activity.activeVsn, vsn);
			strcpy(m6activity.scanName, fullFileName);
			// send percent complete in last 3 digits and file index in preceding digits
			m6activity.position = (((fileidx + 1) * 100) / numfiles) + 1000 * (fileidx + 1);
			m6activity.dataMJD = mjd1;

			difxMessageSendMark6Activity(&m6activity);
		}
		fprintf(summaryFile, "%s %14.8f %14.8f %Ld\n", fullFileName, mjd1, mjd2, sum.fileSize);
	}
}

void processMark6ScansSlot(int slot, char *vsn, char activityMsg, char verbose, const char *mk5dirpath)
{
	char **fileList;
	int n;
	char summaryFilePath[256];
	FILE *summaryFile;
	
	sprintf(summaryFilePath, "%s/%s.filelist", mk5dirpath, vsn);
	summaryFile = fopen(summaryFilePath, "w");
	if(!summaryFile)
	{
		fprintf(stderr, "mk6dir in processMark6ScansSlot() could not open summaryFile %s!\n", summaryFilePath);
		return;
	}
	setbuf(summaryFile, NULL);

	n = getMark6SlotFileList(slot, &fileList);

	if(verbose)
	{
		printf("Summarizing %d files on %s in slot %d.\n", n, vsn, slot);
	}

	if(n == 0)
	{
		printf("No Mark6 files found in /mnt/disks/%d/*/data\n", slot);
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			summarizeFile(fileList[i], vsn, activityMsg, verbose, i, n, summaryFile);
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}

	fclose(summaryFile);
}

int getSlot(char *vsn)
{
	char path[30];
	FILE *fp;
	char line[9];

	for(int j = 1; j <= 4; j++)
	{
		for(int i = 0; i < 8; i++)
		{
			sprintf(path, "/mnt/disks/.meta/%d/%d/eMSN", j, i);
			fp = fopen(path, "r");
			if(!fp)
			{
				continue;
			}
			else
			{
				fgets(line, 9, fp);
				fclose(fp);
				if(strncmp(line, vsn, 8) == 0)
				{
					return j;
				}
				else
				{
					continue;
				}
			}
		}
	}

	return 0;
}

int getVSN(int slot, char *vsn)
{
	char path[30];
	FILE *fp;
	char line[9];

	for(int i = 0; i < 8; i++)
	{
		sprintf(path, "/mnt/disks/.meta/%d/%d/eMSN", slot, i);
		fp = fopen(path, "r");
		if(!fp)
		{
			continue;
		}
		else
		{
			fgets(line, 9, fp);
			fclose(fp);
			if(strlen(line) == 8 && strchr(line, '%') != 0)
			{
				strcpy(vsn, line);
				return 1;
			}
			else
			{
				continue;
			}
		}

	}

	return 0;
}

int main(int argc, char **argv)
{
	const char *mk5dirpath;
	char verbose = 0;
	char activityMsg = 0;
	char catalogState = 0;
	char vsn[9];

	mk5dirpath = getenv("MARK5_DIR_PATH");
	if(!mk5dirpath)
	{
		fprintf(stderr, "mk6dir in main() could not get MARK5_DIR_PATH environment variable\n");
		exit(EXIT_FAILURE);
	}

	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}
	else
	{
		int a, slot = -1;

		for(a = 1; a < argc; ++a)
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "-v") == 0 ||
			        strcmp(argv[a], "--verbose") == 0)
			{
				verbose = 1;
			}
			else if(strcmp(argv[a], "-m") == 0 ||
			        strcmp(argv[a], "--message") == 0)
			{
				activityMsg = 1;
				difxMessageInit(-1, "mk6dir");
			}
			else if(strcmp(argv[a], "-c") == 0 ||
			        strcmp(argv[a], "--catalog") == 0)
			{
				catalogState = 1;
			}
			else if(strlen(argv[a]) == 1 && atoi(argv[a]) >= 1 && atoi(argv[a]) <= 4)
			{
                                slot = atoi(argv[a]);

				if(!getVSN(slot, vsn))
				{
					fprintf(stderr, "mk6dir in main() could not get VSN for slot %d\n", slot);
					exit(EXIT_FAILURE);
				}

				if(verbose)
				{
					printf("Slot %d contains VSN %s.\n", slot, vsn);
				}
                                
				processMark6ScansSlot(slot, vsn, activityMsg, verbose, mk5dirpath);
				
				break;
			}
			else if(strlen(argv[a]) == 8 && strchr(argv[a], '%') != 0)
			{
                                strcpy(vsn, (argv[a]));

				slot = getSlot(vsn);

				if(!slot)
				{
					fprintf(stderr, "mk6dir in main() could not get slot for VSN %s\n", vsn);
					exit(EXIT_FAILURE);
				}
                                
				if(verbose)
				{
					printf("Module with VSN %s is in slot %d.\n", vsn, slot);
				}
                                
				processMark6ScansSlot(slot, vsn, activityMsg, verbose, mk5dirpath);
				
				break;
			}
			else
			{
				fprintf(stderr, "mk6dir in main() - invalid argument: %s\n", argv[a]);
				exit(EXIT_FAILURE);
			}
		}

		// change state if requested
		if(catalogState)
		{
			if(slot < 0)
			{
				printf("No slot was identified via command line.  Doing nothing.\n");
			}
			else
			{
				char cmd[21];

				if(verbose)
				{
					printf("Changing state of module in slot %d to \'cataloged\'.\n", slot);
				}

				sprintf(cmd, "mk6state cataloged %d", slot);
				system(cmd);
			}
		}
	}

	return 0;
}

