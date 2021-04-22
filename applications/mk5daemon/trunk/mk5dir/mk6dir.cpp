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
#include <sys/time.h>

#define READBUF_SIZE 100000

const char program[] = "mk6dir";
const char author[]  = "Mark Wainright <mwainrig@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20190604";

unsigned char readbuf[READBUF_SIZE];
time_t t1, t2;

// FIXME: module groups are not supported; use VSN of specified slot, but look up groups in '/mnt/disks/.meta/<slot>/*/group', pass those slots as integer 1/2/3/4/12/34/1234 to processMark6ScansSlot()

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


void summarizeFile(const char *fileName, const char* filePattern, char *vsn, char activityMsg, char verbose, int fileidx, int numfiles, FILE *summaryFile)
{
    uint64_t filesize;
    uint64_t datasize = 0;
    uint32_t blocksize;
    int packetformat;
    int packetsize;
    int m6headersize;
    int m6blockheadersize;
    uint64_t numblocks;
    uint64_t datasizeinfile;
    struct vdif_file_summary sum;
    struct mark5b_file_summary sum5b;
    int r, i, j, rv, N;
    const int MaxFilenameLength = 512;
    double mjd1, mjd2;
    double mjdstart, mjdend;
    char fullFileName[MaxFilenameLength];
    char pattern[256];
    char *diskIndex;
    FILE *fp;
    struct stat st;
    unsigned char *p;
    Mark6Header *m6header;
    struct vdif_header *vdifhdr, *vh0;

    mjdstart = 0;
    mjdend = 0;
    r = 0;

    strcpy(pattern, filePattern);
    diskIndex = strchr(pattern, '*');

    m6headersize = sizeof(Mark6Header);

    // open all the files
    for(i = 0; i < 8; i++)
    {
        diskIndex[0] = '0' + i;
        fp = fopen(pattern, "r");
        if(fp)
        {
            fread(readbuf, 1, READBUF_SIZE, fp);
            m6header = (Mark6Header*)readbuf;
            blocksize = m6header->block_size;
            packetformat = m6header->packet_format;
	    packetsize = m6header->packet_size;
	    m6blockheadersize = mark6BlockHeaderSize(m6header->version);
            stat(pattern, &st);
                
            mjd1 = 0;
            mjd2 = 0;
            // mark5b format
            if(packetformat == 1)
            {
                r = determinemark5bframeoffset(readbuf, READBUF_SIZE);
		if(r >= 0)
		{
                    // get start day and second
                    p = readbuf + r;
                    sum5b.startDay = (p[11] >> 4)*100 + (p[11] & 0x0F)*10 + (p[10] >> 4);
                    sum5b.startSecond = (p[10] & 0x0F)*10000 + (p[9] >> 4)*1000 + (p[9] & 0x0F)*100 + (p[8] >> 4)*10 +  (p[8] & 0x0F);
                    mark5bfilesummaryfixmjdtoday(&sum5b);
                    mjd1 = sum5b.startDay + (sum5b.startSecond % 86400)/86400.0;

                    // get end day and second
                    rv = fseeko(fp, st.st_size - READBUF_SIZE, SEEK_SET);
                    if(rv == 0)
                    {
                        rv = fread(readbuf, 1, READBUF_SIZE, fp);
                        r = determinelastmark5bframeoffset(readbuf, READBUF_SIZE);
                        p = readbuf + r;
                        sum5b.endSecond = (p[10] & 0x0F)*10000 + (p[9] >> 4)*1000 + (p[9] & 0x0F)*100 + (p[8] >> 4)*10 +  (p[8] & 0x0F);
                        if(sum5b.endSecond < sum5b.startSecond)
                        {
                            sum5b.endSecond += 86400;
                        }
                        mjd2 = mjd1 + (sum5b.endSecond - sum5b.startSecond + 1)/86400.0;
                    }
		}
            }
            // vdif format
            else if(packetformat == 0)
            {
                sum.frameSize = packetsize;
                r = determinevdifframeoffset(readbuf, READBUF_SIZE, packetsize);
                if(r >= 0)
                {
                    // get start day and second
                    vdifhdr = (vdif_header*)(readbuf + r);
                    sum.epoch = getVDIFEpoch(vdifhdr);
                    sum.startSecond = getVDIFFrameEpochSecOffset(vdifhdr);
                    sum.endSecond = sum.startSecond;
                    sum.nBit = getVDIFBitsPerSample(vdifhdr);
                    mjd1 = vdiffilesummarygetstartmjd(&sum) + (sum.startSecond % 86400)/86400.0;

                    // get end day and second
                    rv = fseeko(fp, st.st_size - READBUF_SIZE, SEEK_SET);
                    if(rv == 0)
                    {
                        rv = fread(readbuf, 1, READBUF_SIZE, fp);
                        r = determinevdifframeoffset(readbuf, READBUF_SIZE, packetsize);
                        vh0 = (struct vdif_header *)(readbuf + r);
                        N = READBUF_SIZE - packetsize - VDIF_HEADER_BYTES;

                        for(j = 0; j < N; )
                        {
                            struct vdif_header *vh;
                            int s;

                            vh = (struct vdif_header *)(readbuf + j);
                            s = getVDIFFrameEpochSecOffset(vh);

                            if(getVDIFFrameBytes(vh) == packetsize &&
                                getVDIFEpoch(vh) == sum.epoch &&
                                getVDIFBitsPerSample(vh) == sum.nBit &&
                                !getVDIFFrameInvalid(vh) &&
                                abs(s - getVDIFFrameEpochSecOffset(vh0)) < 2)
                            {
			        if(s > sum.endSecond)
			        {
                                    sum.endSecond = s;
                                }
			            j += packetsize;
                            }
                            else
                            {
                                /* Not a good frame. */
                                ++j;
                            }
                        }
                        if(sum.endSecond < sum.startSecond)
                        {
                            sum.endSecond += 86400;
                        }
                        mjd2 = mjd1 + (sum.endSecond - sum.startSecond + 1)/86400.0;
                    }
		}
            }

            if(mjdstart == 0.0 && mjd1 > 0.0)
	    {
	        mjdstart = mjd1;
	    }
	    else if(mjd1 < mjdstart)
	    {
	        mjdstart = mjd1;
	    }
            if(mjdend == 0.0 && mjd2 > 0.0)
	    {
	        mjdend = mjd2;
	    }
	    else if(mjd2 > mjdend)
	    {
	        mjdend = mjd2;
	    }

            stat(pattern, &st);
            filesize = st.st_size;
	    numblocks = (filesize - m6headersize) / blocksize;
	    datasizeinfile = (blocksize - m6blockheadersize) * numblocks;
            datasize += datasizeinfile;
            fclose(fp);
        }
    }

    snprintf(fullFileName, MaxFilenameLength, "%s", fileName);
    if(verbose)
    {
        printf("File %s\n", filePattern);
        printf("%s %14.8f %14.8f - %4d of %d\n", fullFileName, mjdstart, mjdend, fileidx + 1, numfiles);
    }
    t2 = time(0);
    // send activity every 2 seconds
    if(activityMsg && ((t2 > t1 + 2) || (fileidx + 1 == numfiles)))
    {
        t1 = t2;
        DifxMessageMark6Activity m6activity;
        memset(&m6activity, 0, sizeof(m6activity));

        m6activity.state = MARK6_STATE_GETDIR;
        strcpy(m6activity.activeVsn, vsn);
        strcpy(m6activity.scanName, fullFileName);
        // send percent complete in last 3 digits and file index in preceding digits
        m6activity.position = (((fileidx + 1) * 100) / numfiles) + 1000 * (fileidx + 1);
        m6activity.dataMJD = mjdstart;

        difxMessageSendMark6Activity(&m6activity);
    }
    fprintf(summaryFile, "%s %14.8f %14.8f %lu\n", fullFileName, mjdstart, mjdend, datasize);

}

void processMark6ScansSlot(int slot, char *vsn, char activityMsg, char verbose, const char *mk5dirpath)
{
	char **fileList;
	int n;
	char summaryFilePath[256];
	char filePattern[256];
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

		t1 = 0;
		for(i = 0; i < n; ++i)
		{
			memset(filePattern, 0x00, sizeof(filePattern));
			snprintf(filePattern, sizeof(filePattern)-1, "/mnt/disks/%d/*/data/%s", slot, fileList[i]);
			summarizeFile(fileList[i], filePattern, vsn, activityMsg, verbose, i, n, summaryFile);
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

