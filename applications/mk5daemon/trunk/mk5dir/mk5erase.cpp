/***************************************************************************
 *   Copyright (C) 2010-2012 by Walter Brisken                             *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <vector>
#include <cstdio> 
#include <cstdlib>
#include <cstring> 
#include <unistd.h>
#include <ctype.h>
#include <sys/time.h>
#include <signal.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include <xlrapi.h>
#include "watchdog.h"
#include "mark5dir.h"

/* Note: this program is largely based on Haystack's SSErase.  Thanks
 * to John Ball and Dan Smythe for providing a nice template.
 * Improvements to the original include:
 *  1. Integration to the difxmessage system
 *  2. Watchdog around all XLR function calls
 *  3. The module rate is based on lowest performance portion of test
 *  4. Fast modes (read-only or write-only) possible
 *  5. Both new and legacy directory formats supported
 *
 * Other differences:
 *  1. Can only operate on one module at a time
 *  2. Reported progress considers 2 passes
 *  3. Increased computer friendliness of text output
 */

/* TODO
 *  1. Find a way to save the hostname to the condition database
 *  2. Allow erasing and VSN setting on drives last used with newer SKD version
 */

const char program[] = "mk5erase";
const char author[]  = "Walter Brisken";
const char version[] = "0.5";
const char verdate[] = "20120801";


#define MJD_UNIX0       40587.0
#define SEC_DAY         86400.0
#define USEC_DAY        86400000000.0 


int die = 0;
const int statsRange[] = { 75000, 150000, 300000, 600000, 1200000, 2400000, 4800000, -1 };


enum ConditionMode
{
	CONDITION_ERASE_ONLY = 0,
	CONDITION_READ_WRITE,
	CONDITION_READ_ONLY,
	CONDITION_WRITE_ONLY
};

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to erase or condition a Mark5 module.\n");
	printf("\nUsage : %s [<options>] <vsn>\n\n", pgm);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("  --quiet\n");
	printf("  -q             Be less verbose\n\n");
	printf("  --condition\n");
	printf("  -c             Do full conditioning, not just erasing\n\n");
	printf("  --readonly\n");
	printf("  -r             Perform read-only conditioning mode\n\n");
	printf("  --writeonly\n");
	printf("  -w             Perform write-only conditioning mode\n\n");
	printf("  --force\n");
	printf("  -f             Don't ask to continue\n\n");
	printf("  --getdata\n");
	printf("  -d             Save time domain performance data\n\n");
	printf("  --legacydir\n");
	printf("  -l             Force a legacy directory on the module\n\n");
	printf("  --nodir\n");
	printf("  -0             Put a zero length directory on the module\n\n");
	printf("  --newdir\n");
	printf("  -n             Force a new directory structure on the module\n\n");
	printf("<vsn> is a valid module VSN (8 characters)\n\n");
	printf("Note: A single Mark5 unit needs to be installed in bank A for\n");
	printf("proper operation.  If the VSN is not set, use the  vsn  utility\n");
	printf("To assign it prior to erasure or conditioning.\n\n");
	printf("Ctrl-C can be used to safely abort.  The module will be left in\n");
	printf("the Error state afterwards.\n\n");
	printf("This program appears to be compiled for SDK version %d\n\n", SDKVERSION);
}

void siginthand(int j)
{
	printf("SIGINT caught; aborting operation.\n");
	die = 1;
}

int getModuleLabel(SSHANDLE xlrDevice, int bank, char label[XLR_LABEL_LENGTH+1])
{
	S_BANKSTATUS bankStatus;
	S_DIR dir;

	label[0] = 0;

	WATCHDOGTEST( XLRGetBankStatus(xlrDevice, bank, &bankStatus) );
	if(bankStatus.State != STATE_READY)
	{
		return -1;
	}
	
	/* get label */
	
	WATCHDOGTEST( XLRSelectBank(xlrDevice, bank) );

	/* the following line is essential to work around an apparent streamstor bug */
	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );

	label[XLR_LABEL_LENGTH] = 0;

	return 0;
}

int erase(SSHANDLE xlrDevice)
{
	WATCHDOGTEST( XLRErase(xlrDevice, SS_OVERWRITE_NONE) );

	return 0;
}

double summarizeRates(const std::vector<double> &r, const char *passName)
{
	double min = 1.0e19;
	double max = 0.0;
	double avg = 0.0;
	printf("Summary for %s pass\n", passName);
	printf("  Number of rate samples: %d\n", r.size());
	if(r.size() < 4)
	{
		printf("  (This is too few samples for statistical analysis)\n");
	}
	else
	{
		unsigned int n = r.size();
		for(unsigned int i = 2; i < n-2; ++i)	/* don't include end points in statistics */
		{
			avg += r[i];
			if(r[i] > max)
			{
				max = r[i];
			}
			if(r[i] < min)
			{
				min = r[i];
			}
		}
		avg /= (r.size() - 4);
		printf("  Maximum rate = %7.2f Mbps\n", max);
		printf("  Average rate = %7.2f Mbps\n", avg);
		printf("  Minimum rate = %7.2f Mbps\n", min);
		printf("  First three rates were %7.2f, %7.2f and %7.2f Mbps\n", r[0], r[1], r[2]);
		printf("  Last three rates were  %7.2f, %7.2f and %7.2f Mbps\n", r[n-3], r[n-2], r[n-1]);
	}

	return min;
}

int condition(SSHANDLE xlrDevice, const char *vsn, enum ConditionMode mode, DifxMessageMk5Status *mk5status, int verbose, int getData, const struct DriveInformation drive[8], int *rate)
{
	XLR_RETURN_CODE xlrRC;
	S_DEVSTATUS devStatus;
	S_DRIVESTATS driveStats[XLR_MAXBINS];
	S_DEVINFO devInfo;
	S_DRIVEINFO driveInfo;
	long long len, lenLast=-1;
	long long lenFirst=0;
	struct timeval time1, time2;
	double dt;
	int nPass, pass = 0;
	char opName[10] = "";
	FILE *out=0;
	std::vector<double> rate0;
	std::vector<double> rate1;
	char message[DIFX_MESSAGE_LENGTH];
	DifxMessageDriveStats driveStatsMessage;
	const int printInterval = 10;

	mk5status->state = MARK5_STATE_CONDITION;
	difxMessageSendMark5Status(mk5status);
	
	WATCHDOGTEST( XLRGetDeviceInfo(xlrDevice, &devInfo) );

	/* configure collection of drive statistics */
	WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_DRVSTATS) );
	for(int b = 0; b < XLR_MAXBINS; ++b)
	{
		driveStats[b].range = statsRange[b];
		driveStats[b].count = 0;
	}
	WATCHDOGTEST( XLRSetDriveStats(xlrDevice, driveStats) );

	if(mode == CONDITION_WRITE_ONLY)
	{
		WATCHDOGTEST( XLRErase(xlrDevice, SS_OVERWRITE_RANDOM_PATTERN) );
		nPass = 1;
		strcpy(opName, "W");
	}
	else if(mode == CONDITION_READ_ONLY)
	{
		WATCHDOGTEST( XLRErase(xlrDevice, SS_OVERWRITE_RW_PATTERN) );
		nPass = 1;
		strcpy(opName, "R");
	}
	else
	{
		WATCHDOGTEST( XLRErase(xlrDevice, SS_OVERWRITE_RW_PATTERN) );
		nPass = 2;
		strcpy(opName, "RW");
	}

	gettimeofday(&time1, 0);

	if(getData)
	{
		const int FilenameLength = 256;
		char fileName[FilenameLength];

		snprintf(fileName, FilenameLength, "%s.timedata", vsn);
		printf("Opening %s for output...\n", fileName);
		out = fopen(fileName, "w");
		if(!out)
		{
			fprintf(stderr, "Warning: cannot open %s for output.\nContuniung anyway.\n", fileName);
		}
	}

	for(int n = 0; ; ++n)
	{
		WATCHDOG( len = XLRGetLength(xlrDevice) );
		if(lenLast < 0)
		{
			lenFirst = lenLast = len;
		}
		WATCHDOGTEST( XLRGetDeviceStatus(xlrDevice, &devStatus) );
		if(!devStatus.Recording)
		{
			if(lenLast > 1000000000LL)
			{
				// more than 5 seconds at 2 Gbps remaining, indicates early completion
				snprintf(message, DIFX_MESSAGE_LENGTH, "Pass %d of conditioning ended prematurely at around %Ld bytes", pass+1, lenLast);
				fprintf(stderr, "Warning: %s\n", message);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			}

			break;	/* must be done */
		}
		if(die)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Conditioning aborted.");
			fprintf(stderr, "%s\n", message);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);

			break;
		}
		if(n == printInterval || lenLast < len)
		{
			long long bytes;
			double done;

			if(n == printInterval)
			{
				n = 0;
			}

			bytes = -(len-lenLast);	// It is counting down
			if(lenLast < len)
			{
				if(lenLast > 1000000000LL)
				{
					// more than 5 seconds at 2 Gbps remaining, indicates early completion
					snprintf(message, DIFX_MESSAGE_LENGTH, "Pass %d of conditioning ended prematurely at around %Ld bytes", pass+1, lenLast);
					fprintf(stderr, "Warning: %s\n", message);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				}

				++pass;
				if(pass >= nPass)
				{
					break;
				}
				bytes += lenFirst;
			}
			done = 100.0*(double)(lenFirst-len)/(double)lenFirst;
			done = done/nPass + 50.0*pass;
			mk5status->position = devInfo.NumBuses*len;
			mk5status->rate = 8*devInfo.NumBuses*bytes/(printInterval*1000000.0);
			snprintf(mk5status->scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "%s[%4.2f%%]", opName, done);
			if(bytes > 0 && mk5status->rate < 6000)
			{
				if(pass == 0)
				{
					rate0.push_back(mk5status->rate);
				}
				else
				{
					rate1.push_back(mk5status->rate);
				}
			}
			gettimeofday(&time2, 0);
			dt = (time2.tv_sec - time1.tv_sec) + (time2.tv_usec - time1.tv_usec)/1000000.0;
			
			if(len != lenLast)
			{
				mk5status->state = MARK5_STATE_CONDITION;
			}
			else
			{
				mk5status->state = MARK5_STATE_COND_ERROR;
			}
			mk5status->dataMJD = dt/SEC_DAY;
			difxMessageSendMark5Status(mk5status);

			snprintf(message, DIFX_MESSAGE_LENGTH, ". Time = %8.3f  Pos = %14Ld  Rate = %7.2f  Done = %5.2f %%\n", dt, mk5status->position, mk5status->rate, done);
			printf("%s", message);
			if(out)
			{
				fprintf(out, "%s", message);
				fflush(out);
			}

			lenLast = len;
		}
		sleep(1);
	}


	if(out)
	{
		fclose(out);
	}

	gettimeofday(&time2, 0);
	dt = (time2.tv_sec - time1.tv_sec) + (time2.tv_usec - time1.tv_usec)/1000000.0;

	printf("\n");

	if(devStatus.Recording)
	{
		WATCHDOG( xlrRC = XLRStop(xlrDevice) );
	}

	if(!die)
	{
		const int hostnameLength = 32;
		char hostname[hostnameLength];
		double lowestRate = 0.0;

		printf("> %s Conditioning %s took %7.2f seconds\n", opName, vsn, dt);

		gethostname(hostname, hostnameLength);
		printf("> Hostname %s\n", hostname);

		driveStatsMessage.startMJD = MJD_UNIX0 + time1.tv_sec/SEC_DAY + time1.tv_usec/USEC_DAY;
		driveStatsMessage.stopMJD = MJD_UNIX0 + time2.tv_sec/SEC_DAY + time2.tv_usec/USEC_DAY;
		strncpy(driveStatsMessage.moduleVSN, vsn, DIFX_MESSAGE_MARK5_VSN_LENGTH);
		driveStatsMessage.moduleVSN[DIFX_MESSAGE_MARK5_VSN_LENGTH] = 0;
		driveStatsMessage.startByte = 0LL;
		switch(mode)
		{
		case CONDITION_WRITE_ONLY:
			lowestRate = summarizeRates(rate0, "Write");
			driveStatsMessage.type = DRIVE_STATS_TYPE_CONDITION_W;
			break;
		case CONDITION_READ_ONLY:
			lowestRate = summarizeRates(rate0, "Read");
			driveStatsMessage.type = DRIVE_STATS_TYPE_CONDITION_R;
			break;
		default:
			summarizeRates(rate0, "Read");
			lowestRate = summarizeRates(rate1, "Write");
			driveStatsMessage.type = DRIVE_STATS_TYPE_CONDITION;
			break;
		}

		*rate = 128*(int)(lowestRate/128.0);

		for(int d = 0; d < 8; ++d)
		{
			WATCHDOG( xlrRC = XLRGetDriveInfo(xlrDevice, d/2, d%2, &driveInfo) );
			if(xlrRC == XLR_SUCCESS)
			{
				driveStatsMessage.moduleSlot = d;
				strncpy(driveStatsMessage.serialNumber, drive[d].serial, DIFX_MESSAGE_DISC_SERIAL_LENGTH);
				strncpy(driveStatsMessage.modelNumber, drive[d].model, DIFX_MESSAGE_DISC_MODEL_LENGTH);
				driveStatsMessage.diskSize = drive[d].capacity/1000000000LL;
				printf("> Disk %d stats : %s", d, drive[d].serial);
				for(int i = 0; i < DIFX_MESSAGE_N_CONDITION_BINS; ++i)
				{
					driveStatsMessage.bin[i] = -1;
				}
				WATCHDOG( xlrRC = XLRGetDriveStats(xlrDevice, d/2, d%2, driveStats) );
				if(xlrRC == XLR_SUCCESS)
				{
					for(int i = 0; i < XLR_MAXBINS; ++i)
					{
						if(i < DIFX_MESSAGE_N_CONDITION_BINS)
						{
							driveStatsMessage.bin[i] = driveStats[i].count;
							printf(" : %d", driveStatsMessage.bin[i]);
						}
					}
				}
				else
				{
					XLRGetErrorMessage(message, XLRGetLastError());
					printf(" : %s", message);
				}
				printf("\n");
				difxMessageSendDriveStats(&driveStatsMessage);
			}
			else
			{
				printf("! Disk %d stats : Not found!\n", d);
			}
		}
	}

	return 0;
}
	
int mk5erase(const char *vsn, enum ConditionMode mode, int verbose, int dirVersion, int getData)
{
	SSHANDLE xlrDevice;
	S_DEVSTATUS devStatus;
	char label[XLR_LABEL_LENGTH+1];
	struct DriveInformation drive[8];
	int nDrive;
	int totalCapacity;		/* in GB (approx) */
	int rate = 1024;		/* default Mbps, for label */
	int v;
	int newStatus = MODULE_STATUS_ERASED;
	DifxMessageMk5Status mk5status;
	char message[DIFX_MESSAGE_LENGTH];

	memset((char *)(&mk5status), 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );

	getModuleLabel(xlrDevice, BANK_A, label);
	strncpy(mk5status.vsnA, label, 8);
	getModuleLabel(xlrDevice, BANK_B, label);

	if(mode != CONDITION_ERASE_ONLY)
	{
		if(label[0])
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Conditioning requires module %s to be alone in bank A.", vsn);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			fprintf(stderr, "%s\n", message);
			WATCHDOG( XLRClose(xlrDevice) );

			return -1;
		}
	}

	if(strncmp(vsn, label, 8) != 0)
	{
		getModuleLabel(xlrDevice, BANK_A, label);
		if(strncmp(vsn, label, 8) != 0)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Module %s not found", vsn);
			fprintf(stderr, "%s\n", message);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			WATCHDOG( XLRClose(xlrDevice) );

			return -1;
		}
		else
		{
			strncpy(mk5status.vsnA, label, 8);
			mk5status.activeBank = 'A';
		}
	}
	else
	{
		strncpy(mk5status.vsnB, label, 8);
		mk5status.activeBank = 'B';
	}

	if(strlen(label) > 10)
	{
		int c, r, n;

		n = sscanf(label+9, "%d/%d", &c, &r);
		if(n == 2 && c > 0 && r > 0 && r < 100000)
		{
			rate = r;
		}
		else
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Extended VSN is corrupt.  Assuming rate = 1024 Mbps.");
			fprintf(stderr, "Warning: %s\n", message);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			rate = 1024;
		}
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "No extended VSN found.  Assuming rate = 1024 Mbps.");
		fprintf(stderr, "Warning: %s\n", message);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		rate = 1024;
	}

	/* set operational mode */
	WATCHDOGTEST( XLRSetMode(xlrDevice, SS_MODE_PCI) );
	WATCHDOGTEST( XLRGetDeviceStatus(xlrDevice, &devStatus) );
	WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	WATCHDOGTEST( XLRClearWriteProtect(xlrDevice) );

	/* Get drive info */
	nDrive = getDriveInformation(xlrDevice, drive, &totalCapacity);

	printf("> Module %s consists of %d drives totalling about %d GB:\n", vsn, nDrive, totalCapacity);
	for(int d = 0; d < 8; ++d)
	{
		if(drive[d].model[0] == 0)
		{
			continue;
		}
		printf("> Disk %d info : %s : %s : %s : %d : %s\n",
			d, drive[d].model, drive[d].serial, drive[d].rev, 
			roundModuleSize(drive[d].capacity),
			drive[d].failed ? "FAILED" : "OK");
	}

	/* Module gets erased first, regardless of conditioning... */
	v = erase(xlrDevice);
	if(v < 0)
	{
		/* Something bad happened.  Bail! */
		return v;
	}
	
	if(mode != CONDITION_ERASE_ONLY)
	{
		/* here doing the full condition */
		v = condition(xlrDevice, vsn, mode, &mk5status, verbose, getData, drive, &rate);
		if(v < 0)
		{
			/* Something bad happened.  Bail! */
			return v;
		}
	}

	if(die)
	{
		newStatus = MODULE_STATUS_UNKNOWN;
	}

	v = resetModuleDirectory(xlrDevice, vsn, newStatus, dirVersion, totalCapacity, rate);
	if(v < 0)
	{
		/* Something bad happened to the XLR device.  Bail! */
		return v;
	}
	dirVersion = v;

	v = setModuleLabel(xlrDevice, vsn, newStatus, dirVersion, totalCapacity, rate);
	if(v < 0)
	{
		/* Something bad happened to the XLR device.  Bail! */
		return v;
	}

	/* finally close the device */
	WATCHDOG( XLRClose(xlrDevice) );

	mk5status.scanName[0] = 0;
	mk5status.state = MARK5_STATE_IDLE;
	mk5status.rate = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

int main(int argc, char **argv)
{
	enum ConditionMode mode = CONDITION_ERASE_ONLY;
	int verbose = 0;
	int force = 0;
	int getData = 0;
	char vsn[10] = "";
	char resp[12] = " ";
	char *rv;
	int v;
	int dirVersion = -1;
	int lockWait = MARK5_LOCK_DONT_WAIT;
	int retval = EXIT_SUCCESS;

	if(argc < 2)
	{
		printf("Please run with -h for help\n");
		
		return EXIT_FAILURE;
	}

	for(int a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		        strcmp(argv[a], "--verbose") == 0)
		{
			++verbose;
		}
		else if(strcmp(argv[a], "-q") == 0 ||
		        strcmp(argv[a], "--quiet") == 0)
		{
			--verbose;
		}
		else if(strcmp(argv[a], "-c") == 0 ||
		        strcmp(argv[a], "--condition") == 0)
		{
			mode = CONDITION_READ_WRITE;
		}
		else if(strcmp(argv[a], "-r") == 0 ||
		        strcmp(argv[a], "--readonly") == 0)
		{
			mode = CONDITION_READ_ONLY;
		}
		else if(strcmp(argv[a], "-w") == 0 ||
		        strcmp(argv[a], "--writeonly") == 0)
		{
			mode = CONDITION_WRITE_ONLY;
		}
		else if(strcmp(argv[a], "-f") == 0 ||
		        strcmp(argv[a], "--force") == 0)
		{
			force = 1;
		}
		else if(strcmp(argv[a], "-l") == 0 ||
		        strcmp(argv[a], "--legacydir") == 0)
		{
			dirVersion = 0;
		}
		else if(strcmp(argv[a], "-n") == 0 ||
		        strcmp(argv[a], "--newdir") == 0)
		{
			dirVersion = 1;
		}
		else if(strcmp(argv[a], "-0") == 0 ||
		        strcmp(argv[a], "--nodir") == 0)
		{
			dirVersion = -2;
		}
		else if(strcmp(argv[a], "-d") == 0 ||
		        strcmp(argv[a], "--getdata") == 0)
		{
			getData = 1;
		}
		else if(strcmp(argv[a], "--wait-forever") == 0)
		{
			lockWait = MARK5_LOCK_WAIT_FOREVER;
		}
		else if(argv[a][0] == '-')
		{
			fprintf(stderr, "Unknown option %s provided\n", argv[a]);
			
			return EXIT_FAILURE;
		}
		else
		{
			if(vsn[0])
			{
				fprintf(stderr, "Error: two VSNs provided : %s and %s\n",
					vsn, argv[a]);
				
				return EXIT_FAILURE;
			}
			strncpy(vsn, argv[a], 10);
			vsn[9] = 0;
			if(strlen(vsn) != 8)
			{
				fprintf(stderr, "Error: VSN %s not 8 chars long!\n", argv[a]);
				
				return EXIT_FAILURE;
			}
			for(int i = 0; i < 8; ++i)
			{
				vsn[i] = toupper(vsn[i]);
			}
		}
	}

	if(vsn[0] == 0)
	{
		printf("Error: no VSN provided!\n");
		
		return EXIT_FAILURE;
	}

	printf("About to proceed.  verbose=%d mode=%d force=%d vsn=%s\n", verbose, mode, force, vsn);

	if(!force)
	{
		if(mode != CONDITION_ERASE_ONLY)
		{
			printf("\nAbout to condition module %s.  This will destroy all\n", vsn);
			printf("contents of the module and will take a long time.  Are\n");
			printf("you sure you want to continue? [y|n]\n");
		}
		else
		{
			printf("\nAbout to erase module %s.  This will destroy all\n", vsn);
			printf("contents of the module.  Are you sure you want to\n");
			printf("continue? [y|n]\n");
		}
		for(;;)
		{
			rv = fgets(resp, 10, stdin);
			if(!rv)
			{
				/* must be ^D or similar */

				return EXIT_SUCCESS;
			}
			if(strcmp(resp, "Y\n") == 0 || strcmp(resp, "y\n") == 0)
			{
				if(verbose)
				{
					printf("OK.  Continuing\n\n");
				}
				break;
			}
			else if(strcmp(resp, "N\n") == 0 || strcmp(resp, "n\n") == 0)
			{
				printf("Module erasure cancelled.\n\n");
				
				return EXIT_SUCCESS;
			}
			else
			{
				printf("Your response was not understood.\n");
				printf("Continue? [y|n]\n");
			}
		}
	}

	v = initWatchdog();
	if(v < 0)
	{
		fprintf(stderr, "Error: Cannot start watchdog!\n");
		
		return EXIT_FAILURE;
	}

	signal(SIGINT, siginthand);

	/* 60 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(60);

	difxMessageInit(-1, program);

	/* *********** */

	v = lockMark5(lockWait);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = mk5erase(vsn, mode, verbose, dirVersion, getData);
		if(v < 0)
		{
			if(watchdogXLRError[0] != 0)
			{
				char message[DIFX_MESSAGE_LENGTH];
				snprintf(message, DIFX_MESSAGE_LENGTH, 
					"Streamstor error executing: %s : %s",
					watchdogStatement, watchdogXLRError);
				difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			}
			retval = EXIT_FAILURE;
		}
	}

	unlockMark5();

	/* *********** */

	stopWatchdog();

	return retval;
}
