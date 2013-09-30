/***************************************************************************
 *   Copyright (C) 2013 by Walter Brisken                                  *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: mk5dir.cpp 3216 2011-04-11 14:55:49Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/mk5dir/mk5dir.cpp $
 * $LastChangedRevision: 3216 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2011-04-11 08:55:49 -0600 (Mon, 11 Apr 2011) $
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <list>
#include <signal.h>
#include <unistd.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>
#include <mark5access.h>
#include "mark5dir.h"
#include "watchdog.h"
#include "config.h"

const char program[] = "mk5map";
const char author[]  = "Walter Brisken";
const char version[] = "0.2";
const char verdate[] = "20130930";

const int defaultGrid = 20;
const int defaultPrecision = 1<<25;
const double defaultFraction = 1.0e-6;
const int BufferLength = 1<<20;	// 1 MiB

enum DMS_Mode
{
	DMS_MODE_UPDATE = 0,
	DMS_MODE_NO_UPDATE,
	DMS_MODE_UPDATE_IF_SAFE,
	DMS_MODE_FAIL_UNLESS_SAFE
};


int verbose = 1;
int die = 0;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to extract Mark5 module directory information via XLR calls\n");
	printf("\nUsage : %s [<options>] { <bank> | <vsn> }\n\n", pgm);
	printf("<options> can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("  --quiet\n");
	printf("  -q             Be more verbose\n\n");
	printf("  --realtime     Use real-time mode when reading\n\n");
	printf("  --rate <R>\n");
	printf("  -r <R>         Try to match a fixed rate of <R> bytes per second\n\n");
	printf("  --fraction <f>\n");
	printf("  -f <f>         Match rates to within <f>*rate of rate [%10.8f]\n\n", defaultFraction);
	printf("  --precision <P>\n");
	printf("  -p <P>         Determine the scan boundaries to <P> bytes or better [%d]\n\n", defaultPrecision);
	printf("  --grid <G>\n");
	printf("  -g <G>         Initially sample the module at <G> equally spaced spots [%d]\n\n", defaultGrid);
	printf("  --begin <B>\n");
	printf("  -b <B>         Begin search at byte position <B> [0]\n\n");
	printf("  --end <E>\n");
	printf("  -e <E>         End search at byte position <E> [full data length]\n\n");
}

void siginthand(int j)
{
	if(verbose)
	{
		printf("Being killed\n");
	}
	die = 1;
}

class Datum
{
public:
	Datum() : byte(-1), mjd(0), sec(0), ns(0), frame(0), framespersecond(0), frameoffset(0), framebytes(0), format(0), tracks(0) {}
	Datum(SSHANDLE *xlrDev, int64_t byte) { populate(xlrDev, byte); }
	int populate(SSHANDLE *xlrDev, int64_t byte);
	void print() const;

	int64_t byte;
	int mjd;
	int sec;
	int ns;
	int frame;
	int framespersecond;
	int frameoffset;

	int framebytes;
	int format;
	int tracks;
};

int Datum::populate(SSHANDLE *xlrDev, int64_t pos)
{
	static streamstordatatype *buffer = 0;
	static int mjdref = 0;
	unsigned int a, b;
	struct mark5_format *mf;
	int n;

	byte = -1;

	if(!buffer)
	{
		buffer = (streamstordatatype *)malloc(BufferLength);
	}

	if(mjdref == 0)
	{
		mjdref = (int)(40587.0 + time(0)/86400.0);
	}

	a = pos >> 32;
	b = pos % (1LL << 32);
	WATCHDOGTEST( XLRReadData(*xlrDev, buffer, a, b, BufferLength));

	mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, BufferLength));
	if(!mf)
	{
		format = -1;
	}
	else
	{
		if(mf->format == 0 || mf->format == 2)  /* VLBA or Mark5B format */
		{
			n = (mjdref - mf->mjd + 500) / 1000;
			mf->mjd += n*1000;
		}
		else if(mf->format == 1)	/* Mark4 format */
		{
			n = (int)((mjdref - mf->mjd + 1826)/3652.4);
			mf->mjd = addDecades(mf->mjd, n);
		}
		
		mjd = mf->mjd;
		sec = mf->sec;
		ns  = mf->ns;
		format = mf->format;
		if(mf->ntrack <= 0 && mf->format == 2)
		{
			mf->ntrack = 32;
		}
		tracks = mf->ntrack;
		framespersecond = int(1000000000.0/mf->framens + 0.5);
		frame = int(mf->ns/mf->framens + 0.5);
		framebytes = mf->framebytes;
		frameoffset = mf->frameoffset;

		printf("tracks=%d framespersecond=%d framebytes=%d framenum=%d frameoffset=%d framens=%f\n", tracks, framespersecond, framebytes, frame, frameoffset, mf->framens);
		
		delete_mark5_format(mf);

		byte = pos + mf->frameoffset;

		if(mf->format == 2)
		{
			unsigned char *ptr;

			ptr = (unsigned char *)buffer + mf->frameoffset;
			frame = ptr[4] + 256*ptr[5];
		}
	}


	if(verbose > 0)
	{
		print();
	}

	return 0;
}

void Datum::print() const
{
	printf("%14Ld  %5d %5d %5d/%5d  %d %d %d  %d\n", byte, mjd, sec, frame, framespersecond, framebytes, format, tracks, frameoffset);
	fflush(stdout);
}

double bytespersecond(const Datum &d1, const Datum &d2)
{
	double dt;	// seconds

	if(d1.byte < 0LL || d2.byte < 0LL || d1.framespersecond <= 0 || d2.framespersecond <= 0)
	{
		return -1;
	}

	dt = (d2.mjd - d1.mjd)*86400.0 + (d2.sec - d1.sec)*1.0 + ((double)(d2.frame)/(double)(d2.framespersecond) - (double)(d1.frame)/(double)(d1.framespersecond));
	if(fabs(dt) < 1e-6)
	{
		return -1;
	}

	return ((double)d2.byte - (double)d1.byte)/dt;
}

double duration(const Datum &d1, const Datum &d2)
{
	double dt;	// seconds

	if(d1.byte < 0LL || d2.byte < 0LL || d1.framespersecond <= 0 || d2.framespersecond <= 0)
	{
		return -1;
	}

	dt = (d2.mjd - d1.mjd)*86400.0 + (d2.sec - d1.sec)*1.0 + ((double)(d2.frame)/(double)(d2.framespersecond) - (double)(d1.frame)/(double)(d1.framespersecond));

	return fabs(dt);
}

static int getBankInfo(SSHANDLE xlrDevice, DifxMessageMk5Status * mk5status, char bank)
{
	S_BANKSTATUS bank_stat;

	if(bank == 'A' || bank == 'a' || bank == ' ')
	{
		WATCHDOGTEST( XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat) );
		if(bank_stat.Label[8] == '/')
		{
			strncpy(mk5status->vsnA, bank_stat.Label, 8);
			mk5status->vsnA[8] = 0;
		}
		else
		{
			mk5status->vsnA[0] = 0;
		}
	}
	if(bank == 'B' || bank == 'b' || bank == ' ')
	{
		WATCHDOGTEST( XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat) );
		if(bank_stat.Label[8] == '/')
		{
			strncpy(mk5status->vsnB, bank_stat.Label, 8);
			mk5status->vsnB[8] = 0;
		}
		else
		{
			mk5status->vsnB[0] = 0;
		}
	}

	return 0;
}

static int mk5map(char *vsn, double rate, double fraction, int64_t precision, int grid, uint64_t begin, uint64_t end, enum Mark5ReadMode readMode)
{
	SSHANDLE xlrDevice;
	S_DIR dir;
	char label[XLR_LABEL_LENGTH+1];
	DifxMessageMk5Status mk5status;
	char message[DIFX_MESSAGE_LENGTH];
	int v;
	double dRate;
	std::list<Datum> data;
	int64_t done = 0LL;
	int nPeek = 0;
	int bank;
	unsigned int signature;
	int len;
	char *dirData;
	int n=0;
	char outFile[32];
	FILE *out;
	int64_t lastnewpos;
	double r;

	dRate = rate * fraction;	

	memset(&mk5status, 0, sizeof(mk5status));

	WATCHDOGTEST( XLROpen(1, &xlrDevice) );
	WATCHDOGTEST( XLRSetBankMode(xlrDevice, SS_BANKMODE_NORMAL) );
	if(readMode == MARK5_READ_MODE_RT)
	{
		printf("Setting real-time playback mode\n");
		WATCHDOGTEST( XLRSetFillData(xlrDevice, MARK5_FILL_PATTERN) );
		WATCHDOGTEST( XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	}
	else
	{
		WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_REALTIMEPLAYBACK) );
		WATCHDOGTEST( XLRClearOption(xlrDevice, SS_OPT_SKIPCHECKDIR) );
	}

	v = getBankInfo(xlrDevice, &mk5status, ' ');
	if(v < 0)
	{
		WATCHDOG( XLRClose(xlrDevice) );

		return -1;
	}

	if(strcasecmp(vsn, "A") == 0 && mk5status.vsnA[0] != 0)
	{
		mk5status.activeBank = 'A';
		strcpy(vsn, mk5status.vsnA);
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_A) );
	}
	if(strcasecmp(vsn, "B") == 0 && mk5status.vsnB[0] != 0)
	{
		mk5status.activeBank = 'B';
		strcpy(vsn, mk5status.vsnB);
		WATCHDOGTEST( XLRSelectBank(xlrDevice, BANK_B) );
	}

	bank = Mark5BankSetByVSN(xlrDevice, vsn);

	WATCHDOGTEST( XLRGetDirectory(xlrDevice, &dir) );

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );
	label[XLR_LABEL_LENGTH] = 0;

	signature = 1;
	WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );
	printf("User Dir Length = %d\n", len);
	if(len > 128)
	{
		dirData = new char[len];

		WATCHDOGTEST( XLRGetUserDir(xlrDevice, len, 0, dirData) );

		for(int j = 32; j < len/4; ++j)
		{
			unsigned int x = ((unsigned int *)dirData)[j] + 1;
			signature = signature ^ x;
		}

		/* prevent a zero signature */
		if(signature == 0)
		{
			signature = 0x55555555;
		}

		delete[] dirData;
	}

	printf("About to map module %s with %Ld recorded bytes\n\n", label, dir.Length);

	if(end == 0 || end > dir.Length)
	{
		end = dir.Length;
	}

	mk5status.state = MARK5_STATE_GETDIR;
	difxMessageSendMark5Status(&mk5status);

	oldsiginthand = signal(SIGINT, siginthand);

	// The action starts here

	// First sample the recorded length with grid points

	for(int i = 0; i < grid; ++i)
	{
		double complete = double(i+1)*100.0/(double)grid;
		int64_t byte = begin + (end - begin - BufferLength - 128)*i/(grid-1);

		byte -= (byte % 8);

		if(verbose > 0)
		{
			printf("[%6.3f%%] ", complete);
		}
		data.push_back(Datum(&xlrDevice, byte));
		++nPeek;
		mk5status.scanNumber = nPeek;
		mk5status.position = byte;
		snprintf(mk5status.scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "Pass 1: %6.3f%%", complete);
		difxMessageSendMark5Status(&mk5status);

		if(die)
		{
			break;
		}
	}

	// Next, go through the list and densify as needed

	printf("\nAllowed rate range = %10.0f to %10.0f bytes per second\n\n", rate - dRate, rate + dRate);

	lastnewpos = 0LL;
	for(std::list<Datum>::iterator d1 = data.begin(); ; ++d1)
	{
		std::list<Datum>::iterator d2 = d1;
		++d2;	// d2 points one ahead
		if(d2 == data.end())
		{
			break;
		}
		if(bytespersecond(*d1, *d2) < 0)
		{
			printf("Rate is negative: %e\n", bytespersecond(*d1, *d2));
			printf("  "); d1->print();
			printf("  "); d2->print();
		}
		else while( (fabs(bytespersecond(*d1, *d2) - rate) > dRate) && (d2->byte-d1->byte > precision) )
		{
			double complete = (double)done/(double)(end-begin)*100.0;
			int64_t newpos = (d1->byte + d2->byte)/2LL;

			if(die)
			{
				break;
			}

			if(verbose > 0)
			{
				printf("[%6.3f%%] ", complete);
			}
			newpos = newpos - (newpos % 8);
			if(newpos == lastnewpos)
			{
				break;
			}
			lastnewpos = newpos;
			data.insert(d2, Datum(&xlrDevice, newpos));
			++nPeek;
			mk5status.scanNumber = nPeek;
			mk5status.position = newpos;
			snprintf(mk5status.scanName, DIFX_MESSAGE_MAX_SCANNAME_LEN, "Pass 2: %6.3f%%", complete);
			difxMessageSendMark5Status(&mk5status);
			--d2;	// back off to point to the newly added member
		}
		done += d2->byte-d1->byte;
		if(die)
		{
			break;
		}
	}

	// Finally, go through and remove redundant points
	if(verbose > 2)
	{
		printf("---\n");
		for(std::list<Datum>::const_iterator d = data.begin(); d != data.end(); ++d)
		{
			d->print();
		}
		printf("---\n");
	}

	for(std::list<Datum>::iterator d1 = data.begin(); ; ++d1)
	{
		for(;;)
		{
			std::list<Datum>::iterator d2 = d1;
			++d2;
			if(d2 == data.end())
			{
				d1 = data.end();
				break;
			}
			std::list<Datum>::iterator d3 = d2;
			++d3;
			if(d3 == data.end())
			{
				d1 = data.end();
				break;
			}

			double r = bytespersecond(*d1, *d3);
			double r1 = bytespersecond(*d1, *d2);
			double r2 = bytespersecond(*d2, *d3);
			if(fabs(r - rate) <= dRate || (fabs(r1 - rate) > dRate && fabs(r2 - rate) > dRate))
			{
				if(verbose > 2)
				{
					if(fabs(r - rate) <= dRate)
					{
						printf("Removing point at %Ld because it is in the middle of a good period\n", d2->byte);
					}
					else
					{
						printf("Removing point at %Ld because it is in the middle of a bad period\n", d2->byte);
					}
					printf("Rates were %10.0f  (%10.0f %10.0f)  [%10.0f]\n", r, r1, r2, rate);
					printf("Data were (%d %d  %Ld) (%d %d %Ld) (%d %d %Ld)\n\n",
						d1->sec, d1->ns, d1->byte,
						d2->sec, d2->ns, d2->byte,
						d3->sec, d3->ns, d3->byte);
				}
				data.erase(d2);
			}
			else
			{
				if(verbose > 2)
				{
					printf("Keeping point at %Ld\n", d2->byte);
					printf("Rates were %10.0f  (%10.0f %10.0f)  [%10.0f]\n", r, r1, r2, rate);
					printf("Data were (%d %d  %Ld) (%d %d %Ld) (%d %d %Ld)\n\n",
						d1->sec, d1->ns, d1->byte,
						d2->sec, d2->ns, d2->byte,
						d3->sec, d3->ns, d3->byte);
				}
				break;
			}
		}

		if(d1 == data.end())
		{
			break;
		}
	}

	// Make sure ends are OK 
	{
		std::list<Datum>::iterator d1 = data.begin();
		std::list<Datum>::iterator d2 = d1;
		++d2;
		r = bytespersecond(*d1, *d2);
		if(fabs(r - rate) > dRate)
		{
			data.erase(d1);
			if(verbose > 1)
			{
				printf("Removing first point because rate = %10.0f\n", r);
			}
		}

		d2 = data.end();
		--d2;
		d1 = d2;
		--d1;
		r = bytespersecond(*d1, *d2);
		if(fabs(r - rate) > dRate)
		{
			data.erase(d2);
			if(verbose > 1)
			{
				printf("Removing last point because rate = %10.0f\n", r);
			}
		}
	}

	if(verbose > 1)
	{
		printf("---\n");
		for(std::list<Datum>::const_iterator d = data.begin(); d != data.end(); ++d)
		{
			d->print();
		}
		printf("---\n");
	}
	
	printf("\n");

	snprintf(outFile, 31, "%s.mk5map.dir", vsn);
	out = fopen(outFile, "w");
	
	if(out)
	{
		fprintf(out, "%s %d %c %u 1 NORMAL Synth\n", vsn, data.size()/2, 'A' + bank, signature);
	}
	if(verbose > -1 || !out)
	{
		printf("%s %d %c %u 1 NORMAL Synth\n", vsn, data.size()/2, 'A' + bank, signature);
	}
	for(std::list<Datum>::const_iterator d1 = data.begin(); d1 != data.end(); ++d1)
	{
		++n;
		std::list<Datum>::const_iterator d2 = d1;
		++d2;
		if(d2 == data.end())
		{
			printf("*** Developer error: fractional number of scans not expected!\n");
			printf("*** This probably means the recovered directory is not right!\n");

			break;
		}
		double dur = duration(*d1, *d2);
		if(out)
		{
			fprintf(out, "%14Ld %14Ld %5d %5d %5d %5d %f %d  %d %d %d No%04d\n",
				d1->byte, d2->byte-d1->byte, d1->mjd, d1->sec, d1->frame, d1->framespersecond, dur, d1->framebytes, 0, d1->tracks, d1->format, n);
		}
		if(verbose > -1 || !out)
		{
			printf("%14Ld %14Ld %5d %5d %5d %5d %f %d  %d %d %d No%04d\n",
				d1->byte, d2->byte-d1->byte, d1->mjd, d1->sec, d1->frame, d1->framespersecond, dur, d1->framebytes, 0, d1->tracks, d1->format, n);
		}

		d1 = d2;
	}
	

	if(verbose > 0)
	{
		printf("\nDirectory solved with %d peeks\n", nPeek);
	}

	printf("\n");

	if(out)
	{
		printf("--> Directory saved as %s\n\n", outFile);
		fclose(out);
	}
	else
	{
		printf("*** Warning: could not open output file %s.  You can cut and paste the above! ***\n\n", outFile);
	}

	if(die)
	{
		difxMessageSendDifxAlert("Module mapping terminated by signal.", DIFX_ALERT_LEVEL_WARNING);
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Successful module mapping read for %s", label);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
	}

	WATCHDOG( XLRClose(xlrDevice) );

	mk5status.state = MARK5_STATE_IDLE;
	mk5status.scanNumber = 0;
	mk5status.scanName[0] = 0;
	mk5status.position = 0;
	mk5status.activeBank = ' ';
	difxMessageSendMark5Status(&mk5status);

	return 0;
}

int main(int argc, char **argv)
{
	enum Mark5ReadMode readMode = MARK5_READ_MODE_NORMAL;
	char vsn[16] = "";
	enum DMS_Mode dmsMode = DMS_MODE_FAIL_UNLESS_SAFE;
	int v;
	int grid = defaultGrid;
	const char *dmsMaskStr;
	int dmsMask = 7;
	double rate = 25600*10016;	// now hardcoded for Mark5B
	int64_t precision = defaultPrecision;
	uint64_t begin = 0LL;
	uint64_t end = 0LL;
	int retval = EXIT_SUCCESS;
	double fraction = defaultFraction;

	dmsMaskStr = getenv("DEFAULT_DMS_MASK");
	if(dmsMaskStr)
	{
		dmsMask = atoi(dmsMaskStr);
		if((dmsMask & 2) == 0)
		{
			dmsMode = DMS_MODE_NO_UPDATE;
		}
	}

	difxMessageInit(-1, program);

	if(argc < 2)
	{
		usage(argv[0]);

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
/* the three options below are not yet implemented */
#if 0
		else if(strcmp(argv[a], "-n") == 0 ||
			strcmp(argv[a], "--nodms") == 0)
		{
			dmsMode = DMS_MODE_NO_UPDATE;
		}
		else if(strcmp(argv[a], "-s") == 0 ||
			strcmp(argv[a], "--safedms") == 0)
		{
			dmsMode = DMS_MODE_UPDATE_IF_SAFE;
		}
		else if(strcmp(argv[a], "-d") == 0 ||
			strcmp(argv[a], "--dmsforce") == 0)
		{
			dmsMode = DMS_MODE_UPDATE;
		}
#endif
		else if(strcmp(argv[a], "--realtime") == 0)
		{
			readMode = MARK5_READ_MODE_RT;
		}
		else if(argv[a][0] == '-' && a+1 < argc)
		{
			if(strcmp(argv[a], "-r") == 0 ||
			   strcmp(argv[a], "--rate") == 0)
			{
				++a;
				rate = atof(argv[a]);
			}
			else if(strcmp(argv[a], "-f") == 0 ||
				strcmp(argv[a], "--fraction") == 0)
			{
				++a;
				fraction = atof(argv[a]);
			}
			else if(strcmp(argv[a], "-p") == 0 ||
			        strcmp(argv[a], "--precision") == 0)
			{
				++a;
				precision = atoi(argv[a]);
			}
			else if(strcmp(argv[a], "-g") == 0 ||
			        strcmp(argv[a], "--grid") == 0)
			{
				++a;
				grid = atoi(argv[a]);
			}
			else if(strcmp(argv[a], "-b") == 0 ||
			        strcmp(argv[a], "--begin") == 0)
			{
				++a;
				begin = atoll(argv[a]);
			}
			else if(strcmp(argv[a], "-e") == 0 ||
			        strcmp(argv[a], "--end") == 0)
			{
				++a;
				end = atoll(argv[a]);
			}
			else
			{
				usage(argv[0]);

				return EXIT_FAILURE;
			}
		}
		else if(vsn[0] == 0)
		{
			strncpy(vsn, argv[a], 8);
			vsn[8] = 0;
		}
		else
		{
			usage(argv[0]);

			return EXIT_FAILURE;
		}
	}

	if(vsn[0] == 0)
	{
		printf("Error: no module or bank specified\n");

		return EXIT_FAILURE;
	}

	v = initWatchdog();
	if(v < 0)
	{
		return EXIT_FAILURE;
	}

	/* 60 seconds should be enough to complete any XLR command */
	setWatchdogTimeout(60);

	setWatchdogVerbosity(verbose);

	/* *********** */

	v = lockMark5(MARK5_LOCK_DONT_WAIT);

	if(v < 0)
	{
		fprintf(stderr, "Another process (pid=%d) has a lock on this Mark5 unit\n", getMark5LockPID());
	}
	else
	{
		v = mk5map(vsn, rate, fraction, precision, grid, begin, end, readMode);
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
