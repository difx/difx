/***************************************************************************
 *   Copyright (C) 2008-2014 by Walter Brisken                             *
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
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <iostream>
#include <vector>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <algorithm>
#include <unistd.h>
#include <difxmessage.h>
#include <mark5access.h>
#include <mark5directorystructs.h>
#include "mark5dir.h"
#include "watchdog.h"


const char *moduleStatusName(int status)
{
	if(status & MODULE_STATUS_RECORDED)
	{
		return "Recorded";
	}
	else if(status & MODULE_STATUS_PLAYED)
	{
		return "Played";
	}
	else if(status & MODULE_STATUS_ERASED)
	{
		return "Erased";
	}

	return "Unknown";
}


/* Warning!  Endianness not corrected for in decoding of
 * module directories!
 */

using namespace std;

char Mark5ReadModeName[][10] =
{
	"NORMAL",
	"RT"
};

char Mark5DirDescription[][20] =
{
	"Short scan",
	"XLR Read error",
	"Decode error",
	"Decoded",
	"Decoded WR",
	"Copy error",
	"Copied",
	"Data is zeros"
};

char ScanFormatErrorName[][40] =
{
	"not an error",
	"cannot decode scan",
	"StreamStor read failure",
	"data is mostly zero",
	"scan is too short",
	"unsupported format",
	"?",	// placeholder
	"??",	// placeholder
	"scan number out of range"
};

//--------------------- Random helper functions ------------------

int countZeros(const streamstordatatype *data, int len)
{
	int i;
	int nZero = 0;

	for(i = 0; i < len; ++i)
	{
		if(data[i] == 0)
		{
			++nZero;
		}
	}

	return nZero;
}

void countReplaced(const streamstordatatype *data, int len, 
	long long *wGood, long long *wBad)
{
	int i;
	int nBad=0;

	for(i = 0; i < len; ++i)
	{
		if(data[i] == MARK5_FILL_PATTERN)
		{
			++nBad;
		}
	}

	*wGood += (len-nBad);
	*wBad += nBad;
}


static int mjd2ymd(long mjd, int *pYear, int *pMonth, int *pDay)
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * This function converts the given date to a year, month, and day.  If the 
 * given date does not fall between 0001JAN01 AD (MJD = -678,575) and 
 * 10000JAN00 AD (MJD = 2,973,483) ERROR is returned.
 */
{
/* 2,400,000 (difference between Julian Date and Modified Julian Date) 
   minus # days from jan 1, 4713 BC (beginning of Julian calendar) */
#define AD 678576

	static int monlen[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

	int icen4, icen, iyr4, iyr, imon, iday;

	/* check input range and calc days since jan 1 1 AD (Gregorian Calendar) */
	if (mjd > 2973483)
	{
		return -1;
	}
	if ((mjd += AD - 1) < 0)
	{
		return -1;
	}
	/* calc number of fours of Gregorian centuries */
	icen4 = mjd / 146097;

	/* calc number of centuries since last 
	fours of Gregorian centuries (e.g. since 1600 or 2000) */
	mjd -= (icen4 * 146097);
	if ((icen = mjd / 36524) == 4)
	{
		icen = 3; 
	}

	/* calc number of quadrenia(four years) since jan 1, 1901 */
	mjd -= (icen * 36524);
	iyr4 = mjd / 1461;

	/* calc number of years since last quadrenia */
	mjd -= (iyr4 * 1461);
	if ((iyr = mjd / 365) == 4)
	{
		iyr = 3;
	}

	/* calc number of months, days since jan 1 of current year */
	iday = mjd - iyr * 365;
	for(imon = 0; iday >= 0; ++imon)
	{
		iday = iday - monlen[imon] - ((iyr == 3 && imon == 1) ? 1 : 0);
	}
	imon--;		/* restore imon, iday to last loop value */
	iday = iday + monlen[imon] + ((iyr == 3 && imon == 1) ? 1 : 0);

	/* calc return values */
	*pYear = icen4 * 400 + icen * 100 + iyr4 * 4 + iyr + 1;
	*pMonth = imon + 1;
	*pDay = iday + 1;

	return 0;
}

/* return day of year given year, month, day */
int ymd2doy(int yr, int mo, int day)
{
	int monstart1[] = {0,31,59,90,120,151,181,212,243,273,304,334};
	int monstart2[] = {0,31,60,91,121,152,182,213,244,274,305,335};
	int L2;
	
	L2 = yr/4-(yr+7)/4-yr/100+(yr+99)/100+yr/400-(yr+399)/400;
	if(L2 == -1)
	{
		return day + monstart2[mo-1];
	}
	else
	{
		return day + monstart1[mo-1];
	}
}

int ymd2mjd(int yr, int mo, int day)
{
	int doy;
	int yr1 = yr - 1;

	doy = ymd2doy(yr, mo, day);

	return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
}

int doy2mjd(int yr, int doy)
{
	int yr1 = yr - 1;

	return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
}

int addDecades(int mjd, int nDecade)
{
	int y=0, m=0, d=0;
	int doy;

	mjd2ymd(mjd, &y, &m, &d);
	doy = ymd2doy(y, m, d);

	y += 10*nDecade;

	return doy2mjd(y, doy);
}

//------------------------- Mark5Scan ------------------------------

Mark5Scan::Mark5Scan()
{
	name[0] = 0;
	start = 0;
	length = 0;
	duration = 0;
	mjd = 0;
	sec = 0;
	framenuminsecond = 0;
	framespersecond = 0;
	framebytes = 0;
	frameoffset = 0;
	tracks = 0;
	format = 0;
}

Mark5Scan::~Mark5Scan()
{
}

void Mark5Scan::print() const
{
	printf("%1d %-32s %13Ld %13Ld %5d %2d %5d %5d+%d/%d %6.4f\n",
		format, name.c_str(), start, start+length, frameoffset, tracks,
		mjd, sec, framenuminsecond, framespersecond, duration);
}

// Parse a line from a .dir file into an existing Mark5Scan class
void Mark5Scan::parseDirEntry(const char *line)
{
	char scanName[MODULE_LEGACY_SCAN_LENGTH];

	sscanf(line, "%Ld%Ld%d%d%d%d%lf%d%d%d%d%63s",
		&start, &length, &mjd, &sec, &framenuminsecond, &framespersecond,
		&duration, &framebytes, &frameoffset, &tracks, &format, scanName);

	name = scanName;
}

int Mark5Scan::writeDirEntry(FILE *out) const
{
	const int ErrorStrLen = 60;
	char errorStr[ErrorStrLen];
	int v;

	if(format < 0 && format >= -NUM_SCAN_FORMAT_ERRORS)
	{
		v = snprintf(errorStr, ErrorStrLen, " Error='%s'", ScanFormatErrorName[-format]);
		if(v >= ErrorStrLen)
		{
			fprintf(stderr, "Developer error: ErrorStrLen too small in function Mark5Scan::writeDirEntry");
		}
	}
	else
	{
		strcpy(errorStr, "");
	}

	v = fprintf(out, "%14Ld %14Ld %5d %d %d %d %12.6f %6d %6d %2d %1d %s%s\n",
		start, length, mjd, sec, framenuminsecond, framespersecond, duration,
		framebytes, frameoffset, tracks, format, name.c_str(), errorStr);

	return v;
}

int Mark5Scan::sanityCheck() const
{
	int nError = 0;

	if(format < 0)
	{
		++nError;
	}

	return nError;
}

int Mark5Scan::nsStart() const
{
	return static_cast<int>(1000000000.0*framenuminsecond/framespersecond + 0.1);
}

double Mark5Scan::secStart() const
{
	int scanns = static_cast<int>(1000000000.0*framenuminsecond/framespersecond + 0.1);

	return sec + scanns*1.e-9;
}

double Mark5Scan::mjdStart() const
{
	int scanns = static_cast<int>(1000000000.0*framenuminsecond/framespersecond + 0.1);

	return mjd + (sec + scanns*1.e-9)/86400.0;
}

double Mark5Scan::mjdEnd() const
{
	int scanns = static_cast<int>(1000000000.0*framenuminsecond/framespersecond + 0.1);

	return mjd + (sec + scanns*1.e-9 + duration)/86400.0;
}


bool operator<(const Mark5Scan &a, const Mark5Scan &b)
{
	if(a.mjd < b.mjd)
	{
		return true;
	}
	else if(a.mjd > b.mjd)
	{
		return false;
	}
	else if(a.sec < b.sec)
	{
		return true;
	}
	else if(a.sec > b.sec)
	{
		return false;
	}
	else if(a.framenuminsecond < b.framenuminsecond)
	{
		return true;
	}
	else if(a.framenuminsecond > b.framenuminsecond)
	{
		return false;
	}
	else
	{
		return (a.name < b.name);
	}
}

//------------------------- Mark5Module ----------------------------

Mark5Module::Mark5Module()
{
	clear();
}

Mark5Module::~Mark5Module()
{
}

void Mark5Module::clear()
{
	label.clear();
	error.str("");
	bank = -1;
	scans.clear();
	signature = 0;
	mode = MARK5_READ_MODE_NORMAL;
	dirVersion = 0;
	dirSubversion = -1;
	fast = 0;
	synthetic = 0;
}

void Mark5Module::print() const
{
	int i = 0;

	if(bank < 0)
	{
		return;
	}
	
	if(dirSubversion >= 0)
	{
		printf("VSN=%s  nScan=%d  bank=%c  sig=%u  dirVer=%d  dirSubver=%d  mode=%s\n", 
			label.c_str(), nScans(), bank+'A', signature, dirVersion, dirSubversion, Mark5ReadModeName[mode]);
	}
	else
	{
		printf("VSN=%s  nScan=%d  bank=%c  sig=%u  dirVer=%d  mode=%s\n", 
			label.c_str(), nScans(), bank+'A', signature, dirVersion, Mark5ReadModeName[mode]);
	}

	if(error.str().size() > 0)
	{
		printf("Error condition: %s\n", error.str().c_str());
	}

	for(vector<Mark5Scan>::const_iterator s = scans.begin(); s != scans.end(); ++s)
	{
		printf("%3d ", ++i);
		s->print();
	}
}

int Mark5Module::load(const char *filename)
{
	const int MaxLineLength = 255;
	FILE *in;
	char line[MaxLineLength+1];
	int i, j, nscans, n;
	char *v;
	char bankName;
	char dirLabel[XLR_LABEL_LENGTH];
	char extra[5][20];
	int nNumber = 0;

	clear();

	in = fopen(filename, "r");
	if(!in)
	{
		error << "Cannot load file " << filename << "\n";

		return -1;
	}

	v = fgets(line, MaxLineLength, in);
	if(!v)
	{
		error << "Directory file " << filename << " is corrupt.\n";
		fclose(in);

		return -1;
	}

	n = sscanf(line, "%8s %d %c %u %19s %19s %19s %19s %19s",
		dirLabel, &nscans, &bankName, &signature, extra[0], extra[1], extra[2], extra[3], extra[4]);
	if(n < 3)
	{
		error << "Directory file " << filename << " is corrupt.\n";
		fclose(in);

		return -1;
	}
	if(n == 3)
	{
		signature = ~0;
	}

	for(j = 4; j < n; ++j)
	{
		if(strcmp(extra[j-4], "RT") == 0)
		{
			mode = MARK5_READ_MODE_RT;
		}
		else if(strcmp(extra[j-4], "Fast") == 0)
		{
			fast = 1;
		}
		else if(strcmp(extra[j-4], "Synth") == 0)
		{
			synthetic = 1;
		}
		else if(sscanf(extra[j-4], "%d", &i) == 1)
		{
			switch(nNumber)
			{
			case 0:
				dirVersion = i;
				break;
			case 1:
				dirSubversion = i;
				break;
			}
			++nNumber;
		}
	}

	if(nscans < 0)
	{
		error << "Directory file " << filename << " is corrupt (nscans < 0).\n";
		fclose(in);

		return -1;
	}

	label = dirLabel;
	bank = bankName-'A';
	scans.resize(nscans);

	for(vector<Mark5Scan>::iterator s = scans.begin(); s != scans.end(); ++s)
	{
		line[0] = 0;
		v = fgets(line, MaxLineLength, in);
		if(!v)
		{
			error << "Directory file " << filename << " is corrupt (file too short).\n";
			fclose(in);

			return -1;
		}
		
		s->parseDirEntry(line);

		if(s->framespersecond <= 0 || s->length <= 0)
		{
			error << "Directory file " << filename << " has a corrupt scan (posibly at the end?).\n";
			fclose(in);

			return -1;
		}
	}

	fclose(in);
	
	return 0;
}

void Mark5Module::sort()
{
	std::sort(scans.begin(), scans.end());
}

int Mark5Module::sanityCheck()
{
	int nError = 0;

	for(vector<Mark5Scan>::const_iterator s = scans.begin(); s != scans.end(); ++s)
	{
		nError += s->sanityCheck();
	}

	return nError;
}

int Mark5Module::uniquifyScanNames()
{
	vector<string> scanNames;
	vector<int> nameCount;
	vector<int> origIndex;
	char extension[8];
	int i, j, n=0;

	if(nScans() < 2)
	{
		return 0;
	}

	scanNames.resize(nScans());
	nameCount.resize(nScans());
	origIndex.resize(nScans());

	scanNames[0] = scans[0].name;
	nameCount[0] = 1;
	origIndex[0] = 0;
	n = 1;

	for(i = 1; i < nScans(); ++i)
	{
		for(j = 0; j < n; ++j)
		{
			if(scanNames[j] == scans[i].name)
			{
				++nameCount[j];
				snprintf(extension, 8, "_%04d", nameCount[j]);
				scans[i].name += extension;
				break;
			}
		}
		if(j == n)
		{
			scanNames[n] = scans[i].name;
			nameCount[n] = 1;
			origIndex[n] = i;
			++n;
		}
	}

	/* rename those that would have had name extension _0001 */
	for(j = 0; j < n; ++j)
	{
		if(nameCount[j] > 1)
		{
			i = origIndex[j];
			scans[i].name = scanNames[j] + "_0001";
		}
	}

	return 0;
}

//------------------------------------------------------------------

// returns active bank, or -1 if none, or -2 or -3 if error retrieving status
int Mark5BankGet(SSHANDLE xlrDevice)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	int b = -1;

	WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat) );
	if(xlrRC == XLR_SUCCESS)
	{
		if(bank_stat.Selected)
		{
			b = 0;
		}
	}
	else
	{
		b = -2;
	}
	if(b == -1)
	{
		WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat) );
		if(xlrRC == XLR_SUCCESS)
		{
			if(bank_stat.Selected)
			{
				b = 1;
			}
		}
		else
		{
			b = -3;
		}
	}

	return b;
}

int Mark5GetActiveBankWriteProtect(SSHANDLE xlrDevice)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	int b = -1;

	WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat) );
	if(xlrRC == XLR_SUCCESS)
	{
		if(bank_stat.Selected)
		{
			b = bank_stat.WriteProtected;
		}
	}
	else
	{
		b = -2;
	}
	if(b == -1)
	{
		WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat) );
		if(xlrRC == XLR_SUCCESS)
		{
			if(bank_stat.Selected)
			{
				b = bank_stat.WriteProtected;
			}
		}
		else
		{
			b = -3;
		}
	}

	return b;
}

/* returns 0 or 1 for bank A or B, or < 0 if module not found or on error */
int Mark5BankSetByVSN(SSHANDLE xlrDevice, const char *vsn)
{
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	S_DIR dir;
	int b = -1;
	int bank=-1;

	WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat) );
	if(xlrRC == XLR_SUCCESS)
	{
		if(strncasecmp(bank_stat.Label, vsn, 8) == 0)
		{
			b = 0;
			bank = BANK_A;
		}
	}
	else
	{
		return -2;
	}

	if(b == -1)
	{
		WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat) );
		if(xlrRC == XLR_SUCCESS)
		{
			if(strncasecmp(bank_stat.Label, vsn, 8) == 0)
			{
				b = 1;
				bank = BANK_B;
			}
		}
		else
		{
			return -3;
		}
	}

	if(bank < 0)
	{
		return -1;
	}

	WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
	if(xlrRC != XLR_SUCCESS)
	{
		return -4;
	}

	WATCHDOG( xlrRC = XLRSelectBank(xlrDevice, bank) );
	if(xlrRC != XLR_SUCCESS)
	{
		b = -5 - b;
	}
	else
	{
		for(int i = 0; i < 100; ++i)
		{
			WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
			if(xlrRC != XLR_SUCCESS)
			{
				return -7;
			}
			if(bank_stat.State == STATE_READY && bank_stat.Selected)
			{
				break;
			}
			usleep(100000);
		}

		if(bank_stat.State != STATE_READY || !bank_stat.Selected)
		{
			b = -8;
		}
	}

	/* the following line is essential to work around an apparent streamstor bug */
	WATCHDOG( xlrRC = XLRGetDirectory(xlrDevice, &dir) );
	if(xlrRC != XLR_SUCCESS)
	{
		return -9;
	}

	return b;
}

int Mark5Module::readDirectory(SSHANDLE xlrDevice, int mjdref, int (*callback)(int, int, int, void *), void *data, float *replacedFrac, int cacheOnly, int startScan, int stopScan, const char *binFilename)
{
	XLR_RETURN_CODE xlrRC;
	struct Mark5LegacyDirectory *legacyDir;
	struct Mark5NeoLegacyDirectory *neolegacyDir;
	unsigned char *dirData;
	int dirLength;
	char newLabel[XLR_LABEL_LENGTH];
	int newBank;
	int bufferLength;
	struct Mark5DirectoryInfo dirInfo;
	int v;

	if(replacedFrac)
	{
		*replacedFrac = 0.0;
	}

	/* allocate a bit more than the minimum needed */
	bufferLength = 20160*8*4;

	newBank = Mark5BankGet(xlrDevice);
	if(newBank < 0)
	{
		return -1;
	}

	WATCHDOG( xlrRC = XLRGetLabel(xlrDevice, newLabel) );
	if(xlrRC != XLR_SUCCESS)
	{
		return -2;
	}
	newLabel[8] = 0;

	WATCHDOG( dirLength = XLRGetUserDirLength(xlrDevice) );
	dirData = (unsigned char *)calloc(dirLength, 1);
	if(dirData == 0)
	{
		return -4;
	}
	WATCHDOG( xlrRC = XLRGetUserDir(xlrDevice, dirLength, 0, dirData) );
	v = getMark5DirectoryInfo(&dirInfo, dirData, dirLength);
	if(v != Mark5DirectoryInfoSuccess)
	{
		FILE *out;
		const char dumpFile[] = "/tmp/dir.dump";

		fprintf(stderr, "\nModule directory parse error %d encountered: %s\n", v, Mark5DirectoryInfoStatusStrings[v]);
		fprintf(stderr, "The directory size was %d\n", dirLength);
		fprintf(stderr, "The best guess about the directory content is printed below:\n");
		fprintMark5DirectoryInfo(stderr, &dirInfo);
		fprintf(stderr, "The binary directory was dumped to %s\n", dumpFile);

		out = fopen(dumpFile, "w");
		fwrite(dirData, 1, dirLength, out);
		fclose(out);

		free(dirData);

		return -3;
	}

	fprintMark5DirectoryInfo(stdout, &dirInfo);

	legacyDir = (struct Mark5LegacyDirectory *)dirData;
	neolegacyDir = (struct Mark5NeoLegacyDirectory *)dirData;

	if(xlrRC != XLR_SUCCESS)
	{
		free(dirData);

		return -5;
	}

	if(signature == dirInfo.signature && nScans() > 0)
	{
		/* Cached version seems up to date */

		bank = newBank;
		if(dirVersion != dirInfo.dirClass)
		{
			fprintf(stderr, "Warning: disagreement in directory version! %d != %d\n", dirVersion, dirInfo.dirClass);
			dirVersion = dirInfo.dirClass;
			dirSubversion = dirInfo.dirVersion;
		}
		free(dirData);

		return 0;
	}
	else if(cacheOnly)
	{
		return DIRECTORY_NOT_CACHED;
	}

	return DIRECTORY_NOT_CACHED;

#if 0
	
	// This is commented out because we don't ever read directories from within mpifxcorr.


	/* If we got this far, we're going to attempt to do the directory read */

	bufferStart = (streamstordatatype *)malloc(bufferLength);
	bufferStop  = (streamstordatatype *)malloc(bufferLength*2);
	bufferPrefil = (streamstordatatype *)((char *)bufferStop + bufferLength);

	oldFast = fast;		/* don't forget if we want to do this in fast mode or not */
	clear();
	fast = oldFast;
	
	bank = newBank;
	label = newLabel;
	dirVersion = dirInfo.dirClass;
	dirSubversion = dirInfo.dirVersion;
	signature = dirInfo.signature;
	scans.resize(dirInfo.nScan);
	mode = MARK5_READ_MODE_NORMAL;

	/* perhaps limit the range of scans to interrogate */
	if(startScan < 0)
	{
		startScan = 0;
	}
	if(stopScan < 0 || stopScan > nScans())
	{
		stopScan = nScans();
	}

	if(fast && dirInfo.dirClass == Mark5DirClassMark5C)
	{
		newdir2scans(scans, dirData, dirLength, startScan, stopScan);
	}
	else
	{
		long long wGoodSum=0, wBadSum=0;
		int lastread = -10;
		
		/* Work around possible streamstor bug */
		WATCHDOG( xlrRC = XLRReadData(xlrDevice, bufferStart, 0, 0, bufferLength) );

		for(int i = 0; i < nScans(); ++i)
		{
			unsigned long a, b;
			long long wGood=0, wBad=0;
			int nZero;
			
			Mark5Scan &scan = scans[i];

			if(fast)
			{
				printf("Fast directory read requested but not supported for this module.\n");
				printf("Performing normal directory read\n");
				fast = 0;
			}

			if(dirInfo.dirClass == Mark5DirClassLegacy)
			{
				scan.name   = neolegacyDir->scanName[i];
				scan.start  = neolegacyDir->start[i];
				scan.length = neolegacyDir->length[i];
			}
			if(dirInfo.dirClass == Mark5DirClassNeoLegacy)
			{
				scan.name   = neolegacyDir->scanName[i];
				scan.start  = neolegacyDir->start[i];
				scan.length = neolegacyDir->length[i];
			}
			else if(dirInfo.dirClass == Mark5DirClassMark5C)
			{
				const struct Mark5DirectoryScanHeaderVer1 *scanHeader;
				scanHeader = (struct Mark5DirectoryScanHeaderVer1 *)(dirData + 128*i + 128);
				expandScanName1(scan.name, scanHeader);
				scan.start  = scanHeader->startByte;
				scan.length = scanHeader->stopByte - scanHeader->startByte;
			}
			if(scan.length < bufferLength)
			{
				if(callback)
				{
					die = callback(i, nScans(), MARK5_DIR_SHORT_SCAN, data);
				}

				scan.format = -SCAN_FORMAT_ERROR_TOOSHORT;

				continue;
			}

			if(die)
			{
				break;
			}

			if(scan.start & 4)
			{
				scan.start -= 4;
				scan.length -= 4;
			}

			if(i < startScan || i >= stopScan)
			{
				scan.format = -SCAN_FORMAT_ERROR_SCANNUMRANGE;

				continue;
			}

			if(lastread == i-1)
			{
				memcpy(bufferStart, bufferPrefil, bufferLength);
			}
			else
			{
				a = scan.start>>32;
				b = scan.start % (1LL<<32);
				WATCHDOG( xlrRC = XLRReadData(xlrDevice, bufferStart, a, b, bufferLength) );
			}
			a = (scan.start+scan.length-bufferLength)>>32;
			b = (scan.start+scan.length-bufferLength) % (1LL<<32);
			if(i < nScans() - 1)
			{
				WATCHDOG( xlrRC = XLRReadData(xlrDevice, bufferStop, a, b, 2*bufferLength) );
				lastread = i;
			}
			else
			{
				WATCHDOG( xlrRC = XLRReadData(xlrDevice, bufferStop, a, b, bufferLength) );
			}

			if(xlrRC == XLR_FAIL)
			{
				if(callback)
				{
					die = callback(i, nScans(), MARK5_DIR_READ_ERROR, data);
				}
				scan.format = -SCAN_FORMAT_ERROR_READERROR;

				continue;
			}

			nZero = countZeros(bufferStart, bufferLength/4);
			if(nZero > bufferLength/8)	/* more than half 32-bit words are pure zero */
			{
				if(callback)
				{
					die = callback(i, nScans(), MARK5_DIR_ZEROS, data);
				}
				scan.format = -SCAN_FORMAT_ERROR_ZEROS;
			}

			countReplaced(bufferStart, bufferLength/4, &wGood, &wBad);
			countReplaced(bufferStop, bufferLength/4, &wGood, &wBad);

			if(die)
			{
				break;
			}

			if( (mf = new_mark5_format_from_stream(new_mark5_stream_memory(bufferStart, bufferLength))) != 0)
			{
				/* Fix mjd. */
#warning FIXME: this should be in mark5access
				if(mf->format == MK5_FORMAT_VLBA || mf->format == MK5_FORMAT_MARK5B)
				{
					int n = (mjdref - mf->mjd + 500) / 1000;
					mf->mjd += n*1000;
				}
				else if(mf->format == MK5_FORMAT_MARK4)
				{
					int n = static_cast<int>((mjdref - mf->mjd + 1826)/3652.4);
					mf->mjd = addDecades(mf->mjd, n);
				}
				
				scan.mjd = mf->mjd;
				scan.sec = mf->sec;
				scan.format      = mf->format;
				scan.frameoffset = mf->frameoffset;
				scan.tracks      = mf->ntrack;
				scan.framespersecond = int(1000000000.0/mf->framens + 0.5);
				scan.framenuminsecond = int(mf->ns/mf->framens + 0.5);
				scan.framebytes  = mf->framebytes;
				scan.duration    = (int)((scan.length - scan.frameoffset) / scan.framebytes)/(double)(scan.framespersecond);

				delete_mark5_format(mf);

				// Look at end of scan to verify things look OK
				mf = new_mark5_format_from_stream(new_mark5_stream_memory(bufferStop, bufferLength));

				if(mf)
				{
					const int maxIter = 5;	// max number of times to loop in calculating frame rate
					long long deltaBytes;
					int iter;

					/* Fix mjd. */
#warning FIXME: this should be in mark5access
					if(mf->format == MK5_FORMAT_VLBA || mf->format == MK5_FORMAT_MARK5B)
					{
						int n = (mjdref - mf->mjd + 500) / 1000;
						mf->mjd += n*1000;
					}
					else if(mf->format == MK5_FORMAT_MARK4)
					{
						int n = static_cast<int>((mjdref - mf->mjd + 1826)/3652.4);
						mf->mjd = addDecades(mf->mjd, n);
					}

					deltaBytes = (scan.start+scan.length-bufferLength+mf->frameoffset) - (scan.start+scan.frameoffset);
					int deltaFrames = int(mf->ns/mf->framens + 0.5) - scan.framenuminsecond;

					scan.duration = mf->sec - scan.sec;
					if(scan.duration < 0)
					{
						scan.duration += 86400.0;
					}

					if(repeatingData((const char *)bufferStart, mf->framebytes) && repeatingData((const char *)bufferStop, mf->framebytes))
					{
						/* look for unusual repetition of payload bytes */

						static bool first = true;

						if(first)
						{
							FILE *out;
							const char sampleFile[] = "/tmp/repeat.sample";

							out = fopen(sampleFile, "w");
							if(out)
							{
								fwrite(bufferStop, 1, bufferLength, out);
								fclose(out);

								fprintf(stderr, "Wrote %d bytes of repeating data to %s for inspection\n", bufferLength, sampleFile);
							}

							first = false;
						}
						scan.format = -SCAN_FORMAT_ERROR_REPEAT;

						continue;
					}

					if(scan.duration < 1)
					{
						scan.format = -SCAN_FORMAT_ERROR_TOOSHORT;
						scan.framespersecond = 0;
						scan.duration = 0.0;
						scan.framebytes = 0;
						scan.mjd = 0;
						scan.sec = 0;
						scan.frameoffset = 0;
						scan.framenuminsecond = 0;

						continue;
					}

					for(iter = 0; iter < maxIter; ++iter)
					{
						int frameRate;		// [frames per second]
						int dataRate;		// [Mbps]

						frameRate = scan.framespersecond;
						dataRate = static_cast<int>((deltaBytes*static_cast<double>(mf->databytes)/static_cast<double>(mf->framebytes))/((scan.duration + deltaFrames/static_cast<double>(scan.framespersecond))*125000.0 + 0.9));

						if(mf->format == MK5_FORMAT_VDIF)
						{
							dataRate /= mf->ntrack;		// div by nThread
						}
						if(dataRate <= 1)
						{
							dataRate = 1;
						}
						else
						{
							// Change to nearest power of two
							int n = static_cast<int>(log(static_cast<double>(dataRate))/log(2.0) + 0.5);
							dataRate = 1 << n;
						}
						scan.framespersecond = dataRate*125000/mf->databytes;
				
						if(mf->format == MK5_FORMAT_VDIF)
						{
							dataRate *= mf->ntrack;
						}
						if(frameRate == scan.framespersecond)
						{
							break;
						}
					}
					if(iter >= maxIter)
					{
						fprintf(stderr, "Developer error: Cannot converge on frame rate.  Last value was %d\n", scan.framespersecond);
					}

					scan.duration += static_cast<double>(deltaFrames)/scan.framespersecond;

					if(mf->format == MK5_FORMAT_VDIF)
					{
						getVdifThreads(scan.startThreads, reinterpret_cast<char *>(bufferStart) + scan.frameoffset, bufferLength - scan.frameoffset, scan.framebytes);
						getVdifThreads(scan.endThreads, reinterpret_cast<char *>(bufferStop) + mf->frameoffset, bufferLength - mf->frameoffset, scan.framebytes);
					}
					
					delete_mark5_format(mf);
				}
			}
			else 
			{
				if(callback)
				{
					die = callback(i, nScans(), MARK5_DIR_DECODE_ERROR, data);
				}
				scan.format = -SCAN_FORMAT_ERROR_DECODE;

				continue;
			}

			if(die)
			{
				break;
			}

			if(callback)
			{
				enum Mark5DirStatus s;

				if(wBad > 8)
				{
					s = MARK5_DIR_DECODE_WITH_REPLACEMENTS;
				}
				else
				{
					s = MARK5_DIR_DECODE_SUCCESS;
				}
				die = callback(i, nScans(), s, data);
			}

			wGoodSum += wGood;
			wBadSum += wBad;

			if(die)
			{
				break;
			}
		}

		if(replacedFrac)
		{
			*replacedFrac = (double)wBadSum/(double)wGoodSum;
		}
	}

	free(bufferStart);
	free(bufferStop);

	if(binFilename)
	{
		FILE *out;

		out = fopen(binFilename, "w");
		fwrite(dirData, dirLength, 1, out);
		fclose(out);
	}

	free(dirData);

	uniquifyScanNames();

	sort();

	return -die;

#endif
}

/* retrieves directory (either from cache or module) and makes sure
 * desired module is the active one.  On any failure return < 0 
 */
int Mark5Module::getCachedDirectory(SSHANDLE xlrDevice, 
	int mjdref, const char *vsn, const char *dir,
	int (*callback)(int, int, int, void *), void *data,
	float *replacedFrac, int force, int optionFast, int cacheOnly,
	int startScan, int stopScan)
{
	const int FilenameLength = 256;
	char filename[FilenameLength];
	int v, curbank;

	clear();

	curbank = Mark5BankSetByVSN(xlrDevice, vsn);
	if(curbank < 0)
	{
		error << "Setting bank for module " << vsn << " failed.  Error code=" << curbank << "\n";

		return -1;
	}
	
	snprintf(filename, FilenameLength, "%s/%s.dir", dir, vsn);
	
	v = load(filename);
	if(force)
	{
		signature = 0;
	}
	if(v < 0)
	{
		error << "Loading directory file " << filename << " failed.  Error code=" << v << "\n";
	}

	fast = optionFast;
	v = readDirectory(xlrDevice, mjdref, callback, data, replacedFrac, cacheOnly, startScan, stopScan);

	if(v >= 0)
	{
		error.str("");

		if(v < 0)
		{
			error << "Saving directory file " << filename << " failed.  Error code=" << v << "\n";
		}
	}
	else if(v == DIRECTORY_NOT_CACHED)
	{
		error << "Directory file ( " << filename << " ) for module " << vsn << " is not up to date.\n";
	}
	else
	{
		error << "Directory read error for module " << vsn << " .  Error code=" << v << "\n";
	}

	return v;
}

int getByteRange(const struct Mark5Scan *scan, long long *byteStart, long long *byteStop, double mjdStart, double mjdStop)
{
	double scanStart, scanStop, R;
	long long delta;

	if(scan->length <= 0)
	{
		return 0;
	}

	scanStart = scan->mjd + (scan->sec + (float)(scan->framenuminsecond)/(float)(scan->framespersecond))/86400.0;
	scanStop = scanStart + scan->duration/86400.0;

	if(scanStart >= mjdStop || scanStop <= mjdStart)
	{
		return 0;
	}

	R = scan->length*86400.0/scan->duration;

	if(mjdStart <= scanStart)
	{
		*byteStart = scan->start;
	}
	else
	{
		*byteStart = (long long)(scan->start + R*(mjdStart - scanStart));
	}

	if(mjdStop >= scanStop)
	{
		*byteStop = scan->start + scan->length;
	}
	else
	{
		*byteStop = (long long)(scan->start + R*(mjdStop - scanStart));
	}

	/* make sure read is aligned with data frames */
	delta = (*byteStart - scan->frameoffset) % scan->framebytes;
	*byteStart -= delta;
	if(*byteStart < scan->start)
	{
		*byteStart += scan->framebytes;
	}

	delta = (*byteStop - scan->frameoffset) % scan->framebytes;
	*byteStop -= delta;

	return 1;
}

int isLegalModuleLabel(const char *label)
{
	int i;
	int ns=0, nd=0;

	for(i = 0; label[i] > ' '; ++i)
	{
		if(label[i] == '-' || label[i] == '+')
		{
			if(i > 7)
			{
				return 0;
			}
			++nd;
		}
		else if(label[i] == '/')
		{
			++ns;
		}
		else if(nd == 0 && !isalpha(label[i]))
		{
			return 0;
		}
		else if(nd == 1 && ns == 0 && !isdigit(label[i]))
		{
			return 0;
		}
	}

	if(ns != 2 || nd != 1)
	{
		return 0;
	}

	return 1;
}

int parseModuleLabel(const char *label, char *vsn, int *totalCapacity, int *rate, int *moduleStatus)
{
	int n;
	int i;

	if(!isLegalModuleLabel(label))
	{
		return -1;
	}

	if(vsn)
	{
		strncpy(vsn, label, 8);
		vsn[8] = 0;
	}

	if(totalCapacity && rate)
	{
		n = sscanf(label+9, "%d/%d", totalCapacity, rate);
		if(n != 2)
		{
			return -1;
		}
	}

	if(moduleStatus)
	{
		*moduleStatus = MODULE_STATUS_UNKNOWN;
		for(i = 0; label[i]; ++i)
		{
			if(label[i] == 30)	/* Record separator */
			{
				break;
			}
		}
		if(label[i])
		{
			if(strstr(label+i, "Erased") != 0)
			{
				*moduleStatus = MODULE_STATUS_ERASED;
			}
			else if(strstr(label+i, "Recorded") != 0)
			{
				*moduleStatus = MODULE_STATUS_RECORDED;
			}
			else if(strstr(label, "Played") != 0)
			{
				*moduleStatus = MODULE_STATUS_PLAYED;
			}
		}
	}
	
	return 0;
}

int getModuleDirectoryVersion(SSHANDLE xlrDevice, int *dirVersion, int *dirLength, int *moduleStatus)
{
	int len;
	int ver = 0;
	struct Mark5DirectoryHeaderVer1 *dirHeader;
	
	if(moduleStatus)
	{
		*moduleStatus = MODULE_STATUS_UNKNOWN;
	}

	WATCHDOG( len = XLRGetUserDirLength(xlrDevice) );

	if(dirLength)
	{
		*dirLength = len;
	}

	if(len == 0)
	{
		ver = -1;
	}
	if(len % 128 == 0)
	{
		char *dirData;
		int nScan;
		
		dirData = (char *)calloc(len, 1);
		WATCHDOGTEST( XLRGetUserDir(xlrDevice, len, 0, dirData) );
		dirHeader = (struct Mark5DirectoryHeaderVer1 *)dirData;
		ver = dirHeader->version;
		if(moduleStatus)
		{
			*moduleStatus = dirHeader->status;
		}
		if(ver < 2 || ver > 1000)
		{
			ver = 1;
		}

		nScan = len/128 - 1;

#ifdef DEBUG
		printf("Directory information:");
		printf("  Ver = %d  VSN = %s\n", ver, dirHeader->vsn);
		for(int s = 0; s < nScan; ++s)
		{
			struct Mark5DirectoryScanHeaderVer1 *p;
			p = (struct Mark5DirectoryScanHeaderVer1 *)(dirData + 128*(s+1));
			int type = p->typeNumber & 0xFF;
			printf("  scan %d:\n", s+1);
			printf("    type = %d\n", type);
			printf("    frame length = %d\n", p->frameLength);
			printf("    station = %c%c\n", p->station[0], p->station[1]);
			printf("    scan name = %-32s\n", p->scanName);
			printf("    exp name = %-8s\n", p->expName);
			printf("    byte range = %Ld to %Ld\n", p->startByte, p->stopByte);
			if(type >= 3 && type <= 9)
			{
				struct Mark5DirectoryLegacyBodyVer1 *q;
				q = (struct Mark5DirectoryLegacyBodyVer1 *)(dirData + 192 + 128*s);
				printf("  * time (BCD) = ");
				for(int j = 7; j >= 0; j--)
				{
					printf("%d%d", q->timeBCD[j]/16,
							q->timeBCD[j]%16);
				}
				printf("\n");
				printf("    first frame = %d\n", q->firstFrame);
				printf("    byte offset = %d\n", q->byteOffset);
				printf("    bitstream rate = %d\n", q->trackRate);
				printf("    bitstream mask = 0x%8x\n", q->nTrack);
			}
		}
#endif
		
		free(dirData);
	}

	if(dirVersion)
	{
		*dirVersion = ver;
	}

	return 0;
}

int roundModuleSize(long long a)
{
	a /= 1000000000;
	a = (a+2)/5;

	return a*5;
}

static void trim(char *out, const char *in)
{
	int i, s=-1, e=0;

	for(i = 0; in[i]; ++i)
	{
		if(in[i] > ' ')
		{
			if(s == -1) 
			{
				s = e = i;
			}
			else
			{
				e = i;
			}
		}
	}

	if(s == -1)
	{
		out[0] = 0;
	}
	else
	{
		strncpy(out, in+s, e-s+1);
		out[e-s+1] = 0;
	}
}


int getDriveInformation(SSHANDLE xlrDevice, struct DriveInformation drive[8], int *totalCapacity)
{
	XLR_RETURN_CODE xlrRC;
	S_DRIVEINFO driveInfo;
	int nDrive = 0;
	long long minCapacity = 0;
	char message[DIFX_MESSAGE_LENGTH];

	for(int d = 0; d < 8; ++d)
	{
		WATCHDOG( xlrRC = XLRGetDriveInfo(xlrDevice, d/2, d%2, &driveInfo) );
		if(xlrRC != XLR_SUCCESS)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "XLRGetDriveInfo failed for disk %d (perhaps the disk is not present)", d);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);

			drive[d].model[0] = 0;
			drive[d].serial[0] = 0;
			drive[d].rev[0] = 0;
			drive[d].failed = 0;
			drive[d].smartCapable = 0;
		
			continue;
		}

		trim(drive[d].model, driveInfo.Model);
		trim(drive[d].serial, driveInfo.Serial);
		trim(drive[d].rev, driveInfo.Revision);
		drive[d].capacity = driveInfo.Capacity * 512LL;
		drive[d].smartCapable = driveInfo.SMARTCapable;

		if(driveInfo.SMARTCapable && !driveInfo.SMARTState)
		{
			drive[d].failed = 1;
		}
		else
		{
			drive[d].failed = 0;
		}
		if(drive[d].capacity > 0)
		{
			++nDrive;
		}
		if(minCapacity == 0 || (drive[d].capacity > 0 && drive[d].capacity < minCapacity))
		{
			minCapacity = drive[d].capacity;
		}
	}

	*totalCapacity = (nDrive*minCapacity/10000000000LL)*10;

	return nDrive;
}

