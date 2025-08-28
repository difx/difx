/***************************************************************************
 *   Copyright (C) 2008-2016 by Walter Brisken                             *
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
 * $Id: mark5dir.cpp 7835 2017-05-23 16:54:44Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/mk5daemon/mk5dir/mark5dir.cpp $
 * $LastChangedRevision: 7835 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2017-05-24 00:54:44 +0800 (三, 2017-05-24) $
 *
 *==========================================================================*/

#include <iostream>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <algorithm>
#include <unistd.h>
#include <time.h>
#include <difxmessage.h>
#include <mark5access.h>
#include <stdint.h>
#include <cmath>
#include "mark5dir.h"
#include "mark5directorystructs.h"
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
	"data is fill pattern",	
	"repeating data",
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

/* len is number of 32-bit words */
void countReplaced(const streamstordatatype *data, int len, long long *wGood, long long *wBad)
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

/* len is number of 32-bit words */
/* same as above, but look at 2 words together for near zero false detection rate */
void countReplaced2(const streamstordatatype *data, int len, long long *wGood, long long *wBad)
{
	int i;
	int nBad=0;
	uint64_t pattern = (MARK5_FILL_PATTERN << 32) | MARK5_FILL_PATTERN;	/* two copies of fill pattern */
	const uint64_t *d = (const uint64_t *)data;

	for(i = 0; i < len/2; ++i)
	{
		if(d[i] == pattern)
		{
			nBad += 2;
		}
	}

	if(len % 2 == 1)	/* separately check the odd-ball if exists */
	{
		if(data[len-1] == MARK5_FILL_PATTERN)
		{
			++nBad;
		}
	}

	*wGood += (len-nBad);
	*wBad += nBad;
}

/* This useful routine counts the number of bits set */
int countbits(unsigned long int v)
{
	int c; // c accumulates the total bits set in v

	for(c = 0; v; ++c)
	{
		v &= v - 1; // clear the least significant bit set
	}

	return c;
}

/* round the number up to the nearest power of 2 */
/* from: http://stackoverflow.com/questions/364985/algorithm-for-finding-the-smallest-power-of-two-thats-greater-or-equal-to-a-give */
int upround2(int value)
{
	int result = 1;

	while(result < value)
	{
		result <<= 1;
	}

	return result;
}

int mjd2ymd(long mjd, int *pYear, int *pMonth, int *pDay)
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

static void convertTimeBCD(const unsigned char *timeBCD, int *mjd, int *sec)
{
	if(sec)
	{
		*sec =	((timeBCD[0] & 0x0F) >> 0)*1 + 
			((timeBCD[0] & 0xF0) >> 4)*10 +
			((timeBCD[1] & 0x0F) >> 0)*60 + 
			((timeBCD[1] & 0xF0) >> 4)*600 +
			((timeBCD[2] & 0x0F) >> 0)*3600 +
			((timeBCD[2] & 0xF0) >> 4)*36000;
	}
	if(mjd)
	{
		int year, doy;

		doy =	((timeBCD[3] & 0x0F) >> 0)*1 +
			((timeBCD[3] & 0xF0) >> 4)*10 +
			((timeBCD[4] & 0x0F) >> 0)*100;
		year =	((timeBCD[4] & 0xF0) >> 4) +
			((timeBCD[5] & 0x0F) >> 0)*10 +
			((timeBCD[5] & 0xF0) >> 4)*100 +
			((timeBCD[6] & 0x0F) >> 0)*1000;
		*mjd = doy2mjd(year, doy);
	}
}

static void convertBCDTime(int mjd, int sec, unsigned char *timeBCD)
{
	const int secDivisors[] = {36000, 3600, 600, 60, 10, 1};
	const int doyDivisors[] = {100, 10, 1};
	const int yearDivisors[] = {1000, 100, 10, 1};
	int nibbles[14];
	int year, month, day, doy;

	mjd2ymd(mjd, &year, &month, &day);
	doy = ymd2doy(year, month, day);

	for(int i = 0; i < 6; ++i)
	{
		nibbles[5-i] =  sec / secDivisors[i];
		sec -= nibbles[5-i] * secDivisors[i];
	}
	for(int i = 0; i < 3; ++i)
	{
		nibbles[8-i] =  doy / doyDivisors[i];
		doy -= nibbles[8-i] * doyDivisors[i];
	}
	for(int i = 0; i < 4; ++i)
	{
		nibbles[12-i] =  year / yearDivisors[i];
		year -= nibbles[12-i] * yearDivisors[i];
	}
	nibbles[13] = 0;	/* because it doesn't come out even */

	for(int n = 0; n < 14; n+=2)
	{
		timeBCD[n/2] = nibbles[n] + (nibbles[n+1] << 4);
	}
}

static void expandScanName1(std::string &dest, const struct Mark5DirectoryScanHeaderVer1 *scanHeader)
{
	char str1[MODULE_SCAN_NAME_LENGTH+1];
	char str2[3];
	char str3[9];
	std::stringstream out;

	strncpy(str1, scanHeader->scanName, MODULE_SCAN_NAME_LENGTH);
	str1[MODULE_SCAN_NAME_LENGTH] = 0;

	strncpy(str2, scanHeader->station, 2);
	str2[2] = 0;

	strncpy(str3, scanHeader->expName, 8);
	str3[8] = 0;

	out << str3 << "_" << str2 << "_" << str1;
	dest = out.str();
}

static void scanName2scanHeader(struct Mark5DirectoryScanHeaderVer1 *scanHeader, const char *scanName)
{
	int state = 0;
	int start = 0;
	int len;

	for(int i = 0; ; ++i)
	{
		if(scanName[i] == '_' || scanName[i] == 0)
		{
			len = i - start;
			if(state == 0)
			{
				if(len > 8)
				{
					len = 8;
				}
				strncpy(scanHeader->expName, scanName+start, len);
			}
			else if(state == 1)
			{
				if(len > 2)
				{
					len = 2;
				}
				strncpy(scanHeader->station, scanName+start, len);
			}
			else if(state == 2)
			{
				if(len > MODULE_SCAN_NAME_LENGTH)
				{
					len = MODULE_SCAN_NAME_LENGTH;
				}
				strncpy(scanHeader->scanName, scanName+start, len);
			}

			start = i+1;
			++state;
		}
		if(scanName[i] == 0 || state == 3)
		{
			break;
		}
	}
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
	printf("%1d %-32s %13Ld %13Ld %5d %2d %5d %5d+%d/%d %6.4f",
		format, name.c_str(), start, start+length, frameoffset, tracks,
		mjd, sec, framenuminsecond, framespersecond, duration);

	if(format == MK5_FORMAT_VDIF)
	{
		std::set<int>::const_iterator it;
		printf(" :");
		for(it = startThreads.begin(); it != startThreads.end(); ++it)
		{
			printf(" %d", *it);
		}
		printf(" :");
		for(it = endThreads.begin(); it != endThreads.end(); ++it)
		{
			printf(" %d", *it);
		}
	}

	printf("\n");
}

// Parse a line from a .dir file into an existing Mark5Scan class
// returns 0 on success
int Mark5Scan::parseDirEntry(const char *line)
{
	char scanName[MODULE_LEGACY_SCAN_LENGTH];

	sscanf(line, "%Ld%Ld%d%d%d%d%lf%d%d%d%d%63s",
		&start, &length, &mjd, &sec, &framenuminsecond, &framespersecond,
		&duration, &framebytes, &frameoffset, &tracks, &format, scanName);

	name = scanName;

	if(framebytes <= 0)
	{
		return -1;
	}

	return 0;
}

int Mark5Scan::writeDirEntry(FILE *out) const
{
	const int ErrorStrLen = 60;
	const char *scanLabel;
	char errorStr[ErrorStrLen];
	int v;

	if(format < 0 && format >= -NUM_SCAN_FORMAT_ERRORS)
	{
		v = snprintf(errorStr, ErrorStrLen, " Error='%s'", ScanFormatErrorName[-format]);
		if(v >= ErrorStrLen)
		{
			std::cerr << "Developer error: Mark5Scan::writeDirEntry: ErrorStrLen too small (" << ErrorStrLen << " < " << (v+1) << ") for error number " << -format << ";truncating." << std::endl;
		}
	}
	else
	{
		strcpy(errorStr, "");
	}

	if(!name.empty())
	{
		scanLabel = name.c_str();
	}
	else
	{
		scanLabel = "Unknown";
	}

	v = fprintf(out, "%14Ld %14Ld %5d %d %d %d %12.6f %6d %6d %2d %1d %s%s",
		start, length, mjd, sec, framenuminsecond, framespersecond, duration,
		framebytes, frameoffset, tracks, format, scanLabel, errorStr);

	if(format == MK5_FORMAT_VDIF)
	{
		std::set<int> allThreads;	// union of startThreads and endThreads
		allThreads.clear();
		allThreads.insert(startThreads.begin(), startThreads.end());
		allThreads.insert(endThreads.begin(), endThreads.end());

		fprintf(out, " ");	/* put space between scan name and the thread list */
		for(std::set<int>::const_iterator it = allThreads.begin(); it != allThreads.end(); ++it)
		{
			v += fprintf(out, "%s%d", it == allThreads.begin() ? " Threads=" : ",", *it);
		}
	}

	v += fprintf(out, "\n");

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

	for(std::vector<Mark5Scan>::const_iterator s = scans.begin(); s != scans.end(); ++s)
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
		error << "Cannot load file: " << filename << "\n";

		return -1;
	}

	v = fgets(line, MaxLineLength, in);
	if(!v)
	{
		error << "Directory file: " << filename << " is corrupt.\n";
		fclose(in);

		return -1;
	}

	n = sscanf(line, "%8s %d %c %u %19s %19s %19s %19s %19s",
		dirLabel, &nscans, &bankName, &signature, extra[0], extra[1], extra[2], extra[3], extra[4]);
	if(n < 3)
	{
		error << "Directory file: " << filename << " is corrupt.\n";
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
		error << "Directory file: " << filename << " is corrupt (nscans < 0).\n";
		fclose(in);

		return -1;
	}

	label = dirLabel;
	bank = bankName-'A';
	scans.resize(nscans);

	for(std::vector<Mark5Scan>::iterator s = scans.begin(); s != scans.end(); ++s)
	{
		int rv;

		v = fgets(line, MaxLineLength, in);
		if(!v)
		{
			error << "Directory file: " << filename << " is corrupt (file too short).\n";
			fclose(in);

			return -1;
		}
		
		rv = s->parseDirEntry(line);
		if(rv != 0)
		{
			error << "Directory file: " << filename << " is corrupt: at least one scan line is invalid.\n";
			fclose(in);

			return -1;
		}
	}

	fclose(in);

	return 0;
}

int Mark5Module::save(const char *filename)
{
	FILE *out;
	char sub[10];
	
	if(dirSubversion >= 0)
	{
		snprintf(sub, 10, "%d", dirSubversion);
	}
	else
	{
		sub[0] = 0;
	}

	out = fopen(filename, "w");
	if(!out)
	{
		error << "Cannot write to file: " << filename << "\n";
		fprintf(stderr, "Error: cannot open %s for write\n", filename);

		return -1;
	}

	fprintf(out, "%8s %d %c %u %d %s %s%s%s\n",
		label.c_str(), nScans(), bank+'A', signature, dirVersion,
		sub,
		Mark5ReadModeName[mode], 
		fast ? " Fast" : "",
		synthetic ? " Synth" : "");

	for(std::vector<Mark5Scan>::const_iterator s = scans.begin(); s != scans.end(); ++s)
	{
		s->writeDirEntry(out);
	}

	fclose(out);

	return 0;
}

void Mark5Module::sort()
{
	std::sort(scans.begin(), scans.end());
}

int Mark5Module::sanityCheck()
{
	int nError = 0;

	for(std::vector<Mark5Scan>::const_iterator s = scans.begin(); s != scans.end(); ++s)
	{
		nError += s->sanityCheck();
	}

	return nError;
}

int Mark5Module::uniquifyScanNames()
{
	const int MaxExtensionLength = 8;
	std::vector<std::string> scanNames;
	std::vector<int> nameCount;
	std::vector<int> origIndex;
	char extension[MaxExtensionLength];
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
				snprintf(extension, MaxExtensionLength, "_%04d", nameCount[j]);
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

int getVdifThreads(std::set<int> &threadSet, const char *data, int bytes, int framebytes)
{
	threadSet.clear();
	int lastSecond = -1;
	int index = 0;
	int nError = 0;

	for(;;)
	{
		int second;
		unsigned int thread;
		unsigned int word0, word3;
		while(index+4 < bytes)
		{
			unsigned int *headerwords = (unsigned int *)(data + index);

			word0 = headerwords[0];
			word3 = headerwords[3];

			second = word0 & 0x3FFFFFFF;
			if(lastSecond != -1 && second != lastSecond && second != lastSecond+1)
			{
				index += 4;
			}
			else
			{
				lastSecond = second;

				break;
			}

			++nError;
		}
		if(index+4 >= bytes)
		{
			break;
		}
		thread = (word3 >> 16) & 0x03FF;
		threadSet.insert(thread);

		index += framebytes;
	}

	return nError;
}

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
			struct timespec ts;

			WATCHDOG( xlrRC = XLRGetBankStatus(xlrDevice, bank, &bank_stat) );
			if(xlrRC != XLR_SUCCESS)
			{
				return -7;
			}
			if(bank_stat.State == STATE_READY && bank_stat.Selected)
			{
				break;
			}

			ts.tv_sec = 0;
			ts.tv_nsec = 100000000;
			nanosleep(&ts, 0);
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

char *scans2newdir(const std::vector<Mark5Scan> &scans, const char *vsn)
{
	int totalScans = scans.size();
	int dirLength = totalScans*128 + 128;
	char *dirData;
	int v;

	dirData = (char *)calloc(dirLength, 1);
	if(!dirData)
	{
		return 0;
	}

	struct Mark5DirectoryHeaderVer1 *header = (struct Mark5DirectoryHeaderVer1 *)dirData;
	header->version = 1;
	header->status = MODULE_STATUS_PLAYED;
	strcpy(header->vsn, vsn);
	v = snprintf(header->vsn, MODULE_EXTENDED_VSN_LENGTH, "%s", vsn);
	if(v >= MODULE_EXTENDED_VSN_LENGTH)
	{
		std::cerr << "Developer warning: scans2newdir: vsn length too long (" << v << " > " << (MODULE_EXTENDED_VSN_LENGTH - 1) << std::endl;
	}

	for(int i = 0; i < totalScans; ++i)
	{
		struct Mark5DirectoryScanHeaderVer1 *scanHeader = (struct Mark5DirectoryScanHeaderVer1 *)(dirData + 128*i + 128);
		struct Mark5DirectoryLegacyBodyVer1 *scanBody = (struct Mark5DirectoryLegacyBodyVer1 *)(dirData + 128*i + 192);
		const Mark5Scan &scan = scans[i];
		int type;

		scanName2scanHeader(scanHeader, scan.name.c_str());
		scanHeader->startByte = scan.start;
		scanHeader->stopByte = scan.start + scan.length;
		scanHeader->frameLength = scan.framebytes;
		if(scan.format == MK5_FORMAT_MARK5B) /* currently only handle Mark5B */
		{
			int nTrack;

			nTrack = scan.tracks;
			if(nTrack == 0)
			{
				fprintf(stderr, "Warning: changing nTrack from 0 to 32\n");
				nTrack = 32;
			}

			type = 8;
			scanBody->byteOffset = scan.frameoffset;
			convertBCDTime(scan.mjd, scan.sec, scanBody->timeBCD);
			scanBody->nTrack = (1LL << nTrack) - 1;	/* convert to a bitstream mask */
			scanBody->trackRate = 2*scan.framespersecond/(25*nTrack);
			scanBody->firstFrame = scan.framenuminsecond;
			
		}
		else
		{
			type = 1;
		}
		scanHeader->typeNumber = type + ((i+1) << 8);
	}

	return dirData;
}

/* Take the Mark5 User Directory as a binary record pointed to by dirData and decode into a std::vector of scan structures */
int newdir2scans(std::vector<Mark5Scan> &scans, const unsigned char *dirData, int dirLength, int startScan, int stopScan)
{
	int totalScans = dirLength/128-1;
	double overhead = 0.0;

	if(startScan < 0)
	{
		startScan = 0;
	}
	if(stopScan > totalScans)
	{
		stopScan = totalScans;
	}
	if(startScan >= stopScan)
	{
		return -1;
	}

	for(int i = startScan; i < stopScan; ++i)
	{
		const struct Mark5DirectoryScanHeaderVer1 *scanHeader = (struct Mark5DirectoryScanHeaderVer1 *)(dirData + 128*i + 128);
		const struct Mark5DirectoryLegacyBodyVer1 *scanBody = (struct Mark5DirectoryLegacyBodyVer1 *)(dirData + 128*i + 192);
		Mark5Scan &scan = scans[i];
		int type = scanHeader->typeNumber & 0xFF;
		
		expandScanName1(scan.name, scanHeader);
		scan.start  = scanHeader->startByte;
		scan.length = scanHeader->stopByte - scanHeader->startByte;
		scan.framebytes = scanHeader->frameLength;

		if(type >= 3 && type <= 9)
		{
			switch(type)
			{
			case 3:
				scan.format = MK5_FORMAT_VLBA;
				overhead = 1.008;
				break;
			case 4:
				scan.format = MK5_FORMAT_MARK4;
				overhead = 1.0;
				break;
			case 8:
			case 9:
				scan.format = MK5_FORMAT_MARK5B;
				scan.framebytes = 10016;
				overhead = 1.0016;
				break;
			default:
				scan.format = -1;
				continue;
			}
			convertTimeBCD(scanBody->timeBCD, &scan.mjd, &scan.sec);
			scan.frameoffset = scanBody->byteOffset;
			if(scan.format == MK5_FORMAT_MARK5B)
			{
				scan.tracks = upround2(countbits(scanBody->nTrack));
			}
			else
			{
				scan.tracks = scanBody->nTrack;
			}
			scan.framespersecond = int(125000.0*(double)(scanBody->trackRate)*overhead*(double)(scan.tracks)/(double)(scan.framebytes) + 0.5);
			scan.framenuminsecond = scanBody->firstFrame;
			if(scan.framespersecond > 0)
			{
				scan.duration = (int)((scan.length - scan.frameoffset) / scan.framebytes)/(double)(scan.framespersecond);
			}
		}
		else
		{
			/* Currently unsupported type */
			scan.format = -SCAN_FORMAT_ERROR_UNSUPPORTED;
		}
	
	}

	return 0;
}

static int repeatingData(const char *bufferStart, int framebytes)
{
	uint32_t *p32 = (uint32_t *)bufferStart;
	uint64_t *p64 = (uint64_t *)bufferStart;

	if(framebytes < 500)
	{
		/* don't bother */

		return 0;
	}

	/* look for 32-bit repetitions */
	if(p32[20] == p32[21] && p32[20] == p32[23] && p32[20] == p32[100])
	{
		return 1;
	}

	if(p64[20] == p64[21] && p64[20] == p64[23] && p64[20] == p64[40])
	{
		return 1;
	}

	return 0;
}

int Mark5Module::readDirectory(SSHANDLE xlrDevice, int mjdref, int (*callback)(int, int, int, void *), void *data, float *replacedFrac, int cacheOnly, int startScan, int stopScan, int forceVersion, const char *binFilename)
{
	XLR_RETURN_CODE xlrRC;
	struct Mark5LegacyDirectory *legacyDir;
	struct Mark5NeoLegacyDirectory *neolegacyDir;
	unsigned char *dirData;
	int dirLength;
	struct mark5_format *mf;
	char newLabel[XLR_LABEL_LENGTH];
	int newBank;
	int bufferLength;
	struct Mark5DirectoryInfo dirInfo;
	int die = 0;
	int oldFast;
	int v;

	streamstordatatype *bufferStart, *bufferStop, *bufferPrefil;

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
	v = getMark5DirectoryInfo(&dirInfo, dirData, dirLength, forceVersion);
	if(v != Mark5DirectoryInfoSuccess)
	{
		FILE *out;
		const char dumpFile[] = "/tmp/dir.dump";

		fprintf(stderr, "\nModule directory parse error %d encountered: %s\n", v, Mark5DirectoryInfoStatusStrings[v]);
		fprintf(stderr, "The directory size was %d\n", dirLength);
		fprintf(stderr, "The best guess about the directory content is printed below:\n");
		fprintMark5DirectoryInfo(stderr, &dirInfo);

		out = fopen(dumpFile, "w");
		if(!out)
		{
			fprintf(stderr, "Tried to dump directory to %s, but could not open for write.\n", dumpFile);
		}
		else
		{
			fwrite(dirData, 1, dirLength, out);
			fclose(out);
			fprintf(stderr, "The binary directory was dumped to %s\n", dumpFile);
		}

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
				printf("Performing normal directory read.\n");
				fast = 0;
			}

			if(dirInfo.dirClass == Mark5DirClassLegacy)
			{
				scan.name   = legacyDir->scanName[i];
				scan.start  = legacyDir->start[i];
				scan.length = legacyDir->length[i];
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
				printf("Short scan found: %s start=%Ld length=%Ld bufferLength=%d\n", scan.name.c_str(), scan.start, scan.length, bufferLength);
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

			countReplaced2(bufferStart, bufferLength/4, &wGood, &wBad);
			countReplaced2(bufferStop, bufferLength/4, &wGood, &wBad);

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
		if(!out)
		{
			fprintf(stderr, "Cannot open %s for write.  No binary directory will be created.\n", binFilename);
		}
		else
		{
			fwrite(dirData, dirLength, 1, out);
			fclose(out);
		}
	}

	free(dirData);

	uniquifyScanNames();

	sort();

	return -die;
}

int Mark5Module::writeDirectory(SSHANDLE xlrDevice) const
{
	char *dirData;
	int v;

	dirData = scans2newdir(scans, label.c_str());

	if(dirData)
	{
		int dirLength;
		
		dirLength = scans.size()*128 + 128;
		WATCHDOGTEST( XLRSetUserDir(xlrDevice, dirData, dirLength) );
		free(dirData);
	
		v = 0;
	}
	else
	{
		v = -1;
	}

	return v;
}


/* retrieves directory (either from cache or module) and makes sure
 * desired module is the active one.  On any failure return < 0 
 */
int Mark5Module::getCachedDirectory(SSHANDLE xlrDevice, 
	int mjdref, const char *vsn, const char *dir,
	int (*callback)(int, int, int, void *), void *data,
	float *replacedFrac, int force, int optionFast, int cacheOnly,
	int startScan, int stopScan, int forceVersion)
{
	const int FilenameLength = 256;
	char filename[FilenameLength];
	char binFilename[FilenameLength];
	int v, curbank;

	clear();

	curbank = Mark5BankSetByVSN(xlrDevice, vsn);
	if(curbank < 0)
	{
		error << "Setting bank for module " << vsn << " failed.  Error code=" << curbank << "\n";

		return -1;
	}
	
	snprintf(filename, FilenameLength, "%s/%s.dir", dir, vsn);
	snprintf(binFilename, FilenameLength, "%s/%s.bindir", dir, vsn);
	
	v = load(filename);
	if(force)
	{
		signature = 0;
	}
	if(v < 0)
	{
		error << "Loading directory file " << filename << " failed.  Error code=" << v << "\n";
		if(cacheOnly)
		{
			return -1;
		}
	}

	fast = optionFast;
	v = readDirectory(xlrDevice, mjdref, callback, data, replacedFrac, cacheOnly, startScan, stopScan, forceVersion, binFilename);

	if(v >= 0)
	{
		if(!cacheOnly)
		{
			error.str("");

			v = save(filename);
			if(v < 0)
			{
				error << "Saving directory file " << filename << " failed.  Error code=" << v << "\n";
			}
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
		int n = sscanf(label+9, "%d/%d", totalCapacity, rate);
		if(n != 2)
		{
			return -1;
		}
	}

	if(moduleStatus)
	{
		int i;

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

int setModuleLabel(SSHANDLE xlrDevice, const char *vsn,  int newStatus, int dirVersion, int totalCapacity, int rate)
{
	const char RecordSeparator = 30;
	char label[XLR_LABEL_LENGTH+1];
	int v;

	/* reset the label */
	if(dirVersion == 0)
	{
		v = snprintf(label, XLR_LABEL_LENGTH+1, "%8s/%d/%d%c%s", vsn, totalCapacity, rate, RecordSeparator, moduleStatusName(newStatus) );
	}
	else
	{
		v = snprintf(label, XLR_LABEL_LENGTH+1, "%8s/%d/%d", vsn, totalCapacity, rate);
	}
	
	if(v > XLR_LABEL_LENGTH)
	{
		std::cerr << "Developer error: setModuleLabel: label too long (" << v << " > " << (XLR_LABEL_LENGTH-1) << "); truncating!" << std::endl;
	}

	WATCHDOGTEST( XLRSetLabel(xlrDevice, label, strlen(label)) );

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

#ifdef DEBUG
		int nScan = len/128 - 1;

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

int resetModuleDirectory(SSHANDLE xlrDevice, const char *vsn, int newStatus, int dirVersion, int totalCapacity, int rate)
{
	int dirLength;
	char *dirData;
	struct Mark5DirectoryHeaderVer1 *dirHeader;

	WATCHDOG( dirLength = XLRGetUserDirLength(xlrDevice) );
	
	dirData = 0;
	if(dirVersion == -2)
	{
		dirLength = 0;
		dirVersion = 0;
		dirData = 0;
	}
	else if(dirVersion == -1)
	{
		if(dirLength < 80000 || dirLength % 128 == 0)
		{
			dirLength = 128;
			dirData = (char *)calloc(dirLength, 1);
			WATCHDOGTEST( XLRGetUserDir(xlrDevice, dirLength, 0, dirData) );
			dirHeader = (struct Mark5DirectoryHeaderVer1 *)dirData;
			dirVersion = dirHeader->version;
			if(dirVersion < 2 || dirVersion > 100)
			{
				dirVersion = 1;
			}
			memset(dirData, 0, 128);
			snprintf(dirHeader->vsn, MODULE_EXTENDED_VSN_LENGTH,
				"%s/%d/%d", vsn, totalCapacity, rate);
			dirHeader->status = newStatus;
			dirHeader->version = dirVersion;
		}
		else
		{
			dirVersion = 0;
		}
	}
	else if(dirVersion == 0)
	{
		dirLength = 83424;
	}
	else
	{
		dirLength = 128;
		dirData = (char *)calloc(dirLength, 1);
		dirHeader = (struct Mark5DirectoryHeaderVer1 *)dirData;
		dirHeader->version = dirVersion;
		dirHeader->status = newStatus;
		snprintf(dirHeader->vsn, MODULE_EXTENDED_VSN_LENGTH, "%s/%d/%d", vsn, totalCapacity, rate);
		strcpy(dirHeader->vsnPrev, "NA");
		strcpy(dirHeader->vsnNext, "NA");
	}

	if(dirData == 0)
	{
		dirData = (char *)calloc(dirLength, 1);
	}

	printf("> Dir Size = %d  Dir Version = %d  Status = %d\n", 
		dirLength, dirVersion, newStatus);

	WATCHDOGTEST( XLRSetUserDir(xlrDevice, dirData, dirLength) );
	if(dirData)
	{
		free(dirData);
	}

	return dirVersion;
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
			snprintf(message, DIFX_MESSAGE_LENGTH, "XLRGetDriveInfo failed for disk %d (perhaps the disk isn't present)", d);
			printf("%s\n", message);
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


int setDiscModuleStateLegacy(SSHANDLE xlrDevice, int newState)
{
	const char RecordSeparator = 30;	// ASCII "RS" == "Record separator"
	char label[XLR_LABEL_LENGTH];
	int labelLength = 0, rs = 0;
	int wp;

	wp = Mark5GetActiveBankWriteProtect(xlrDevice);

	WATCHDOGTEST( XLRGetLabel(xlrDevice, label) );

	for(labelLength = 0; labelLength < XLR_LABEL_LENGTH; ++labelLength)
	{
		if(!label[labelLength])
		{
			break;
		}
	}
	if(labelLength >= XLR_LABEL_LENGTH)
	{
		std::cerr << "Module label is not terminated!" << std::endl;

		return -1;
	}

	for(rs = 0; rs < labelLength; ++rs)
	{
		if(label[rs] == RecordSeparator)
		{
			break;
		}
	}
	if(rs >= labelLength)
	{
		std::cerr << "Warning: module label record separator not found!" << std::endl;
		label[rs] = RecordSeparator;
		label[rs+1] = 0;
	}

	label[rs] = 0;

	if(strcmp(label+rs+1, moduleStatusName(newState)) != 0)
	{
		int v;

		std::cout << "Directory version 0: setting module DMS to " << moduleStatusName(newState) << std::endl;
		label[rs] = RecordSeparator;	// ASCII "RS" == "Record separator"
		v = snprintf(label+rs+1, XLR_LABEL_LENGTH-rs-1, "%s", moduleStatusName(newState));
		if(v >= XLR_LABEL_LENGTH-rs-1)
		{
			std::cerr << "Developer error: setDiscModuleStateLegacy: label length overflow" << std::endl;
		}
		if(wp == 1)
		{
			WATCHDOGTEST( XLRClearWriteProtect(xlrDevice) );
		}
		WATCHDOGTEST( XLRSetLabel(xlrDevice, label, strlen(label)) );
		if(wp == 1)
		{
			WATCHDOGTEST( XLRSetWriteProtect(xlrDevice) );
		}
	}

	return 0;
}

int setDiscModuleStateNew(SSHANDLE xlrDevice, int newState)
{
	int dirLength;
	char *dirData;
	struct Mark5DirectoryHeaderVer1 *dirHead;
	int wp;

	wp = Mark5GetActiveBankWriteProtect(xlrDevice);

	WATCHDOG( dirLength = XLRGetUserDirLength(xlrDevice) );

	if(dirLength < 128 || dirLength % 128 != 0)
	{
		// Version must be > 0!
		return -1;
	}

	dirData = (char *)calloc(dirLength, 1);
	WATCHDOGTEST( XLRGetUserDir(xlrDevice, dirLength, 0, dirData) );
	
	dirHead = (struct Mark5DirectoryHeaderVer1 *)dirData;

	if(dirHead->status != newState)
	{
		std::cout << "Directory version " << dirHead->version << ": setting module DMS to " << moduleStatusName(newState) << std::endl;
		dirHead->status = newState;

		if(wp == 1)
		{
			WATCHDOGTEST( XLRClearWriteProtect(xlrDevice) );
		}
		WATCHDOGTEST( XLRSetUserDir(xlrDevice, dirData, dirLength) );
		if(wp == 1)
		{
			WATCHDOGTEST( XLRSetWriteProtect(xlrDevice) );
		}
	}

	free(dirData);

	return 0;
}

int setDiscModuleVSNNew(SSHANDLE xlrDevice, int newStatus, const char *newVSN, int capacity, int rate)
{
	struct Mark5DirectoryHeaderVer1 *dirHeader;
	int dirLength;
	char *data;

	WATCHDOG( dirLength = XLRGetUserDirLength(xlrDevice) );

	data = (char *)malloc(dirLength);
	dirHeader = (struct Mark5DirectoryHeaderVer1 *)data;

	WATCHDOGTEST( XLRGetUserDir(xlrDevice, dirLength, 0, data) );

	if(newStatus >= 0)
	{
		dirHeader->status = newStatus;
	}
	if(newVSN)
	{
		snprintf(dirHeader->vsn, MODULE_EXTENDED_VSN_LENGTH, "%s/%d/%d", newVSN, capacity, rate);
	}

	WATCHDOGTEST( XLRSetUserDir(xlrDevice, data, dirLength) );

	free(data);

	return 0;
}
