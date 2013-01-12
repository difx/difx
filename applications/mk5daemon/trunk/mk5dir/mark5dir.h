/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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

#ifndef __MARK5DIR_H__
#define __MARK5DIR_H__

#include <vector>
#include <string>
#include <set>
#include <sstream>
#include <xlrapi.h>
#include "mark5directorystructs.h"

#ifndef MARK5_FILL_PATTERN
#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_PATTERN 0x44332211UL
#else
#define MARK5_FILL_PATTERN 0x11223344UL
#endif
#endif


#define DIRECTORY_NOT_CACHED		-7

enum Mark5ReadMode
{
	MARK5_READ_MODE_NORMAL = 0,
	MARK5_READ_MODE_RT
};

// Test for SDK 9+
#ifdef XLR_MAX_IP_ADDR
#define SDKVERSION 9
typedef unsigned int streamstordatatype;
#else
#define SDKVERSION 8
typedef unsigned long streamstordatatype;
#endif

/* Internal representation of .dir files */
class Mark5Scan
{
public:
	std::string name;
	long long start;
	long long length;
	double duration;	/* scan duration in seconds */
	int mjd, sec;		/* timestamp of first frame */
	int framenuminsecond;	/* frame number since last 1 second tick */
	int framespersecond;	/* number of frames per second (always integer) */
	int framebytes;		/* length of entire frame in bytes */
	int frameoffset;	/* bytes to start of first frame */
	int tracks;
	int format;
	std::set<int> startThreads;	// set of threads present at beginning of the stream
	std::set<int> endThreads;	// set of threads present at end of the stream

	Mark5Scan();
	~Mark5Scan();
	void print() const;
	void parseDirEntry(const char *line);
	int writeDirEntry(FILE *out) const;
	int sanityCheck() const;
	int nsStart() const;
	double secStart() const;
	double mjdStart() const;
	double mjdEnd() const;
};

bool operator<(const Mark5Scan &a, const Mark5Scan &b);

class Mark5Module
{
public:
	std::vector<Mark5Scan> scans;
	//char label[XLR_LABEL_LENGTH];
	std::string label;
	std::stringstream error;
	int bank;
	unsigned int signature;	/* a hash code used to determine if dir is current */
	enum Mark5ReadMode mode;
	int dirVersion;		/* directory version = 0 for pre memo 81 */
	int fast;		/* if true, the directory came from the ModuleUserDirectory only */
	int synthetic;		/* directory was synthesized */	

	Mark5Module();
	~Mark5Module();
	void clear();
	int nScans() const { return scans.size(); }
	void print() const;
	int load(const char *filename);
	int save(const char *filename);
	void sort();
	int sanityCheck();
	int uniquifyScanNames();
	int readDirectory(SSHANDLE xlrDevice, int mjdref,
		int (*callback)(int, int, int, void *), void *data,
		float *replacedFrac, int cacheOnly, int startScan, int stopScan);
	int getCachedDirectory(SSHANDLE xlrDevice, int mjdref, const char *vsn, 
		const char *dir, int (*callback)(int, int, int, void *), void *data,
		float *replacedFrac, int force, int optionFast, int cacheOnly, int startScan, int stopScan);
	int writeDirectory(SSHANDLE xlrDevice) const;
};


enum Mark5DirStatus
{
	MARK5_DIR_SHORT_SCAN,
	MARK5_DIR_READ_ERROR,
	MARK5_DIR_DECODE_ERROR,
	MARK5_DIR_DECODE_SUCCESS,
	MARK5_DIR_DECODE_WITH_REPLACEMENTS,
	MARK5_COPY_ERROR,
	MARK5_COPY_SUCCESS,
	MARK5_DIR_ZEROS
};

enum ScanFormatError
{
	SCAN_FORMAT_ERROR_DECODE	= 1,
	SCAN_FORMAT_ERROR_READERROR	= 2,
	SCAN_FORMAT_ERROR_ZEROS		= 3,
	SCAN_FORMAT_ERROR_TOOSHORT	= 4,
	SCAN_FORMAT_ERROR_UNSUPPORTED	= 5,
	SCAN_FORMAT_ERROR_SCANNUMRANGE	= 8,
	NUM_SCAN_FORMAT_ERRORS
};

struct DriveInformation
{
	char model[XLR_MAX_DRIVENAME+1];
	char serial[XLR_MAX_DRIVESERIAL+1];
	char rev[XLR_MAX_DRIVEREV+1];
	int failed;
	int smartCapable;
	long long capacity;	/* in bytes */
};


extern char Mark5DirDescription[][20];
extern char Mark5ReadModeName[][10];
extern char ScanFormatErrorName[][40];

const char *moduleStatusName(int status);


/* returns active bank: 0 or 1 for bank A or B, or -1 if none */
int Mark5BankGet(SSHANDLE xlrDevice);

int Mark5GetActiveBankWriteProtect(SSHANDLE xlrDevice);

/* returns 0 or 1 for bank A or B, or < 0 if module not found */
int Mark5BankSetByVSN(SSHANDLE xlrDevice, const char *vsn);

void countReplaced(const streamstordatatype *data, int len, long long *wGood, long long *wBad);

int countbits(unsigned long int v);
int upround2(int value);
int mjd2ymd(long mjd, int *pYear, int *pMonth, int *pDay);
int ymd2doy(int yr, int mo, int day);

int addDecades(int mjd, int nDecade);

int getByteRange(const struct Mark5Scan *scan, long long *byteStart, long long *byteStop, double mjdStart, double mjdStop);

int getModuleDirectoryVersion(SSHANDLE xlrDevice, int *dirVersion, int *dirLength, int *moduleStatus);

int isLegalModuleLabel(const char *label);

int parseModuleLabel(const char *label, char *vsn, int *totalCapacity, int *rate, int *moduleStatus);

int setModuleLabel(SSHANDLE xlrDevice, const char *vsn, int newStatus, int dirVersion, int totalCapacity, int rate);

int resetModuleDirectory(SSHANDLE xlrDevice, const char *vsn, int newStatus, int dirVersion, int totalCapacity, int rate);

int getDriveInformation(SSHANDLE xlrDevice, struct DriveInformation drive[8], int *totalCapacity);

int roundModuleSize(long long a);

int setDiscModuleStateLegacy(SSHANDLE xlrDevice, int newState);

int setDiscModuleStateNew(SSHANDLE xlrDevice, int newState);

int setDiscModuleVSNNew(SSHANDLE xlrDevice, int newStatus, const char *newVSN, int capacity, int rate);

char *scans2newdir(const std::vector<Mark5Scan> &scans, const char *vsn);


#endif
