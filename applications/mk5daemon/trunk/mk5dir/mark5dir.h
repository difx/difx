/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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

#ifndef __MARK5ACCESS_H__
#define __MARK5ACCESS_H__

#include <xlrapi.h>

#ifdef __cplusplus
extern "C" {
#endif


#ifndef MARK5_FILL_PATTERN
#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_PATTERN 0x44332211UL
#else
#define MARK5_FILL_PATTERN 0x11223344UL
#endif
#endif


#define DIRECTORY_NOT_CACHED	-7

#define MODULE_STATUS_UNKNOWN	0x00
#define MODULE_STATUS_ERASED	0x01
#define MODULE_STATUS_PLAYED	0x02
#define MODULE_STATUS_RECORDED	0x04
#define MODULE_STATUS_BANK_MODE	0x08
#define MODULE_EXTENDED_VSN_LENGTH	32
#define MODULE_SCAN_NAME_LENGTH		32
#define MODULE_LEGACY_SCAN_LENGTH	64
#define MODULE_MAX_SCANS	1024 /* Maximum number of scans in SDir */

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

/* as implemented in Mark5A */
struct Mark5Directory
{
	int nscans; /* Number of scans herein */
	int n; /* Next scan to be accessed by "next_scan" */
	char scanName[MODULE_MAX_SCANS][MODULE_LEGACY_SCAN_LENGTH]; /* Extended name */
	unsigned long long start[MODULE_MAX_SCANS]; /* Start byte position */
	unsigned long long length[MODULE_MAX_SCANS]; /* Length in bytes */
	unsigned long long recpnt; /* Record offset, bytes (not a pointer) */
	long long plapnt; /* Play offset, bytes */
	double playRate; /* Playback clock rate, MHz */
};

/* first updated version as defined by Hastack Mark5 Memo #081 */
struct Mark5DirectoryHeaderVer1
{
	int version;		/* should be 1 */
	int status;		/* bit field: see MODULE_STATUS_xxx above */
	char vsn[MODULE_EXTENDED_VSN_LENGTH];
	char vsnPrev[MODULE_EXTENDED_VSN_LENGTH];	/* "continued from" VSN */
	char vsnNext[MODULE_EXTENDED_VSN_LENGTH];	/* "continued to" VSN */
	char zeros[24];
};

struct Mark5DirectoryScanHeaderVer1
{
	unsigned int typeNumber;	/* and scan number; see memo 81 */
	unsigned short frameLength;
	char station[2];
	char scanName[MODULE_SCAN_NAME_LENGTH];
	char expName[8];
	long long startByte;
	long long stopByte;
};

struct Mark5DirectoryLegacyBodyVer1
{
	unsigned char timeBCD[8];	/* version dependent time code. */
	int firstFrame;
	int byteOffset;
	int trackRate;
	int nTrack;
	char zeros[40];
};

struct Mark5DirectoryVDIFBodyVer1
{
	unsigned short data[8][4];	/* packed bit fields for up to 8 thread groups */
};

/* Internal representation of .dir files */
struct Mark5Scan
{
	char name[MODULE_SCAN_NAME_LENGTH];
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
};

struct Mark5Module
{
	char label[XLR_LABEL_LENGTH];
	int bank;
	int nscans;
	Mark5Scan scans[MODULE_MAX_SCANS];
	unsigned int signature;	/* a hash code used to determine if dir is current */
	enum Mark5ReadMode mode;
	int dirVersion;		/* directory version = 0 for pre memo 81 */
	int fast;		/* if true, the directory came from the ModuleUserDirectory only */
};

enum Mark5DirStatus
{
	MARK5_DIR_SHORT_SCAN,
	MARK5_DIR_READ_ERROR,
	MARK5_DIR_DECODE_ERROR,
	MARK5_DIR_DECODE_SUCCESS,
	MARK5_DIR_DECODE_WITH_REPLACEMENTS,
	MARK5_COPY_ERROR,
	MARK5_COPY_SUCCESS
};

struct DriveInformation
{
	char model[XLR_MAX_DRIVENAME+1];
	char serial[XLR_MAX_DRIVESERIAL+1];
	char rev[XLR_MAX_DRIVEREV+1];
	int failed;
	long long capacity;	/* in bytes */
};


extern char Mark5DirDescription[][20];
extern char Mark5ReadModeName[][10];

const char *moduleStatusName(int status);

/* returns active bank: 0 or 1 for bank A or B, or -1 if none */
int Mark5BankGet(SSHANDLE xlrDevice);

int Mark5GetActiveBankWriteProtect(SSHANDLE xlrDevice);

/* returns 0 or 1 for bank A or B, or < 0 if module not found */
int Mark5BankSetByVSN(SSHANDLE xlrDevice, const char *vsn);

int getMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, int mjdref,
	int (*callback)(int, int, int, void *), void *data);

void printMark5Module(const struct Mark5Module *module);

int loadMark5Module(struct Mark5Module *module, const char *filename);

int saveMark5Module(struct Mark5Module *module, const char *filename);

int sanityCheckModule(const struct Mark5Module *module);

int getCachedMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, 
	int mjdref, const char *vsn, const char *dir,
	int (*callback)(int, int, int, void *), void *data,
	float *replacedFrac, int force, int fast, int cacheOnly, int startScan, int stopScan);

void countReplaced(const streamstordatatype *data, int len,
	long long *wGood, long long *wBad);

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

#ifdef __cplusplus
}
#endif


#endif
