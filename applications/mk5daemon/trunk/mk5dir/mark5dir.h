/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/

#ifndef __MARK5ACCESS_H__
#define __MARK5ACCESS_H__

#include <xlrapi.h>

#define MAXSCANS  1024 /* Maximum number of scans in SDir */
#define MAXLENGTH   64 /* Maximum length of a scan's extended name +1 */

/* as implemented in Mark5 */
struct Mark5Directory
{
	int nscans; /* Number of scans herein */
	int n; /* Next scan to be accessed by "next_scan" */
	char scanName[MAXSCANS][MAXLENGTH]; /* Extended name */
	unsigned long long start[MAXSCANS]; /* Start byte position */
	unsigned long long length[MAXSCANS]; /* Length in bytes */
	unsigned long long recpnt; /* Record offset, bytes (not a pointer) */
	long long plapnt; /* Play offset, bytes */
	double playRate; /* Playback clock rate, MHz */
};

struct Mark5Scan
{
	char name[MAXLENGTH];
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
	Mark5Scan scans[MAXSCANS];
	unsigned int signature;
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

extern char Mark5DirDescription[][20];

/* returns active bank: 0 or 1 for bank A or B, or -1 if none */
int Mark5BankGet(SSHANDLE xlrDevice);

/* returns 0 or 1 for bank A or B, or < 0 if module not found */
int Mark5BankSetByVSN(SSHANDLE xlrDevice, const char *vsn);

int getMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, int mjdref,
	int (*callback)(int, int, int, void *), void *data);

void printMark5Module(const struct Mark5Module *module);

int loadMark5Module(struct Mark5Module *module, const char *filename);

int saveMark5Module(struct Mark5Module *module, const char *filename);

int getCachedMark5Module(struct Mark5Module *module, SSHANDLE xlrDevice, 
	int mjdref, const char *vsn, const char *dir,
	int (*callback)(int, int, int, void *), void *data,
	float *replacedFrac);



#endif
