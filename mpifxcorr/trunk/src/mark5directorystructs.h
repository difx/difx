#ifndef __MARK5_DIRECTORY_STRUCTS__
#define __MARK5_DIRECTORY_STRUCTS__

#define MODULE_LEGACY_MAX_SCANS		1024
#define MODULE_NEOLEGACY_MAX_SCANS	65536
#define MODULE_EXTENDED_VSN_LENGTH	32
#define MODULE_STATION_NAME_LENGTH	2
#define MODULE_SCAN_NAME_LENGTH         32
#define MODULE_EXPERIMENT_NAME_LENGTH	8
#define MODULE_LEGACY_SCAN_LENGTH       64

#define MODULE_STATUS_UNKNOWN           0x00
#define MODULE_STATUS_ERASED            0x01
#define MODULE_STATUS_PLAYED            0x02
#define MODULE_STATUS_RECORDED          0x04
#define MODULE_STATUS_BANK_MODE         0x08


/* This set of routines and structures handles Mark5 directory structures as documented in:
 * Mark5 memo 81: http://www.haystack.mit.edu/tech/vlbi/mark5/mark5_memos/081.pdf
 * and
 * Mark5 memo 100: http://www.haystack.mit.edu/tech/vlbi/mark5/mark5_memos/100.pdf
 */


enum Mark5DirClass
{
	Mark5DirClassLegacy = 0,	/* Mark5A or B, with 1024 scan max */
	Mark5DirClassMark5C,		/* Mark5C */
	Mark5DirClassNeoLegacy,		/* Mark5A or B, with 65536 scan max */
	Mark5DirClassNone,		/* no dir on module */
	Mark5DirClassIllegal		/* does not map to any legal dir type */
};

struct Mark5DirectoryInfo
{
	enum Mark5DirClass dirClass;
	int dirVersion;
	int nScan;
	unsigned int signature;
};

/* as implemented in Mark5A and Mark5B */
struct Mark5LegacyDirectory
{
	int nscans; /* Number of scans herein */
	int n; /* Next scan to be accessed by "next_scan" */
	char scanName[MODULE_LEGACY_MAX_SCANS][MODULE_LEGACY_SCAN_LENGTH]; /* Extended name */
	unsigned long long start[MODULE_LEGACY_MAX_SCANS]; /* Start byte position */
	unsigned long long length[MODULE_LEGACY_MAX_SCANS]; /* Length in bytes */
	unsigned long long recpnt; /* Record offset, bytes (not a pointer) */
	long long plapnt; /* Play offset, bytes */
	double playRate; /* Playback clock rate, MHz */
};

struct Mark5NeoLegacyDirectory
{
	int nscans; /* Number of scans herein */
	int n; /* Next scan to be accessed by "next_scan" */
	char scanName[MODULE_NEOLEGACY_MAX_SCANS][MODULE_LEGACY_SCAN_LENGTH]; /* Extended name */
	unsigned long long start[MODULE_NEOLEGACY_MAX_SCANS]; /* Start byte position */
	unsigned long long length[MODULE_NEOLEGACY_MAX_SCANS]; /* Length in bytes */
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
	char station[MODULE_STATION_NAME_LENGTH];
	char scanName[MODULE_SCAN_NAME_LENGTH];
	char expName[MODULE_EXPERIMENT_NAME_LENGTH];
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

enum Mark5DirectoryInfoStatus
{
	Mark5DirectoryInfoSuccess = 0,
	Mark5DirectoryInfoErrorSize,
	Mark5DirectoryInfoErrorGranularity,
	Mark5DirectoryInfoErrorConnection,
	Mark5DirectoryInfoErrorVersion,
	Mark5DirectoryInfoErrorNScan,

	Mark5DirectoryInfoEnumSize	/* list terminator */
};

extern const char Mark5DirectoryInfoStatusStrings[][40];


int calculateMark5DirSignature(const unsigned char *data, int size);

enum Mark5DirectoryInfoStatus getMark5DirectoryInfo(struct Mark5DirectoryInfo *info, const unsigned char *dirData, int dirSize);

void fprintMark5DirectoryInfo(FILE *out, const struct Mark5DirectoryInfo *info);

#endif
