#ifndef __MARK5_DIRECTORY_STRUCTS__
#define __MARK5_DIRECTORY_STRUCTS__

#define MODULE_LEGACY_MAX_SCANS		1024
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



/* as implemented in Mark5A */
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

#endif
