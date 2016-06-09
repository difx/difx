#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "mark5directorystructs.h"


/* Note!  Still need to disambiguate SDK8/16DSK/B and SDK9/16DSK formats */

const char Mark5DirectoryInfoStatusStrings[][40] =
{
	"Success",
	"Illegal directory size",
	"Illegal directory pointers",
	"Non-contiguous directory structure",
	"Illegal directory version",
	"Illegal number of scans for format",

	"(List Terminator)"		/* This must remain the last item of this list */
};


int calculateMark5DirSignature(const unsigned char *data, int size)
{
	int signature;

	signature = 1;

	if(size > 0)
	{
		for(int j = 0; j < size/4; ++j)
		{
			unsigned int x = ((unsigned int *)data)[j] + 1;
			signature = signature ^ x;
		}

		/* prevent a zero signature */
		if(signature == 0)
		{
			signature = 0x55555555;
		}
	}

	return signature;
}

enum Mark5DirectoryInfoStatus getMark5DirectoryInfo(struct Mark5DirectoryInfo *info, const unsigned char *dirData, int dirSize, int forceVersion)
{
	const int LegacyScanDirectorySize = 81952;
	const int NeoLegacyScanDirectorySize = 5242912;
	const int DiskInfoSizes[8] = { 768, 1472, 832, 1536, 800, 1536, 864, 1600 };
	enum Mark5DirectoryInfoStatus status = Mark5DirectoryInfoErrorSize;

	memset(info, 0, sizeof(struct Mark5DirectoryInfo));
	info->dirClass = Mark5DirClassIllegal;

	if(dirSize == 0)
	{
		info->dirClass = Mark5DirClassNone;
		info->dirVersion = 0;
		info->nScan = 0;
		info->signature = 0;

		return Mark5DirectoryInfoSuccess;
	}
	if(dirSize % 128 == 0 && dirSize > 0)
	{
		const struct Mark5DirectoryHeaderVer1 *head = (struct Mark5DirectoryHeaderVer1 *)dirData;
		int nConnect = 0;
		int nBad = 0;

		/* likely a Mark5C type; proceed as if, at least for now */
		info->dirClass = Mark5DirClassMark5C;
		if(forceVersion >= 0)
		{
			info->dirVersion = forceVersion;
		}
		else
		{
			info->dirVersion = head->version;
		}
		info->nScan = dirSize / 128 - 1;
		info->signature = calculateMark5DirSignature(dirData + 128, dirSize-128);

		if(info->dirVersion == 0)
		{
			printf("Weird: version reported to be zero.  Changing to 1.\n");

			info->dirVersion = 1;
		}

		/* now start the plausibility checking */
		if(info->dirVersion == 1)
		{
			if(info->nScan > 1)
			{
				const struct Mark5DirectoryScanHeaderVer1 *A, *B;
	
				B = (const struct Mark5DirectoryScanHeaderVer1 *)(dirData + 128);

				for(int i = 1; i < info->nScan; ++i)
				{
					A = B;
					B = (const struct Mark5DirectoryScanHeaderVer1 *)(dirData + 128 + 128*i);
					if(A->stopByte == B->startByte)
					{
						/* these scans but up against each other */
						++nConnect;
					}
					if(B->startByte % 4 || B->stopByte % 4)
					{
						/* this scan has illegal scan boundaries */
						++nBad;
					}
				}
			}
		}

		if(info->dirVersion != 1)
		{
			status = Mark5DirectoryInfoErrorVersion;
		}
		else if(nConnect < info->nScan/2-1)
		{
			status = Mark5DirectoryInfoErrorConnection;
		}
		else if(nBad > info->nScan/2)
		{
			status = Mark5DirectoryInfoErrorGranularity;
		}
		else
		{
			/* Mark5C seems to fit */
			return Mark5DirectoryInfoSuccess;
		}
	}
	for(int d = 0; d < 8; ++d)
	{
		if(dirSize == LegacyScanDirectorySize + DiskInfoSizes[d])
		{
			const struct Mark5LegacyDirectory *legacyDir;

			/* Likely a Legacy dir type */
			legacyDir = (const struct Mark5LegacyDirectory *)dirData;
			info->dirClass = Mark5DirClassLegacy;
			info->dirVersion = d;
			info->nScan = legacyDir->nscans;
			info->signature = calculateMark5DirSignature(dirData, LegacyScanDirectorySize);

			/* now start the plausibility checking */
			if(info->nScan < 0 || info->nScan > 1024)
			{
				status = Mark5DirectoryInfoErrorNScan;
			}
			else
			{
				int nConnect = 0;
				int nBad = 0;

				if(info->nScan > 1)
				{
					for(int i = 1; i < info->nScan; ++i)
					{
						if(legacyDir->start[i-1] + legacyDir->length[i-1] == legacyDir->start[i])
						{
							++nConnect;
						}
						if(legacyDir->start[i] % 4 || legacyDir->length[i] % 4)
						{
							++nBad;
						}
					}
				}
				if(nConnect < info->nScan/2-1)
				{
					status = Mark5DirectoryInfoErrorConnection;
				}
				else if(nBad > info->nScan/2)
				{
					status = Mark5DirectoryInfoErrorGranularity;
				}
				else
				{
					/* Legacy seems to fit */
					return Mark5DirectoryInfoSuccess;
				}
			}
		}
		if(dirSize == NeoLegacyScanDirectorySize + DiskInfoSizes[d])
		{
			const struct Mark5NeoLegacyDirectory *neoDir;

			/* Likely a NeoLegacy dir type */
			neoDir = (const struct Mark5NeoLegacyDirectory *)dirData;
			info->dirClass = Mark5DirClassNeoLegacy;
			info->dirVersion = d;
			info->nScan = neoDir->nscans;
			info->signature = calculateMark5DirSignature(dirData, NeoLegacyScanDirectorySize);

			/* now start the plausibility checking */
			if(info->nScan < 0 || info->nScan > 65536)
			{
				status = Mark5DirectoryInfoErrorNScan;
			}
			else
			{
				int nConnect = 0;
				int nBad = 0;

				if(info->nScan > 1)
				{
					for(int i = 1; i < info->nScan; ++i)
					{
						if(neoDir->start[i-1] + neoDir->length[i-1] == neoDir->start[i])
						{
							++nConnect;
						}
						if(neoDir->start[i] % 4 || neoDir->length[i] % 4)
						{
							++nBad;
						}
					}
				}
				if(nConnect < info->nScan/2-1)
				{
					status = Mark5DirectoryInfoErrorConnection;
				}
				else if(nBad > info->nScan/2)
				{
					status = Mark5DirectoryInfoErrorGranularity;
				}
				else
				{
					/* NeoLegacy seems to fit */
					return Mark5DirectoryInfoSuccess;
				}
			}
		}
	}

	return status;
}

void fprintMark5DirectoryInfo(FILE *out, const struct Mark5DirectoryInfo *info)
{
	const char jive5abSubformats[8][20] = 
	{
		"8DisksSDK8",
		"16DisksSDK8",
		"8DisksSDK8BankB",
		"16DisksSDK8BankB",
		"8DisksSDK9",
		"16DisksSDK9",
		"8DisksSDK9BankB",
		"16DisksSDK9BankB",
	};

	switch(info->dirClass)
	{
	case Mark5DirClassNone:
		fprintf(out, "No module directory exists.\n");
		break;
	case Mark5DirClassIllegal:
		fprintf(out, "The module directory is corrupt.\n");
		break;
	case Mark5DirClassLegacy:
		fprintf(out, "This module contains a Legacy directory:\n");
		fprintf(out, "  Number of scans on the directory = %d\n", info->nScan);
		fprintf(out, "  Directory version = %d subversion = %d\n", info->dirClass, info->dirVersion);
		fprintf(out, "  jive5ab format designator = Mark5A%s\n", jive5abSubformats[info->dirVersion]);
		fprintf(out, "  DiFX signature = %u\n", info->signature);
		break;
	case Mark5DirClassNeoLegacy:
		fprintf(out, "This module contains a NeoLegacy directory (capable of storing up to 65536 scans):\n");
		fprintf(out, "  Number of scans on the directory = %d\n", info->nScan);
		fprintf(out, "  Directory version = %d subversion = %d\n", info->dirClass, info->dirVersion);
		fprintf(out, "  jive5ab format designator = Mark5B%s\n", jive5abSubformats[info->dirVersion]);
		fprintf(out, "  DiFX signature = %u\n", info->signature);
		break;
	case Mark5DirClassMark5C:
		fprintf(out, "This module contains a Mark5C directory:\n");
		fprintf(out, "  Number of scans on the directory = %d\n", info->nScan);
		fprintf(out, "  Directory version = %d subversion = %d\n", info->dirClass, info->dirVersion);
		fprintf(out, "  jive5ab format designator = Mark5CLayout\n");
		fprintf(out, "  DiFX signature = %u\n", info->signature);
		break;
	}
}

