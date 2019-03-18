#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <difxio/difx_input.h>
#include "vmf.h"

const char VMFNameMap[][2][32] =
{
	{ "Brewster", "BR-VLBA" },
	{ "Fort Davis", "FD-VLBA" },
	{ "Hancock", "HN-VLBA" },
	{ "Kitt Peak", "KP-VLBA" },
	{ "Los Alamos",  "LA-VLBA" },
	{ "Mauna Kea", "MK-VLBA" },
	{ "North Liberty", "NL-VLBA" },
	{ "Owens Valley", "OV-VLBA" },
	{ "Pie Town", "PT-VLBA" },
	{ "Saint Croix", "SC-VLBA" },

	{ "", "" }	/* list terminator */
};



static int monlen[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

int mjd2dayno	/* convert MJD to day number of year */
    (
    long mjd,		/* input Modified Julian Date */
    int *pDayNo		/* returned day number (1-366) */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * This function converts the given Modified Julian Date to a day number.  
 * If the date does not fall between 0001JAN01 AD (MJD = -678,575) and 
 * 10000JAN00 AD (MJD = 2,973,483) ERROR is returned.
 */
{
    int year, month, day;
    int status;
    int i;

    /* convert MJD to year, month, and day */
    if ((status = mjd2date (mjd, &year, &month, &day)) != 0)
	return status;

    /* add days previous to current month */
    for (i = 1; i < month; i++)
	day += monlen[i - 1];

    /* add a day if leap year and past February
       (algorithm does NOT work for 2100 but we won't be here) */
    if (year % 4 == 0 && month > 2)
	day++;

    /* set returned day number and return */
    *pDayNo = day;
    return 0;
}

/*******************************************************************************
*/
int mjd2date		/* convert MJD to date */
    (
    long mjd,		/* input Modified Julian Date */
    int *pYear,		/* pointer to returned year (1-10000) */
    int *pMonth,	/* pointer to returned month (1-12) */
    int *pDay		/* pointer to returned day (1-31) */
    )
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

    int icen4;
    int icen;
    int iyr4;
    int iyr;
    int imon;
    int iday;

    /* check input range and calc days since jan 1 1 AD (Gregorian Calendar) */
    if (mjd > 2973483)
	return -1;
    if ((mjd += AD - 1) < 0)
        return -1;

    /* calc number of fours of Gregorian centuries */
    icen4 = mjd / 146097;

    /* calc number of centuries since last 
	fours of Gregorian centuries (e.g. since 1600 or 2000) */
    mjd -= (icen4 * 146097);
    if ((icen = mjd / 36524) == 4)
        icen = 3; 

    /* calc number of quadrenia(four years) since jan 1, 1901 */
    mjd -= (icen * 36524);
    iyr4 = mjd / 1461;

    /* calc number of years since last quadrenia */
    mjd -= (iyr4 * 1461);
    if ((iyr = mjd / 365) == 4)
        iyr = 3;

    /* calc number of months, days since jan 1 of current year */
    iday = mjd - iyr * 365;
    for (imon = 0; iday >= 0; imon++)
	iday = iday - monlen[imon] - ((iyr == 3 && imon == 1) ? 1 : 0);
    imon--;		/* restore imon, iday to last loop value */
    iday = iday + monlen[imon] + ((iyr == 3 && imon == 1) ? 1 : 0);

    /* calc return values */
    *pYear = icen4 * 400 + icen * 100 + iyr4 * 4 + iyr + 1;
    *pMonth = imon + 1;
    *pDay = iday + 1;

    return 0;
}



int loadVMFData(VMFData *data, int maxRows, int mjdStart, int nDay, int verbose)
{
	const int MaxLength = 256;
	const int MaxURLLength = 512;
	const int MaxCommandLength = 768;
	int mjd;
	int row = 0;
	const char *vmfDir;
	struct stat st;

	vmfDir = getenv("DIFX_VMF_DATA");

	for(mjd = mjdStart; mjd < mjdStart + nDay; ++mjd)
	{
		int year, month, day, doy;
		char fileName[MaxLength];
		char filePath[MaxLength];
		int n;
		FILE *in;

		/* FIXME calculate year, doy */
		mjd2date(mjd, &year, &month, &day);
		mjd2dayno(mjd, &doy);

		n = snprintf(fileName, MaxLength, "%d%03d.vmf3_r", year, doy);
		if(n >= MaxLength)
		{
			fprintf(stderr, "Developer error: loadVMFData: fileName too short: %d < %d\n", MaxLength, n+1);

			return -1;
		}

		n = snprintf(filePath, MaxLength, "%s/%s", vmfDir, fileName);
		if(n >= MaxLength)
		{
			fprintf(stderr, "Developer error: loadVMFData: filePath too short: %d < %d\n", MaxLength, n+1);

			return -2;
		}
		
		n = stat(filePath, &st);
		if(n < 0)
		{
			char url[MaxURLLength];
			char cmd[MaxCommandLength];
			/* get the file */

			n = snprintf(url, MaxCommandLength, "http://vmf.geo.tuwien.ac.at/trop_products/VLBI/VMF3/VMF3_OP/daily/%d/%s", year, fileName);
			if(n >= MaxURLLength)
			{
				fprintf(stderr, "Developer error: loadVMFData: url too short: %d < %d\n", MaxURLLength, n+1);

				return -3;
			}
			n = snprintf(cmd, MaxCommandLength, "wget %s -O %s\n", url, filePath);
			if(n >= MaxURLLength)
			{
				fprintf(stderr, "Developer error: loadVMFData: cmd too short: %d < %d\n", MaxCommandLength, n+1);

				return -4;
			}
			
			if(verbose > 0)
			{
				printf("Fetching file for mjd=%d (year=%d doy=%d): %s\n", mjd, year, doy, cmd);
			}
			system(cmd);

			n = stat(filePath, &st);
			if(n < 0)
			{
				fprintf(stderr, "File fetch failed.  Command was: %s\n", cmd);

				return -5;
			}
		}

		in = fopen(filePath, "r");
		if(in == 0)
		{
			fprintf(stderr, "Error: cannot open local file: %s\n", filePath);

			return -6;
		}

		if(verbose > 1)
		{
			printf("Reading from %s...", filePath);
			fflush(stdout);
		}

		for(;;)
		{
			const int MaxLineLength=256;
			char line[MaxLineLength];
			char *rv;

			rv = fgets(line, MaxLineLength-1, in);
			if(!rv)
			{
				break;
			}

			if(row >= maxRows)
			{
				fprintf(stderr, "Error: too much data found.  Internal buffer too small: maxRows=%d\n", maxRows);

				return -7;
			}

			n = sscanf(line, "%s%lf%lf%lf%lf%lf%lf%lf%lf", 
				data[row].antennaName,
				&data[row].mjd,
				&data[row].a_hydrostatic,
				&data[row].a_wet,
				&data[row].zd_hydrostatic,
				&data[row].zd_wet,
				&data[row].pressure,
				&data[row].temperature,
				&data[row].pressure_wv);
			if(n == 9)
			{
				++row;
			}
			else
			{
				if(verbose > 1)
				{
					printf("Warning: VMF line not understood: %s\n", line);
				}
			}
		}

		if(verbose > 1)
		{
			printf(" %d rows total.\n", row);
		}

		fclose(in);
	}

	return row;
}

int selectVMDData(const char *antennaName, VMFData **antennaData, int maxOut, VMFData *vmfData, int nData)
{
	int nOut = 0;
	int i;

	for(i = 0; i < nData; ++i)
	{
		if(nOut >= maxOut)
		{
			break;
		}
		if(strcmp(antennaName, vmfData[i].antennaName) == 0)
		{
			antennaData[i] = vmfData + i;
		}
	}

	return nOut;
}

static int processScan(int scanId, DifxInput *D, int verbose)
{
	int antId;

	DifxScan *scan;
	DifxJob *job;
	int polyOrder;
	int polyInterval;	/* (sec) length of valid polynomial */

	scan = D->scan + scanId;
	job = D->job;

	polyOrder = job->polyOrder;
	polyInterval = job->polyInterval;

	for(antId = 0; antId < scan->nAntenna; ++antId)
	{
		int k;

		for(k = 0; k < scan->nPhaseCentres + 1; ++k)
		{
			int i;

			for(i = 0; i < scan->nPoly; ++i)
			{
				/* 1. Remove atmosphere from Delay; due to sign convention, ADD the wet and dry to delay */
				

				/* 2. Recompute atmosphere from scratch */

				/* 3. Add atmosphere back; due to sign convention, SUBTRACT the wet and dry from delay */
			}
		}
	}
	
}

int calculateVMFDifxInput(DifxInput *D, const VMFData *vmfData, int vmfRows, int verbose)
{
	int status = 0;
	int scanId;

	if(!D)
	{
		fprintf(stderr, "Error: calculateVMFDifxInput: D==0\n");

		return -1;
	}

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		if(D->scan[scanId].im == 0)
		{
			fprintf(stderr, "Warning: no existing interferometer model for scan %d\n", scanId);
		}
		else
		{
			processScan(scanId, D, verbose);
		}
	}

	return status;
}
