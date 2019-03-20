#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <difxio/difx_input.h>
#include <difxio/antenna_db.h>
#include "vmf.h"
#include "poly.h"
#include "timeutils.h"
#include "vmf3.h"


/* FIXME: move these to antenna_db.c/h in new column "ivsName" */
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
	{ "Pie Town", "PIETOWN" },
	{ "Saint Croix", "SC-VLBA" },

	{ "", "" }	/* list terminator */
};


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

		year = month = day = doy = 0;

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

int selectVMFData(const char *antennaName, VMFData **antennaData, int maxOut, VMFData *vmfData, int nData)
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
			antennaData[nOut] = vmfData + i;
			++nOut;
		}
	}

	return nOut;
}

/* returns number of records updated */
static int processScan(int scanId, DifxInput *D, VMFData *vmfData, int vmfRows, int verbose)
{
	const int MaxVMFRows = 32;

	int antId;

	DifxScan *scan;
	DifxJob *job;
	int polyOrder;
	int polyInterval;	/* (sec) length of valid polynomial */
	int nRecord = 0;

	scan = D->scan + scanId;
	job = D->job;

	polyOrder = job->polyOrder;
	polyInterval = job->polyInterval;

	for(antId = 0; antId < scan->nAntenna; ++antId)
	{
		const AntennaDBEntry *antInfo;
		const DifxAntenna *da;
		int k;
		int nVMF;
		const char *vmfName;
		VMFData *antVMFData[MaxVMFRows];

		if(scan->im[antId] == 0)
		{
			/* No model for this antenna in this scan */

			continue;
		}

		da = D->antenna + antId;

		antInfo = antennaDBGetByXYZ(da->X, da->Y, da->Z);
		if(antInfo == 0)
		{
			printf("Warning: Antenna %d (%s) is not found in antenna_db database; not including...\n", antId, da->name);

			continue;
		}

		vmfName = 0;
		for(k = 0; VMFNameMap[k][0][0]; ++k)
		{
			if(strcasecmp(VMFNameMap[k][0], antInfo->name) == 0)
			{
				vmfName = VMFNameMap[k][1];
				break;
			}
		}
		if(vmfName == 0)
		{
			printf("Warning: Antenna %d (%s == %s) is not found in VMF lookup table; not including...\n", antId, da->name, antInfo->name);

			continue;
		}

		printf("Processing antId=%d = %s = %s = %s\n", antId, da->name, antInfo->name, vmfName);

		nVMF = selectVMFData(vmfName, antVMFData, MaxVMFRows, vmfData, vmfRows);
		printf("%d VMF data sets for %s\n", nVMF, vmfName);

		for(k = 0; k < scan->nPhaseCentres + 1; ++k)
		{
			int i;

			for(i = 0; i < scan->nPoly; ++i)
			{
				DifxPolyModel *im;
				int t;

				im = &(scan->im[antId][k][i]);

				/* 1. Remove atmosphere from Delay; due to sign convention, ADD the wet and dry to delay */
				for(t = 0; t <= polyOrder; ++t)
				{
					im->delay[t] += (im->dry[t] + im->wet[t]);
				}

				/* 2. Recompute atmosphere from scratch into time series; initially treat polys as time series */
				for(t = 0; t <= polyOrder; ++t)
				{
					double el;		/* [radians] elevation */
					double deltat;		/* [sec] evaluation time for polynomials */
					double mjd;		/* [day] actual time of evaluation */
					double mfh, mfw;	/* hydrostatic and wet mapping function */

					double ah, aw, lat, lon;

					im->dry[t] = im->wet[t] = 0.0;

					deltat = t*polyInterval/(double)polyOrder;
					mjd = im->mjd + (im->sec + deltat)/86400.0;

					el = evaluatePoly(im->elgeom, polyOrder+1, deltat);

					vmf3(&mfh, &mfw, ah, aw, mjd, lat, lon, el);
				}

				/* 3. Turn time series into polynomial */
				computePoly(im->dry, im->order+1, polyInterval/(double)(im->order));

				/* 4. Add atmosphere back; due to sign convention, SUBTRACT the wet and dry from delay */
				for(t = 0; t <= polyOrder; ++t)
				{
					im->delay[t] -= (im->dry[t] + im->wet[t]);
				}

				++nRecord;
			}
		}
	}
	
	return nRecord;
}

int calculateVMFDifxInput(DifxInput *D, VMFData *vmfData, int vmfRows, int verbose)
{
	int nRecord = 0;
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
			int n;

			n = processScan(scanId, D, vmfData, vmfRows, verbose);
			if(n > 0)
			{
				nRecord += n;
			}
		}
	}

	return nRecord;
}
