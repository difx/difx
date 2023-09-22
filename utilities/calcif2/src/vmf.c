/***************************************************************************
 *   Copyright (C) 2019 by Walter Brisken                                  *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <difxio/difx_input.h>
#include <difxio/antenna_db.h>
#include "vmf.h"
#include "poly.h"
#include "timeutils.h"
#include "vmf3.h"


VMFInterpolator *newVMFInterpolator(VMFData **antennaData, int nRow)
{
	double *mjd, *data;
	VMFInterpolator *vi;
	int i;

	mjd = (double *)malloc(nRow*sizeof(double));
	data = (double *)malloc(nRow*sizeof(double));
	vi = (VMFInterpolator *)malloc(sizeof(VMFInterpolator));
	strcpy(vi->antennaName, antennaData[0]->antennaName);

	for(i = 0; i < nRow; ++i)
	{
		mjd[i] = antennaData[i]->mjd;
	}

	vi->acc_ah = gsl_interp_accel_alloc();
	vi->spline_ah = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->a_hydrostatic;
	}
	gsl_spline_init(vi->spline_ah, mjd, data, nRow);

	vi->acc_aw = gsl_interp_accel_alloc();
	vi->spline_aw = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->a_wet;
	}
	gsl_spline_init(vi->spline_aw, mjd, data, nRow);

	vi->acc_zh = gsl_interp_accel_alloc();
	vi->spline_zh = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->zd_hydrostatic;
	}
	gsl_spline_init(vi->spline_zh, mjd, data, nRow);

	vi->acc_zw = gsl_interp_accel_alloc();
	vi->spline_zw = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->zd_wet;
	}
	gsl_spline_init(vi->spline_zw, mjd, data, nRow);

	vi->acc_p = gsl_interp_accel_alloc();
	vi->spline_p = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->pressure;
	}
	gsl_spline_init(vi->spline_p, mjd, data, nRow);

	vi->acc_t = gsl_interp_accel_alloc();
	vi->spline_t = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->temperature;
	}
	gsl_spline_init(vi->spline_t, mjd, data, nRow);

	vi->acc_pw = gsl_interp_accel_alloc();
	vi->spline_pw = gsl_spline_alloc(gsl_interp_cspline, nRow);
	for(i = 0; i < nRow; ++i)
	{
		data[i] = antennaData[i]->pressure_wv;
	}
	gsl_spline_init(vi->spline_pw, mjd, data, nRow);

	free(mjd);
	free(data);

	return vi;
}

void deleteVMFInterpolator(VMFInterpolator *vi)
{
	if(vi)
	{
		gsl_spline_free(vi->spline_ah);
		gsl_interp_accel_free(vi->acc_ah);

		gsl_spline_free(vi->spline_aw);
		gsl_interp_accel_free(vi->acc_aw);

		gsl_spline_free(vi->spline_zh);
		gsl_interp_accel_free(vi->acc_zh);

		gsl_spline_free(vi->spline_zw);
		gsl_interp_accel_free(vi->acc_zw);

		gsl_spline_free(vi->spline_p);
		gsl_interp_accel_free(vi->acc_p);

		gsl_spline_free(vi->spline_t);
		gsl_interp_accel_free(vi->acc_t);

		gsl_spline_free(vi->spline_pw);
		gsl_interp_accel_free(vi->acc_pw);

		free(vi);
	}
}

void interpolateVMFData(VMFData *vmf, VMFInterpolator *vi, double mjd)
{
	strcpy(vmf->antennaName, vi->antennaName);
	vmf->mjd = mjd;
	vmf->a_hydrostatic  = gsl_spline_eval(vi->spline_ah, mjd, vi->acc_ah);
	vmf->a_wet          = gsl_spline_eval(vi->spline_aw, mjd, vi->acc_aw);
	vmf->zd_hydrostatic = gsl_spline_eval(vi->spline_zh, mjd, vi->acc_zh);
	vmf->zd_wet         = gsl_spline_eval(vi->spline_zw, mjd, vi->acc_zw);
	vmf->pressure       = gsl_spline_eval(vi->spline_p,  mjd, vi->acc_p );
	vmf->temperature    = gsl_spline_eval(vi->spline_t,  mjd, vi->acc_t );
	vmf->pressure_wv    = gsl_spline_eval(vi->spline_pw, mjd, vi->acc_pw);
}

void printVMFData(const VMFData *vmf)
{
	printf("VMF: %s %10.4f %f %f %f %f %f %f %f\n", 
		vmf->antennaName,
		vmf->mjd,
		vmf->a_hydrostatic,
		vmf->a_wet,
		vmf->zd_hydrostatic,
		vmf->zd_wet,
		vmf->pressure,
		vmf->temperature,
		vmf->pressure_wv);
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

		year = month = day = doy = 0;

		/* FIXME calculate year, doy */
		mjd2date(mjd, &year, &month, &day);
		mjd2dayno(mjd, &doy);

		n = snprintf(fileName, MaxLength, "%d%03d.vmf3_r", year, doy);
		if(n >= MaxLength)
		{
			fprintf(stderr, "Developer error: loadVMFData(): fileName too short: %d < %d\n", MaxLength, n+1);

			return -1;
		}

		n = snprintf(filePath, MaxLength, "%s/%s", vmfDir, fileName);
		if(n >= MaxLength)
		{
			fprintf(stderr, "Developer error: loadVMFData(): filePath too short: %d < %d\n", MaxLength, n+1);

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
				fprintf(stderr, "Developer error: loadVMFData(): url too short: %d < %d\n", MaxURLLength, n+1);

				return -3;
			}
			n = snprintf(cmd, MaxCommandLength, "wget %s -O %s\n", url, filePath);
			if(n >= MaxURLLength)
			{
				fprintf(stderr, "Developer error: loadVMFData(): cmd too short: %d < %d\n", MaxCommandLength, n+1);

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
				fprintf(stderr, "Error: loadVMFData(): file fetch failed.  Command was: %s\n", cmd);

				return -5;
			}

			if(st.st_size < 1000)
			{
				fprintf(stderr, "Error: loadVMFData(): downloaded file %s is too small (%Ld bytes; should be about 80kB); deleting it.\n", filePath, (long long)(st.st_size));

				n = snprintf(cmd, MaxCommandLength, "rm -f %s", filePath);
				if(n >= MaxURLLength)
				{
					fprintf(stderr, "Developer error: loadVMFData(): url too short: %d < %d\n", MaxURLLength, n+1);

					return -8;
				}
				
				system(cmd);

				return -9;
			}
		}

		in = fopen(filePath, "r");
		if(in == 0)
		{
			fprintf(stderr, "Error: loadVMFData(): cannot open local file: %s\n", filePath);

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
static int processScan(int scanId, DifxInput *D, VMFData *vmfData, int vmfRows, WXData *wxData, int verbose)
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
		double lat, lon;	/* [rad] */
		double alt;		/* [m] height above geoid */
		VMFData *antVMFData[MaxVMFRows];
		VMFInterpolator *vi;

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

		if(antInfo->ivsName[0] == 0)
		{
			printf("Warning: Antenna %d (%s=%s) does not have an IVS name; not including...\n", antId, da->name, antInfo->name);
			
		}

		if(verbose > 1)
		{
			printf("Processing antId=%d = %s = %s = %s\n", antId, da->name, antInfo->name, antInfo->ivsName);
		}

		nVMF = selectVMFData(antInfo->ivsName, antVMFData, MaxVMFRows, vmfData, vmfRows);
		if(verbose > 1)
		{
			printf("%d VMF data sets for %s\n", nVMF, antInfo->ivsName);
		}

		vi = newVMFInterpolator(antVMFData, nVMF);

		ecef2lla(&lat, &lon, &alt, antInfo->x, antInfo->y, antInfo->z);

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
					const double m_us = 299.792458;	/* [m/us] speed of light */
					double el;		/* [degrees] elevation */
					double deltat;		/* [sec] evaluation time for polynomials */
					double mjd;		/* [day] actual time of evaluation */
					double mfh, mfw;	/* hydrostatic and wet mapping function */
					double zdh;		/* [us] hydrostatic zenith delay */
					VMFData vmf;

					deltat = t*polyInterval/(double)polyOrder;
					mjd = im->mjd + (im->sec + deltat)/86400.0;

					el = evaluatePoly(im->elgeom, polyOrder+1, deltat);

					interpolateVMFData(&vmf, vi, mjd);

					vmf3(&mfh, &mfw, vmf.a_hydrostatic, vmf.a_wet, mjd, lat, lon, el*M_PI/180.0);

					zdh = vmf.zd_hydrostatic;
					if(wxData)
					{
						int ok;
						WXDataRow wx;

						ok = interpolateWXData(&wx, wxData + antId, mjd);
						if(ok == 0)
						{
							zdh = zdh * wx.pressure/vmf.pressure;
						}
					}

					im->dry[t] = zdh*mfh/m_us;
					im->wet[t] = vmf.zd_wet*mfw/m_us;

					if(verbose > 2)
					{
						printf("t=%d el=%fdeg mfh=%f mfw=%f  dry=%fus wet=%fus  ", t, el, mfh, mfw, im->dry[t], im->wet[t]);
						printVMFData(&vmf);
					}

				}

				/* 3. Turn time series into polynomial */
				computePoly(im->dry, im->order+1, polyInterval/(double)(im->order));
				computePoly(im->wet, im->order+1, polyInterval/(double)(im->order));

				/* 4. Add atmosphere back; due to sign convention, SUBTRACT the wet and dry from delay */
				for(t = 0; t <= polyOrder; ++t)
				{
					im->delay[t] -= (im->dry[t] + im->wet[t]);
				}

				++nRecord;
			}
		}

		deleteVMFInterpolator(vi);
	}
	
	return nRecord;
}

int calculateVMFDifxInput(DifxInput *D, VMFData *vmfData, int vmfRows, WXData *wxData, int verbose)
{
	int nRecord = 0;
	int scanId;

	if(!D)
	{
		fprintf(stderr, "Error: calculateVMFDifxInput: D==0\n");

		return -1;
	}

	strncat(D->job->calcServer, "+vmf", DIFXIO_HOSTNAME_LENGTH);
	D->job->calcServer[DIFXIO_HOSTNAME_LENGTH-1] = 0;

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		if(D->scan[scanId].im == 0)
		{
			fprintf(stderr, "Warning: no existing interferometer model for scan %d\n", scanId);
		}
		else
		{
			int n;

			n = processScan(scanId, D, vmfData, vmfRows, wxData, verbose);
			if(n > 0)
			{
				nRecord += n;
			}
		}
	}

	return nRecord;
}
