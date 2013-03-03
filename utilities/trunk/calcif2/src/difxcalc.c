/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken & Adam Deller               *
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
#include "MATHCNST.H"
#include "difxcalc.h"
#include "externaldelay.h"
#include "poly.h"

#define MAX_EOPS 5

static struct timeval TIMEOUT = {10, 0};

/* see DifxPolyModel in difxio:difx_input.h */
struct modelTemp
{
	double delay[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double dry[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double wet[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double az[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double elcorr[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double elgeom[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double parangle[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double u[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double v[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
	double w[MAX_MODEL_ORDER*MAX_MODEL_OVERSAMP+1];
};

struct CalcResults
{
	enum AberCorr aberCorr;
	int nRes;	/* 3 if UVW to be calculated via tau derivatives */
	double delta;
	struct getCALC_res res[3];
};

int difxCalcInit(const DifxInput *D, CalcParams *p)
{
	struct getCALC_arg *request;
	int i;

	request = &(p->request);

	memset(request, 0, sizeof(struct getCALC_arg));

	request->request_id = 150;
	for(i = 0; i < 64; ++i)
	{
		request->kflags[i] = -1;
	}
	request->ref_frame = 0;
	
	request->pressure_a = 0.0;
	request->pressure_b = 0.0;

	request->station_a = "EC";
	request->a_x = 0.0;
	request->a_y = 0.0;
	request->a_z = 0.0;
	request->axis_type_a = "altz";
	request->axis_off_a = 0.0;

	if(D->nEOP >= MAX_EOPS)
	{
		for(i = 0; i < MAX_EOPS; ++i)
		{
			request->EOP_time[i] = D->eop[i].mjd;
			request->tai_utc[i]  = D->eop[i].tai_utc;
			request->ut1_utc[i]  = D->eop[i].ut1_utc;
			request->xpole[i]    = D->eop[i].xPole;
			request->ypole[i]    = D->eop[i].yPole;
		}
	}
	else
	{
		fprintf(stderr, "Not enough eop values present (%d < %d)\n", D->nEOP, MAX_EOPS);

		return -1;
	}

	/* check that eops bracket the observation */
	if(D->eop[MAX_EOPS-1].mjd < D->mjdStart ||
	   D->eop[0].mjd          > D->mjdStop)
	{
		fprintf(stderr, "EOPs don't bracket the observation.\n");

		return -2;
	}

	return 0;
}

static int calcSpacecraftPosition(const DifxInput *D, struct getCALC_arg *request, int spacecraftId)
{
	DifxSpacecraft *sc;
	sixVector pos;
	int r;
	long double r2, d;
	
	sc = D->spacecraft + spacecraftId;

	r = evaluateDifxSpacecraft(sc, request->date, request->time, &pos);
	if(r < 0)
	{
		return -1;
	}

	r2 = pos.X*pos.X + pos.Y*pos.Y;
	d = sqrtl(r2 + pos.Z*pos.Z);

	request->dra = 0.0;
	request->ddec = 0.0;
	request->ra  =  atan2(pos.Y, pos.X);
	request->dec =  atan2(pos.Z, sqrtl(r2));
	request->parallax = 3.08568025e16/d;
        request->depoch = pos.mjd + pos.fracDay;

	return 0;
}

static int callCalc(struct getCALC_arg *request, struct CalcResults *results, const CalcParams *p)
{
	double ra, dec;
	int i;
	enum clnt_stat clnt_stat;

	results->aberCorr = p->aberCorr;
	if(p->delta > 0.0)
	{
		results->nRes = 3;
		results->delta = p->delta;
	}
	else
	{
		results->nRes = 1;
		results->delta = 0;
	}

	for(i = 0; i < results->nRes; ++i)
	{
		memset(results->res+i, 0, sizeof(struct getCALC_res));
	}
	clnt_stat = clnt_call(p->clnt, GETCALC,
		(xdrproc_t)xdr_getCALC_arg, 
		(caddr_t)request,
		(xdrproc_t)xdr_getCALC_res, 
		(caddr_t)(results->res),
		TIMEOUT);
	if(clnt_stat != RPC_SUCCESS)
	{
		fprintf(stderr, "Error: callCalc: clnt_call failed!\n");

		return -1;
	}
	if(results->res[0].error)
	{
		fprintf(stderr, "Error: callCalc: %s\n", results->res[0].getCALC_res_u.errmsg);

		return -2;
	}

	if(results->nRes == 3)
	{
		ra  = request->ra;
		dec = request->dec;

		/* calculate delay offset in RA */
		request->ra  = ra - p->delta/cos(dec);
		request->dec = dec;
		clnt_stat = clnt_call(p->clnt, GETCALC,
			(xdrproc_t)xdr_getCALC_arg, 
			(caddr_t)request,
			(xdrproc_t)xdr_getCALC_res, 
			(caddr_t)(results->res + 1),
			TIMEOUT);
		if(clnt_stat != RPC_SUCCESS)
		{
			fprintf(stderr, "Error: callCalc (2): clnt_call failed!\n");

			return -1;
		}
		if(results->res[1].error)
		{
			fprintf(stderr,"Error: callCalc: %s\n", results->res[1].getCALC_res_u.errmsg);

			return -2;
		}

		/* calculate delay offset in Dec */
		request->ra  = ra;
		request->dec = dec + p->delta;
		clnt_stat = clnt_call(p->clnt, GETCALC,
			(xdrproc_t)xdr_getCALC_arg, 
			(caddr_t)request,
			(xdrproc_t)xdr_getCALC_res, 
			(caddr_t)(results->res + 2),
			TIMEOUT);
		if(clnt_stat != RPC_SUCCESS)
		{
			fprintf(stderr, "Error: callCalc (3): clnt_call failed!\n");

			return -1;
		}
		if(results->res[2].error)
		{
			fprintf(stderr,"Error: callCalc: %s\n", results->res[2].getCALC_res_u.errmsg);

			return -2;
		}
		
		request->ra  = ra;
		request->dec = dec;
	}
	
	return 0;
}

static int unwindAzimuth(double *az, int order)
{
	int i;
	double azmax, azmin;

	azmax = azmin = az[0];

	for(i = 1; i < order; ++i)
	{
		if(az[i] > azmax)
		{
			azmax = az[i];
		}
		if(az[i] < azmin)
		{
			azmin = az[i];
		}
	}

	if(fabs(azmax-azmin) > 180.0)
	{
		for(i = 0; i < order; ++i)
		{
			if(az[i] < 180.0)
			{
				az[i] += 360.0;
			}
		}
	}

	return 0;
}

static int extractCalcResults(struct modelTemp *mod, int index, struct CalcResults *results)
{
	struct getCALC_res *res0, *res1, *res2;
	double d, dx, dy;
	int rv=0;

	res0 = &results->res[0];
	res1 = &results->res[1];
	res2 = &results->res[2];

	mod->delay[index] = -res0->getCALC_res_u.record.delay[0]*1.0e6;
	mod->dry[index] = res0->getCALC_res_u.record.dry_atmos[0]*1.0e6;
	mod->wet[index] = res0->getCALC_res_u.record.wet_atmos[0]*1.0e6;
	mod->az[index] = res0->getCALC_res_u.record.az[1]*180.0/M_PI;
	mod->elgeom[index] = res0->getCALC_res_u.record.el[1]*180.0/M_PI;

/* FIXME: add elcorr, elgeom and parangle */
	mod->elcorr[index] = 0.0;
	mod->parangle[index] = 0.0;

	if(results->nRes == 3)
	{
		/* compute u, v, w by taking angular derivative of geometric delay */
		if(results->aberCorr == AberCorrExact)
		{
			d =  res0->getCALC_res_u.record.delay[0] -
			     res0->getCALC_res_u.record.wet_atmos[0] -
			     res0->getCALC_res_u.record.dry_atmos[0];
			dx = res1->getCALC_res_u.record.delay[0] -
			     res1->getCALC_res_u.record.wet_atmos[0] -
			     res1->getCALC_res_u.record.dry_atmos[0];
			dy = res2->getCALC_res_u.record.delay[0] -
			     res2->getCALC_res_u.record.wet_atmos[0] -
			     res2->getCALC_res_u.record.dry_atmos[0];
		}
		else if(results->aberCorr == AberCorrNoAtmos)
		{
			d =  res0->getCALC_res_u.record.delay[0];
			dx = res1->getCALC_res_u.record.delay[0];
			dy = res2->getCALC_res_u.record.delay[0];
		}
		else
		{
			fprintf(stderr, "Developer error: nRes is 3 but aberCorr is not AberCorrExact or AberCorrNoAtmos!\n");

			d = dx = dy = 0.0;

			rv = 1;
		}

		mod->u[index] = (C_LIGHT/results->delta)*(d-dx);
		mod->v[index] = (C_LIGHT/results->delta)*(dy-d);
		mod->w[index] = C_LIGHT*d;
	
		if(isnan(d) || isinf(d) || isnan(dx) || isinf(dx) || isnan(dy) || isinf(dy))
		{
			rv = 1;
		}
	}
	else
	{
		mod->u[index] = res0->getCALC_res_u.record.UV[0];
		mod->v[index] = res0->getCALC_res_u.record.UV[1];
		mod->w[index] = res0->getCALC_res_u.record.UV[2];
		
		if(isnan(mod->delay[index]) || isinf(mod->delay[index]))
		{
			rv = 1;
		}
	}

	return rv;
}

static double computePolyModel(DifxPolyModel *im, const struct modelTemp *mod, double deltaT, int oversamp, int interpolationType)
{
	double r;

	/* FIXME: add interpolation mode */
	r = computePoly2(im->delay,mod->delay,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->dry,      mod->dry,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->wet,      mod->wet,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->az,       mod->az,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->elcorr,   mod->elcorr,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->elgeom,   mod->elgeom,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->parangle, mod->parangle,	im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->u,        mod->u,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->v,        mod->v,		im->order+1, oversamp, deltaT, interpolationType);
	computePoly2(im->w,        mod->w,		im->order+1, oversamp, deltaT, interpolationType);

	return r;	// The RMS interpolation error
}

/* antenna here is a pointer to a particular antenna object */
static int antennaCalc(int scanId, int antId, const DifxInput *D, const char *prefix, CalcParams *p, int phasecentre, int verbose)
{
	struct getCALC_arg *request;
	struct CalcResults results;
	struct modelTemp mod;
	int i, j, v;
	double sec, subInc;
	double lastsec = -1000;
	DifxPolyModel **im;
	DifxScan *scan;
	DifxSource *source;
	DifxAntenna *antenna;
	int nInt;
	int spacecraftId = -1;
	int sourceId;
	int nError = 0;
	char mount[MAX_ANTENNA_MOUNT_NAME_LENGTH];
	char externalDelayFilename[DIFXIO_FILENAME_LENGTH];
	ExternalDelay *ed;

	antenna = D->antenna + antId;
	scan = D->scan + scanId;
	im = scan->im[antId];
	nInt = scan->nPoly;
        if(phasecentre == 0) // this is the pointing centre
	{
		sourceId = scan->pointingCentreSrc;
	}
	else
	{
		sourceId = scan->phsCentreSrcs[phasecentre-1];
	}
	source = D->source + sourceId;
	subInc = p->increment/(double)(p->order*p->oversamp);
	request = &(p->request);
	spacecraftId = source->spacecraftId;

	/* this is needed to get around xdr_string not coping well with const strings */
	strncpy(mount, antennaMountTypeNames[antenna->mount], MAX_ANTENNA_MOUNT_NAME_LENGTH-1);
	mount[MAX_ANTENNA_MOUNT_NAME_LENGTH-1] = 0;

	request->station_b = antenna->name;
	request->b_x = antenna->X;
	request->b_y = antenna->Y;
	request->b_z = antenna->Z;
	request->axis_type_b = mount;
	request->axis_off_b = antenna->offset[0];

	request->source = source->name;
	if(spacecraftId < 0)
	{
	        request->ra       = source->ra;
	        request->dec      = source->dec;
	        request->dra      = source->pmRA;	
	        request->ddec     = source->pmDec;
	        request->parallax = source->parallax;
	        request->depoch   = source->pmEpoch;
	}

	snprintf(externalDelayFilename, DIFXIO_FILENAME_LENGTH, "%s.%s.%s.delay", prefix, antenna->name, source->name);
	ed = newExternalDelay(externalDelayFilename);
	if(!ed)
	{
		snprintf(externalDelayFilename, DIFXIO_FILENAME_LENGTH, "%s_%s.delay", antenna->name, source->name);
		ed = newExternalDelay(externalDelayFilename);
		if(ed)
		{
			fprintf(stderr, "Warning: using %s to drive delays.  This filename designator is obsolete.\n", externalDelayFilename);
		}
	}

	for(i = 0; i < nInt; ++i)
	{
		double e;

		request->date = im[phasecentre][i].mjd;
		sec = im[phasecentre][i].sec;
		for(j = 0; j <= p->order*p->oversamp; ++j)
		{
			request->time = sec/86400.0;

			/* call calc if we didn't just for this time */
			if(fabs(lastsec - sec) > 1.0e-6)
			{
				if(spacecraftId >= 0)
				{
					v = calcSpacecraftPosition(D, request, spacecraftId);
					if(v < 0)
					{
						printf("Error: antennaCalc: Spacecraft %d table out of time range\n", spacecraftId);

						if(ed)
						{
							deleteExternalDelay(ed);
						}

						return -1;
					}
				}
				v = callCalc(request, &results, p);
				if(v < 0)
				{
					printf("Error: antennaCalc: callCalc = %d\n", v);

					if(ed)
					{
						deleteExternalDelay(ed);
					}
					
					return -1;
				}
			}

			/* use result to populate tabulated values */
			nError += extractCalcResults(&mod, j, &results);

			/* override delay and atmosphere values */
			if(ed)
			{
				int v;
				double exDelay, exDry, exWet;

				v = getExternalDelay(ed, request->date+request->time, &exDelay, &exDry, &exWet);
				if(v < 0)
				{
					fprintf(stderr, "Error: request for external delay from stn %s source %s at time %14.8f failed with error code %d\n", antenna->name, source->name, request->date+request->time, v);

					exit(0);
				}

				mod.delay[j] = -(exDelay+exDry+exWet)*1.0e6;
				mod.dry[j] = exDry*1.0e6;
				mod.wet[j] = exWet*1.0e6;
			}

			lastsec = sec;
			sec += subInc;
			if(sec >= 86400.0)
			{
				sec -= 86400.0;
				request->date += 1;
			}
		}
		unwindAzimuth(mod.az, p->order*p->oversamp);
		e = computePolyModel(&im[phasecentre][i], &mod, subInc, p->oversamp, p->interpol);
		if(verbose > 0 && p->oversamp > 1)
		{
			printf("Scan %d antId %d poly %d : max delay interpolation error = %e us\n", scanId, antId, i, e);
		}
	}

	if(ed)
	{
		deleteExternalDelay(ed);
	}

	if(nError > 0)
	{
		fprintf(stderr, "Error: Antenna %s had %d invalid delays\n", D->antenna[antId].name, nError);
	}

	return nError;
}

static int scanCalc(int scanId, const DifxInput *D, const char *prefix, CalcParams *p, int isLast, int verbose)
{
	DifxPolyModel *im;
	int antId;
	int mjd, sec;
	int sec1, sec2;
	int jobStart;	/* seconds since last midnight */
	int int1, int2;	/* polynomial intervals */
	int nInt;
	int i, v, k;
	DifxJob *job;
	DifxScan *scan;

	job = D->job;
	scan = D->scan + scanId;

	scan->nAntenna = D->nAntenna;

	scan->im = (DifxPolyModel ***)calloc(scan->nAntenna, sizeof(DifxPolyModel **));

	mjd = (int)(job->mjdStart);
	jobStart = (int)(86400.0*(job->mjdStart - mjd) + 0.5);

	sec1 = jobStart + scan->startSeconds; 
	sec2 = sec1 + scan->durSeconds;
	int1 = sec1/p->increment;
	int2 = (sec2 + p->increment - 1)/p->increment;
	nInt = int2 - int1;
	if(isLast || sec2 % p->increment == 0)
	{
		++nInt;
	}
	scan->nPoly = nInt;

	for(antId = 0; antId < scan->nAntenna; ++antId)
	{
		scan->im[antId] = (DifxPolyModel **)calloc(scan->nPhaseCentres+1, sizeof(DifxPolyModel*));
		for(k = 0; k < scan->nPhaseCentres + 1; ++k)
		{
			scan->im[antId][k] = (DifxPolyModel *)calloc(nInt, sizeof(DifxPolyModel));
			im = scan->im[antId][k];
			sec = int1*p->increment;
			mjd = (int)(job->mjdStart);
		
			for(i = 0; i < nInt; ++i)
			{
				if(sec >= 86400)
				{
					sec -= 86400;
					++mjd;
				}
	
				/* set up the intervals to calc polys over */
				im[i].mjd = mjd;
				im[i].sec = sec;
				im[i].order = p->order;
				im[i].validDuration = p->increment;
				sec += p->increment;
			}

			/* call calc to derive delay, etc... polys */
			v = antennaCalc(scanId, antId, D, prefix, p, k, verbose);
			if(v < 0)
			{
				return -1;
			}
		}
	}

	return 0;
}

int difxCalc(DifxInput *D, CalcParams *p, const char *prefix, int verbose)
{
	int scanId;
	int v;
	int isLast;
	DifxScan *scan;
	DifxJob *job;

	if(!D)
	{
		return -1;
	}

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		scan = D->scan + scanId;
		job = D->job;

		job->polyOrder = p->order;
		job->polyInterval = p->increment;
		job->aberCorr = p->aberCorr;
		if(scan->im)
		{
			fprintf(stderr, "Error: difxCalc: scan %d: model already exists\n", scanId);

			return -2;
		}
		if(scanId == D->nScan - 1)
		{
			isLast = 1;
		}
		else
		{
			isLast = 0;
		}
		v = scanCalc(scanId, D, prefix, p, isLast, verbose);
		if(v < 0)
		{
			return -1;
		}
	}

	return 0;
}
