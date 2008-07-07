#include <stdio.h>
#include <math.h>
#include "MATHCNST.H"
#include "difxcalc.h"
#include "poly.h"

#define MAX_EOPS 5

/* FIXME check mount type azel/altz */

static struct timeval TIMEOUT = {10, 0};

struct CalcResults
{
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
	for(i = 0; i < 64; i++)
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
		for(i = 0; i < MAX_EOPS; i++)
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
		return -1;
	}

	printf("difxCalcInit complete\n");

	return 0;
}

static int calcSpacecraftPosition(const DifxInput *D,
	struct getCALC_arg *request, int spacecraftId)
{
#if 0
FIXME -- cleanup and move to difxio!
	int nRow;
	const SpacecraftPos *pos;
	long double t0, t1, tMod, t, deltat;
	long double xPoly[4], yPoly[4], zPoly[4];
	int r, r0, r1;
	long double X, Y, Z, dX, dY, dZ;
	long double R2, D;
	double muRA, muDec;
	
	nRow = c_params->sc_rows[spacecraftId];
	pos = &(c_params->sc_pos[spacecraftId][0]);
	
	tMod = request->date + request->time;
	
	/* first find interpolation points */
	t0 = 0.0;
	t1 = pos[0].mjd + pos[0].fracDay;
	for(r = 1; r < nRow; r++)
	{
		t0 = t1;
		t1 = pos[r].mjd + pos[r].fracDay;
		if(t0 <= tMod && tMod <= t1)
		{
			break;
		}
	}
	if(r == nRow)
	{
		return -1;
	}

	/* calculate polynomial for X, Y, Z */
	r0 = r-1;
	r1 = r;
	deltat = t1 - t0;
	t = (tMod - t0)/deltat; /* time, fraction of interval, between 0 and 1 */

	xPoly[0] = pos[r0].X;
	xPoly[1] = pos[r0].dX*deltat;
	xPoly[2] = -3.0L*(pos[r0].X-pos[r1].X) - (2.0L*pos[r0].dX+pos[r1].dX)*deltat;
	xPoly[3] =  2.0L*(pos[r0].X-pos[r1].X) + (    pos[r0].dX+pos[r1].dX)*deltat;
	yPoly[0] = pos[r0].Y;
	yPoly[1] = pos[r0].dY*deltat;
	yPoly[2] = -3.0L*(pos[r0].Y-pos[r1].Y) - (2.0L*pos[r0].dY+pos[r1].dY)*deltat;
	yPoly[3] =  2.0L*(pos[r0].Y-pos[r1].Y) + (    pos[r0].dY+pos[r1].dY)*deltat;
	zPoly[0] = pos[r0].Z;
	zPoly[1] = pos[r0].dZ*deltat;
	zPoly[2] = -3.0L*(pos[r0].Z-pos[r1].Z) - (2.0L*pos[r0].dZ+pos[r1].dZ)*deltat;
	zPoly[3] =  2.0L*(pos[r0].Z-pos[r1].Z) + (    pos[r0].dZ+pos[r1].dZ)*deltat;

	evalPoly(xPoly, t, &X, &dX);
	evalPoly(yPoly, t, &Y, &dY);
	evalPoly(zPoly, t, &Z, &dZ);

	/* convert to m/day */
	dX /= deltat;
	dY /= deltat;
	dZ /= deltat;

	/* Hack here -- this makes much smoother output! */
	dX = pos[r0].dX + t*(pos[r1].dX - pos[r0].dX);
	dY = pos[r0].dY + t*(pos[r1].dY - pos[r0].dY);
	dZ = pos[r0].dZ + t*(pos[r1].dZ - pos[r0].dZ);

	D = sqrtl(X*X + Y*Y + Z*Z);
	R2 = X*X + Y*Y;

	/* proper motion in radians/day */
	muRA = (X*dY - Y*dX)/R2;
	muDec = (R2*dZ - X*Z*dX - Y*Z*dY)/(D*D*sqrtl(R2));
	
	/* convert to arcsec/yr */
	muRA *= (180.0*3600.0/M_PI)*365.24;
	muDec *= (180.0*3600.0/M_PI)*365.24;

	request->ra  =  atan2(Y, X);
	request->dec =  atan2(Z, sqrtl(R2));
	request->dra  = muRA;
	request->ddec = muDec;
	request->parallax = 3.08568025e16/D;
        request->depoch = tMod;
#endif
	return 0;
}

static int callCalc(struct getCALC_arg *request, struct CalcResults *results,
	const CalcParams *p)
{
	double ra, dec;
	int i;
	enum clnt_stat clnt_stat;

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

	for(i = 0; i < results->nRes; i++)
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
		fprintf(stderr, "clnt_call failed!\n");
		return -1;
	}
	if(results->res[0].error)
	{
		fprintf(stderr,"An error occured: %s\n",
			results->res[0].getCALC_res_u.errmsg);
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
			fprintf(stderr, "clnt_call failed!\n");
			return -1;
		}
		if(results->res[1].error)
		{
			fprintf(stderr,"An error occured: %s\n",
				results->res[1].getCALC_res_u.errmsg);
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
			fprintf(stderr, "clnt_call failed!\n");
			return -1;
		}
		if(results->res[2].error)
		{
			fprintf(stderr,"An error occured: %s\n",
				results->res[2].getCALC_res_u.errmsg);
			return -2;
		}
		
		request->ra  = ra;
		request->dec = dec;
	}
	
	return 0;
}

int extractCalcResults(DifxPolyModel *im, int index, 
	struct CalcResults *results)
{
	struct getCALC_res *res0, *res1, *res2;

	res0 = &results->res[0];
	res1 = &results->res[1];
	res2 = &results->res[2];

	
	im->delay[index] = -res0->getCALC_res_u.record.delay[0]*1e6;
	im->dry[index] = -res0->getCALC_res_u.record.dry_atmos[0]*1e6;
	im->wet[index] = -res0->getCALC_res_u.record.wet_atmos[0]*1e6;
	if(results->nRes == 3)
	{
		im->u[index] = (C_LIGHT/results->delta)*
			(res0->getCALC_res_u.record.delay[0] - 
			 res1->getCALC_res_u.record.delay[0]);
		im->v[index] = (C_LIGHT/results->delta)*
			(res2->getCALC_res_u.record.delay[0] - 
			 res0->getCALC_res_u.record.delay[0]);
		im->w[index] = C_LIGHT*res0->getCALC_res_u.record.delay[0];
	}
	else
	{
		im->u[index] = res0->getCALC_res_u.record.UV[0];
		im->v[index] = res0->getCALC_res_u.record.UV[1];
		im->w[index] = res0->getCALC_res_u.record.UV[2];
	}

	return 0;
}

void computePolyModel(DifxPolyModel *im, double deltaT)
{
	computePoly(im->delay, im->order+1, deltaT);
	computePoly(im->dry,   im->order+1, deltaT);
	computePoly(im->wet,   im->order+1, deltaT);
	computePoly(im->u,     im->order+1, deltaT);
	computePoly(im->v,     im->order+1, deltaT);
	computePoly(im->w,     im->order+1, deltaT);
}

/* antenna here is a pointer to a particular antenna object */
static int antennaCalc(int scanId, int antId, const DifxInput *D, CalcParams *p)
{
	struct getCALC_arg *request;
	struct CalcResults results;
	int i, j, s, v;
	int mjd;
	int jobStart, inc;
	double sec, subInc;
	double lastsec = -1000;
	DifxPolyModel *im;
	DifxModel *model;
	DifxScan *scan;
	DifxSource *source;
	DifxAntenna *antenna;
	DifxJob *job;
	int nInt, deltat;
	int spacecraftId = -1;
	int sourceId;

	job = D->job;
	antenna = D->antenna + antId;
	scan = D->scan + scanId;
	im = scan->im[antId];
	model = scan->model[antId];
	nInt = scan->nPoly;
	mjd = (int)(job->mjdStart);
	jobStart = (int)(86400.0*(job->mjdStart - mjd) + 0.5);
	inc = (int)(job->modelInc + 0.5);
	sourceId = scan->sourceId;
	source = D->source + sourceId;
	subInc = p->increment/(double)(p->order);
	request = &(p->request);
	spacecraftId = source->spacecraftId;

	request->station_b = antenna->name;
	request->b_x = antenna->X;
	request->b_y = antenna->Y;
	request->b_z = antenna->Z;
	request->axis_type_b = antenna->mount;
	request->axis_off_b = antenna->offset[0];

	request->source = source->name;
	if(spacecraftId < 0)
	{
		request->ra       = source->ra;
		request->dec      = source->dec;
		request->dra      = 0.0;
		request->ddec     = 0.0;
		request->parallax = 0.0;
		request->depoch   = 0.0;
	}

	for(i = 0; i < nInt; i++)
	{
		request->date = im[i].mjd;
		sec = im[i].sec;
		for(j = 0; j <= p->order; j++)
		{
			request->time = sec/86400.0;

			/* call calc if we didn't just for this time */
			if(fabs(lastsec - sec) > 1.0e-6)
			{
				if(spacecraftId >= 0)
				{
					calcSpacecraftPosition(D, 
						request, spacecraftId);
				}
				v = callCalc(request, &results, p);
				if(v < 0)
				{
					printf("Bad: callCalc = %d\n", v);
					/* oops -- an error! */
					return -1;
				}
			}
			/* use result to populate tabulated values */

			extractCalcResults(&im[i], j, &results);

			lastsec = sec;
			sec += subInc;
			if(sec >= 86400.0)
			{
				sec -= 86400.0;
				request->date += 1;
			}
		}
		computePolyModel(&im[i], subInc);
	}

	/* use polynomial to calculate uvw and delay for difx */
	for(i = -1; i <= scan->nPoint+1; i++)
	{
		/* seconds since beginning of midnight of the day in 
		 * which this job started */
		s = jobStart + (scan->startPoint + i)*inc;

		/* get polynomial index */
		j = s/p->increment - (jobStart + scan->startPoint)/p->increment;
		
		if(j < 0)
		{
			j = 0;
		}
		if(j >= nInt)
		{
			j = nInt - 1;
		}
		
		deltat = s - im[j].sec - (im[j].mjd - mjd)*86400;

		model[i].u    = evaluatePoly(im[j].u,          p->order+1, deltat);
		model[i].v    = evaluatePoly(im[j].v,          p->order+1, deltat);
		model[i].w    = evaluatePoly(im[j].w,          p->order+1, deltat);
		model[i].t    = evaluatePoly(im[j].delay,      p->order+1, deltat);
		model[i].dry  = evaluatePoly(im[j].dry,        p->order+1, deltat);
		model[i].wet  = evaluatePoly(im[j].wet,        p->order+1, deltat);
		continue;
		model[i].dt   = evaluatePolyDeriv(im[j].delay, p->order+1, deltat);
		model[i].ddry = evaluatePolyDeriv(im[j].dry,   p->order+1, deltat);
		model[i].dwet = evaluatePolyDeriv(im[j].wet,   p->order+1, deltat);
	}

	return 0;
}

static int scanCalc(int scanId, const DifxInput *D, CalcParams *p)
{
	DifxPolyModel *im;
	int antId;
	int mjd, sec;
	int sec1, sec2;
	int jobStart;	/* seconds since last midnight */
	int inc;
	int int1, int2;	/* polynomial intervals */
	int nInt;
	int i;
	DifxJob *job;
	DifxAntenna *antenna;
	DifxScan *scan;

	job = D->job;
	antenna = D->antenna;
	scan = D->scan + scanId;

	scan->nAntenna = D->nAntenna;
	scan->model = newDifxModelArray(scan->nAntenna, scan->nPoint);

	scan->im = (DifxPolyModel **)calloc(scan->nAntenna, 
		sizeof(DifxPolyModel *));

	mjd = (int)(job->mjdStart);
	jobStart = (int)(86400.0*(job->mjdStart - mjd) + 0.5);

	inc = (int)(job->modelInc + 0.5);

	sec1 = jobStart + scan->startPoint*inc; 
	sec2 = sec1 + scan->nPoint*inc;
	int1 = sec1/p->increment;
	int2 = (sec2 + p->increment - 1)/p->increment;
	nInt = int2 - int1;
	scan->nPoly = nInt;

	for(antId = 0; antId < scan->nAntenna; antId++)
	{
		scan->im[antId] = (DifxPolyModel *)calloc(nInt, 
			sizeof(DifxPolyModel));
		im = scan->im[antId];
		sec = int1*p->increment;
		mjd = (int)(job->mjdStart);
		
		for(i = 0; i < nInt; i++)
		{
			/* set up the intervals to calc polys over */
			im[i].mjd = mjd;
			im[i].sec = sec;
			im[i].order = p->order;
			im[i].validDuration = p->increment;
			sec += p->increment;
			if(sec >= 86400)
			{
				sec -= 86400;
				mjd++;
			}
		}

		/* call calc to derive delay, etc... polys */
		antennaCalc(scanId, antId, D, p);
	}

	return 0;
}

int difxCalc(DifxInput *D, CalcParams *p)
{
	int scanId;
	DifxScan *scan;

	if(!D)
	{
		return -1;
	}

	for(scanId = 0; scanId < D->nScan; scanId++)
	{
		scan = D->scan + scanId;
		if(scan->im)
		{
			fprintf(stderr, "Error! scan %d: model already "
				"exists\n", scanId);
			return -2;
		}
		scanCalc(scanId, D, p);
	}

	return 0;
}
