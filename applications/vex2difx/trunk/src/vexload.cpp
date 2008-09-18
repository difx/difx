#include "corrparams.h"
#include "vextables.h"
#include "../vex/vex.h"
#include "../vex/vex_parse.h"
#include <cstring>

extern "C" {
int fvex_double(char **field, char **units, double *d);
int fvex_ra(char **field, double *ra);
int fvex_dec(char **field, double *dec);
int fvex_date(char **field, int *iarray, double *seconds);

}

int toMJD(int year, int doy)
{
	return doy-678576+365*(year-1)+(year-1)/4-(year-1)/100+(year-1)/400;
}

int getAntennas(VexData *V, Vex *v, const CorrParams& params)
{
	VexAntenna *A;
	struct site_position *p;
	struct axis_type *q;
	struct dvalue *r;

	for(char *stn = get_station_def(v); stn; stn=get_station_def_next())
	{
		if(!params.useAntenna(stn))
		{
			continue;
		}
		A = V->newAntenna();
		A->name = stn;

		p = (struct site_position *)get_station_lowl(stn, 
			T_SITE_POSITION, B_SITE, v);
		fvex_double(&(p->x->value), &(p->x->units), &A->x);
		fvex_double(&(p->y->value), &(p->y->units), &A->y);
		fvex_double(&(p->z->value), &(p->z->units), &A->z);

		q = (struct axis_type *)get_station_lowl(stn, 
			T_AXIS_TYPE, B_ANTENNA, v);
		A->axisType = string(q->axis1) + string(q->axis2);

		r = (struct dvalue *)get_station_lowl(stn, 
			T_AXIS_OFFSET, B_ANTENNA, v);
		fvex_double(&(r->value), &(r->units), &A->axisOffset);
	}

	return 0;
}

int getSources(VexData *V, Vex *v, const CorrParams& params)
{
	VexSource *S;
	char *p;
	
	for(char *src = get_source_def(v); src; src=get_source_def_next())
	{
		S = V->newSource();
		S->name = src;

		for(p = (char *)get_source_lowl(src, T_SOURCE_NAME, v);
		    p != 0;
		    p = (char *)get_source_lowl_next())
		{
			S->sourceNames.push_back(string(p));
		}

		p = (char *)get_source_lowl(src, T_RA, v);
		fvex_ra(&p, &S->ra);

		p = (char *)get_source_lowl(src, T_DEC, v);
		fvex_dec(&p, &S->dec);
	}

	return 0;
}

int getScans(VexData *V, Vex *v, const CorrParams& params)
{
	VexScan *S;
	char *scanId;
	void *p;
	int link, name;
	char *stn;
	char *value, *units;
	double mjd;
	double startScan, stopScan;
	double startAnt, stopAnt;
	int ints[4];
	double seconds;
	Llist *L;
	map<string,VexInterval> stations;

	for(L = (Llist *)get_scan(&scanId, v);
	    L != 0;
	    L = (Llist *)get_scan_next(&scanId))
	{
		p = get_scan_start(L);
		vex_field(T_START, p, 1, &link, &name, &value, &units);
		fvex_date(&value, ints, &seconds);
		mjd = toMJD(ints[0], ints[1]);
		mjd += ints[2]/24.0 + ints[3]/1440.0 + seconds/86400.0;
		startScan = 1e99;
		stopScan = 0.0;
		stations.clear();
		for(p = get_station_scan(L); 
		    p;
		    p = get_station_scan_next())
		{
			vex_field(T_STATION, p, 1, &link, &name, &stn, &units);
			if(!params.useAntenna(stn))
			{
				continue;
			}

			vex_field(T_STATION, p, 2, &link, &name, &value, &units);
			fvex_double(&value, &units, &startAnt);
			startAnt = mjd + startAnt/86400.0;	// mjd of antenna start
			if(startAnt < startScan)
			{
				startScan = startAnt;
			}

			vex_field(T_STATION, p, 3, &link, &name, &value, &units);
			fvex_double(&value, &units, &stopAnt);
			stopAnt = mjd + stopAnt/86400.0;	// mjd of antenna stop
			if(stopAnt > stopScan)
			{
				stopScan = stopAnt;
			}

			stations[stn] = VexInterval(startAnt, stopAnt);
		}

		if(stations.size() < params.minSubarraySize)
		{
			continue;
		}

		// Make scan
		S = V->newScan();
		S->timeRange = VexInterval(startScan, stopScan);
		S->name = scanId;
		S->stations = stations;
		S->modeName = string((char *)get_scan_mode(L));
		S->sourceName = string((char *)get_scan_source(L));
	}
	

	return 0;
}

VexData *loadVexFile(string vexfilename, const CorrParams& params)
{
	VexData *V;
	Vex *v;
	int r;

	r = vex_open(vexfilename.c_str(), &v);
	if(r != 0)
	{
		return 0;
	}

	V = new VexData();

	getAntennas(V, v, params);
	getSources(V, v, params);
	getScans(V, v, params);

	return V;
}
