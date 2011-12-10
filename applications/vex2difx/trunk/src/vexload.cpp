/***************************************************************************
 *   Copyright (C) 2009-2011 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstring>
#include <cctype>
#include <cstdio>
#include <algorithm>
#include <unistd.h>
#include "util.h"
#include "corrparams.h"
#include "vextables.h"
#include "../vex/vex.h"
#include "../vex/vex_parse.h"

// maximum number of defined IFs
#define MAX_IF 4

class Tracks
{
public:
	vector<int> sign;
	vector<int> mag;
};

static char swapPolarization(char pol)
{
	switch(pol)
	{
	case 'R':
		return 'L';
	case 'L':
		return 'R';
	case 'X':
		return 'Y';
	case 'Y':
		return 'X';
	default:
		cerr << "Error: unknown polarization: " << pol << endl;

		exit(EXIT_FAILURE);
	}
}

static void fixOhs(string &str)
{
	unsigned int i;

	for(i = 0; i < str.length(); ++i)
	{
		if(str[i] == '-' || str[i] == '+')
		{
			break;
		}
		if(str[i] == '0')
		{
			str[i] = 'O';
		}
	}
}

static int getRecordChannel(const string &antName, const string &chanName, const map<string,Tracks> &ch2tracks, const VexFormat &F, unsigned int n)
{
	int delta, track;
	map<string,Tracks>::const_iterator it;

	if(F.format == "VLBA1_1" || F.format == "VLBN1_1" || F.format == "MKIV1_1" ||
	   F.format == "VLBA1_2" || F.format == "VLBN1_2" || F.format == "MKIV1_2" ||
	   F.format == "VLBA1_4" || F.format == "VLBN1_4" || F.format == "MKIV1_4")
	{
		it = ch2tracks.find(chanName);

		if(it == ch2tracks.end())
		{
			return -1;
		}

		const Tracks &T = it->second;
		delta = 2*(T.sign.size() + T.mag.size());
		track = T.sign[0];

		if(track < 34)
		{
			if(track % 2 == 0) 
			{
				return (track-2)/delta;
			}
			else
			{
				return (track+29)/delta;
			}
		}
		else
		{
			if(track % 2 == 0)
			{
				return (track+30)/delta;
			}
			else
			{
				return (track+61)/delta;
			}
		}
	}
	else if(F.format == "MARK5B") 
	{
		it = ch2tracks.find(chanName);

		if(it == ch2tracks.end())
		{
			return -1;
		}

		const Tracks &T = it->second;
		delta = T.sign.size() + T.mag.size();
		track = T.sign[0];

		return (track-2)/delta;
	}
	else if(F.format == "S2" || F.format == "LBASTD" || F.format == "LBAVSOP")
	{
		return n;
	}
	else if(F.format.substr(0, 4) == "VDIF" || F.format.substr(0, 14) == "INTERLACEDVDIF")
	{
		return n;
	}
	else if(F.format == "NONE")
	{
		return 0;
	}
	else
	{
		cerr << "Error: Antenna=" << antName << " format \"" << F.format << "\" is not yet supported" << endl;
		cerr << "Contact developer." << endl;

		exit(EXIT_FAILURE);
	}

	return -1;
}

int DOYtoMJD(int year, int doy)
{
	return doy-678576+365*(year-1)+(year-1)/4-(year-1)/100+(year-1)/400;
}

double vexDate(char *value)
{
	double mjd;
	int n = 0;
	
	for(int i = 0; value[i]; ++i)
	{
		if(isalpha(value[i]))
		{
			++n;
		}
	}

	if(n == 0)
	{
		// assume this is mjd
		mjd = atof(value);
		if(sscanf(value, "%lf", &mjd) != 1)
		{
			cerr << "Error: vex date is not in usable format: " << value << endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		int i;
		int start = 0;
		double years = 0.0;
		double days = 0.0;
		double hours = 0.0;
		double minutes = 0.0;
		double seconds = 0.0;
		double x;

		for(i = 0; value[i]; ++i)
		{
			if(isalpha(value[i]))
			{
				if(sscanf(value+start, "%lf", &x) != 1)
				{
					cerr << "Error: vex date is not in usable format: " << value << endl;

					exit(EXIT_FAILURE);
				}
				switch(value[i])
				{
				case 'y':
					years = x;
					break;
				case 'd':
					days = x;
					break;
				case 'h':
					hours = x;
					break;
				case 'm':
					minutes = x;
					break;
				case 's':
					seconds = x;
					break;
				default:
					cerr << "Error: vex date is not in usable format: " << value << endl;

					exit(EXIT_FAILURE);
				}

				start = i+1;
			}
		}

		if(start != i)
		{
			cerr << "Error: trailing characters in vex date: " << value << endl;

			exit(EXIT_FAILURE);
		}

		mjd = DOYtoMJD( static_cast<int>(floor(years+0.1)), static_cast<int>(floor(days+0.1)) ) + hours/24.0 + minutes/1440.0 + seconds/86400.0;
	}

	return mjd;
}

static int getAntennas(VexData *V, Vex *v, const CorrParams &params)
{
	VexAntenna *A;
	struct site_position *p;
	struct axis_type *q;
	struct dvalue *r;
	Clock_early *C;
	double mjd;
	llist *block;
	Llist *defs;
	Llist *lowls;
	void *c;
	int nWarn = 0;

	block = find_block(B_CLOCK, v);

	for(char *stn = get_station_def(v); stn; stn=get_station_def_next())
	{
		string antName(stn);
		Upper(antName);

		if(!params.useAntenna(antName))
		{
			continue;
		}
		A = V->newAntenna();
		A->name = stn;
		A->defName = stn;
		Upper(A->name);

		p = (struct site_position *)get_station_lowl(stn, T_SITE_POSITION, B_SITE, v);
		if(p == 0)
		{
			cerr << "Error: cannot find site position for antenna " << antName << " in the vex file." << endl;

			exit(EXIT_FAILURE);
		}
		fvex_double(&(p->x->value), &(p->x->units), &A->x);
		fvex_double(&(p->y->value), &(p->y->units), &A->y);
		fvex_double(&(p->z->value), &(p->z->units), &A->z);

		p = (struct site_position *)get_station_lowl(stn, T_SITE_VELOCITY, B_SITE, v);
		if(p)
		{
			fvex_double(&(p->x->value), &(p->x->units), &A->dx);
			fvex_double(&(p->y->value), &(p->y->units), &A->dy);
			fvex_double(&(p->z->value), &(p->z->units), &A->dz);
		}
		else
		{
			A->dx = A->dy = A->dz = 0.0;
		}

		q = (struct axis_type *)get_station_lowl(stn, T_AXIS_TYPE, B_ANTENNA, v);
		if(q == 0)
		{
			cerr << "Error: cannot find axis type for antenna " << antName << " in the vex file." << endl;

			exit(EXIT_FAILURE);
		}
		A->axisType = string(q->axis1) + string(q->axis2);
		if(A->axisType.compare("hadec") == 0)
		{
			A->axisType = "equa";
		}

		r = (struct dvalue *)get_station_lowl(stn, T_SITE_POSITION_EPOCH, B_SITE, v);
		if(r)
		{
			char *value;
			char *units;
			int name;
			int link;
			vex_field(T_SITE_POSITION_EPOCH, (void *)r, 1, &link, &name, &value, &units);

			A->posEpoch = vexDate(value);
		}
		else
		{
			A->posEpoch = 0.0;
		}

		r = (struct dvalue *)get_station_lowl(stn, T_AXIS_OFFSET, B_ANTENNA, v);
		if(r == 0)
		{
			cerr << "Error: cannot find axis offset for antenna " << antName << " in the vex file." << endl;

			exit(EXIT_FAILURE);
		}
		fvex_double(&(r->value), &(r->units), &A->axisOffset);

		const AntennaSetup *antennaSetup = params.getAntennaSetup(antName);
		if(antennaSetup)
		{
			if(antennaSetup->dataSource != DataSourceNone)
			{
				if(antennaSetup->dataSource == DataSourceFile ||
				   antennaSetup->dataSource == DataSourceModule)
				{
					A->basebandFiles = antennaSetup->basebandFiles;
				}
				else
				{
					A->basebandFiles.clear();
				}
				A->dataSource = antennaSetup->dataSource;
			}
		}
		const VexClock *paramClock = params.getAntennaClock(antName);
		if(paramClock)
		{
			A->clocks.push_back(*paramClock);
		}
		else 
		{
			for(c = get_station_lowl(stn, T_CLOCK_EARLY, B_CLOCK, v);
			    c;
			    c = get_station_lowl_next())
			{
				char *value;
				char *units;
				int name;
				int link;

				vex_field(T_CLOCK_EARLY, c, 1, &link, &name, &value, &units);
				if(value)
				{
					mjd = vexDate(value);
				}
				else
				{
					mjd = 0.0;
				}
				A->clocks.push_back(VexClock());
				VexClock &clock = A->clocks.back();
				clock.mjdStart = mjd;
				V->addEvent(mjd, VexEvent::CLOCK_BREAK, antName);

				vex_field(T_CLOCK_EARLY, c, 2, &link, &name, &value, &units);
				if(value && units)
				{
					fvex_double(&value, &units, &clock.offset);
				}

				vex_field(T_CLOCK_EARLY, c, 3, &link, &name, &value, &units);
				if(value)
				{
					clock.offset_epoch = vexDate(value);
					vex_field(T_CLOCK_EARLY, c, 4, &link, &name, &value, &units);
					if(value)
					{
						if(units)
						{
							fvex_double(&value, &units, &clock.rate);
						}
						else
						{
							clock.rate = atof(value);
						}
					}
				}
				clock.flipSign();
			}

			// As a last resort, look for unlinked clock blocks
#warning: "FIXME: note: the following should eventually be removed once proper linking in vex files is in place"
			if(A->clocks.empty() && block)
			{
				defs = ((struct block *)block->ptr)->items;
				if(defs)
				{
					defs = find_def(defs, stn);
				}
				if(defs)
				{
					for(lowls = find_lowl(((Def *)((Lowl *)defs->ptr)->item)->refs, T_CLOCK_EARLY);
					    lowls;
					    lowls = lowls->next)
					{
						if(((Lowl *)lowls->ptr)->statement != T_CLOCK_EARLY)
						{
							continue;
						}

						C = (Clock_early *)(((Lowl *)lowls->ptr)->item);
						if(C)
						{
							if(C->start)
							{
								mjd = vexDate(C->start);
							}
							else
							{
								mjd = 0.0;
							}
							A->clocks.push_back(VexClock());
							VexClock &clock = A->clocks.back();
							clock.mjdStart = mjd;
							V->addEvent(mjd, VexEvent::CLOCK_BREAK, antName);
							if(C->offset)
							{
								fvex_double(&(C->offset->value), &(C->offset->units), &clock.offset);
							}
							if(C->rate && C->origin) 
							{
								clock.rate = atof(C->rate->value);
								clock.offset_epoch = vexDate(C->origin);
							}
							
							// vex has the opposite sign convention, so swap
							clock.flipSign();
						}
					}
				}
			}
		}

		if(!antennaSetup || antennaSetup->dataSource == DataSourceNone)
		{
			for(c = get_station_lowl(stn, T_VSN, B_TAPELOG_OBS, v);
			    c;
			    c = get_station_lowl_next())
			{
				char *value;
				char *units;
				int name;
				int link;
				double t1 = 0.0, t2 = 0.0;

				vex_field(T_VSN, c, 2, &link, &name, &value, &units);
				if(!value)
				{
					cerr << "VSN absent for antenna " << stn << endl;
					break;
				}
				string vsn(value);
				fixOhs(vsn);

				vex_field(T_VSN, c, 3, &link, &name, &value, &units);
				if(value)
				{
					t1 = vexDate(value);
				}
				vex_field(T_VSN, c, 4, &link, &name, &value, &units);
				if(value)
				{
					t2 = vexDate(value);
				}

				if(t1 == 0.0 || t2 == 0.0)
				{
					cerr << "VSN " << vsn << "doesn't have proper time range" << endl;
					break;
				}

				VexInterval vsnTimeRange(t1+0.001/86400.0, t2);

				if(!vsnTimeRange.isCausal())
				{
					cerr << "Error: Record stop (" << t2 << ") precedes record start (" << t1 << ") for antenna " << stn << ", module " << vsn << " ." << endl;
				}
				else
				{
					V->addVSN(antName, vsn, vsnTimeRange);
					V->addEvent(vsnTimeRange.mjdStart, VexEvent::RECORD_START, antName);
					V->addEvent(vsnTimeRange.mjdStop, VexEvent::RECORD_STOP, antName);
				}
			}
		}
	}

	return nWarn;
}

static int getSources(VexData *V, Vex *v, const CorrParams &params)
{
	VexSource *S;
	char *p;
	int nWarn = 0;
	
	for(char *src = get_source_def(v); src; src=get_source_def_next())
	{
		S = V->newSource();
		S->defName = src;
		if(strlen(src) > VexSource::MAX_SRCNAME_LENGTH)
		{
			cerr << "Source name " << src << " is longer than " << 
			VexSource::MAX_SRCNAME_LENGTH << "  characters!" << endl;
			++nWarn;
		}

		for(p = (char *)get_source_lowl(src, T_SOURCE_NAME, v);
		    p != 0;
		    p = (char *)get_source_lowl_next())
		{
			S->sourceNames.push_back(string(p));
			if(strlen(p) > VexSource::MAX_SRCNAME_LENGTH)
			{
				cerr << "Source name " << src << " is longer than " <<
				VexSource::MAX_SRCNAME_LENGTH << "  characters!" << endl;
				++nWarn;
			}
		}

		p = (char *)get_source_lowl(src, T_RA, v);
		if(!p)
		{
			cerr << "Error: Cannot find right ascension for source " << src << endl;

			exit(EXIT_FAILURE);
		}
		fvex_ra(&p, &S->ra);

		p = (char *)get_source_lowl(src, T_DEC, v);
		if(!p)
		{
			cerr << "Error: Cannot find declination for source " << src << endl;

			exit(EXIT_FAILURE);
		}
		fvex_dec(&p, &S->dec);

		p = (char *)get_source_lowl(src, T_REF_COORD_FRAME, v);
		if(!p)
		{
			cerr << "Error: Cannot find ref coord frame for source " << src << endl;

			exit(EXIT_FAILURE);
		}
		if(strcmp(p, "J2000") != 0)
		{
			cerr << "Error: only J2000 ref frame is supported." << endl;

			exit(EXIT_FAILURE);
		}

		const SourceSetup *setup = params.getSourceSetup(S->defName);
		if(setup)
		{
			if(setup->pointingCentre.calCode > ' ')
			{
				S->calCode = setup->pointingCentre.calCode;
			}
		}
	}

	return nWarn;
}

static VexInterval adjustTimeRange(map<string, double> &antStart, map<string, double> &antStop, unsigned int minSubarraySize)
{
	list<double> start;
	list<double> stop;
	map<string, double>::iterator it;
	double mjdStart, mjdStop;

	if(minSubarraySize < 1)
	{
		cerr << "Developer error: adjustTimeRange: minSubarraySize = " << minSubarraySize << " is < 1" << endl;

		exit(EXIT_FAILURE);
	}

	if(antStart.size() != antStop.size())
	{
		cerr << "Developer error: adjustTimeRange: size mismatch" << endl;

		exit(EXIT_FAILURE);
	}

	if(antStart.size() < minSubarraySize)
	{
		// Return an acausal interval
		return VexInterval(1, 0);
	}

	for(it = antStart.begin(); it != antStart.end(); ++it)
	{
		start.push_back(it->second);
	}
	start.sort();
	// Now the start times are sorted chronologically

	for(it = antStop.begin(); it != antStop.end(); ++it)
	{
		stop.push_back(it->second);
	}
	stop.sort();
	// Now stop times are sorted chronologically

	// Pick off times where min subarray condition is met
	// If these are in the wrong order (i.e., no such interval exists)
	// Then these will form an acausal interval which will be caught by
	// the caller.
	for(unsigned int i = 0; i < minSubarraySize-1; ++i)
	{
		start.pop_front();
		stop.pop_back();
	}
	mjdStart = start.front();
	mjdStop = stop.back();

	// Adjust start times where needed
	for(it = antStart.begin(); it != antStart.end(); ++it)
	{
		if(it->second < mjdStart)
		{
			it->second = mjdStart;
		}
	}

	for(it = antStop.begin(); it != antStop.end(); ++it)
	{
		if(it->second > mjdStop)
		{
			it->second = mjdStop;
		}
	}

	return VexInterval(mjdStart, mjdStop);
}

static int getScans(VexData *V, Vex *v, const CorrParams &params)
{
	VexScan *S;
	char *scanId;
	void *p;
	int link, name;
	char *stn;
	string stationName;
	char *value, *units;
	double mjd;
	double startScan, stopScan;
	double startAnt, stopAnt;
	int nScanSkip = 0;
	Llist *L;
	map<string,VexInterval> stations;
	map<string,bool> recordEnable;
	int nWarn = 0;

	for(L = (Llist *)get_scan(&scanId, v);
	    L != 0;
	    L = (Llist *)get_scan_next(&scanId))
	{
		map<string, double> antStart, antStop;

		p = get_scan_start(L);
		vex_field(T_START, p, 1, &link, &name, &value, &units);
		mjd = vexDate(value);
		startScan = 1e99;
		stopScan = 0.0;
		stations.clear();
		recordEnable.clear();
		for(p = get_station_scan(L); 
		    p;
		    p = get_station_scan_next())
		{
			vex_field(T_STATION, p, 1, &link, &name, &stn, &units);
			stationName = string(stn);
			Upper(stationName);
			if(!params.useAntenna(stationName))
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

			vex_field(T_STATION, p, 7, &link, &name, &value, &units);
			recordEnable[stationName] = (atoi(value) != 0);

			stations[stationName] = VexInterval(startAnt, stopAnt);

			antStart[stationName] = startAnt;
			antStop[stationName] = stopAnt;
		}

		if(stations.size() < params.minSubarraySize)
		{
			continue;
		}

		// Adjust start and stop times so that the minimum subarray size is
		// always honored.  The return value becomes
		VexInterval timeRange = adjustTimeRange(antStart, antStop, params.minSubarraySize);

		// If the min subarray condition never occurs, then skip the scan
		if(timeRange.duration_seconds() < 0.5)
		{
			continue;
		}

		string scanDefName(scanId);
		string sourceDefName((char *)get_scan_source(L));
		string modeDefName((char *)get_scan_mode(L));

		const VexSource *src = V->getSourceByDefName(sourceDefName);
		if(src == 0)
		{
			cerr << "Vex error! Scan " << scanDefName << " references source " << sourceDefName << " which is not in the source table." << endl;

			exit(EXIT_FAILURE);
		}

		string corrSetupName = params.findSetup(scanDefName, sourceDefName, modeDefName, src->calCode, 0);

		if(corrSetupName == "" || corrSetupName == "SKIP")
		{
			continue;
		}

		if(params.getCorrSetup(corrSetupName) == 0)
		{
			cerr << "Error: Scan=" << scanDefName << " correlator setup " << corrSetupName << " not defined!" << endl;

			exit(EXIT_FAILURE);
		}

		if(params.mjdStart > stopScan || params.mjdStop < startScan)
		{
			++nScanSkip;
			continue;
		}

		if(startScan < params.mjdStart)
		{
			startScan = params.mjdStart;
		}
		if(stopScan > params.mjdStop)
		{
			stopScan = params.mjdStop;
		}

		// Make scan
		S = V->newScan();
		S->setTimeRange(timeRange);
		S->defName = scanDefName;
		S->stations = stations;
		S->recordEnable = recordEnable;
		S->modeDefName = modeDefName;
		S->sourceDefName = sourceDefName;
		S->corrSetupName = corrSetupName;
		S->mjdVex = mjd;

		// Add to event list
		V->addEvent(S->mjdStart, VexEvent::SCAN_START, scanId, scanId);
		V->addEvent(S->mjdStop,  VexEvent::SCAN_STOP,  scanId, scanId);
		for(map<string, double>::const_iterator it = antStart.begin(); it != antStart.end(); ++it)
		{
			V->addEvent(max(it->second, startScan), VexEvent::ANT_SCAN_START, it->first, scanId);
		}
		for(map<string, double>::const_iterator it = antStop.begin(); it != antStop.end(); ++it)
		{
			V->addEvent(min(it->second, stopScan), VexEvent::ANT_SCAN_STOP, it->first, scanId);
		}
	}

	if(nScanSkip > 0)
	{
		cout << "FYI: " << nScanSkip << " scans skipped because of time range selection." << endl;
	}
	
	return nWarn;
}

static int getModes(VexData *V, Vex *v, const CorrParams &params)
{
	VexMode *M;
	void *p, *p2;
    // p2 will hold pointers to the special comments attached to if_def; max of 4
    void *p2array[MAX_IF];
    int p2count;
	char *modeDefName;
	int link, name;
	char *value, *units;
	char *bbcname;
	double freq, bandwidth, sampRate;
	string format, chanName;
	int chanNum;
	int nTrack, fanout;
	int nBit;
	int dasNum;
	int subbandId, recChanId;
	bool sign;
	map<string,char> bbc2pol;
	map<string,string> bbc2ifname;
	map<string,Tracks> ch2tracks;
	int nWarn =0;
	double phaseCal;

	for(modeDefName = get_mode_def(v);
	    modeDefName;
	    modeDefName = get_mode_def_next())
	{
		// don't bother building up modes that are not used
		if(!V->usesMode(modeDefName))
		{
			continue;
		}

		M = V->newMode();
		M->defName = modeDefName;

		// get FREQ info
		for(unsigned int a = 0; a < V->nAntenna(); ++a)
		{
			const string &antName = V->getAntenna(a)->defName;
			string antName2 = V->getAntenna(a)->defName;
			const AntennaSetup *antennaSetup;
			map<string, vector<int> > pcalMap;

			Upper(antName2);
			bool swapPol = params.swapPol(antName2);
			bbc2pol.clear();
			bbc2ifname.clear();
			ch2tracks.clear();
			nTrack = 0;
			nBit = 1;
			VexSetup &setup = M->setups[V->getAntenna(a)->name];
			VexFormat &F = setup.format;
			antennaSetup = params.getAntennaSetup(antName2);
			if(antennaSetup)
			{
				if(!antennaSetup->format.empty())
				{
					cout << "Setting antenna format to " << antennaSetup->format << " for antenna " << antName << endl;
				}
				F.format = antennaSetup->format;
			}

			// Get sample rate
			p = get_all_lowl(antName.c_str(), modeDefName, T_SAMPLE_RATE, B_FREQ, v);
			if(p == 0)
			{
				continue;
			}
			vex_field(T_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
			fvex_double(&value, &units, &sampRate);

			M->sampRate = sampRate;

            // init array to all zeroes
            for( p2count = 0; p2count < MAX_IF; p2count++ )
                p2array[p2count] = 0;
			// read all comment fields into array for later processing
			// NOTE - need to do it this way as the *_lowl_* functions use static vars and we
			// we can't run this where we actually process the comments as that would mess up
			// the IF_DEF loop
			// TODO we assign the comments below to vif.comment; should check if there is a way
			// to do so here instead of going through this temp array
            p2count = 0;
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_COMMENT_TRAILING, B_IF, v);
			    p;
			    p = get_all_lowl_next())
			{
			    p2array[p2count++] = p;
            }

            // the collected comments and ifdef fields will always line up, so just run a count
            p2count = 0;
			// Derive IF map
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_IF_DEF, B_IF, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_IF_DEF, p, 1, &link, &name, &value, &units);
				VexIF &vif = setup.ifs[string(value)];

				vex_field(T_IF_DEF, p, 2, &link, &name, &value, &units);
				vif.name = value;
				
				vex_field(T_IF_DEF, p, 3, &link, &name, &value, &units);
				vif.pol = value[0];
				if(swapPol)
				{
					vif.pol = swapPolarization(vif.pol);
				}

				vex_field(T_IF_DEF, p, 4, &link, &name, &value, &units);
				fvex_double(&value, &units, &vif.ifSSLO);

				vex_field(T_IF_DEF, p, 5, &link, &name, &value, &units);
				vif.ifSideBand = value[0];

				vex_field(T_IF_DEF, p, 6, &link, &name, &value, &units);
				if(value)
				{
					fvex_double(&value, &units, &phaseCal);
				}
				else
				{
					phaseCal = 0.0;
				}
				if(fabs(phaseCal) < 1.0)
				{
					vif.phaseCalIntervalMHz = 0;
				}
				else if(fabs(phaseCal-1000000.0) < 1.0)
				{
					vif.phaseCalIntervalMHz = 1;
				}
				else if(fabs(phaseCal-5000000.0) < 1.0)
				{
					vif.phaseCalIntervalMHz = 5;
				}
				else
				{
					cerr << "Warning: Unsupported pulse cal interval of " << (phaseCal/1000000.0) << " MHz requested for antenna " << antName << "." << endl;
					++nWarn;
					vif.phaseCalIntervalMHz = static_cast<int>((phaseCal + 0.5)/1000000.0);
				}

                p2 = p2array[p2count++];
				if( !p2 )
				{
					// check if this is a VLBA antenna; these require the comments for proper
					// operation, so exit in that case
					if( strcmp(antName.c_str(), "Sc") == 0 ||
						strcmp(antName.c_str(), "Hn") == 0 ||
						strcmp(antName.c_str(), "Nl") == 0 ||
						strcmp(antName.c_str(), "Fd") == 0 ||
						strcmp(antName.c_str(), "La") == 0 ||
						strcmp(antName.c_str(), "Pt") == 0 ||
						strcmp(antName.c_str(), "Kp") == 0 ||
						strcmp(antName.c_str(), "Ov") == 0 ||
						strcmp(antName.c_str(), "Br") == 0 ||
						strcmp(antName.c_str(), "Mk") == 0 )
					{
						static bool first = true;
						if(first)
						{
							cerr << "Warning: VLBA antenna detected, but no comment for if_def; can't do switching" << endl;
							first = false;
						}
					}
				}
				// carry comment forward as it might contain information about IF
				vex_field(T_COMMENT, p2, 1, &link, &name, &value, &units);
				if(value)
				{
					vif.comment = value;
				} 
				else
				{
					vif.comment = "\0";
				}
			}

			// Get BBC to pol map for this antenna
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_BBC_ASSIGN, B_BBC, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_BBC_ASSIGN, p, 3, &link, &name, &value, &units);
				VexIF &vif = setup.ifs[string(value)];

				vex_field(T_BBC_ASSIGN, p, 1, &link, &name, &value, &units);
				bbc2pol[value] = vif.pol;
				bbc2ifname[value] = vif.name;
			}

			// Get datastream assignments and formats

			// Is it a Mark5 mode?
			if(F.format == "") // Enter here if no format has been specified in the .v2d file
			{
				p = get_all_lowl(antName.c_str(), modeDefName, T_TRACK_FRAME_FORMAT, B_TRACKS, v);
				if(p)
				{
					vex_field(T_TRACK_FRAME_FORMAT, p, 1, &link, &name, &value, &units);
					F.format = string(value);
					if(F.format == "Mark4")
					{
						F.format = "MKIV";
					}
					else if(F.format == "Mark5B")
					{
						F.format = "MARK5B";
					}
					else if(F.format == "NONE")
					{
						F.format = "NONE";
					}
				}
				else
				{
					cerr << "Unable to determine data format for antenna " << antName << endl;

					//exit(EXIT_FAILURE);
					F.format = "NONE";
				}
			}

			if(F.format == "VLBA" || F.format == "VLBN" || F.format == "MKIV" || F.format == "MARK5B")
			{
				for(p = get_all_lowl(antName.c_str(), modeDefName, T_FANOUT_DEF, B_TRACKS, v);
				    p;
				    p = get_all_lowl_next())
				{
					vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
					chanName = value;
					vex_field(T_FANOUT_DEF, p, 3, &link, &name, &value, &units);
					sign = (value[0] == 's');
					vex_field(T_FANOUT_DEF, p, 4, &link, &name, &value, &units);
					sscanf(value, "%d", &dasNum);

					for(int k = 5; k < 9; ++k)
					{
						if(vex_field(T_FANOUT_DEF, p, k, &link, &name, &value, &units) < 0)
						{
							break;
						}
						++nTrack;
						sscanf(value, "%d", &chanNum);
						chanNum += 32*(dasNum-1);
						if(sign)
						{
							ch2tracks[chanName].sign.push_back(chanNum);
						}
						else
						{
							nBit = 2;
							ch2tracks[chanName].mag.push_back(chanNum);
						}
					}
				}
				fanout = nTrack/ch2tracks.size()/nBit;
				if(F.format != "MARK5B")
				{
					switch(fanout)
					{
						case 1: 
							F.format += "1_1"; 
							break;
						case 2: 
							F.format += "1_2"; 
							break;
						case 4: 
							F.format += "1_4"; 
							break;
						default: 
							cerr << "Error: Antenna=" << antName << " fanout=" << fanout << " not legal for format " << F.format << ".  This could be a subtle problem in the vex file." << endl;

							exit(EXIT_FAILURE);
					}
				}
				F.nRecordChan = ch2tracks.size();
				F.nBit = nBit;
			}
			else if(F.format == "VDIF")
			{
				cout << "Warning: Antenna " << antName << " has incompletely defined VDIF format.  Assuming 2 bits." << endl;

				F.nBit = 2;
			}
			else if(F.format.find_first_of("VDIF") != string::npos)
			{
				F.nBit = atoi(F.format.substr(F.format.find_last_of('/') + 1).c_str());
				F.format = F.format.substr(0, F.format.find_last_of('/'));
			}

			// Is it an S2 mode?
			p = get_all_lowl(antName.c_str(), modeDefName, T_S2_RECORDING_MODE, B_TRACKS, v);
			if(p)
			{
				vex_field(T_S2_RECORDING_MODE, p, 1, &link, &name, &value, &units);
				string s2mode(value);
				if(F.format == "")
				{
					F.format = "S2";
				}

				if (s2mode != "none")
				{
					size_t f = s2mode.find_last_of("x");
					size_t g = s2mode.find_last_of("-");

					if(f == string::npos || g == string::npos || f > g)
					{
						cerr << "Error: Antenna=" << antName << " malformed S2 mode : " << string(value) << endl;
					
						exit(EXIT_FAILURE);
					}

					string tracks = s2mode.substr(f+1, g-f-1);
					string bits = s2mode.substr(g+1);

					F.nBit = atoi(bits.c_str());
					F.nRecordChan = atoi(tracks.c_str())/F.nBit; // should equal bbc2pol.size();
					//F.nRecordChan = atoi(tracks.c_str());
				} 
				else 
				{
					F.nBit = 2;
					F.nRecordChan = 0;
				}
			}

			// Get pulse cal extraction information
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_PHASE_CAL_DETECT, B_PHASE_CAL_DETECT, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_PHASE_CAL_DETECT, p, 1, &link, &name, &value, &units);
				vector<int> &Q = pcalMap[string(value)];
				
				for(int q = 2; ; ++q)
				{
					int y = vex_field(T_PHASE_CAL_DETECT, p, q, &link, &name, &value, &units);
					if(y < 0)
					{
						break;
					}
					int v = atoi(value);
					if(v == 0)
					{
						// zero value implies next value indicates state counting
						++q;
					}
					else
					{
						// Move from vex's 1-based tones to DiFX's 0-based; negative tone numbers don't change
						int difxToneId = (v > 1) ? (v - 1) : v;
						Q.push_back(difxToneId);
					}
				}
				sort(Q.begin(), Q.end());
			}

			// Get rest of Subband information
			unsigned int i = 0;
			
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_CHAN_DEF, B_FREQ, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_CHAN_DEF, p, 2, &link, &name, &value, &units);
				fvex_double(&value, &units, &freq);

				vex_field(T_CHAN_DEF, p, 3, &link, &name, &value, &units);
				char sideBand = value[0];
				
				vex_field(T_CHAN_DEF, p, 4, &link, &name, &value, &units);
				fvex_double(&value, &units, &bandwidth);

				vex_field(T_CHAN_DEF, p, 6, &link, &name, &bbcname, &units);
				subbandId = M->addSubband(freq, bandwidth, sideBand, bbc2pol[bbcname]);

				vex_field(T_CHAN_DEF, p, 7, &link, &name, &value, &units);
				string phaseCalName(value);

				vex_field(T_CHAN_DEF, p, 5, &link, &name, &value, &units);
				recChanId = getRecordChannel(antName, value, ch2tracks, F, i);
				if(recChanId >= 0)
				{
					F.channels.push_back(VexChannel());
					F.channels.back().subbandId = subbandId;
					F.channels.back().recordChan = recChanId;
					F.channels.back().ifname = bbc2ifname[bbcname];
					F.channels.back().bbcFreq = freq;
					F.channels.back().bbcBandwidth = bandwidth;
					F.channels.back().bbcSideBand = sideBand;
					if(antennaSetup && antennaSetup->toneSelection == ToneSelectionVex)
					{
						F.channels.back().tones = pcalMap[phaseCalName];
					}

					const VexIF *vif = setup.getIF(F.channels.back().ifname);

					// This is called even for vex selected tones as negative tones 
					// are turned positive and result tone order becomes sorted
					if(antennaSetup)
					{
						F.channels.back().selectTones(
							(antennaSetup->phaseCalIntervalMHz >= 0 ? antennaSetup->phaseCalIntervalMHz : vif->phaseCalIntervalMHz), 
							antennaSetup->toneSelection,
							antennaSetup->toneGuardMHz);
					}
					else
					{
						F.channels.back().selectTones(vif->phaseCalIntervalMHz, ToneSelectionVex, 0);
					}
				}

				++i;
			}

			if(i != F.nRecordChan)
			{
				if (F.nRecordChan==0)
				{
					F.nRecordChan = i;
				}
				else
				{
					cerr << "Warning: Antenna=" << antName << " nchan=" << i << " != F.nRecordChan=" << F.nRecordChan << endl;
				}
			}
		} // End of antenna loop

		for(vector<VexSubband>::iterator it = M->subbands.begin(); it != M->subbands.end(); ++it)
		{
			int overSamp = static_cast<int>(M->sampRate/(2.0*it->bandwidth) + 0.001);
			if(params.overSamp > 0)
			{
				if(params.overSamp > overSamp)
				{
					cerr << "Warning: Mode=" << M->defName << " subband=" << M->overSamp.size() << 
						": requested oversample factor " << params.overSamp << 
						" is greater than the observed oversample factor " << overSamp << endl;
					++nWarn;
				}
				overSamp = params.overSamp;
			}
			else if(overSamp > 8)
			{
				overSamp = 8;
			}

			M->overSamp.push_back(overSamp);
		}
		M->overSamp.sort();
		M->overSamp.unique();
	}

	return nWarn;
}

static int getVSN(VexData *V, Vex *v, const char *station)
{
	Vsn *p;
	llist *block;
	Llist *defs;
	Llist *lowls;
	bool quit = false;

	string antName(station);

	Upper(antName);

	block = find_block(B_TAPELOG_OBS, v);

	if(!block)
	{
		return -1;
	}

	defs = ((struct block *)block->ptr)->items;
	if(!defs)
	{
		return -2;
	}

	defs = find_def(defs, station);
	if(!defs)
	{
		return -3;
	}

	for(lowls = find_lowl(((Def *)((Lowl *)defs->ptr)->item)->refs, T_VSN);
	    lowls;
	    lowls = lowls->next)
	{
		if(((Lowl *)lowls->ptr)->statement != T_VSN)
		{
			continue;
		}
		
		p = (Vsn *)(((Lowl *)lowls->ptr)->item);
		if(!p)
		{
			return -4;
		}

		string vsn(p->label);
		fixOhs(vsn);

		VexInterval vsnTimeRange(vexDate(p->start)+0.001/86400.0, vexDate(p->stop));

		if(!vsnTimeRange.isCausal())
		{
			cerr << "Error: Record stop (" << p->stop << ") precedes record start (" << p->start << ") for antenna " << antName << ", module " << vsn << " ." << endl;
			quit = true;
		}
		else
		{
			V->addVSN(antName, vsn, vsnTimeRange);
			V->addEvent(vsnTimeRange.mjdStart, VexEvent::RECORD_START, antName);
			V->addEvent(vsnTimeRange.mjdStop, VexEvent::RECORD_STOP, antName);
		}
	}

	if(quit)
	{
		exit(EXIT_FAILURE);
	}

	return 0;
}

static int getVSNs(VexData *V, Vex *v, const CorrParams &params)
{
	int nWarn = 0;

	for(char *stn = get_station_def(v); stn; stn=get_station_def_next())
	{
		string ant(stn);
		Upper(ant);

		if(V->nVSN(ant) > 0)
		{
			// Media already populated
			continue;
		}

		if(params.useAntenna(ant))
		{
			const AntennaSetup *antennaSetup = params.getAntennaSetup(ant);

			if(antennaSetup)
			{
				// If media is provided via v2d file, don't bother
				if(antennaSetup->dataSource != DataSourceNone)
				{
					continue;
				}
			}
			getVSN(V, v, stn);
		}
	}

	return nWarn;
}

static int getEOPs(VexData *V, Vex *v, const CorrParams &params)
{
	llist *block;
	Llist *defs;
	Llist *lowls, *refs;
	int link, name;
	char *value, *units;
	void *p;
	dvalue *r;
	double tai_utc, ut1_utc, x_wobble, y_wobble;
	double interval;
	VexEOP *E;
	int N = 0;
	int nWarn = 0;

	block = find_block(B_EOP, v);

	if(block)
	{
		for(defs=((struct block *)block->ptr)->items;
	   	    defs;
	    	    defs=defs->next)
		{
			int nEop;
			int statement;
			double refEpoch;
			
			statement = ((Lowl *)defs->ptr)->statement;

			if(statement == T_COMMENT || statement == T_COMMENT_TRAILING)
			{
				continue;
			}
			if(statement != T_DEF)
			{
				break;
			}

			refs = ((Def *)((Lowl *)defs->ptr)->item)->refs;

			lowls = find_lowl(refs, T_TAI_UTC);
			r = (struct dvalue *)(((Lowl *)lowls->ptr)->item);
			fvex_double(&r->value, &r->units, &tai_utc);

			lowls = find_lowl(refs, T_EOP_REF_EPOCH);
			p = (((Lowl *)lowls->ptr)->item);
			refEpoch = vexDate((char *)p);

			lowls = find_lowl(refs, T_NUM_EOP_POINTS);
			r = (struct dvalue *)(((Lowl *)lowls->ptr)->item);
			nEop = atoi(r->value);
			N += nEop;

			lowls = find_lowl(refs, T_EOP_INTERVAL);
			r = (struct dvalue *)(((Lowl *)lowls->ptr)->item);
			fvex_double(&r->value, &r->units, &interval);

			for(int i = 0; i < nEop; ++i)
			{	
				lowls = find_lowl(refs, T_UT1_UTC);
				vex_field(T_UT1_UTC, ((Lowl *)lowls->ptr)->item, i+1, &link, &name, &value, &units);
				fvex_double(&value, &units, &ut1_utc);

				lowls = find_lowl(refs, T_X_WOBBLE);
				vex_field(T_X_WOBBLE, ((Lowl *)lowls->ptr)->item, i+1, &link, &name, &value, &units);
				fvex_double(&value, &units, &x_wobble);

				lowls = find_lowl(refs, T_Y_WOBBLE);
				vex_field(T_Y_WOBBLE, ((Lowl *)lowls->ptr)->item, i+1, &link, &name, &value, &units);
				fvex_double(&value, &units, &y_wobble);

				E = V->newEOP();
				E->mjd = refEpoch + i*interval/86400.0;
				E->tai_utc = tai_utc;
				E->ut1_utc = ut1_utc;
				E->xPole = x_wobble;
				E->yPole = y_wobble;
			}
		}
	}

	if(!params.eops.empty())
	{
		if(N > 0)
		{
			cerr << "Warning: Mixing EOP values from vex and v2d files.  Your mileage may vary!" << endl;
			++nWarn;
		}
		for(vector<VexEOP>::const_iterator e = params.eops.begin(); e != params.eops.end(); ++e)
		{
			E = V->newEOP();
			*E = *e;
			++N;
		}
	}

	return nWarn;
}

static int getExper(VexData *V, Vex *v, const CorrParams &params)
{
	llist *block;
	Llist *defs;
	Llist *lowls, *refs;
	int statement;
	void *p;
	double start=0.0, stop=0.0;
	string name;
	int nWarn = 0;

	block = find_block(B_EXPER, v);

	if(!block)
	{
		return -1;
	}

	for(defs=((struct block *)block->ptr)->items;
	    defs;
	    defs=defs->next)
	{
		statement = ((Lowl *)defs->ptr)->statement;
		if(statement == T_COMMENT || statement == T_COMMENT_TRAILING)
		{
			continue;
		}
		if(statement != T_DEF)
		{
			break;
		}

		refs = ((Def *)((Lowl *)defs->ptr)->item)->refs;

		lowls = find_lowl(refs, T_EXPER_NAME);

		name = (char *)(((Lowl *)lowls->ptr)->item);

		lowls = find_lowl(refs, T_EXPER_NOMINAL_START);
		if(lowls)
		{
			p = (((Lowl *)lowls->ptr)->item);
			start = vexDate((char *)p);
		}

		lowls = find_lowl(refs, T_EXPER_NOMINAL_STOP);
		if(lowls)
		{
			p = (((Lowl *)lowls->ptr)->item);
			stop = vexDate((char *)p);
		}
	}

	Upper(name);

	V->setExper(name, VexInterval(start, stop));

	return nWarn;
}

// Note: this is approximate, assumes all polarizations matched and no IFs being selected out
static void calculateScanSizes(VexData *V, const CorrParams &P)
{
	int nScan, nSubband, nBaseline;
	const VexScan *scan;
	const VexMode *mode;
	const CorrSetup *setup;

	nScan = V->nScan();

	for(int s = 0; s < nScan; ++s)
	{
		scan = V->getScan(s);
		mode = V->getModeByDefName(scan->modeDefName);
		setup = P.getCorrSetup(scan->corrSetupName);
		nSubband = mode->subbands.size();
		nBaseline = scan->stations.size()*(scan->stations.size()+1)/2;
		V->setScanSize(s, scan->duration()*86400*nBaseline*nSubband*setup->bytesPerSecPerBLPerBand());
	}
}

VexData *loadVexFile(const CorrParams &P, int * numWarnings)
{
	VexData *V;
	Vex *v;
	int r;
	int nWarn = 0;

	r = vex_open(P.vexFile.c_str(), &v);
	if(r != 0)
	{
		return 0;
	}

	V = new VexData();

	V->setDirectory(P.vexFile.substr(0, P.vexFile.find_last_of('/')));

	nWarn += getAntennas(V, v, P);
	nWarn += getSources(V, v, P);
	nWarn += getScans(V, v, P);
	nWarn += getModes(V, v, P);
	nWarn += getVSNs(V, v, P);
	nWarn += getEOPs(V, v, P);
	nWarn += getExper(V, v, P);
	*numWarnings = *numWarnings + nWarn;

	calculateScanSizes(V, P);
	V->findLeapSeconds();
	V->addBreaks(P.manualBreaks);
	V->sortEvents();

	return V;
}
