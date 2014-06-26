/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken                             *
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

#include <fstream>
#include <cstring>
#include <cstdlib>

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

using namespace std;

class Tracks
{
public:
	std::vector<int> sign;
	std::vector<int> mag;
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
		std::cerr << "Error: unknown polarization: " << pol << std::endl;

		exit(EXIT_FAILURE);
	}
}

static void fixOhs(std::string &str)
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

static int getRecordChannel(const std::string &antName, const std::string &chanName, const std::map<std::string,Tracks> &ch2tracks, const VexSetup &setup, unsigned int n)
{
	if(setup.formatName == "VLBA1_1" || setup.formatName == "VLBN1_1" || setup.formatName == "MKIV1_1" ||
	   setup.formatName == "VLBA1_2" || setup.formatName == "VLBN1_2" || setup.formatName == "MKIV1_2" ||
	   setup.formatName == "VLBA1_4" || setup.formatName == "VLBN1_4" || setup.formatName == "MKIV1_4")
	{
		int delta, track;
		std::map<std::string,Tracks>::const_iterator it = ch2tracks.find(chanName);

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
	else if(setup.formatName == "MARK5B"||setup.formatName == "KVN5B") 
	{
		int delta, track;
		std::map<std::string,Tracks>::const_iterator it = ch2tracks.find(chanName);

		if(it == ch2tracks.end())
		{
			return -1;
		}

		const Tracks &T = it->second;
		delta = T.sign.size() + T.mag.size();
		track = T.sign[0];

		return (track-2)/delta;
	}
	else if(setup.formatName == "LBAVSOP" || setup.formatName == "LBASTD") 
	{
		int delta, track;
		std::map<std::string,Tracks>::const_iterator it = ch2tracks.find(chanName);

		if(it == ch2tracks.end())
		{
			return -1;
		}

		const Tracks &T = it->second;
		delta = T.sign.size() + T.mag.size();
		track = T.sign[0];

                return track/delta;
	}
	else if(setup.formatName == "S2" )
	{
		return n;
	}
	else if(setup.formatName.substr(0, 4) == "VDIF" || setup.formatName.substr(0, 5) == "VDIFL" || setup.formatName.substr(0, 14) == "INTERLACEDVDIF")
	{
		return n;
	}
	else if(setup.formatName == "NONE")
	{
		return 0;
	}
	else
	{
		std::cerr << "Error: Antenna=" << antName << " format \"" << setup.formatName << "\" is not yet supported" << std::endl;
		std::cerr << "Contact developer." << std::endl;

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
			std::cerr << "Error: vex date is not in usable format: " << value << std::endl;

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
					std::cerr << "Error: vex date is not in usable format: " << value << std::endl;

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
					std::cerr << "Error: vex date is not in usable format: " << value << std::endl;

					exit(EXIT_FAILURE);
				}

				start = i+1;
			}
		}

		if(start != i)
		{
			std::cerr << "Error: trailing characters in vex date: " << value << std::endl;

			exit(EXIT_FAILURE);
		}

		mjd = DOYtoMJD( static_cast<int>(floor(years+0.1)), static_cast<int>(floor(days+0.1)) ) + hours/24.0 + minutes/1440.0 + seconds/86400.0;
	}

	return mjd;
}

static int getAntennas(VexData *V, Vex *v, const CorrParams &params)
{
	struct dvalue *r;
	llist *block;
	int nWarn = 0;

	block = find_block(B_CLOCK, v);

	for(char *stn = get_station_def(v); stn; stn=get_station_def_next())
	{
		struct site_position *p;
		struct axis_type *q;
		VexAntenna *A;

		std::string antName(stn);
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
			std::cerr << "Error: cannot find site position for antenna " << antName << " in the vex file." << std::endl;

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
			std::cerr << "Error: cannot find axis type for antenna " << antName << " in the vex file." << std::endl;

			exit(EXIT_FAILURE);
		}
		A->axisType = std::string(q->axis1) + std::string(q->axis2);
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
			std::cerr << "Error: cannot find axis offset for antenna " << antName << " in the vex file." << std::endl;

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
			for(void *c = get_station_lowl(stn, T_CLOCK_EARLY, B_CLOCK, v); c; c = get_station_lowl_next())
			{
				char *value;
				char *units;
				int name;
				int link;
				double mjd;

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
				Llist *defs;
				
				defs = ((struct block *)block->ptr)->items;
				if(defs)
				{
					defs = find_def(defs, stn);
				}
				if(defs)
				{
					for(Llist *lowls = find_lowl(((Def *)((Lowl *)defs->ptr)->item)->refs, T_CLOCK_EARLY); lowls; lowls = lowls->next)
					{
						Clock_early *C;
						
						if(((Lowl *)lowls->ptr)->statement != T_CLOCK_EARLY)
						{
							continue;
						}

						C = (Clock_early *)(((Lowl *)lowls->ptr)->item);
						if(C)
						{
							double mjd;
							
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
			if(params.fakeDatasource)
			{
				A->dataSource = DataSourceFake;
			}
			else
			{
				for(void *c = get_station_lowl(stn, T_VSN, B_TAPELOG_OBS, v); c; c = get_station_lowl_next())
				{
					char *value;
					char *units;
					int name;
					int link;
					double t1 = 0.0, t2 = 0.0;

					vex_field(T_VSN, c, 2, &link, &name, &value, &units);
					if(!value)
					{
						std::cerr << "VSN absent for antenna " << stn << std::endl;
						break;
					}
					std::string vsn(value);
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
						std::cerr << "VSN " << vsn << "doesn't have proper time range" << std::endl;
						break;
					}

					VexInterval vsnTimeRange(t1+0.001/86400.0, t2);

					if(!vsnTimeRange.isCausal())
					{
						std::cerr << "Error: Record stop (" << t2 << ") precedes record start (" << t1 << ") for antenna " << stn << ", module " << vsn << " ." << std::endl;
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
	}

	return nWarn;
}

static int getSources(VexData *V, Vex *v, const CorrParams &params)
{
	int nWarn = 0;
	
	for(char *src = get_source_def(v); src; src=get_source_def_next())
	{
		VexSource *S;
		char *p;

		S = V->newSource();
		S->defName = src;
		if(strlen(src) > VexSource::MAX_SRCNAME_LENGTH)
		{
			std::cerr << "Source name " << src << " is longer than " << 
			VexSource::MAX_SRCNAME_LENGTH << "  characters!" << std::endl;
			++nWarn;
		}

		for(p = (char *)get_source_lowl(src, T_SOURCE_NAME, v);
		    p != 0;
		    p = (char *)get_source_lowl_next())
		{
			S->sourceNames.push_back(std::string(p));
			if(strlen(p) > VexSource::MAX_SRCNAME_LENGTH)
			{
				std::cerr << "Source name " << src << " is longer than " <<
				VexSource::MAX_SRCNAME_LENGTH << "  characters!" << std::endl;
				++nWarn;
			}
		}

		p = (char *)get_source_lowl(src, T_RA, v);
		if(!p)
		{
			std::cerr << "Error: Cannot find right ascension for source " << src << std::endl;

			exit(EXIT_FAILURE);
		}
		fvex_ra(&p, &S->ra);

		p = (char *)get_source_lowl(src, T_DEC, v);
		if(!p)
		{
			std::cerr << "Error: Cannot find declination for source " << src << std::endl;

			exit(EXIT_FAILURE);
		}
		fvex_dec(&p, &S->dec);

		p = (char *)get_source_lowl(src, T_REF_COORD_FRAME, v);
		if(!p)
		{
			std::cerr << "Error: Cannot find ref coord frame for source " << src << std::endl;

			exit(EXIT_FAILURE);
		}
		if(strcmp(p, "J2000") != 0)
		{
			std::cerr << "Error: only J2000 ref frame is supported." << std::endl;

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

static VexInterval adjustTimeRange(std::map<std::string, double> &antStart, std::map<std::string, double> &antStop, unsigned int minSubarraySize)
{
	std::list<double> start;
	std::list<double> stop;
	double mjdStart, mjdStop;

	if(minSubarraySize < 1)
	{
		std::cerr << "Developer error: adjustTimeRange: minSubarraySize = " << minSubarraySize << " is < 1" << std::endl;

		exit(EXIT_FAILURE);
	}

	if(antStart.size() != antStop.size())
	{
		std::cerr << "Developer error: adjustTimeRange: size mismatch" << std::endl;

		exit(EXIT_FAILURE);
	}

	if(antStart.size() < minSubarraySize)
	{
		// Return an acausal interval
		return VexInterval(1, 0);
	}

	for(std::map<std::string, double>::iterator it = antStart.begin(); it != antStart.end(); ++it)
	{
		start.push_back(it->second);
	}
	start.sort();
	// Now the start times are sorted chronologically

	for(std::map<std::string, double>::iterator it = antStop.begin(); it != antStop.end(); ++it)
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
	for(std::map<std::string, double>::iterator it = antStart.begin(); it != antStart.end(); ++it)
	{
		if(it->second < mjdStart)
		{
			it->second = mjdStart;
		}
	}

	for(std::map<std::string, double>::iterator it = antStop.begin(); it != antStop.end(); ++it)
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
	char *scanId;
	int nScanSkip = 0;
	int nWarn = 0;

	for(Llist *L = (Llist *)get_scan(&scanId, v); L != 0; L = (Llist *)get_scan_next(&scanId))
	{
		VexScan *S;
		std::map<std::string,double> antStart, antStop;
		std::map<std::string,bool> recordEnable;
		std::map<std::string,VexInterval> stations;
		double startScan, stopScan;
		double mjd;
		int link, name;
		char *value, *units;
		void *p;
		std::string intent = "";
		std::string tmpIntent;

		stations.clear();
		recordEnable.clear();
		antStart.clear();
		antStop.clear();

		Llist *lowls = L;
		lowls=find_lowl(lowls,T_COMMENT);
		while(lowls != NULL) {
			int pos;
			// assume our comments are clustered together at beginning of scan definition
	        if(((Lowl *)lowls->ptr)->statement != T_COMMENT) {
            	break;
        	}
			// get comment content
			vex_field(T_COMMENT, (void *)((Lowl *)lowls->ptr)->item, 1, &link, &name, &value, &units);

			tmpIntent = (!value)?"":value;
			pos = tmpIntent.find("intent = \"", 0);
			if( pos != string::npos ) {
				// +10 to skip the search string
				pos += 10;
				// trim everything except the actual tmpIntent string
				tmpIntent = tmpIntent.substr(pos, tmpIntent.size()-pos-1);
				intent += tmpIntent + ",";
			}
			lowls = lowls->next;
		}

		p = get_scan_start(L);

		vex_field(T_START, p, 1, &link, &name, &value, &units);
		mjd = vexDate(value);
		startScan = 1e99;
		stopScan = 0.0;
		for(p = get_station_scan(L); p; p = get_station_scan_next())
		{
			std::string stationName;
			double startAnt, stopAnt;
			char *stn;
			
			vex_field(T_STATION, p, 1, &link, &name, &stn, &units);
			stationName = std::string(stn);
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
//printf("***** record: %i for %s for scan %s\n", (recordEnable[stationName]?1:0), stationName.c_str(), scanId);

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

		std::string scanDefName(scanId);
		std::string sourceDefName((char *)get_scan_source(L));
		std::string modeDefName((char *)get_scan_mode(L));

		const VexSource *src = V->getSourceByDefName(sourceDefName);
		if(src == 0)
		{
			std::cerr << "Vex error! Scan " << scanDefName << " references source " << sourceDefName << " which is not in the source table." << std::endl;

			exit(EXIT_FAILURE);
		}

		std::string corrSetupName = params.findSetup(scanDefName, sourceDefName, modeDefName, src->calCode, 0);

		if(corrSetupName == "" || corrSetupName == "SKIP")
		{
			continue;
		}

		if(params.getCorrSetup(corrSetupName) == 0)
		{
			std::cerr << "Error: Scan=" << scanDefName << " correlator setup " << corrSetupName << " not defined!" << std::endl;

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
		S->intent = intent;
		S->mjdVex = mjd;

		// Add to event list
		V->addEvent(S->mjdStart, VexEvent::SCAN_START, scanId, scanId);
		V->addEvent(S->mjdStop,  VexEvent::SCAN_STOP,  scanId, scanId);
		for(std::map<std::string, double>::const_iterator it = antStart.begin(); it != antStart.end(); ++it)
		{
			V->addEvent(std::max(it->second, startScan), VexEvent::ANT_SCAN_START, it->first, scanId);
		}
		for(std::map<std::string, double>::const_iterator it = antStop.begin(); it != antStop.end(); ++it)
		{
			V->addEvent(std::min(it->second, stopScan), VexEvent::ANT_SCAN_STOP, it->first, scanId);
		}
	}

	if(nScanSkip > 0)
	{
		std::cout << "FYI: " << nScanSkip << " scans skipped because of time range selection." << std::endl;
	}
	
	return nWarn;
}

static int getModes(VexData *V, Vex *v, const CorrParams &params)
{
	int nWarn = 0;

	for(const char *modeDefName = get_mode_def(v); modeDefName; modeDefName = get_mode_def_next())
	{
		VexMode *M;
		
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
			int link, name;
			char *value, *units;
			void *p, *p2;
			const std::string &antName = V->getAntenna(a)->defName;
			std::string antName2 = V->getAntenna(a)->defName;
			const AntennaSetup *antennaSetup;
			std::map<std::string,std::vector<int> > pcalMap;
			std::map<std::string,char> bbc2pol;
			std::map<std::string,std::string> bbc2ifName;
			std::map<std::string,Tracks> ch2tracks;

			// p2 will hold pointers to the special comments attached to if_def; max of 4
			void *p2array[MAX_IF];
			int p2count;

			int nBit = 1;
			int nTrack = 0;


			bbc2pol.clear();
			bbc2ifName.clear();
			ch2tracks.clear();

			Upper(antName2);
			bool swapPol = params.swapPol(antName2);
			VexSetup &setup = M->setups[V->getAntenna(a)->name];
			antennaSetup = params.getAntennaSetup(antName2);
			if(antennaSetup)
			{
				if(!antennaSetup->format.empty())
				{
					std::cout << "Setting antenna format to " << antennaSetup->format << " for antenna " << antName << std::endl;
				}
				setup.formatName = antennaSetup->format;
			}

			// Get sample rate
			p = get_all_lowl(antName.c_str(), modeDefName, T_SAMPLE_RATE, B_FREQ, v);
			if(p == 0)
			{
				continue;
			}
			vex_field(T_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
			fvex_double(&value, &units, &setup.sampRate);

			// init array to all zeroes
			for(p2count = 0; p2count < MAX_IF; ++p2count)
			{
				p2array[p2count] = 0;
			}
			// read all comment fields into array for later processing
			// NOTE - need to do it this way as the *_lowl_* functions use static vars and we
			// we can't run this where we actually process the comments as that would mess up
			// the IF_DEF loop
			// TODO we assign the comments below to vif.comment; should check if there is a way
			// to do so here instead of going through this temp array
			p2count = 0;
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_COMMENT_TRAILING, B_IF, v); p; p = get_all_lowl_next())
			{
				p2array[p2count++] = p;
			}

			// the collected comments and ifdef fields will always line up, so just run a count
			p2count = 0;
			
			// Derive IF map
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_IF_DEF, B_IF, v); p; p = get_all_lowl_next())
			{
				double phaseCal;
				
				vex_field(T_IF_DEF, p, 1, &link, &name, &value, &units);
				VexIF &vif = setup.ifs[std::string(value)];

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
					std::cerr << "Warning: Unsupported pulse cal interval of " << (phaseCal/1000000.0) << " MHz requested for antenna " << antName << "." << std::endl;
					++nWarn;
					vif.phaseCalIntervalMHz = static_cast<int>((phaseCal + 0.5)/1000000.0);
				}

				p2 = p2array[p2count++];
				if(!p2)
				{
					// check if this is a VLBA antenna; these require the comments for proper
					// observe-time operation, so exit in that case
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
							std::cerr << "Warning: VLBA antenna detected, but no comment for if_def; can't do switching" << std::endl;
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
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_BBC_ASSIGN, B_BBC, v); p; p = get_all_lowl_next())
			{
				vex_field(T_BBC_ASSIGN, p, 3, &link, &name, &value, &units);
				VexIF &vif = setup.ifs[std::string(value)];

				vex_field(T_BBC_ASSIGN, p, 1, &link, &name, &value, &units);
				bbc2pol[value] = vif.pol;
				bbc2ifName[value] = vif.name;
			}

			// Get datastream assignments and formats

			// Is it a Mark5 mode?
			if(setup.formatName == "") // Enter here if no format has been specified in the .v2d file
			{
				p = get_all_lowl(antName.c_str(), modeDefName, T_TRACK_FRAME_FORMAT, B_TRACKS, v);
				if(p)
				{
					vex_field(T_TRACK_FRAME_FORMAT, p, 1, &link, &name, &value, &units);
					setup.formatName = std::string(value);
					if(setup.formatName == "Mark4")
					{
						setup.formatName = "MKIV";
					}
					else if(setup.formatName == "Mark5B")
					{
						setup.formatName = "MARK5B";
					}
					else if(setup.formatName == "KVN5B")
					{
						setup.formatName = "KVN5B";
					}
					else if(setup.formatName == "NONE")
					{
						setup.formatName = "NONE";
					}
				}
				else
				{
					std::cerr << "Unable to determine data format for antenna " << antName << std::endl;

					//exit(EXIT_FAILURE);
					setup.formatName = "NONE";
				}
			}

			if(setup.formatName == "VLBA" || setup.formatName == "VLBN")
			{
				float totalDataRate = setup.sampRate*setup.nBit*setup.nRecordChan;

				if(totalDataRate > 0.0 && totalDataRate < 1.7e7)
				{
					std::cout << "*** Warning: a " << setup.formatName << " mode was found with sample rate " << (totalDataRate*10e-6) << " Mbps, which is less than the minimum 16 Mbps that can make use of this record format.  It is likely this mode will produce unusual results.  The suggested fix is to add additional channels in the vex file to bring the total bit rate up to 16 Mbps, which is very likely what the formatter did at the time of recording." << std::endl;
				}
			}

			if(setup.formatName == "VLBA" || setup.formatName == "VLBN" || setup.formatName == "MKIV" || setup.formatName == "MARK5B" || setup.formatName == "KVN5B" || setup.formatName.substr(0, 4) == "VDIF" || setup.formatName.substr(0, 5) == "VDIFL")
			{
				for(p = get_all_lowl(antName.c_str(), modeDefName, T_FANOUT_DEF, B_TRACKS, v); p; p = get_all_lowl_next())
				{
					std::string chanName;
					bool sign;
					int dasNum;
					
					vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
					chanName = value;
					vex_field(T_FANOUT_DEF, p, 3, &link, &name, &value, &units);
					sign = (value[0] == 's');
					vex_field(T_FANOUT_DEF, p, 4, &link, &name, &value, &units);
					sscanf(value, "%d", &dasNum);

					for(int k = 5; k < 9; ++k)
					{
						int chanNum;
						
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
				if(ch2tracks.empty())
				{
				  if (setup.formatName.substr(0, 4) == "VDIF" || setup.formatName.substr(0, 5) == "VDIFL")
					{
						// non-interlaced:  VDIF/size/bits, use all recorded channels, find 7777 below
						setup.nRecordChan = 7777;
						setup.nBit = atoi(setup.formatName.substr(setup.formatName.find_last_of('/') + 1).c_str());
						setup.formatName = setup.formatName.substr(0, setup.formatName.find_last_of('/'));
					}
					else
					{
						setup.formatName = "NONE";
						setup.nRecordChan = 0;
						setup.nBit = 0;
					}
				}
				else
				{
					int fanout;

					fanout = nTrack/ch2tracks.size()/nBit;
					if(!(setup.formatName == "MARK5B" || setup.formatName == "KVN5B" || setup.formatName.substr(0, 4) == "VDIF")) 
					{
						switch(fanout)
						{
							case 1: 
								setup.formatName += "1_1"; 
								break;
							case 2: 
								setup.formatName += "1_2"; 
								break;
							case 4: 
								setup.formatName += "1_4"; 
								break;
							default: 
								std::cerr << "Error: Antenna=" << antName << " fanout=" << fanout << " not legal for format " << setup.formatName << ".  This could be a subtle problem in the vex file." << std::endl;

								exit(EXIT_FAILURE);
						}
					}
					setup.nRecordChan = ch2tracks.size();
					setup.nBit = nBit;
				}
			}
			else if(setup.formatName == "LBAVSOP" || setup.formatName == "LBASTD")
			{
				for(p = get_all_lowl(antName.c_str(), modeDefName, T_FANOUT_DEF, B_TRACKS, v); p; p = get_all_lowl_next())
				{
				    std::string chanName;
				    bool sign;
				    int dasNum;
				    
				    vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
				    chanName = value;
				    vex_field(T_FANOUT_DEF, p, 3, &link, &name, &value, &units);
				    sign = (value[0] == 's');
				    vex_field(T_FANOUT_DEF, p, 4, &link, &name, &value, &units);
				    sscanf(value, "%d", &dasNum);

				    int chanNum;
				    
				    if(vex_field(T_FANOUT_DEF, p, 5, &link, &name, &value, &units) < 0)
				    {
				    	break;
				    }
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
				setup.nRecordChan = ch2tracks.size();
				setup.nBit = nBit;
			}
//			else if(setup.formatName == "VDIF")
//			{
//				std::cerr << "Warning: Antenna " << antName << " format treated as (one channel) VDIF/5032/2." << std::endl;
//				setup.nBit = 2;
//				setup.nRecordChan = 1;
//			}
			else if(setup.formatName.find("VDIF") != std::string::npos)  // INTERLACEDVDIF...
			{
#warning "handling of INTERLACEDVDIF nRecordChan may not be correct in all cases"
				setup.nBit = atoi(setup.formatName.substr(setup.formatName.find_last_of('/') + 1).c_str());
				setup.formatName = setup.formatName.substr(0, setup.formatName.find_last_of('/'));
				setup.nRecordChan = 1;
				size_t lpos = setup.formatName.find_first_of(':');
				if(lpos == std::string::npos)
				{
					setup.nRecordChan = 7777;	// use all channels of vex file, see below
				}
				while(lpos != std::string::npos)	// else
				{
					// and an additional channel for every ':' in INTERLACEDVDIF/.../size/bits
					++setup.nRecordChan;
					lpos = setup.formatName.find_first_of(':', lpos + 1);
				}
			}

			// Is it an S2 mode?
			p = get_all_lowl(antName.c_str(), modeDefName, T_S2_RECORDING_MODE, B_TRACKS, v);
			if(p)
			{
				vex_field(T_S2_RECORDING_MODE, p, 1, &link, &name, &value, &units);
				std::string s2mode(value);
				if(s2mode != "none")
				{
					if(setup.formatName == "")
					{
						setup.formatName = "S2";
					}

					size_t f = s2mode.find_last_of("x");
					size_t g = s2mode.find_last_of("-");

					if(f == std::string::npos || g == std::string::npos || f > g)
					{
						std::cerr << "Error: Antenna=" << antName << " malformed S2 mode : " << std::string(value) << std::endl;
					
						exit(EXIT_FAILURE);
					}

					std::string tracks = s2mode.substr(f+1, g-f-1);
					std::string bits = s2mode.substr(g+1);

					setup.nBit = atoi(bits.c_str());
					setup.nRecordChan = atoi(tracks.c_str())/setup.nBit; // should equal bbc2pol.size();
					if(ch2tracks.empty())
					{
						setup.formatName = "NONE";
						setup.nRecordChan = 0;
						setup.nBit = 0;
					}
					else
					{
						setup.nRecordChan = ch2tracks.size();
						setup.nBit = nBit;
					}
				} 
			}

			if(setup.formatName == "MARK5B"||setup.formatName == "KVN5B")
			{
				// Because Mark5B formatters can apply a bitmask, the track numbers may not be contiguous.  Here we go through and reorder track numbers in sequence, starting with 2

				const int MaxTrackNumber = 66;
				int order[MaxTrackNumber+1];

				for(int i = 0; i <= MaxTrackNumber; ++i)
				{
					order[i] = 0;
				}

				std::map<std::string,Tracks>::iterator it;
				for(it = ch2tracks.begin(); it != ch2tracks.end(); ++it)
				{
					const Tracks &T = it->second;

					for(std::vector<int>::const_iterator b = T.sign.begin(); b != T.sign.end(); ++b)
					{
						if(*b < 0 || *b > MaxTrackNumber)
						{
							cerr << "Error: track number " << *b << " is out of range (0.." << MaxTrackNumber << ").  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						if(order[*b] != 0)
						{
							cerr << "Error: track number " << *b << " is repeated.  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						order[*b] = 1;
					}

					for(std::vector<int>::const_iterator b = T.mag.begin(); b != T.mag.end(); ++b)
					{
						if(*b < 0 || *b > MaxTrackNumber)
						{
							cerr << "Error: track number " << *b << " is out of range (0.." << MaxTrackNumber << ").  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						if(order[*b] != 0)
						{
							cerr << "Error: track number " << *b << " is repeated.  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						order[*b] = 1;
					}
				}

				int newTrackNumber = 2;
				for(int i = 0; i < 67; ++i)
				{
					if(order[i] == 1)
					{
						order[i] = newTrackNumber;
						++newTrackNumber;
					}
				}

				for(it = ch2tracks.begin(); it != ch2tracks.end(); ++it)
				{
					Tracks &T = it->second;

					for(std::vector<int>::iterator b = T.sign.begin(); b != T.sign.end(); ++b)
					{
						*b = order[*b];
					}

					for(std::vector<int>::iterator b = T.mag.begin(); b != T.mag.end(); ++b)
					{
						*b = order[*b];
					}
				}
			}

			// Get pulse cal extraction information
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_PHASE_CAL_DETECT, B_PHASE_CAL_DETECT, v); p; p = get_all_lowl_next())
			{
				vex_field(T_PHASE_CAL_DETECT, p, 1, &link, &name, &value, &units);
				std::vector<int> &Q = pcalMap[std::string(value)];
				
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
			unsigned int nRecordChan = 0;
			
			for(p = get_all_lowl(antName.c_str(), modeDefName, T_CHAN_DEF, B_FREQ, v); p; p = get_all_lowl_next())
			{
				int recChanId;
				int subbandId;
				char *bbcName;
				double freq;
				double bandwidth;
				double origBandwidth;

				vex_field(T_CHAN_DEF, p, 2, &link, &name, &value, &units);
				fvex_double(&value, &units, &freq);

				vex_field(T_CHAN_DEF, p, 3, &link, &name, &value, &units);
				char sideBand = value[0];
				
				vex_field(T_CHAN_DEF, p, 4, &link, &name, &value, &units);
				fvex_double(&value, &units, &bandwidth);

				origBandwidth = bandwidth;

				if(bandwidth > setup.sampRate/2)
				{
					std::cerr << "Error: sample rate = " << setup.sampRate << " bandwidth = " << bandwidth << std::endl;
					std::cerr << "Sample rate must be no less than twice the bandwidth in all cases." << std::endl;

					exit(EXIT_FAILURE);
				}

				if(bandwidth < setup.sampRate/2)
				{
					// Note: this is tested in a sanity check later.  This behavior is not always desirable.
					bandwidth = setup.sampRate/2;
				}

				vex_field(T_CHAN_DEF, p, 6, &link, &name, &bbcName, &units);
				subbandId = M->addSubband(freq, bandwidth, sideBand, bbc2pol[bbcName], static_cast<int>(bandwidth/origBandwidth+0.5));

				vex_field(T_CHAN_DEF, p, 7, &link, &name, &value, &units);
				std::string phaseCalName(value);

				vex_field(T_CHAN_DEF, p, 5, &link, &name, &value, &units);
				std::string chanName(value);

				recChanId = getRecordChannel(antName, chanName, ch2tracks, setup, nRecordChan);
				setup.channels.push_back(VexChannel());
				setup.channels.back().subbandId = subbandId;
				setup.channels.back().ifName = bbc2ifName[bbcName];
				setup.channels.back().bbcFreq = freq;
				setup.channels.back().bbcBandwidth = bandwidth;
				setup.channels.back().bbcSideBand = sideBand;
				setup.channels.back().bbcName = bbcName;
				setup.channels.back().name = chanName;
				setup.channels.back().oversamp = static_cast<int>(bandwidth/origBandwidth+0.5);
				if(recChanId >= 0)
				{
					setup.channels.back().recordChan = recChanId;
					if(antennaSetup && antennaSetup->toneSelection == ToneSelectionVex)
					{
						setup.channels.back().tones = pcalMap[phaseCalName];
					}

					const VexIF *vif = setup.getIF(setup.channels.back().ifName);

					// This is called even for vex selected tones as negative tones 
					// are turned positive and result tone order becomes sorted
					if(antennaSetup)
					{
						setup.channels.back().selectTones(
							(antennaSetup->phaseCalIntervalMHz >= 0 ? antennaSetup->phaseCalIntervalMHz : vif->phaseCalIntervalMHz), 
							antennaSetup->toneSelection,
							antennaSetup->toneGuardMHz);
					}
					else
					{
						setup.channels.back().selectTones(vif->phaseCalIntervalMHz, ToneSelectionSmart, bandwidth/1000000.0/8.0);
					}
				}
				else
				{
					setup.channels.back().recordChan = -1;
				}

				++nRecordChan;
			}

			if(setup.nRecordChan == 7777)     // then use the number of that we just counted out
			{
				setup.nRecordChan = nRecordChan;
				std::cout << "FYI: Antenna=" << antName << " will use the full number of recorded channels, " << setup.nRecordChan << std::endl;
			}

			if(nRecordChan != setup.nRecordChan && setup.nRecordChan != 0)
			{
				std::cerr << "Warning: Antenna=" << antName << " nchan=" << nRecordChan << " != setup.nRecordChan=" << setup.nRecordChan << std::endl;
			}

			// Sort channels by name and then assign sequential thread Id
			std::sort(setup.channels.begin(), setup.channels.end());
			for(int threadId = 0; threadId < setup.channels.size(); ++threadId)
			{
				setup.channels[threadId].threadId = threadId;
			}
		} // End of antenna loop

#if 0
		for(std::vector<VexSubband>::iterator it = M->subbands.begin(); it != M->subbands.end(); ++it)
		{
			int overSamp = static_cast<int>(setup.sampRate/(2.0*it->bandwidth) + 0.001);

			if(params.overSamp > 0)
			{
				if(params.overSamp > overSamp)
				{
					std::cerr << "Warning: Mode=" << M->defName << " subband=" << M->overSamp.size() << 
						": requested oversample factor " << params.overSamp << 
						" is greater than the observed oversample factor " << overSamp << std::endl;
					std::cerr << "This was triggered because the sample rate was " << setup.sampRate <<
						" and the bandwidth was " << it->bandwidth << std::endl;
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
#endif
		M->overSamp.push_back(1);

		M->overSamp.sort();
		M->overSamp.unique();
	}

	return nWarn;
}

static int getVSN(VexData *V, Vex *v, const char *station)
{
	llist *block;
	Llist *defs;
	bool quit = false;

	std::string antName(station);

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

	for(Llist *lowls = find_lowl(((Def *)((Lowl *)defs->ptr)->item)->refs, T_VSN); lowls; lowls = lowls->next)
	{
		Vsn *p;

		if(((Lowl *)lowls->ptr)->statement != T_VSN)
		{
			continue;
		}
		
		p = (Vsn *)(((Lowl *)lowls->ptr)->item);
		if(!p)
		{
			return -4;
		}

		std::string vsn(p->label);
		fixOhs(vsn);

		VexInterval vsnTimeRange(vexDate(p->start)+0.001/86400.0, vexDate(p->stop));

		if(!vsnTimeRange.isCausal())
		{
			std::cerr << "Error: Record stop (" << p->stop << ") precedes record start (" << p->start << ") for antenna " << antName << ", module " << vsn << " ." << std::endl;
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
		std::string ant(stn);
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
	int N = 0;
	int nWarn = 0;

	block = find_block(B_EOP, v);

	if(block)
	{
		for(Llist *defs=((struct block *)block->ptr)->items; defs; defs=defs->next)
		{
			int nEop;
			int statement;
			double refEpoch;
			Llist *lowls, *refs;
			int link, name;
			char *value, *units;
			double interval;
			dvalue *r;
			void *p;
			double tai_utc, ut1_utc, x_wobble, y_wobble;
			
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
				VexEOP *E;
				
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
			std::cerr << "Warning: Mixing EOP values from vex and v2d files.  Your mileage may vary!" << std::endl;
			++nWarn;
		}
		for(std::vector<VexEOP>::const_iterator e = params.eops.begin(); e != params.eops.end(); ++e)
		{
			VexEOP *E;
			
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
	double start=0.0, stop=0.0;
	int nWarn = 0;
	std::string experimentName;

	block = find_block(B_EXPER, v);

	if(!block)
	{
		return -1;
	}

	for(Llist *defs=((struct block *)block->ptr)->items; defs; defs=defs->next)
	{
		int statement;
		Llist *lowls, *refs;
		
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

		experimentName = (char *)(((Lowl *)lowls->ptr)->item);

		lowls = find_lowl(refs, T_EXPER_NOMINAL_START);
		if(lowls)
		{
			void *p;
			
			p = (((Lowl *)lowls->ptr)->item);
			cerr << "start date: " << ((char *)p) << endl;
			start = vexDate((char *)p);
			strncpy(V->vexStartTime, ((char *)p), 50);
			V->vexStartTime[4] = '\0';
			V->vexStartTime[8] = '\0';
			V->vexStartTime[11] = '\0';
			V->vexStartTime[14] = '\0';
			V->vexStartTime[17] = '\0';
		}

		lowls = find_lowl(refs, T_EXPER_NOMINAL_STOP);
		if(lowls)
		{
			void *p;

			p = (((Lowl *)lowls->ptr)->item);
			cerr << "stop date: " << ((char *)p) << endl;
			stop = vexDate((char *)p);
			strncpy(V->vexStopTime, ((char *)p), 50);
			V->vexStopTime[4] = '\0';
			V->vexStopTime[8] = '\0';
			V->vexStopTime[11] = '\0';
			V->vexStopTime[14] = '\0';
			V->vexStopTime[17] = '\0';
		}
	}

	Upper(experimentName);

	V->setExper(experimentName, VexInterval(start, stop));

	return nWarn;
}

// Note: this is approximate, assumes all polarizations matched and no IFs being selected out
static void calculateScanSizes(VexData *V, const CorrParams &P)
{
	int nScan;

	nScan = V->nScan();

	for(int s = 0; s < nScan; ++s)
	{
		const VexScan *scan;
		const VexMode *mode;
		const CorrSetup *setup;
		int nSubband, nBaseline;
		
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
