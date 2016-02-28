/***************************************************************************
 *   Copyright (C) 2009-2016 by Walter Brisken                             *
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
#include "vex_data.h"
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

// Move to be a member of stream?
static int getRecordChannel(const std::string &antName, const std::string &chanName, const std::map<std::string,Tracks> &ch2tracks, const VexStream &stream, unsigned int n)
{
	if(stream.formatHasFanout())
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
	else if(stream.format == VexStream::FormatMark5B || stream.format == VexStream::FormatKVN5B) 
	{
		int delta, track;
		std::map<std::string,Tracks>::const_iterator it = ch2tracks.find(chanName);

		if(it == ch2tracks.end())
		{
			return -1;
		}

		const Tracks &T = it->second;

		if(T.sign.empty())
		{
			cerr << "Note: antenna " << antName << " has Mark5B format but no tracks defined in the vex file." << endl;

			return -1;
		}

		delta = T.sign.size() + T.mag.size();
		track = T.sign[0];

		return (track-2)/delta;
	}
	else if(stream.isLBAFormat())
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
	else if(stream.format == VexStream::FormatS2)
	{
		return n;
	}
	else if(stream.isVDIFFormat())
	{
		return n;
	}
	else if(stream.format == VexStream::FormatNone)
	{
		return 0;
	}
	else
	{
		std::cerr << "Error: Antenna=" << antName << " format \"" << VexStream::DataFormatNames[stream.format] << "\" is not yet supported" << std::endl;
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

static int getAntennas(VexData *V, Vex *v)
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

	return nWarn;
}

static int getSources(VexData *V, Vex *v)
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
	}

	return nWarn;
}

static int getScans(VexData *V, Vex *v)
{
	char *scanId;
	int nWarn = 0;

	for(Llist *L = (Llist *)get_scan(&scanId, v); L != 0; L = (Llist *)get_scan_next(&scanId))
	{
		VexScan *S;
		std::map<std::string,bool> recordEnable;
		std::map<std::string,Interval> stations;
		double startScan, stopScan;
		double mjd;
		int link, name;
		char *value, *units;
		void *p;
		std::string intent = "";
		std::string tmpIntent;

		stations.clear();
		recordEnable.clear();

		Llist *lowls = L;
		lowls=find_lowl(lowls,T_COMMENT);
		while(lowls != NULL)
		{
			size_t pos;
			// assume our comments are clustered together at beginning of scan definition
			if(((Lowl *)lowls->ptr)->statement != T_COMMENT)
			{
				break;
			}
			// get comment content
			vex_field(T_COMMENT, (void *)((Lowl *)lowls->ptr)->item, 1, &link, &name, &value, &units);

			tmpIntent = (!value) ? "" : value;
			pos = tmpIntent.find("intent = \"", 0);
			if(pos != string::npos)
			{
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
			recordEnable[stationName] = (atoi(value) > 0);

			stations[stationName] = Interval(startAnt, stopAnt);
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

		// Make scan
		S = V->newScan();
		S->setTimeRange(Interval(startScan, stopScan));
		S->defName = scanDefName;
		S->stations = stations;
		S->recordEnable = recordEnable;
		S->modeDefName = modeDefName;
		S->sourceDefName = sourceDefName;
		S->intent = intent;
		S->mjdVex = mjd;
	}

	return nWarn;
}

static int getModes(VexData *V, Vex *v)
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
			std::map<std::string,std::vector<unsigned int> > pcalMap;
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

			// Get sample rate
			p = get_all_lowl(antName.c_str(), modeDefName, T_SAMPLE_RATE, B_FREQ, v);
			if(p == 0)
			{
				continue;
			}

			// if we made it this far the antenna is involved in this mode

			VexSetup &setup = M->setups[V->getAntenna(a)->name];
			VexStream &stream = setup.streams[0];	// the first stream is created by default

			vex_field(T_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
			fvex_double(&value, &units, &stream.sampRate);

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
				else if(fabs(phaseCal-200000000.0) < 1.0)
				{
					vif.phaseCalIntervalMHz = 200;
				}
				else
				{
					std::cerr << "Warning: Unsupported pulse cal interval of " << (phaseCal/1000000.0) << " MHz requested for antenna " << antName << "." << std::endl;
					++nWarn;
					vif.phaseCalIntervalMHz = static_cast<int>((phaseCal + 0.5)/1000000.0);
				}

				p2 = p2array[p2count++];
#if 0
				if(!p2)
				{
					// check if this is a VLBA antenna; these require the comments for proper
					// observe-time operation, so exit in that case
					if(isVLBA(antName))
					{
						static bool first = true;
						if(first)
						{
							std::cerr << "Warning: VLBA antenna detected, but no comment for if_def; can't do switching" << std::endl;
							first = false;
						}
					}
				}
#endif

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

			p = get_all_lowl(antName.c_str(), modeDefName, T_TRACK_FRAME_FORMAT, B_TRACKS, v);
			if(p)
			{
				vex_field(T_TRACK_FRAME_FORMAT, p, 1, &link, &name, &value, &units);
				stream.parseFormatString(value);
			}
			else
			{
				std::cerr << "Note: Unable to determine data format for antenna " << antName << " based on vex file.  Will rely on other information." << std::endl;

				stream.format = VexStream::FormatNone;
			}

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
			if(!ch2tracks.empty())
			{
				int fanout;

				fanout = nTrack/ch2tracks.size()/nBit;
				if(stream.formatHasFanout())
				{
					stream.setFanout(fanout);
				}

				// FIXME: what to do if nBit and nRecordChan already set but they disagree?

				stream.nRecordChan = ch2tracks.size();
				stream.nBit = nBit;
			}
			if(stream.isLBAFormat())
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
				stream.nRecordChan = ch2tracks.size();
				stream.nBit = nBit;
			}

			// Is it an S2 mode?
			p = get_all_lowl(antName.c_str(), modeDefName, T_S2_RECORDING_MODE, B_TRACKS, v);
			if(p)
			{
				vex_field(T_S2_RECORDING_MODE, p, 1, &link, &name, &value, &units);
				std::string s2mode(value);
				if(s2mode != "none")
				{
					if(stream.format == VexStream::FormatNone)
					{
						stream.format = VexStream::FormatS2;
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

					// Weird.  Why the first two lines below whent they will just be overwritten?  -WFB
					stream.nBit = atoi(bits.c_str());
					stream.nRecordChan = atoi(tracks.c_str())/stream.nBit; // should equal bbc2pol.size();
					if(ch2tracks.empty())
					{
						stream.format = VexStream::FormatNone;
						stream.nRecordChan = 0;
						stream.nBit = 0;
					}
					else
					{
						stream.nRecordChan = ch2tracks.size();
						stream.nBit = nBit;
					}
				} 
			}

			if(stream.format == VexStream::FormatMark5B || stream.format == VexStream::FormatKVN5B)
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
							cerr << "Error: sign track number " << *b << " for channel " << it->first << " is out of range (0.." << MaxTrackNumber << ").  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						if(order[*b] != 0)
						{
							cerr << "Error: sign track number " << *b << " for channel " << it->first << " is repeated.  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						order[*b] = 1;
					}

					for(std::vector<int>::const_iterator b = T.mag.begin(); b != T.mag.end(); ++b)
					{
						if(*b < 0 || *b > MaxTrackNumber)
						{
							cerr << "Error: mag track number " << *b << " for channel " << it->first << " is out of range (0.." << MaxTrackNumber << ").  Must quit." << endl;

							exit(EXIT_FAILURE);
						}
						if(order[*b] != 0)
						{
							cerr << "Error: mag track number " << *b << " for channel " << it->first << " is repeated.  Must quit." << endl;

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
				std::vector<unsigned int> &Q = pcalMap[std::string(value)];
				
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

				vex_field(T_CHAN_DEF, p, 2, &link, &name, &value, &units);
				fvex_double(&value, &units, &freq);

				vex_field(T_CHAN_DEF, p, 3, &link, &name, &value, &units);
				char sideBand = value[0];
				
				vex_field(T_CHAN_DEF, p, 4, &link, &name, &value, &units);
				fvex_double(&value, &units, &bandwidth);

				if(bandwidth > stream.sampRate/2)
				{
					std::cerr << "Error: sample rate = " << stream.sampRate << " bandwidth = " << bandwidth << std::endl;
					std::cerr << "Sample rate must be no less than twice the bandwidth in all cases." << std::endl;

					exit(EXIT_FAILURE);
				}

				if(bandwidth < stream.sampRate/2)
				{
					// Note: this is tested in a sanity check later.  This behavior is not always desirable.
					bandwidth = stream.sampRate/2;
				}

				vex_field(T_CHAN_DEF, p, 6, &link, &name, &bbcName, &units);
				subbandId = M->addSubband(freq, bandwidth, sideBand, bbc2pol[bbcName]);

				vex_field(T_CHAN_DEF, p, 7, &link, &name, &value, &units);
				std::string phaseCalName(value);

				vex_field(T_CHAN_DEF, p, 5, &link, &name, &value, &units);
				std::string chanName(value);

				recChanId = getRecordChannel(antName, chanName, ch2tracks, stream, nRecordChan);
				setup.channels.push_back(VexChannel());
				setup.channels.back().subbandId = subbandId;
				setup.channels.back().ifName = bbc2ifName[bbcName];
				setup.channels.back().bbcFreq = freq;
				setup.channels.back().bbcBandwidth = bandwidth;
				setup.channels.back().bbcSideBand = sideBand;
				setup.channels.back().bbcName = bbcName;
				setup.channels.back().name = chanName;
				if(recChanId >= 0)
				{
					setup.channels.back().recordChan = recChanId;
					setup.channels.back().tones = pcalMap[phaseCalName];
				}
				else
				{
					setup.channels.back().recordChan = -1;
				}

				++nRecordChan;
			}

			if(stream.nRecordChan == 0)     // then use the number of that we just counted out
			{
				if(stream.nThread > 1)
				{
					// FIXME: test that nThread divies into nRecordChan
				}
				stream.nRecordChan = nRecordChan;
			}

			if(stream.isVDIFFormat())
			{
				// Sort channels by name and then assign sequential thread Id
				std::sort(setup.channels.begin(), setup.channels.end());
				for(unsigned int recChan = 0; recChan < setup.channels.size(); ++recChan)
				{
					setup.channels[recChan].threadId = recChan;
					setup.channels[recChan].recordChan = recChan;
				}
			}
		} // End of antenna loop
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
		int drive;

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

		drive = atoi(p->drive->value);

		Interval vsnTimeRange(vexDate(p->start)+0.001/86400.0, vexDate(p->stop));

		if(!vsnTimeRange.isCausal())
		{
			std::cerr << "Error: Record stop (" << p->stop << ") precedes record start (" << p->start << ") for antenna " << antName << ", module " << vsn << " ." << std::endl;
			quit = true;
		}
		else
		{
			if(vsn[0] == '/')
			{
				V->addFile(antName, drive, vsn, vsnTimeRange);
			}
			else
			{
				V->addVSN(antName, drive, vsn, vsnTimeRange);
			}
		}
	}

	if(quit)
	{
		exit(EXIT_FAILURE);
	}

	return 0;
}

static int getVSNs(VexData *V, Vex *v)
{
	int nWarn = 0;

	for(char *stn = get_station_def(v); stn; stn=get_station_def_next())
	{
		getVSN(V, v, stn);
	}

	return nWarn;
}

static int getEOPs(VexData *V, Vex *v)
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

	return nWarn;
}

static int getExper(VexData *V, Vex *v)
{
	llist *block;
	double start, stop;
	int nWarn = 0;
	std::string experimentName;

	start = V->getEarliestScanStart() - 1.0/86400.0;
	stop = V->getLatestScanStop() + 1.0/86400.0;

	block = find_block(B_EXPER, v);

	if(!block)
	{
		std::cerr << "Warning: no EXPER block found in the vex file.  That is bad." << std::endl;

		V->setExper("RANDOM", Interval(start, stop));
		
		++nWarn;

		return nWarn;
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
		else
		{
			std::cerr << "Note: The vex file has no exper_nominal_start parameter defined in the EXPER section.  Making assumptions..." << std::endl;
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
		else
		{
			std::cerr << "Note: The vex file has no exper_nominal_stop parameter defined in the EXPER section.  Making assumptions..." << std::endl;
		}
	}

	Upper(experimentName);

	V->setExper(experimentName, Interval(start, stop));

	return nWarn;
}


VexData *loadVexFile(const std::string &vexFile, int *numWarnings)
{
	VexData *V;
	Vex *v;
	int r;
	int nWarn = 0;

	r = vex_open(vexFile.c_str(), &v);
	if(r != 0)
	{
		return 0;
	}

	V = new VexData();

	V->setDirectory(vexFile.substr(0, vexFile.find_last_of('/')));

	nWarn += getAntennas(V, v);
	nWarn += getSources(V, v);
	nWarn += getScans(V, v);
	nWarn += getModes(V, v);
	nWarn += getVSNs(V, v);
	nWarn += getEOPs(V, v);
	nWarn += getExper(V, v);
	*numWarnings = *numWarnings + nWarn;

	return V;
}
