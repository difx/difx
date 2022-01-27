/***************************************************************************
 *   Copyright (C) 2009-2022 by Walter Brisken                             *
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
#include "vex_utility.h"
#include "vex_data.h"
#include "../vex/vex.h"
#include "../vex/vex_parse.h"

// maximum number of defined IFs
#define MAX_IF 64

using namespace std;

class BitAssignments
{
public:
	std::vector<int> sign;
	std::vector<int> mag;
};

std::ostream& operator << (std::ostream &os, const BitAssignments &x)
{
	int s = x.sign.size();
	int m = x.mag.size();

	os << "[ ";
	for(int i = 0; i < s; ++i)
	{
		os << "(" << x.sign[i];
		if(i < m)
		{
			os << " " << x.mag[i];
		}
		os << ") ";
	}
	os << "]";

	return os;
}

int reorderBitAssignments(std::map<std::string,BitAssignments> &bitAssignements, int startSlotNumber)
{
	const int MaxSlotNumber = 66;
	int order[MaxSlotNumber+1];

	for(int i = 0; i <= MaxSlotNumber; ++i)
	{
		order[i] = -2;
	}

	for(std::map<std::string,BitAssignments>::iterator it = bitAssignements.begin(); it != bitAssignements.end(); ++it)
	{
		const BitAssignments &T = it->second;

		for(std::vector<int>::const_iterator b = T.sign.begin(); b != T.sign.end(); ++b)
		{
			if(*b < 0 || *b > MaxSlotNumber)
			{
				cerr << "Error: sign assignment " << *b << " for channel " << it->first << " is out of range (0.." << MaxSlotNumber << ").  Must quit." << endl;

				exit(EXIT_FAILURE);
			}
			if(order[*b] != -2)
			{
				cerr << "Error: sign assignment " << *b << " for channel " << it->first << " is repeated.  Must quit." << endl;

				exit(EXIT_FAILURE);
			}
			order[*b] = -1;
		}

		for(std::vector<int>::const_iterator b = T.mag.begin(); b != T.mag.end(); ++b)
		{
			if(*b < 0 || *b > MaxSlotNumber)
			{
				cerr << "Error: mag assignment " << *b << " for channel " << it->first << " is out of range (0.." << MaxSlotNumber << ").  Must quit." << endl;

				exit(EXIT_FAILURE);
			}
			if(order[*b] != -2)
			{
				cerr << "Error: mag assignment " << *b << " for channel " << it->first << " is repeated.  Must quit." << endl;

				exit(EXIT_FAILURE);
			}
			order[*b] = -1;
		}
	}

	int newSlotNumber = startSlotNumber;
	for(int i = 0; i < 67; ++i)
	{
		if(order[i] == -1)
		{
			order[i] = newSlotNumber;
			++newSlotNumber;
		}
	}

	for(std::map<std::string,BitAssignments>::iterator it = bitAssignements.begin(); it != bitAssignements.end(); ++it)
	{
		BitAssignments &T = it->second;

		for(std::vector<int>::iterator b = T.sign.begin(); b != T.sign.end(); ++b)
		{
			*b = order[*b];
		}

		for(std::vector<int>::iterator b = T.mag.begin(); b != T.mag.end(); ++b)
		{
			*b = order[*b];
		}
	}

	return 0;
}

static void fixOhs(std::string &str)
{
	unsigned int i;

	// The format of Mark5/6 disk packs is name&number where name
	// is a four character string for Mark5 and a 3 character stream for
	// Mark6.  The delimiter for Mark5 is a + or a - while the Mark5 uses
	// the % sign.  This method fixes any zeroes that happen to be in the
	// name section by turning them to capital O's.

	for(i = 0; i < str.length(); ++i)
	{
		if(str[i] == '-' || str[i] == '+' || str[i] == '%')
		{
			break;
		}
		if(str[i] == '0')
		{
			str[i] = 'O';
		}
	}
}

static int getRecordChannelFromTracks(const std::string &antName, const std::string &chanLink, const std::map<std::string,BitAssignments> &ch2tracks, const VexStream &stream, unsigned int n)
{
	std::map<std::string,BitAssignments>::const_iterator it = ch2tracks.find(chanLink);
	if(it == ch2tracks.end())
	{
		return -1;
	}
	const BitAssignments &T = it->second;

	if(stream.formatHasFanout())
	{
		int delta, track;
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

static int getRecordChannelFromBitstreams(const std::string &antName, const std::string &chanLink, const std::map<std::string,BitAssignments> &ch2bitstreams, const VexStream &stream, unsigned int n)
{
	int delta, track;
	std::map<std::string,BitAssignments>::const_iterator it = ch2bitstreams.find(chanLink);
	if(it == ch2bitstreams.end())
	{
		return -1;
	}
	const BitAssignments &T = it->second;

	if(T.sign.empty())
	{
		cerr << "Note: antenna " << antName << " has Mark5B format but no tracks defined in the vex file." << endl;

		return -1;
	}

	delta = T.sign.size() + T.mag.size();
	track = T.sign[0];

	return track/delta;
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

/* Gets extensions from the $GLOBAL block */
static int getExtensions(VexData *V, Vex *v)
{
	int nWarn = 0;

	for(void *p = get_global_lowl(T_EXTENSION, B_EXTENSIONS, v); p; p = get_global_lowl_next())
	{
		char *value, *units;
		int name, link;
		VexExtension *E;

		E = V->newExtension();

		vex_field(T_EXTENSION, p, 1, &link, &name, &value, &units);
		if(value)
		{
			E->owner = value;

			vex_field(T_EXTENSION, p, 2, &link, &name, &value, &units);
			if(value)
			{
				E->name = value;
			}
			for(int i = 3; value; ++i)
			{
				vex_field(T_EXTENSION, p, i, &link, &name, &value, &units);
				if(value)
				{
					E->value.push_back(value);
					E->units.push_back( (units ? units : "") );
				}
			}
		}
	}

	return nWarn;
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
		struct site_id *s;
		struct nasmyth *n;
		VexAntenna *A;

		std::string antName(stn);
		Upper(antName);

		A = V->newAntenna();
		A->defName = stn;
		A->name = A->defName;
		Upper(A->name);
		A->difxName = A->name;

		s = (struct site_id *)get_station_lowl(stn, T_SITE_ID, B_SITE, v);
		if(s != 0)
		{
			if(s->code2)
			{
				A->twoCharSiteCode = s->code2;
			}
			if(s->code1)
			{
				A->oneCharSiteCode = s->code1;
			}
		}

		p = (struct site_position *)get_station_lowl(stn, T_SITE_POSITION, B_SITE, v);
		if(p == 0)
		{
			std::cerr << "Warning: cannot find site position for antenna " << antName << " in the vex file." << std::endl;
			++nWarn;
		}
		else
		{
			fvex_double(&(p->x->value), &(p->x->units), &A->x);
			fvex_double(&(p->y->value), &(p->y->units), &A->y);
			fvex_double(&(p->z->value), &(p->z->units), &A->z);
		}

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
			std::cerr << "Warning: cannot find axis type for antenna " << antName << " in the vex file." << std::endl;
			++nWarn;
		}
		else
		{
			A->axisType = std::string(q->axis1) + std::string(q->axis2);
			if(A->axisType.compare("hadec") == 0)
			{
				A->axisType = "equa";
			}
		}

		for(n = (struct nasmyth *)get_station_lowl(stn, T_NASMYTH, B_ANTENNA, v); n != 0; n = (struct nasmyth *)get_station_lowl_next())
		{
			if(n)
			{
				VexAntenna::NasmythType t;
			
				t = stringToNasmyth(n->platform);
				if(t == VexAntenna::NasmythError)
				{
					std::cerr << "Warning: unsupported Nasmyth platform type: " << n->platform << std::endl;
					++nWarn;
				}
				else
				{
					A->nasmyth[n->band] = t;
				}
			}
		}

		r = (struct dvalue *)get_station_lowl(stn, T_SITE_POSITION_EPOCH, B_SITE, v);
		if(r)
		{
			char *value, *units;
			int name, link;

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
			std::cerr << "Warning: cannot find axis offset for antenna " << antName << " in the vex file." << std::endl;
		}
		else
		{
			fvex_double(&(r->value), &(r->units), &A->axisOffset);
		}

		for(void *c = get_station_lowl(stn, T_CLOCK_EARLY, B_CLOCK, v); c; c = get_station_lowl_next())
		{
			char *value, *units;
			int name, link;
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
				vex_field(T_CLOCK_EARLY, c, 5, &link, &name, &value, &units);
				if(value)
				{
					if(units)
					{
						fvex_double(&value, &units, &clock.accel);
					}
					else
					{
						clock.accel = atof(value);
						std::cerr << "Warning: second order clock term did not have units." << std::endl;
						++nWarn;
					}
				}
				vex_field(T_CLOCK_EARLY, c, 6, &link, &name, &value, &units);
				if(value)
				{
					if(units)
					{
						fvex_double(&value, &units, &clock.jerk);
					}
					else
					{
						clock.jerk = atof(value);
						std::cerr << "Warning: third order clock term did not have units." << std::endl;
						++nWarn;
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
							if(C->rate->units)
							{
								fvex_double(&(C->rate->value), &(C->rate->units), &clock.rate);
							}
							else
							{
								clock.rate = atof(C->rate->value);
							}
							clock.offset_epoch = vexDate(C->origin);

							if(C->accel)
							{
								fvex_double(&(C->accel->value), &(C->accel->units), &clock.accel);
							}
							if(C->jerk)
							{
								fvex_double(&(C->jerk->value), &(C->jerk->units), &clock.jerk);
							}
						}
						
						// vex has the opposite sign convention, so swap
						clock.flipSign();
					}
				}
			}
		}
		for(void *p = get_station_lowl(stn, T_EXTENSION, B_EXTENSIONS, v); p; p = get_station_lowl_next())
		{
			char *value, *units;
			int name, link;
			
			A->extensions.push_back(VexExtension());
			VexExtension &E = A->extensions.back();

			vex_field(T_EXTENSION, p, 1, &link, &name, &value, &units);
			if(value)
			{
				E.owner = value;

				vex_field(T_EXTENSION, p, 2, &link, &name, &value, &units);
				if(value)
				{
					E.name = value;
				}
				for(int i = 3; value; ++i)
				{
					vex_field(T_EXTENSION, p, i, &link, &name, &value, &units);
					if(value)
					{
						E.value.push_back(value);
						E.units.push_back( (units ? units : "") );
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
			std::cerr << "Source name " << src << " is longer than " << VexSource::MAX_SRCNAME_LENGTH << "  characters!" << std::endl;
			++nWarn;
		}

		for(p = (char *)get_source_lowl(src, T_SOURCE_NAME, v); p; p = (char *)get_source_lowl_next())
		{
			S->sourceNames.push_back(std::string(p));
			if(strlen(p) > VexSource::MAX_SRCNAME_LENGTH)
			{
				std::cerr << "Source name " << src << " is longer than " << VexSource::MAX_SRCNAME_LENGTH << "  characters!" << std::endl;
				++nWarn;
			}
		}

		p = (char *)get_source_lowl(src, T_SOURCE_TYPE, v);
		if(p)
		{
			int link, name;
			char *arg1 = 0, *arg2 = 0, *arg3 = 0, *units = 0;
			bool ok;

			vex_field(T_SOURCE_TYPE, p, 1, &link, &name, &arg1, &units); // first field
			vex_field(T_SOURCE_TYPE, p, 2, &link, &name, &arg2, &units); // second field
			vex_field(T_SOURCE_TYPE, p, 3, &link, &name, &arg3, &units); // third field

			ok = S->setSourceType(arg1, arg2, arg3);
			if(!ok && V->getVersion() < 1.8)
			{
				S->setBSP(arg1, arg2);	// A hack used pre-Vex2 at VLBA
			}

			if(V->getVersion() >= 1.8)
			{
				if(S->type == VexSource::BSP)
				{
					void *p2;

					arg1 = arg2 = 0;
					p2 = get_source_lowl(src, T_BSP_FILE_NAME, v);
					if(p2)
					{
						vex_field(T_BSP_FILE_NAME, p2, 1, &link, &name, &arg1, &units);
					}
					p2 = get_source_lowl(src, T_BSP_OBJECT_ID, v);
					if(p2)
					{
						vex_field(T_BSP_OBJECT_ID, p2, 1, &link, &name, &arg2, &units);
					}

					if(arg1 && arg2)
					{
						S->setBSP(arg1, arg2);
					}
					else
					{
						std::cerr << "Error: BSP source type, but incomplete parameter set for source " << S->defName << std::endl;

						exit(EXIT_FAILURE);
					}
				}
				else if(S->type == VexSource::TLE)
				{
					void *p2;
					char *arg0 = 0;

					arg1 = arg2 = 0;
					p2 = get_source_lowl(src, T_TLE0, v);
					if(p2)
					{
						vex_field(T_TLE0, p2, 1, &link, &name, &arg0, &units);
					}
					p2 = get_source_lowl(src, T_TLE1, v);
					if(p2)
					{
						vex_field(T_TLE1, p2, 1, &link, &name, &arg1, &units);
					}
					p2 = get_source_lowl(src, T_TLE2, v);
					if(p2)
					{
						vex_field(T_TLE2, p2, 1, &link, &name, &arg2, &units);
					}

					if(arg0)
					{
						S->setTLE(0, arg0);
					}
					else
					{
						S->setTLE(0, S->defName.c_str());
					}
					if(arg1 && arg2)
					{
						S->setTLE(1, arg1);
						S->setTLE(2, arg2);
					}
					else
					{
						std::cerr << "Error: TLE source type, but incomplete ephemeris provided for source " << S->defName << std::endl;

						exit(EXIT_FAILURE);
					}
				}
			}
		}
		else
		{
			// default is to configure as a "star" type
			S->setSourceType("star");
		}

		if(S->type == VexSource::Star)
		{
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

			S->type = VexSource::Star;
		}

		if(S->type == VexSource::Unsupported)
		{
			std::cerr << "Warning: Source " << S->defName << " is of an unsupported source type" << std::endl;
			++nWarn;
		}

		p = (char *)get_source_lowl(src, T_REF_COORD_FRAME, v);
		if(!p)
		{
			std::cerr << "Warning: Cannot find ref coord frame for source " << src << " .  Assuming J2000." << std::endl;

			++nWarn;
		}
		else if(strcmp(p, "J2000") != 0)
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
		std::string sourceDefName;

		stations.clear();
		recordEnable.clear();
		sourceDefName.clear();

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
		std::string modeDefName((char *)get_scan_mode(L));

		// Make scan
		S = V->newScan();
		S->setTimeRange(Interval(startScan, stopScan));
		S->defName = scanDefName;
		S->stations = stations;
		S->recordEnable = recordEnable;
		S->modeDefName = modeDefName;
		S->intent = intent;
		S->mjdVex = mjd;

		for(p = get_scan_source2(L); p; p = get_scan_source2_next())
		{
			bool point;
			bool corr;

			point = corr = true;

			vex_field(T_SOURCE, p, 1, &link, &name, &value, &units);
			sourceDefName = value;

			const VexSource *src = V->getSourceByDefName(sourceDefName);
			if(src == 0)
			{
				std::cerr << "Vex error! Scan " << scanDefName << " references source " << sourceDefName << " which is not in the source table." << std::endl;

				exit(EXIT_FAILURE);
			}

			vex_field(T_SOURCE, p, 2, &link, &name, &value, &units);
			if(value)
			{
				if(atoi(value) == 0)
				{
					point = false;
				}
			}
			if(point)
			{
				if(S->sourceDefName.empty())
				{
					S->sourceDefName = sourceDefName;
				}
				else
				{
					std::cerr << "Vex error! Scan " << scanDefName << " has multiple pointing center sources." << std::endl;

					exit(EXIT_FAILURE);
				}
			}
			vex_field(T_SOURCE, p, 3, &link, &name, &value, &units);
			if(value)
			{
				if(atoi(value) == 0)
				{
					corr = false;
				}
			}
			if(corr)
			{
				S->phaseCenters.push_back(sourceDefName);
			}
		}

		// capture intents
		for(p = get_scan_intent(L); p; p = get_scan_intent_next())
		{
			char *src;
			char *id;

			vex_field(T_INTENT, p, 1, &link, &name, &src, &units);
			vex_field(T_INTENT, p, 2, &link, &name, &id, &units);
			vex_field(T_INTENT, p, 3, &link, &name, &value, &units);
			S->scanIntent.push_back(VexIntent(src, id, value));
		}

		if(S->sourceDefName.empty())
		{
			std::cerr << "Vex error! Scan " << scanDefName << " has no pointing center sources." << std::endl;

			exit(EXIT_FAILURE);
		}
	}

	return nWarn;
}

static VexSetup::SetupType getSetupType(Vex *v, std::string &format, const char *antDefName, const char *modeDefName)
{
	void *trackFormat;

	format = "";

	// Look for things needed for all kinds of modes
	if(get_all_lowl(antDefName, modeDefName, T_IF_DEF, B_IF, v) == 0 ||
	   get_all_lowl(antDefName, modeDefName, T_BBC_ASSIGN, B_BBC, v) == 0 ||
	   get_all_lowl(antDefName, modeDefName, T_CHAN_DEF, B_FREQ, v) == 0)
	{
		return VexSetup::SetupIncomplete;
	}

	if(get_all_lowl(antDefName, modeDefName, T_STREAM_DEF, B_BITSTREAMS, v))
	{
		// looks like bitstream; make sure supporting defs are in place

		return VexSetup::SetupBitstreams;
	}
	else if(get_all_lowl(antDefName, modeDefName, T_MERGED_DATASTREAM, B_DATASTREAMS, v))
	{
		return VexSetup::SetupMergedDatastreams;
	}
	else if(get_all_lowl(antDefName, modeDefName, T_DATASTREAM, B_DATASTREAMS, v))
	{
		return VexSetup::SetupDatastreams;
	}
	else if((trackFormat = get_all_lowl(antDefName, modeDefName, T_TRACK_FRAME_FORMAT, B_TRACKS, v)) != 0)
	{
		int link, name;
		char *value, *units;
		
		vex_field(T_TRACK_FRAME_FORMAT, trackFormat, 1, &link, &name, &value, &units);
		if(value)
		{
			format = value;
		}
		if(get_all_lowl(antDefName, modeDefName, T_S2_RECORDING_MODE, B_TRACKS, v))
		{
			std::cerr << "Note: Both track frame format and s2 recording mode were specified.  S2 information will be ignored." << std::endl;
		}

		return VexSetup::SetupTracks;
	}
	else if(get_all_lowl(antDefName, modeDefName, T_S2_RECORDING_MODE, B_TRACKS, v))
	{
		return VexSetup::SetupS2;
	}

	return VexSetup::SetupIncomplete;
}

static int collectIFInfo(VexSetup &setup, VexData *V, Vex *v, const char *antDefName, const char *modeDefName)
{
	int nWarn = 0;
	void *p2array[MAX_IF];
	int p2count = 0;

	for(int i = 0; i < MAX_IF; ++i)
	{
		p2array[i] = 0;
	}

	for(void *p = get_all_lowl(antDefName, modeDefName, T_IF_DEF, B_IF, v); p; p = get_all_lowl_next())
	{
		double phaseCal, phaseCalBase;
		int link, name;
		char *value, *units;
		void *p2;
		
		vex_field(T_IF_DEF, p, 1, &link, &name, &value, &units);
		VexIF &vif = setup.ifs[std::string(value)];
		vif.ifLink = value;

		if(V->getVersion() < 2.0)
		{
			vex_field(T_IF_DEF, p, 2, &link, &name, &value, &units);
			vif.ifName = value;
		}
		else
		{
			if(strncmp(value, "IF_", 3) == 0 && strlen(value) > 3)
			{
				vif.ifName = value + 3;
			}
			else
			{
				vif.ifName = value;
			}
		}
		
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
			vif.phaseCalIntervalMHz = 0.0f;
		}
		else if(fabs(phaseCal-1000000.0) < 1.0)
		{
			vif.phaseCalIntervalMHz = 1.0f;
		}
		else if(fabs(phaseCal-5000000.0) < 1.0)
		{
			vif.phaseCalIntervalMHz = 5.0f;
		}
		else if(fabs(phaseCal-200000000.0) < 1.0)
		{
			vif.phaseCalIntervalMHz = 200.0f;
		}
		else
		{
			std::cerr << "Warning: Unsupported pulse cal interval of " << (phaseCal/1000000.0) << " MHz requested for antenna " << antDefName << "." << std::endl;
			++nWarn;
			vif.phaseCalIntervalMHz = static_cast<float>(phaseCal/1000000.0);
		}

		vex_field(T_IF_DEF, p, 7, &link, &name, &value, &units);
		if(value)
		{
			fvex_double(&value, &units, &phaseCalBase);
		}
		else
		{
			phaseCalBase = 0.0;
		}
		vif.phaseCalBaseMHz = static_cast<float>(phaseCalBase/1000000.0);

		vex_field(T_IF_DEF, p, 8, &link, &name, &value, &units);
		if(value)
		{
			fvex_double(&value, &units, &vif.ifSampleRate);
		}

		if(p2count >= MAX_IF)
		{
			std::cerr << "Developer error: Value of MAX_IF is too small in vexload.cpp, instance 2" << std::endl;

			exit(0);
		}
		p2 = p2array[p2count++];

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

	for(void *p = get_all_lowl(antDefName, modeDefName, T_RECEIVER_NAME, B_IF, v); p; p = get_all_lowl_next())
	{
		int link, name;
		char *value, *units;

		vex_field(T_RECEIVER_NAME, p, 1, &link, &name, &value, &units);
		VexIF *vif = setup.getVexIFByLink(value);

		if(vif == 0)
		{
			std::cerr << "Warning: IF receiver name line provided with link " << value << " that is not defined in setup:" << std::endl;
			std::cerr << "  " << setup << std::endl;

			++nWarn;
		}
		else
		{
			vex_field(T_RECEIVER_NAME, p, 2, &link, &name, &value, &units);
			if(value && value[0])
			{
				vif->rxName = value;
			}
		}
	}

	for(void *p = get_all_lowl(antDefName, modeDefName, T_SUB_LO_FREQUENCIES, B_IF, v); p; p = get_all_lowl_next())
	{
		int link, name;
		char *value, *units;

		vex_field(T_SUB_LO_FREQUENCIES, p, 1, &link, &name, &value, &units);
		VexIF *vif = setup.getVexIFByLink(value);
		if(vif == 0)
		{
			std::cerr << "Warning: IF sub LO frequencies line provided with link " << value << " that is not defined in setup:" << std::endl;
			std::cerr << "  " << setup << std::endl;

			++nWarn;
		}
		else
		{
			for(int field = 2; ; ++field)
			{
				vex_field(T_SUB_LO_FREQUENCIES, p, field, &link, &name, &value, &units);
				if(value == 0 || value[0] == 0)
				{
					break;
				}
				vif->upstreamSSLO.push_back(atof(value));
			}
		}
	}

	for(void *p = get_all_lowl(antDefName, modeDefName, T_SUB_LO_SIDEBANDS, B_IF, v); p; p = get_all_lowl_next())
	{
		int link, name;
		char *value, *units;

		vex_field(T_SUB_LO_SIDEBANDS, p, 1, &link, &name, &value, &units);
		VexIF *vif = setup.getVexIFByLink(value);
		if(vif == 0)
		{
			std::cerr << "Warning: IF sub LO frequencies line provided with link " << value << " that is not defined in setup:" << std::endl;
			std::cerr << "  " << setup << std::endl;

			++nWarn;
		}
		else
		{
			for(int field = 2; ; ++field)
			{
				char sb;

				vex_field(T_SUB_LO_SIDEBANDS, p, field, &link, &name, &value, &units);
				if(value == 0 || value[0] == 0)
				{
					break;
				}
				sb = value[0];
				if(sb >= 'a')
				{
					sb = sb -'a' + 'A';
				}
				vif->upstreamSideBand.push_back(sb);
				if(sb == 'L')
				{
					int i = vif->upstreamSideBand.size() - 1;
					vif->upstreamSSLO[i] = - vif->upstreamSSLO[i];
				}
				else if(sb != 'R')
				{
					std::cerr << "Warning: IF sub LO sideband with value " << value << " was provided.  This is not supported.  Assuming upper sideband." << std::endl;

					++nWarn;
				}
			}

		}
	}

	for(void *p = get_all_lowl(antDefName, modeDefName, T_SWITCHED_POWER, B_IF, v); p; p = get_all_lowl_next())
	{
		int link, name;
		char *value, *units;

		vex_field(T_SWITCHED_POWER, p, 1, &link, &name, &value, &units);
		VexIF *vif = setup.getVexIFByLink(value);
		if(vif == 0)
		{
			std::cerr << "Warning: IF sub LO frequencies line provided with link " << value << " that is not defined in setup:" << std::endl;
			std::cerr << "  " << setup << std::endl;

			++nWarn;
		}
		else
		{
			vex_field(T_SUB_LO_SIDEBANDS, p, 2, &link, &name, &value, &units);
			if(value && value[0])
			{
				vif->spAmp = stringToSwitchedPowerAmplitude(value);
				if(vif->spAmp == VexIF::SP_error)
				{
					std::cerr << "Warning: IF switched power amplitude setting, '" << value << "', is not one of those supported: 'Off', 'Low' and 'High'.  Ignoring." << std::endl;
					vif->spAmp = VexIF::SP_unset;
					
					++nWarn;
				}
				vex_field(T_SUB_LO_SIDEBANDS, p, 3, &link, &name, &value, &units);
				if(value && value[0])
				{
					fvex_double(&value, &units, &vif->spFreq);
				}
			}
			else
			{
				std::cerr << "Warning: IF switched power statement does not specify amplitude.  Ignoring." << std::endl;
			}
		}
	}

	return nWarn;
}

// FIXME: common code for pulse cal info
static int collectPcalInfo(std::map<std::string,std::vector<unsigned int> > &pcalMap, VexData *V, Vex *v, const char *antDefName, const char *modeDefName)
{
	int nWarn = 0;

	pcalMap.clear();

	for(void *p = get_all_lowl(antDefName, modeDefName, T_PHASE_CAL_DETECT, B_PHASE_CAL_DETECT, v); p; p = get_all_lowl_next())
	{
		int link, name;
		char *value, *units;

		vex_field(T_PHASE_CAL_DETECT, p, 1, &link, &name, &value, &units);
		std::vector<unsigned int> &Q = pcalMap[std::string(value)];
		
		for(int q = 2; ; ++q)
		{
			int v, y;
			
			y = vex_field(T_PHASE_CAL_DETECT, p, q, &link, &name, &value, &units);
			if(y < 0)
			{
				break;
			}
			
			v = atoi(value);
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

	return nWarn;
}

// collect channel information from a $FREQ section
int collectFreqChannels(std::vector<VexChannel> &freqChannels, VexSetup &setup, VexMode &mode, Vex *v, const char *antDefName, const char *modeDefName)
{
	int nWarn = 0;
	std::map<std::string,char> bbc2pol;
	std::map<std::string,std::string> bbc2ifLink;
	int link, name;
	char *value, *units;

	// Get BBC to pol map; only needed to complete the frequency channel info
	for(void *p = get_all_lowl(antDefName, modeDefName, T_BBC_ASSIGN, B_BBC, v); p; p = get_all_lowl_next())
	{
		vex_field(T_BBC_ASSIGN, p, 3, &link, &name, &value, &units);
		VexIF *vif = setup.getVexIFByLink(value);

		if(vif == 0)
		{
			std::cerr << "Error: mode=" << modeDefName << " antenna=" << antDefName << " cannot find ifLink " << value << " in setup structure:" << std::endl;
			std::cerr << "Setup = " << setup << std::endl;

			exit(EXIT_FAILURE);
		}
		vex_field(T_BBC_ASSIGN, p, 1, &link, &name, &value, &units);
		bbc2pol[value] = vif->pol;
		bbc2ifLink[value] = vif->ifLink;
	}

	freqChannels.clear();

	for(void *p = get_all_lowl(antDefName, modeDefName, T_CHAN_DEF, B_FREQ, v); p; p = get_all_lowl_next())
	{
		int subbandId;
		char *bbcName;
		double freq;
		double bandwidth;
		std::string chanName;
		std::string chanLink;
		std::string bandLink;
		std::string phaseCalName;

		vex_field(T_CHAN_DEF, p, 1, &link, &name, &value, &units);
		if(value && value[0])
		{
			bandLink = value;
		}
		else
		{
			bandLink = "";
		}

		vex_field(T_CHAN_DEF, p, 2, &link, &name, &value, &units);
		fvex_double(&value, &units, &freq);

		vex_field(T_CHAN_DEF, p, 3, &link, &name, &value, &units);
		char sideBand = value[0];
		
		vex_field(T_CHAN_DEF, p, 4, &link, &name, &value, &units);
		fvex_double(&value, &units, &bandwidth);

		vex_field(T_CHAN_DEF, p, 5, &link, &name, &value, &units);
		if(value && value[0])
		{
			chanLink = value;
		}

		vex_field(T_CHAN_DEF, p, 6, &link, &name, &bbcName, &units);
		subbandId = mode.addSubband(freq, bandwidth, sideBand, bbc2pol[bbcName]);

		vex_field(T_CHAN_DEF, p, 7, &link, &name, &value, &units);
		if(value && value[0])
		{
			phaseCalName = value;
		}

		vex_field(T_CHAN_DEF, p, 8, &link, &name, &value, &units);
		if(value)
		{
			chanName = value;
		}
		else // Fall back to hack use of the BBC link name as the channel name
		{
			chanName = chanLink;
		}

		freqChannels.push_back(VexChannel());
		freqChannels.back().subbandId = subbandId;
		freqChannels.back().ifLink = bbc2ifLink[bbcName];
		freqChannels.back().bbcFreq = freq;
		freqChannels.back().bbcBandwidth = bandwidth;
		freqChannels.back().bbcSideBand = sideBand;
		freqChannels.back().bbcName = bbcName;
		freqChannels.back().chanName = chanName;
		freqChannels.back().chanLink = chanLink;
		freqChannels.back().bandLink = bandLink;
		freqChannels.back().recordChan = -1;
		freqChannels.back().phaseCalName = phaseCalName;
	}

	return nWarn;
}

static int collectExtensions(VexSetup &setup, Vex *v, const char *antDefName, const char *modeDefName)
{
	int nWarn = 0;

	for(void *p = get_mode_lowl(antDefName, modeDefName, T_EXTENSION, B_EXTENSIONS, v); p; p = get_mode_lowl_next())
	{
		char *value, *units;
		int name, link;
		
		setup.extensions.push_back(VexExtension());
		VexExtension &E = setup.extensions.back();

		vex_field(T_EXTENSION, p, 1, &link, &name, &value, &units);
		if(value)
		{
			E.owner = value;

			vex_field(T_EXTENSION, p, 2, &link, &name, &value, &units);
			if(value)
			{
				E.name = value;
			}
			for(int i = 3; value; ++i)
			{
				vex_field(T_EXTENSION, p, i, &link, &name, &value, &units);
				if(value)
				{
					E.value.push_back(value);
					E.units.push_back( (units ? units : "") );
				}
			}
		}
	}

	return nWarn;
}

VexChannel *getVexChannelByLink(std::vector<VexChannel> &freqChannels, const std::string chanLink)
{
	for(std::vector<VexChannel>::iterator it = freqChannels.begin(); it != freqChannels.end(); ++it)
	{
		if(!it->chanLink.empty() && it->chanLink == chanLink)
		{
			return &(*it);
		}
	}

	return 0;
}

bool cmpRecChan(const VexChannel &a, const VexChannel &b)
{
	return (a.recordChan < b.recordChan);
}

static int getS2Setup(VexSetup &setup, Vex *v, const char *antDefName, const char *modeDefName, std::map<std::string,std::vector<unsigned int> > &pcalMap, std::vector<VexChannel> &freqChannels, const std::string &format)
{
	VexStream &stream = setup.streams[0];	// the first stream is created by default
	int nWarn = 0;
	int link, name;
	char *value, *units;
	void *p, *q;
	std::map<std::string,BitAssignments> ch2tracks;		// indexed by channel link
	int nBit = 1;

	p = get_all_lowl(antDefName, modeDefName, T_SAMPLE_RATE, B_FREQ, v);
	if(p)
	{
		vex_field(T_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
		fvex_double(&value, &units, &stream.sampRate);
	}

	q = get_all_lowl(antDefName, modeDefName, T_S2_RECORDING_MODE, B_TRACKS, v);

	vex_field(T_S2_RECORDING_MODE, q, 1, &link, &name, &value, &units);
	std::string s2mode(value);
	if(s2mode == "none")
	{
		// Note: current practice (via Cormac Reynolds, 15 Aug 2016) suggests two LBA modes
		// that set S2_recording_mode = none :
		//   S2_data_source = VLBA or S2_data_source = LBAVSOP  --> format = VexStream::FormatLBAVSOP
		//   S2_data_source = LBASTD                            --> format = VexStream::FormatLBASTD

		q = get_all_lowl(antDefName, modeDefName, T_S2_DATA_SOURCE, B_TRACKS, v);
		if(!q)
		{
			std::cerr << "Error: S2 mode is 'none' but no S2 Data Source is provided" << std::endl;

			exit(EXIT_FAILURE);
		}
		vex_field(T_S2_DATA_SOURCE, q, 1, &link, &name, &value, &units);
		std::string s2datasource(value);

		if(s2datasource == "VLBA" || s2datasource == "LBAVSOP")
		{
			stream.format = VexStream::FormatLBAVSOP;
		}
		else if(s2datasource == "LBASTD")
		{
			stream.format = VexStream::FormatLBASTD;
		}
		else
		{
			std::cerr << "Error: unknown data mode: s2_recording_mode = none and s2_data_source = " << s2datasource << " for antenna " << antDefName << ".  S2_data_source should be one of VLBA, LBAVSOP or LBASTD in this case." << std::endl;

			exit(EXIT_FAILURE);
		}
	}
	else
	{
		std::cerr << "Error: S2 track formats are no longer supported." << std::endl;

		exit(EXIT_FAILURE);
	}

	for(p = get_all_lowl(antDefName, modeDefName, T_FANOUT_DEF, B_TRACKS, v); p; p = get_all_lowl_next())
	{
		std::string chanLink;
		bool sign;
		int dasNum;

		vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
		chanLink = value;
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
			ch2tracks[chanLink].sign.push_back(chanNum);
		}
		else
		{
			nBit = 2;
			ch2tracks[chanLink].mag.push_back(chanNum);
		}
	}
	stream.nRecordChan = ch2tracks.size();
	stream.nBit = nBit;

	// Generate channels
	unsigned int nRecordChan = 0;
	
	for(std::vector<VexChannel>::const_iterator it = freqChannels.begin(); it != freqChannels.end(); ++it)
	{
		int recChanId;

		setup.channels.push_back(*it);
		VexChannel &channel = setup.channels.back();

		recChanId = getRecordChannelFromTracks(antDefName, it->chanLink, ch2tracks, stream, nRecordChan);
		if(recChanId >= 0)
		{
			if(channel.bbcBandwidth - stream.sampRate/2 > 1e-6)
			{
				std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " has sample rate = " << stream.sampRate << " bandwidth = " << channel.bbcBandwidth << std::endl;
				std::cerr << "Sample rate must be no less than twice the bandwidth in all cases." << std::endl;

				exit(EXIT_FAILURE);
			}

			if(channel.bbcBandwidth - stream.sampRate/2 < -1e-6)
			{
				// Note: this is tested in a sanity check later.  This behavior is not always desirable.
				channel.bbcBandwidth = stream.sampRate/2;
			}

			channel.recordChan = recChanId;
			if(!channel.phaseCalName.empty())
			{
				channel.tones = pcalMap[channel.phaseCalName];
			}

			++nRecordChan;
		}
	}

	std::sort(setup.channels.begin(), setup.channels.end(), cmpRecChan);

	if(stream.nRecordChan == 0)	// then use the number of that we just counted out
	{
		stream.nRecordChan = nRecordChan;
	}

	return nWarn;
}

static int getTracksSetup(VexSetup &setup, Vex *v, const char *antDefName, const char *modeDefName, std::map<std::string,std::vector<unsigned int> > &pcalMap, std::vector<VexChannel> &freqChannels, const std::string &format)
{
	VexStream &stream = setup.streams[0];	// the first stream is created by default
	int nWarn = 0;
	int link, name;
	char *value, *units;
	void *p;
	std::map<std::string,BitAssignments> ch2tracks;		// indexed by channel link
	int nBit = 1;
	int nTrack = 0;

	p = get_all_lowl(antDefName, modeDefName, T_SAMPLE_RATE, B_FREQ, v);
	if(p)
	{
		vex_field(T_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
		fvex_double(&value, &units, &stream.sampRate);
	}

	stream.parseFormatString(format);

	if(stream.isLBAFormat())
	{
		// FIXME: This if() block may not be needed.  Perhaps watermark this and delete section if nobody reports its use by year 2025

		for(p = get_all_lowl(antDefName, modeDefName, T_FANOUT_DEF, B_TRACKS, v); p; p = get_all_lowl_next())
		{
			std::string chanLink;
			bool sign;
			int dasNum;

			vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
			chanLink = value;
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
				ch2tracks[chanLink].sign.push_back(chanNum);
			}
			else
			{
				nBit = 2;
				ch2tracks[chanLink].mag.push_back(chanNum);
			}
		}
	}
	else
	{
		for(p = get_all_lowl(antDefName, modeDefName, T_FANOUT_DEF, B_TRACKS, v); p; p = get_all_lowl_next())
		{
			std::string chanLink;
			bool sign;
			int dasNum;
			
			vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
			chanLink = value;
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
					ch2tracks[chanLink].sign.push_back(chanNum);
				}
				else
				{
					if(stream.nBit == 0)
					{
						nBit = 2;
					}
					else
					{
						nBit = stream.nBit;
					}
					ch2tracks[chanLink].mag.push_back(chanNum);
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
		}
	}

	// FIXME: what to do if nBit and nRecordChan already set but they disagree?
	stream.nRecordChan = ch2tracks.size();
	if(ch2tracks.empty())
	{
		/* here use contents of the format name, if any, to fill in what we can. */
		/* FIXME */
	}
	else
	{
		stream.nBit = nBit;
	}

	if(stream.format == VexStream::FormatMark5B || stream.format == VexStream::FormatKVN5B)
	{
		// Because Mark5B formatters can apply a bitmask, the track numbers may not be contiguous.  Here we go through and reorder track numbers in sequence, starting with 2
		reorderBitAssignments(ch2tracks, 2);
	}

	// Generate channels
	unsigned int nRecordChan = 0;
	
	for(std::vector<VexChannel>::const_iterator it = freqChannels.begin(); it != freqChannels.end(); ++it)
	{
		int recChanId;

		setup.channels.push_back(*it);
		VexChannel &channel = setup.channels.back();

		recChanId = getRecordChannelFromTracks(antDefName, it->chanLink, ch2tracks, stream, nRecordChan);
		if(recChanId >= 0)
		{
			if(channel.bbcBandwidth - stream.sampRate/2 > 1e-6)
			{
				std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " has sample rate = " << stream.sampRate << " bandwidth = " << channel.bbcBandwidth << std::endl;
				std::cerr << "Sample rate must be no less than twice the bandwidth in all cases." << std::endl;

				exit(EXIT_FAILURE);
			}

			if(channel.bbcBandwidth - stream.sampRate/2 < -1e-6)
			{
				// Note: this is tested in a sanity check later.  This behavior is not always desirable.
				channel.bbcBandwidth = stream.sampRate/2;
			}

			channel.recordChan = recChanId;
			if(!channel.phaseCalName.empty())
			{
				channel.tones = pcalMap[channel.phaseCalName];
			}

			++nRecordChan;
		}
	}

	if(stream.nRecordChan == 0)	// then use the number of that we just counted out
	{
		if(stream.nThread() > 1)
		{
			// Test that nThread divides into nRecordChan
			if(nRecordChan % stream.nThread() != 0)
			{
				std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " number of threads (" << stream.nThread() << ") does not divide into number of record channels (" << nRecordChan << ")." << std::endl;

				exit(EXIT_FAILURE);
			}
		}
		stream.nRecordChan = nRecordChan;
	}

	// This is really a hack -- VDIF is not properly supported by $TRACKS
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
	else
	{
		std::sort(setup.channels.begin(), setup.channels.end(), cmpRecChan);
	}

	return nWarn;
}

static int getBitstreamsSetup(VexSetup &setup, Vex *v, const char *antDefName, const char *modeDefName, std::map<std::string,std::vector<unsigned int> > &pcalMap, std::vector<VexChannel> &freqChannels, const std::string &format)
{
	const int MaxBitstreams = 64;
	VexStream &stream = setup.streams[0];	// the first stream is created by default
	int nWarn = 0;
	int link, name;
	char *value, *units;
	void *p;
	std::map<std::string,BitAssignments> ch2bitstreams;
	int nBit = 1;
	int nBitstream = 0;

	p = get_all_lowl(antDefName, modeDefName, T_STREAM_SAMPLE_RATE, B_BITSTREAMS, v);
	if(p)
	{
		vex_field(T_STREAM_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
		fvex_double(&value, &units, &stream.sampRate);
	}

	stream.parseFormatString(format);

	// Only Mark5B format is supported when using BITSTREAMS
	if(stream.format != VexStream::FormatMark5B)
	{
		std::cerr << "Error: bitstream setup found, but format was " << format << " , but only Mark5B formats are supported." << std::endl;
		exit(0);
	}

	for(p = get_all_lowl(antDefName, modeDefName, T_STREAM_DEF, B_BITSTREAMS, v); p; p = get_all_lowl_next())
	{
		std::string chanName;
		bool sign;
		int bitstreamNum;
		
		vex_field(T_STREAM_DEF, p, 1, &link, &name, &value, &units);
		chanName = value;
		vex_field(T_STREAM_DEF, p, 2, &link, &name, &value, &units);
		sign = (value[0] == 's');
		vex_field(T_STREAM_DEF, p, 4, &link, &name, &value, &units);
		sscanf(value, "%d", &bitstreamNum);

		if(bitstreamNum < 0 || bitstreamNum >= MaxBitstreams)
		{
			std::cerr << "Error: Antenna " << antDefName << " Mode " << modeDefName << " Chan " << chanName << " has bitstream number out of range." << std::endl;
			std::cerr << "  bitstream number was " << bitstreamNum << " but it must be in [0.." << (MaxBitstreams-1) << "] inclusive." << std::endl;
			exit(0);
		}

		++nBitstream;
		if(sign)
		{
			ch2bitstreams[chanName].sign.push_back(bitstreamNum);
		}
		else
		{
			nBit = 2;
			ch2bitstreams[chanName].mag.push_back(bitstreamNum);
		}
	}
	if(ch2bitstreams.empty())
	{
		std::cerr << "Error: Antenna " << antDefName << " Mode " << modeDefName << " has no bitstreams defined." << std::endl;
		exit(0);
	}

	stream.nBit = nBit;
	stream.nRecordChan = ch2bitstreams.size();
	// verify sign-mag are all consecutive and sanely populated
	if(nBit == 2)
	{
		for(std::map<std::string,BitAssignments>::const_iterator it = ch2bitstreams.begin(); it != ch2bitstreams.end(); ++it)
		{
			const BitAssignments &ba = it->second;
			if(ba.sign.size() != 1 || ba.mag.size() != 1)
			{
				std::cerr << "Error: Antenna " << antDefName << " Mode " << modeDefName << " Chan " << it->first << " does not have exactly one sign and one mag bit assigned." << std::endl;
				std::cerr << "  nSign = " << ba.sign.size() <<" nMag = " << ba.mag.size() << std::endl;
				exit(0);
			}
			else if(ba.mag[0] != ba.sign[0] + 1 || ba.sign[0] % 2 == 1)
			{
				std::cerr << "Error: Antenna " << antDefName << " Mode " << modeDefName << " Chan " << it->first << " violates vex2difx's constraint on bit stream assignments." << std::endl;
				std::cerr << "  2-bit data must have the sign bit on an even channel with the magnitude bit in the next bitstream." << std::endl;
				std::cerr << "  Sign assignment = " << ba.sign[0] <<" Mag assignemnt = " << ba.mag[0] << std::endl;
				exit(0);
			}
		}
	}
	else
	{
		for(std::map<std::string,BitAssignments>::const_iterator it = ch2bitstreams.begin(); it != ch2bitstreams.end(); ++it)
		{
			const BitAssignments &ba = it->second;
			if(ba.sign.size() != 1 || ba.mag.size() != 0)
			{
				std::cerr << "Error: Antenna " << antDefName << " Mode " << modeDefName << " Chan " << it->first << " does not have exactly one sign and zero mag bit assigned." << std::endl;
				std::cerr << "  nSign = " << ba.sign.size() <<" nMag = " << ba.mag.size() << std::endl;
				exit(0);
			}
		}
	}

	// Because Mark5B formatters can apply a bitmask, the track numbers may not be contiguous.  Here we go through and reorder track numbers in sequence, starting with 0
	reorderBitAssignments(ch2bitstreams, 0);

	// Generate channels
	unsigned int nRecordChan = 0;
	
	for(std::vector<VexChannel>::const_iterator it = freqChannels.begin(); it != freqChannels.end(); ++it)
	{
		int recChanId;

		setup.channels.push_back(*it);
		VexChannel &channel = setup.channels.back();

		recChanId = getRecordChannelFromBitstreams(antDefName, it->chanLink, ch2bitstreams, stream, nRecordChan);
		if(recChanId >= 0)
		{
			if(channel.bbcBandwidth - stream.sampRate/2 > 1e-6)
			{
				std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " has sample rate = " << stream.sampRate << " bandwidth = " << channel.bbcBandwidth << std::endl;
				std::cerr << "Sample rate must be no less than twice the bandwidth in all cases." << std::endl;

				exit(EXIT_FAILURE);
			}

			if(channel.bbcBandwidth - stream.sampRate/2 < -1e-6)
			{
				// Note: this is tested in a sanity check later.  This behavior is not always desirable.
				channel.bbcBandwidth = stream.sampRate/2;
			}

			channel.recordChan = recChanId;
			if(!channel.phaseCalName.empty())
			{
				channel.tones = pcalMap[channel.phaseCalName];
			}

			++nRecordChan;
		}
	}

	return nWarn;
}

static int getDatastreamsSetup(VexSetup &setup, Vex *v, const char *antDefName, const char *modeDefName, std::map<std::string,std::vector<unsigned int> > &pcalMap, std::vector<VexChannel> &freqChannels, const std::string &format)
{
	int nWarn = 0;
	int link, name;
	char *value, *units;
	void *p;
	int nStream = 0;	

	// Loop over datastreams
	for(p = get_all_lowl(antDefName, modeDefName, T_DATASTREAM, B_DATASTREAMS, v); p; p = get_all_lowl_next())
	{
		if(nStream > 0)
		{
			setup.streams.push_back(VexStream());
		}

		VexStream &stream = setup.streams[nStream];

		vex_field(T_DATASTREAM, p, 1, &link, &name, &value, &units);
		if(!value)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : a datastream parameter lacks a link name." << std::endl;

			exit(EXIT_FAILURE);
		}
		stream.streamLink = value;

		vex_field(T_DATASTREAM, p, 2, &link, &name, &value, &units);
		if(!value)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : datastream " << stream.streamLink << " lacks a data format specification." << std::endl;

			exit(EXIT_FAILURE);
		}
		if(strcasecmp(value, "VDIF") == 0)
		{
			stream.parseFormatString("VDIF");
		}
		else if(strcasecmp(value, "VDIF_legacy") == 0)
		{
			stream.parseFormatString("VDIFL");
		}
		else if(strcasecmp(value, "CODIF") == 0)
		{
			stream.parseFormatString("CODIF");
		}
		else
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : datastream " << stream.streamLink << " has a data format specification that is not handled: " << value << " ." << std::endl;

			exit(EXIT_FAILURE);
		}

		stream.threads.clear();	// Just make sure these are left undefined; they will be completely defined in the code that follows

		vex_field(T_DATASTREAM, p, 3, &link, &name, &value, &units);
		if(value && strlen(value) > 0)
		{
			stream.streamName = value;
		}

		++nStream;
	}

	// Loop over threads
	for(p = get_all_lowl(antDefName, modeDefName, T_THREAD, B_DATASTREAMS, v); p; p = get_all_lowl_next())
	{
		VexStream *stream;
		int threadId;

		vex_field(T_THREAD, p, 1, &link, &name, &value, &units);
		if(!value)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : a thread parameter lacks a datastream link." << std::endl;

			exit(EXIT_FAILURE);
		}
		stream = setup.getVexStreamByLink(value);
		if(!stream)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : a thread parameter datastream link does not point to a datastream." << std::endl;

			exit(EXIT_FAILURE);
		}

		vex_field(T_THREAD, p, 3, &link, &name, &value, &units);
		threadId = atoi(value);
		if(find(stream->threads.begin(), stream->threads.end(), threadId) != stream->threads.end())
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : a duplicate thread number was encountered." << std::endl;

			exit(EXIT_FAILURE);
		}
		stream->threads.push_back(VexThread(threadId));
		VexThread &T = stream->threads.back();
		if(stream->nThread() > 1)
		{
			stream->singleThread = false;
		}

		vex_field(T_THREAD, p, 2, &link, &name, &value, &units);
		T.threadLink = value;

		vex_field(T_THREAD, p, 4, &link, &name, &value, &units);
		T.nChan = atoi(value);
		stream->nRecordChan += T.nChan;

		vex_field(T_THREAD, p, 5, &link, &name, &value, &units);
		fvex_double(&value, &units, &T.sampRate);
		if(stream->sampRate == 0.0)
		{
			stream->sampRate = T.sampRate;
		}
		else if(stream->sampRate != T.sampRate)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : sample rate mismatch: " << stream->sampRate << " Hz != " << T.sampRate << " Hz." << std::endl;

			exit(EXIT_FAILURE);
		}

		vex_field(T_THREAD, p, 6, &link, &name, &value, &units);
		T.nBit = atoi(value);
		if(stream->nBit == 0)
		{
			stream->nBit = T.nBit;
		}
		else if(stream->nBit != T.nBit)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : nBit mismatch: " << stream->nBit << " != " << T.nBit << " ." << std::endl;

			exit(EXIT_FAILURE);
		}

		vex_field(T_THREAD, p, 7, &link, &name, &value, &units);
		if(value)
		{
			if(strcasecmp(value, "real") == 0)
			{
				stream->dataSampling = SamplingReal;
			}
			else if(strcasecmp(value, "complex") == 0)
			{
				stream->dataSampling = SamplingComplex;
			}
			else if(strcasecmp(value, "complexDSB") == 0)
			{
				stream->dataSampling = SamplingComplexDSB;
			}
			else
			{
				std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : data representation [" << value << "] is unrecognized.  Kegal values are 'real', 'complex' and 'complexDSV'." << std::endl;

				exit(EXIT_FAILURE);
			}
		}

		vex_field(T_THREAD, p, 8, &link, &name, &value, &units);
		T.dataBytes = atoi(value);
		if(stream->format == VexStream::FormatLegacyVDIF)
		{
			stream->VDIFFrameSize = T.dataBytes + 16;
		}
		else
		{
			stream->VDIFFrameSize = T.dataBytes + 32;
		}
	}

	// Go through each datastream: sort threadId list and set startRecordChan for each
	for(std::vector<VexStream>::iterator it = setup.streams.begin(); it != setup.streams.end(); ++it)
	{
		int startRecordChan;

		startRecordChan = 0;
		sort(it->threads.begin(), it->threads.end());
		for(std::vector<VexThread>::iterator t = it->threads.begin(); t != it->threads.end(); ++t)
		{
			t->startRecordChan = startRecordChan;
			startRecordChan += t->nChan;
		}
	}

	// Loop over channels
	for(p = get_all_lowl(antDefName, modeDefName, T_CHANNEL, B_DATASTREAMS, v); p; p = get_all_lowl_next())
	{
		VexStream *stream;
		VexThread *thread;
		VexChannel *channel;
		char *chanLink;
		int threadChan;

		vex_field(T_CHANNEL, p, 1, &link, &name, &value, &units);
		if(!value)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel parameter without datastream link." << std::endl;

			exit(EXIT_FAILURE);
		}
		stream = setup.getVexStreamByLink(value);
		if(!stream)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel parameter with datastream link that does not correspond: " << value << " ." << std::endl;

			exit(EXIT_FAILURE);
		}

		vex_field(T_CHANNEL, p, 2, &link, &name, &value, &units);
		if(!value)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel parameter without a thread link." << std::endl;

			exit(EXIT_FAILURE);
		}
		thread = stream->getVexThreadByLink(value);
		if(!thread)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel parameter with a thread link that does not correspond: " << value << " ." << std::endl;

			exit(EXIT_FAILURE);
		}

		vex_field(T_CHANNEL, p, 3, &link, &name, &chanLink, &units);
		if(!chanLink || chanLink[0] == 0)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel parameter without channel link." << std::endl;

			exit(EXIT_FAILURE);
		}

		vex_field(T_CHANNEL, p, 4, &link, &name, &value, &units);
		if(!value)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel without thread number." << std::endl;

			exit(EXIT_FAILURE);
		}
		threadChan = atoi(value);
		if(threadChan < 0 || threadChan >= thread->nChan)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : has channel number," << threadChan << ", out of range [0.." << (thread->nChan-1) << "]." << std::endl;

			exit(EXIT_FAILURE);
		}

		channel = getVexChannelByLink(freqChannels, chanLink);
		if(!channel)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " : referenced channel " << chanLink << " does not exist in $FREQ block." << std::endl;
			std::cerr << "Freq block had the following entries:" << std::endl;
			for(std::vector<VexChannel>::const_iterator it = freqChannels.begin(); it != freqChannels.end(); ++it)
			{
				std::cerr << "  " << *it << std::endl;
			}

			exit(EXIT_FAILURE);
		}

		setup.channels.push_back(*channel);
		channel = &setup.channels.back();

		if(channel->bbcBandwidth - stream->sampRate/2 > 1e-6)
		{
			std::cerr << "Error: " << modeDefName << " antenna " << antDefName << " has sample rate = " << stream->sampRate << " bandwidth = " << channel->bbcBandwidth << " ." << std::endl;
			std::cerr << "Sample rate must be no less than twice the bandwidth in all cases." << std::endl;

			exit(EXIT_FAILURE);
		}

		if(channel->bbcBandwidth - stream->sampRate/2 < -1e-6)
		{
			// Note: this is tested in a sanity check later.  This behavior is not always desirable.
			channel->bbcBandwidth = stream->sampRate/2;
		}

		channel->recordChan = thread->startRecordChan + threadChan;
		if(!channel->phaseCalName.empty())
		{
			channel->tones = pcalMap[channel->phaseCalName];
		}
	}

	return nWarn;
}

static int getModes(VexData *V, Vex *v)
{
	int nWarn = 0;

	for(const char *modeDefName = get_mode_def(v); modeDefName; modeDefName = get_mode_def_next())
	{
		// don't bother building up modes that are not used
		if(!V->usesMode(modeDefName))
		{
			continue;
		}

		VexMode &mode = *(V->newMode());
		mode.defName = modeDefName;

		for(unsigned int a = 0; a < V->nAntenna(); ++a)
		{
			std::string format;
			const char *antDefName = V->getAntenna(a)->defName.c_str();
			VexSetup::SetupType type;
			std::map<std::string,std::vector<unsigned int> > pcalMap;
			std::vector<VexChannel> freqChannels;		// list of channels from relevant $FREQ section

			type = getSetupType(v, format, antDefName, modeDefName);

			if(type == VexSetup::SetupIncomplete)
			{
				std::cerr << "Note: Incomplete description for " << antDefName << " in mode " << modeDefName << ". The vex file might need editing.  This antenna/mode will be ignored." << std::endl;
				continue;
			}

			// if we made it this far the antenna is involved in this mode
			VexSetup &setup = mode.setups[V->getAntenna(a)->name];	// This creates the new entry  FIXME: really this should be defName, not name...

			setup.type = type;

			nWarn += collectIFInfo(setup, V, v, antDefName, modeDefName);
			nWarn += collectFreqChannels(freqChannels, setup, mode, v, antDefName, modeDefName);
			nWarn += collectPcalInfo(pcalMap, V, v, antDefName, modeDefName);
			nWarn += collectExtensions(setup, v, antDefName, modeDefName);

			switch(type)
			{
			case VexSetup::SetupTracks:
				nWarn += getTracksSetup(setup, v, antDefName, modeDefName, pcalMap, freqChannels, format);
				break;
			case VexSetup::SetupBitstreams:
				nWarn += getBitstreamsSetup(setup, v, antDefName, modeDefName, pcalMap, freqChannels, format);
				break;
			case VexSetup::SetupDatastreams:
				nWarn += getDatastreamsSetup(setup, v, antDefName, modeDefName, pcalMap, freqChannels, format);
				break;
			case VexSetup::SetupS2:
				nWarn += getS2Setup(setup, v, antDefName, modeDefName, pcalMap, freqChannels, format);
				break;
			default:
				std::cerr << "Setup type " << VexSetup::setupTypeName[type] << " is not (yet) supported." << std::endl;
				++nWarn;
			}

		} // End of antenna loop
	} // End of mode loop

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

	start = V->getEarliestScanStart() - 1.0/86400.0;
	stop = V->getLatestScanStop() + 1.0/86400.0;

	if(v->version)
	{
		lowl *w;

		w = (lowl *)(v->version->ptr);

		V->setVersion((char *)(w->item));
	}
	else
	{
		std::cerr << "Warning: VEX_REV not set.  That is bad." << std::endl;

		++nWarn;
	}

	block = find_block(B_EXPER, v);

	if(!block)
	{
		std::cerr << "Warning: no EXPER block found in the vex file.  That is bad." << std::endl;

		V->setExper("RANDOM", Interval(start, stop));
		
		++nWarn;
	}
	else
	{
		std::string experimentName;
		std::string experimentSegment;
		
		for(Llist *defs=((struct block *)block->ptr)->items; defs; defs=defs->next)
		{
			int statement;
			Llist *lowls, *refs;
			Exper_name *en;
			
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

			en = (Exper_name *)(((Lowl *)lowls->ptr)->item);

			experimentName = en->name;
			experimentSegment = (en->segment ? en->segment : "");

			lowls = find_lowl(refs, T_EXPER_NOMINAL_START);
			if(lowls)
			{
				void *p;
				
				p = (((Lowl *)lowls->ptr)->item);
				start = vexDate((char *)p);
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
				stop = vexDate((char *)p);
			}
			else
			{
				std::cerr << "Note: The vex file has no exper_nominal_stop parameter defined in the EXPER section.  Making assumptions..." << std::endl;
			}
		}

		Upper(experimentName);
		Upper(experimentSegment);

		V->setExper(experimentName, experimentSegment, Interval(start, stop));
	}

	return nWarn;
}


VexData *loadVexFile(const std::string &vexFile, unsigned int *numWarnings)
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

	nWarn += getExper(V, v);
	nWarn += getExtensions(V, v);
	nWarn += getAntennas(V, v);
	nWarn += getSources(V, v);
	nWarn += getScans(V, v);
	nWarn += getModes(V, v);
	nWarn += getVSNs(V, v);
	nWarn += getEOPs(V, v);
	*numWarnings = *numWarnings + nWarn;

	return V;
}
