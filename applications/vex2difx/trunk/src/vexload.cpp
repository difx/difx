#include <cstring>
#include <cctype>
#include <algorithm>
#include <unistd.h>
#include "util.h"
#include "corrparams.h"
#include "vextables.h"
#include "../vex/vex.h"
#include "../vex/vex_parse.h"

class Tracks
{
public:
	vector<int> sign;
	vector<int> mag;
};

static int getRecordChannel(const string chanName, const map<string,Tracks>& ch2tracks, const VexFormat& F, int n)
{
	int delta, track;
	map<string,Tracks>::const_iterator it;

	if(F.format == "VLBA1_1" || F.format == "MKIV1_1" ||
	   F.format == "VLBA1_2" || F.format == "MKIV1_2" ||
	   F.format == "VLBA1_4" || F.format == "MKIV1_4")
	{
		it = ch2tracks.find(chanName);

		if(it == ch2tracks.end())
		{
			return -1;
		}

		const Tracks& T = it->second;
		delta = 2*(T.sign.size() + T.mag.size());
		track = T.sign[0];

		if(track < 34)
		{
			if(track % 2 == 0) 
				return (track-2)/delta;
			else 
				return (track+29)/delta;
		}
		else
		{
			if(track % 2 == 0)
				return (track+30)/delta;
			else
				return (track+61)/delta;
		}
	}
	else if(F.format == "S2")
	{
		return n;
	}
	else
	{
		cerr << "Error: format " << F.format << " is not yet supported" << endl;
		cerr << "Contact developer." << endl;
		exit(0);
	}

	return -1;
}

int DOYtoMJD(int year, int doy)
{
	return doy-678576+365*(year-1)+(year-1)/4-(year-1)/100+(year-1)/400;
}

double vexDate(char *value)
{
	int ints[4];
	double mjd, seconds = 0.0;

	for(int i = 0; i < 4; i++)
	{
		ints[i] = 0;
	}

	sscanf(value, "%dy%dd%dh%dm%lfs", ints, ints+1, ints+2, ints+3, &seconds);
	mjd = DOYtoMJD(ints[0], ints[1]);
	mjd += ints[2]/24.0 + ints[3]/1440.0 + seconds/86400.0;

	return mjd;
}

int getAntennas(VexData *V, Vex *v, const CorrParams& params)
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
		A->nameInVex = stn;
		Upper(A->name);

		p = (struct site_position *)get_station_lowl(stn, T_SITE_POSITION, B_SITE, v);
		fvex_double(&(p->x->value), &(p->x->units), &A->x);
		fvex_double(&(p->y->value), &(p->y->units), &A->y);
		fvex_double(&(p->z->value), &(p->z->units), &A->z);

		p = (struct site_position *)get_station_lowl(stn, T_SITE_VELOCITY, B_SITE, v);
		if(p)
		{
			fvex_double(&(p->x->value), &(p->x->units), &A->dx);
			fvex_double(&(p->y->value), &(p->y->units), &A->dy);
			fvex_double(&(p->z->value), &(p->z->units), &A->dz);

			// Correct for various definitions of year: yr_vex = 365.2524  yr_goddard=365.25
			A->dx *= (365.25/365.2425);
			A->dy *= (365.25/365.2425);
			A->dz *= (365.25/365.2425);
		}
		else
		{
			A->dx = A->dy = A->dz = 0.0;
		}

		q = (struct axis_type *)get_station_lowl(stn, T_AXIS_TYPE, B_ANTENNA, v);
		A->axisType = string(q->axis1) + string(q->axis2);

		r = (struct dvalue *)get_station_lowl(stn, T_SITE_POSITION_EPOCH, B_SITE, v);
		if(r)
		{
			char *value;
			char *units;
			int name;
			int link;
			vex_field(T_SITE_POSITION_EPOCH, (void *)r, 1, &link, &name, &value, &units);

			A->posEpoch = atof(value);
		}
		else
		{
			A->posEpoch = 0.0;
		}

		r = (struct dvalue *)get_station_lowl(stn, T_AXIS_OFFSET, B_ANTENNA, v);
		fvex_double(&(r->value), &(r->units), &A->axisOffset);

		if(block)
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
						if(C->offset)
						{
							fvex_double(&(C->offset->value), &(C->offset->units), &clock.offset);
						}
						if(C->rate && C->origin) 
						{
							clock.rate = atof(C->rate->value);
							clock.offset_epoch = vexDate(C->origin);
						}
					}
				}
			}
		}
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

		p = (char *)get_source_lowl(src, T_REF_COORD_FRAME, v);
		if(strcmp(p, "J2000") != 0)
		{
			cerr << "Error: only J2000 ref frame is supported." << endl;
			exit(0);
		}
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
	string stationName;
	char *value, *units;
	double mjd;
	double startScan, stopScan;
	double startAnt, stopAnt;
	Llist *L;
	map<string, VexInterval> stations;

	for(L = (Llist *)get_scan(&scanId, v);
	    L != 0;
	    L = (Llist *)get_scan_next(&scanId))
	{
		map<string, double> antStart, antStop;
		map<string, double>::const_iterator it;

		p = get_scan_start(L);
		vex_field(T_START, p, 1, &link, &name, &value, &units);
		mjd = vexDate(value);
		startScan = 1e99;
		stopScan = 0.0;
		stations.clear();
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

			stations[stationName] = VexInterval(startAnt, stopAnt);

			antStart[stationName] = startAnt;
			antStop[stationName] = stopAnt;
		}

		if(stations.size() < params.minSubarraySize)
		{
			continue;
		}

		string scanName(scanId);
		string sourceName((char *)get_scan_source(L));
		string modeName((char *)get_scan_mode(L));

		string setupName = params.findSetup(scanName, sourceName, modeName, ' ', 0);

		if(setupName == "" || setupName == "skip")
		{
			continue;
		}

		if(params.getCorrSetup(setupName) == 0)
		{
			cerr << "Error: setup " << setupName << " not defined!" << endl;
			exit(0);
		}

		if(params.mjdStart > stopScan || params.mjdStop < startScan)
		{
			cerr << "FYI: skipping scan " << scanName << " : out of time range." << endl;
			continue;
		}

		// Make scan
		S = V->newScan();
		S->timeRange = VexInterval(startScan, stopScan);
		S->name = scanName;
		S->stations = stations;
		S->modeName = modeName;
		S->sourceName = sourceName;
		S->setupName = setupName;

		// Add to event list
		V->addEvent(startScan, VexEvent::SCAN_START, scanId, scanId);
		V->addEvent(stopScan,  VexEvent::SCAN_STOP,  scanId, scanId);
		for(it = antStart.begin(); it != antStart.end(); it++)
		{
			V->addEvent(it->second, VexEvent::ANT_SCAN_START, it->first, scanId);
		}
		for(it = antStop.begin(); it != antStop.end(); it++)
		{
			V->addEvent(it->second, VexEvent::ANT_SCAN_STOP, it->first, scanId);
		}
	}
	
	return 0;
}

int getModes(VexData *V, Vex *v, const CorrParams& params)
{
	VexMode *M;
	void *p;
	char *modeId;
	int link, name;
	char *value, *units;
	double freq, bandwidth, sampRate;
	char sideBand;
	char polarization;
	string format, chanName;
	int chanNum;
	int nTrack, fanout;
	int nBit;
	int dasNum;
	int subbandId, recChanId;
	bool sign;
	map<string,char> if2pol;
	map<string,char> bbc2pol;
	map<string,Tracks> ch2tracks;

	for(modeId = get_mode_def(v);
	    modeId;
	    modeId = get_mode_def_next())
	{
		// don't bother building up modes that are not used
		if(!V->usesMode(modeId))
		{
			continue;
		}

		M = V->newMode();
		M->name = modeId;

		// get FREQ info
		for(int a = 0; a < V->nAntenna(); a++)
		{
			const string& antName = V->getAntenna(a)->nameInVex;
			if2pol.clear();
			bbc2pol.clear();
			ch2tracks.clear();
			nTrack = 0;
			nBit = 1;
			VexFormat& F = M->formats[V->getAntenna(a)->name] = VexFormat();

			// Get sample rate
			p = get_all_lowl(antName.c_str(), modeId, T_SAMPLE_RATE, B_FREQ, v);
			if(p == 0)
			{
				continue;
			}
			vex_field(T_SAMPLE_RATE, p, 1, &link, &name, &value, &units);
			fvex_double(&value, &units, &sampRate);

			M->sampRate = sampRate;

			// Get IF to pol map for this antenna
			for(p = get_all_lowl(antName.c_str(), modeId, T_IF_DEF, B_IF, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_BBC_ASSIGN, p, 3, &link, &name, &value, &units);
				polarization = value[0];
				vex_field(T_IF_DEF, p, 1, &link, &name, &value, &units);
				if2pol[value] = polarization;
			}

			// Get BBC to pol map for this antenna
			for(p = get_all_lowl(antName.c_str(), modeId, T_BBC_ASSIGN, B_BBC, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_BBC_ASSIGN, p, 3, &link, &name, &value, &units);
				polarization = if2pol[value];
				vex_field(T_BBC_ASSIGN, p, 1, &link, &name, &value, &units);
				bbc2pol[value] = polarization;
			}

			// Get datastream assignments and formats

			// Is it a Mark5 mode?
			p = get_all_lowl(antName.c_str(), modeId, T_TRACK_FRAME_FORMAT, B_TRACKS, v);
			if(p)
			{
				vex_field(T_TRACK_FRAME_FORMAT, p, 1, &link, &name, &value, &units);
				F.format = string(value);
				if(F.format == "Mark4")
				{
					F.format = "MKIV";
				}

				for(p = get_all_lowl(antName.c_str(), modeId, T_FANOUT_DEF, B_TRACKS, v);
				    p;
				    p = get_all_lowl_next())
				{
					vex_field(T_FANOUT_DEF, p, 2, &link, &name, &value, &units);
					chanName = value;
					vex_field(T_FANOUT_DEF, p, 3, &link, &name, &value, &units);
					sign = (value[0] == 's');
					vex_field(T_FANOUT_DEF, p, 4, &link, &name, &value, &units);
					sscanf(value, "%d", &dasNum);

					for(int k = 5; k < 9; k++)
					{
						if(vex_field(T_FANOUT_DEF, p, k, &link, &name, &value, &units) < 0)
						{
							break;
						}
						nTrack++;
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
						cerr << "Error: fanout=" << fanout << " not legal for format " << F.format << endl;
						exit(0);
				}
				F.nRecordChan = ch2tracks.size();
				F.nBit = nBit;
			}

			// Is it an S2 mode?
			p = get_all_lowl(antName.c_str(), modeId, T_S2_RECORDING_MODE, B_TRACKS, v);
			if(p)
			{
				size_t f, g;

				vex_field(T_S2_RECORDING_MODE, p, 1, &link, &name, &value, &units);
				string s2mode(value);
				F.format = "S2";

				f = s2mode.find_last_of("x");
				g = s2mode.find_last_of("-");

				if(f < 0 || g < 0 || f > g)
				{
					cerr << "Error: malformed S2 mode : " << string(value) << endl;
					exit(0);
				}

				string chans = s2mode.substr(f+1, g-f-1);
				string bits = s2mode.substr(g+1);

				F.nRecordChan = atoi(chans.c_str()); //bbc2pol.size();
				F.nBit = atoi(bits.c_str());
			}

			if(F.nRecordChan != bbc2pol.size())
			{
				cerr << "Warning: F.nRecordChan != bbc2pol.size()  (" << F.nRecordChan << ", " << bbc2pol.size() << ")" << endl;
			}

			// Get rest of Subband information
			int i = 0;
			
			for(p = get_all_lowl(antName.c_str(), modeId, T_CHAN_DEF, B_FREQ, v);
			    p;
			    p = get_all_lowl_next())
			{
				vex_field(T_CHAN_DEF, p, 2, &link, &name, &value, &units);
				fvex_double(&value, &units, &freq);

				vex_field(T_CHAN_DEF, p, 3, &link, &name, &value, &units);
				sideBand = value[0];
				
				vex_field(T_CHAN_DEF, p, 4, &link, &name, &value, &units);
				fvex_double(&value, &units, &bandwidth);

				vex_field(T_CHAN_DEF, p, 6, &link, &name, &value, &units);
				subbandId = M->addSubband(freq, bandwidth, sideBand, bbc2pol[value]);

				vex_field(T_CHAN_DEF, p, 5, &link, &name, &value, &units);
				recChanId = getRecordChannel(value, ch2tracks, F, i);
				if(recChanId >= 0)
				{
					F.ifs.push_back(VexIF());
					F.ifs.back().subbandId = subbandId;
					F.ifs.back().recordChan = recChanId;
				}

				i++;
			}

			if(i != F.nRecordChan)
			{
				cerr << "Warning: nchan != F.nRecordChan!" << endl;
			}
		}
	}

	return 0;
}

static void fixOhs(string &str)
{
	unsigned int i;

	for(i = 0; i < str.length(); i++)
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

int getVSN(VexData *V, Vex *v, const CorrParams& params, const char *station)
{
	Vsn *p;
	llist *block;
	Llist *defs;
	Llist *lowls;
	double start, stop;

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
		start = vexDate(p->start);
		stop = vexDate(p->stop);
		
		V->addVSN(antName, vsn, start, stop);
		V->addEvent(start, VexEvent::RECORD_START, antName);
		V->addEvent(stop, VexEvent::RECORD_STOP, antName);
	}

	return 0;
}

int getVSNs(VexData *V, Vex *v, const CorrParams& params)
{
	int r;

	for(char *stn = get_station_def(v); stn; stn=get_station_def_next())
	{
		r = getVSN(V, v, params, stn);
	}

	return 0;
}

int getEOPs(VexData *V, Vex *v, const CorrParams& params)
{
	llist *block;
	Llist *defs;
	Llist *lowls, *refs;
	int statement;
	int link, name;
	char *value, *units;
	void *p;
	dvalue *r;
	double tai_utc, ut1_utc, x_wobble, y_wobble;
	int nEop;
	double refEpoch, interval;
	VexEOP *E;

	block = find_block(B_EOP, v);

	if(!block)
	{
		return -1;
	}

	for(defs=((struct block *)block->ptr)->items;
	    defs;
	    defs=defs->next)
	{
		statement = ((Lowl *)defs->ptr)->statement;
		if(statement == T_COMMENT)
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

		lowls = find_lowl(refs, T_EOP_INTERVAL);
		r = (struct dvalue *)(((Lowl *)lowls->ptr)->item);
		fvex_double(&r->value, &r->units, &interval);

		for(int i = 0; i < nEop; i++)
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
	return 0;
}

int getExper(VexData *V, Vex *v, const CorrParams& params)
{
	llist *block;
	Llist *defs;
	Llist *lowls, *refs;
	int statement;
	void *p;
	double start=0.0, stop=0.0;
	string name;

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

		if(statement == T_COMMENT)
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

	V->setExper(name, start, stop);

	return 0;
}

VexData *loadVexFile(const CorrParams& P)
{
	VexData *V;
	Vex *v;
	int r;

	r = vex_open(P.vexFile.c_str(), &v);
	if(r != 0)
	{
		return 0;
	}

	V = new VexData();

	V->setDirectory(P.vexFile.substr(0, P.vexFile.find_last_of('/')));

	getAntennas(V, v, P);
	getSources(V, v, P);
	getScans(V, v, P);
	getModes(V, v, P);
	getVSNs(V, v, P);
	getEOPs(V, v, P);
	getExper(V, v, P);

	return V;
}
