/***************************************************************************
 *   Copyright (C) 2011 by Walter Brisken                                  *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/vex2difx.cpp $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <glob.h>
#include <string>
#include <vector>
#include "vextables.h"
#include "vexload.h"
#include "util.h"

const char program[] = "rdbetsys";
const char version[] = "0.1";
const char author[]  = "Walter Brisken";
const char verdate[] = "20111130";

const char defaultSwitchedPowerPath[] = "/home/swc/difx/tsys";
const double defaultTsysInterval = 15.0;	// Seconds
const int MaxFilenameLength = 256;

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("Usage: %s [options] <input vex file> <output tsys file>\n\n", pgm);
	printf("where options can include:\n\n");
	printf("  --help\n");
	printf("  -h          print help info and quit\n\n");
	printf("  --verbose\n");
	printf("  -v          be more verbose in operation\n\n");
	printf("  --quiet\n");
	printf("  -q          be less verbose in operation\n\n");
	printf("Note that env. var. TCAL_FILE must be set to point to TCal data\n\n");
}

void mjd2yearday(int mjd, int *year, int *doy)
{
	const long long mjdAtUnix0 = 40587LL;
	time_t t;
	struct tm tt;

	t = (mjd-mjdAtUnix0)*86400LL;
	gmtime_r(&t, &tt);

	if(year)
	{
		*year = tt.tm_year+1900;
	}
	if(doy)
	{
		*doy = tt.tm_yday+1;
	}	
}

double filename2fracday(const char *fn)
{
	char *f;
	int start = -1;
	int num[5];
	int n = 0;
	double v = -1.0;

	f = strdup(fn);

	for(int i = 0; f[i] && n < 5; ++i)
	{
		if(f[i] == '_' || f[i] == '.')
		{
			f[i] = 0;
			if(start >= 0)
			{
				num[n] = atoi(f+start);
				++n;
			}
			start = i+1;
		}
	}

	if(n == 5)
	{
		v = num[2]/24.0 + num[3]/1440.0 + num[4]/86400.0;
	}

	free(f);

	return v;
}

std::string genFileList(const char *switchedPowerPath, const char *stn, const VexInterval &timeRange)
{
	std::string fileList = "";
	glob_t globbuf;

	for(int mjd = static_cast<int>(timeRange.mjdStart); mjd < timeRange.mjdStop; ++mjd)
	{
		int year, doy;
		char match[MaxFilenameLength];

		mjd2yearday(mjd, &year, &doy);
		snprintf(match, MaxFilenameLength, "%s/%s_%d_%03d_*.switched_power.gz", switchedPowerPath, stn, year, doy);

		if(glob(match, 0, 0, &globbuf) == 0)
		{
			if(globbuf.gl_pathc > 0)
			{
				VexInterval fileRange;

				fileRange.mjdStop = filename2fracday(globbuf.gl_pathv[0]) + mjd;
				for(unsigned int i = 0; i < globbuf.gl_pathc; ++i)
				{
					fileRange.mjdStart = fileRange.mjdStop;
					if(i+1 < globbuf.gl_pathc)
					{
						fileRange.mjdStop = filename2fracday(globbuf.gl_pathv[i+1]) + mjd;
					}
					else
					{
						fileRange.mjdStop = 1.0 + mjd;
					}

					if(timeRange.overlap(fileRange))
					{
						if(fileList.length() > 0)
						{
							fileList += " ";
						}
						fileList += globbuf.gl_pathv[i];
					}

				}
			}
			globfree(&globbuf);
		}
	}

	return fileList;
}




typedef struct
{
	char antenna[8];
	char receiver[8];
	float freq, tcalR, tcalL;
} TCal;


/* This is somewhat ugly.  Perhaps move these internal to some function? */
TCal *tCals = 0;
int nTcal;


int loadTcals(const char *tcalFilename)
{
	const int MaxLineLength = 100;
	char line[MaxLineLength];
	char *rv;
	FILE *in;
	int nAlloc = 0;

	if(tcalFilename == 0)
	{
		return -1;
	}
	in = fopen(tcalFilename, "r");
	if(!in)
	{
		return -2;
	}

	nAlloc = 4000;
	tCals = (TCal *)malloc(nAlloc*sizeof(TCal));

	for(int i = nTcal = 0; ; ++i)
	{
		rv = fgets(line, MaxLineLength, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		if(nTcal >= nAlloc)
		{
			nAlloc += 1000;
			tCals = (TCal *)realloc(tCals, nAlloc*sizeof(TCal));
		}
		sscanf(line, "%7s%7s%f%f%f", tCals[nTcal].antenna, tCals[nTcal].receiver,
			&tCals[nTcal].freq, &tCals[nTcal].tcalR, &tCals[nTcal].tcalL);
		++nTcal;
	}

	fclose(in);

	return 0;
}

float getTcalValue(const char *antname, float freq, char pol)
{
	int i;
	int besti;
	int secondbesti;
	float bestf, w1, w2, df;

	if(nTcal == 0)
	{
		return 1.0;
	}

	besti = -1;
	bestf = 1e9;
	df = 1e9;
	for(i = 0; i < nTcal; ++i)
	{
		df = fabs(freq - tCals[i].freq);
		if(strcasecmp(antname, tCals[i].antenna) == 0 && df < bestf)
		{
			bestf = df;
			besti = i;
		}
	}
	
	if(besti < 0)
	{
		return 1.0;
	}

	bestf = tCals[besti].freq;

	secondbesti = -1;
	if(besti > 0 && bestf > freq && 
	   strcmp(tCals[besti].antenna, tCals[besti-1].antenna) == 0 &&
	   strcmp(tCals[besti].receiver, tCals[besti-1].receiver) == 0)
	{
		secondbesti = besti - 1;
	}
	else if(besti < nTcal-1 && bestf < freq && 
	        strcmp(tCals[besti].antenna, tCals[besti+1].antenna) == 0 &&
	        strcmp(tCals[besti].receiver, tCals[besti+1].receiver) == 0)
	{
		secondbesti = besti + 1;
	}

	if(secondbesti >= 0)
	{
		w1 = fabs( (tCals[secondbesti].freq - freq)/(tCals[secondbesti].freq - bestf) );
		w2 = fabs( (tCals[besti].freq - freq)/(tCals[secondbesti].freq - bestf) );
	}
	else
	{
		secondbesti = besti;
		w1 = 1.0;
		w2 = 0.0;
	}

	if(pol == 'r' || pol == 'R')
	{
		return w1*tCals[besti].tcalR + w2*tCals[secondbesti].tcalR;
	}
	else if(pol == 'l' || pol == 'L')
	{
		return w1*tCals[besti].tcalL + w2*tCals[secondbesti].tcalL;
	}
	else
	{
		return 1.0;
	}
}


class TsysAverager
{
public:
	TsysAverager() : pOn(0.0), pOff(0.0), n(0), receiver("0cm") {}
	void reset() { pOn = pOff = 0.0; n = 0; }
	double Tsys() const { return (n > 0) ? (0.5*tCal*(pOn + pOff)/(pOn - pOff)) : 999.0; }
	void set(const VexChannel &vc);
	void print() const;
	double freq;	// signed, in MHz
	double bw;	// MHz
	double tCal;
	double pOn;
	double pOff;
	int n;
	std::string receiver;
	char pol;
};

void TsysAverager::print() const
{
	std::cout << "freq=" << freq << " bw=" << bw << " pol=" << pol << std::endl;
}

class TsysAccumulator
{
private:
	VexInterval accumTimeRange;
	int nAccum;
	FILE *out;
	std::string stn;
	std::vector<TsysAverager> chans;
public:
	TsysAccumulator();
	~TsysAccumulator();
	void setOutput(FILE *outFile);
	void setStation(const std::string &stnName);
	void setup(const VexSetup &vexSetup, const string &str);
	void flush();
	void feed(const VexInterval &lineTimeRange, const char *data);
};

TsysAccumulator::TsysAccumulator() : nAccum(0), out(0)
{
}

TsysAccumulator::~TsysAccumulator()
{
	flush();
}

void TsysAccumulator::setOutput(FILE *outFile)
{
	out = outFile;
}

void TsysAccumulator::setStation(const std::string &stnName)
{
	stn = stnName;
	Upper(stn);
}

void TsysAccumulator::setup(const VexSetup &vexSetup, const string &stn)
{
	double midFreq;
	std::vector<TsysAverager>::iterator ta;
	std::vector<VexChannel>::const_iterator vc;
	const VexFormat &format = vexSetup.format;
	flush();

	chans.resize(format.channels.size());
	for(vc = format.channels.begin(), ta = chans.begin(); vc != format.channels.end(); ++vc, ++ta)
	{
		ta->reset();

		ta->bw = vc->bbcBandwidth/1000000.0;
		ta->freq = vc->bbcFreq/1000000.0;
		midFreq = ta->freq;
		if(vc->bbcSideBand == 'L')
		{
			ta->freq = -ta->freq;
			midFreq -= ta->bw/2.0;
		}
		else
		{
			midFreq += ta->bw/2.0;
		}
		ta->pol = vexSetup.getIF(vc->ifname)->pol;
		ta->tCal = getTcalValue(stn.c_str(), midFreq, ta->pol);
	}
}

void TsysAccumulator::flush()
{
	int doy;
	int mjd;
	double day;

	if(nAccum > 0)
	{
		mjd = static_cast<int>(accumTimeRange.center());
		mjd2yearday(mjd, 0, &doy);

		day = accumTimeRange.center() - mjd + doy;

		fprintf(out, "%s %12.8f %10.8f %d", stn.c_str(), day, accumTimeRange.duration(), static_cast<int>(chans.size()));
		for(std::vector<TsysAverager>::const_iterator ta = chans.begin(); ta != chans.end(); ++ta)
		{
			fprintf(out, " %5.2f %s", ta->Tsys(), ta->receiver.c_str());
		}
		fprintf(out, "\n");
		nAccum = 0;
	}
}

void TsysAccumulator::feed(const VexInterval &lineTimeRange, const char *data)
{
	double pOn, pOff;
	double freq, bw;
	char pol[100];
	int pos;
	int n;

	if(nAccum == 0)
	{
		accumTimeRange = lineTimeRange;
	}
	else
	{
		accumTimeRange.logicalOr(lineTimeRange);
	}
	++nAccum;

	while(*data)
	{
		n = sscanf(data, "%lf%lf%s%lf%lf%n", &freq, &bw, pol, &pOn, &pOff, &pos);
		if(n != 5)
		{
			break;
		}
		data += pos;

		if(pol[0] == '.')
		{
			continue;
		}

		for(std::vector<TsysAverager>::iterator ta = chans.begin(); ta != chans.end(); ++ta)
		{
			if(ta->pol == pol[0] && 
			   fabs(ta->freq - freq) < 0.005 && 
			   fabs(ta->bw - bw) < 0.0001)
			{
				ta->pOn += pOn;
				ta->pOff += pOff;
				++ta->n;
			}
		}
	}
}

int processStation(FILE *out, const VexData &V, const string &stn, const string &fileList, const VexInterval stnTimeRange, double nominalTsysInterval, int verbose)
{
	const int MaxLineLength = 32768;	// make sure it is large enough!
	char line[MaxLineLength];
	FILE *p;
	string command;
	int nRecord = 0;
	int nSkip = 0;
	int pos, n;
	char *v;
	VexInterval lineTimeRange;
	VexInterval scanTimeRange;
	VexInterval slotTimeRange;
	unsigned int scanNum = 0;
	const VexScan *scan = 0;
	unsigned int nSlot = 0;
	double slotDeltaT = 0.0;
	TsysAccumulator TA;
	const VexMode *mode = 0;
	const VexSetup *setup = 0;

	command = "zcat " + fileList;
	if(verbose > 0)
	{
		printf("opening pipe: %s\n", command.c_str());
	}
	p = popen(command.c_str(), "r");
	if(!p)
	{
		printf("Error: cannot open pipe: %s\n", command.c_str());

		return -1;
	}

	fprintf(out, "# %s %14.8f %14.8f\n", stn.c_str(), stnTimeRange.mjdStart, stnTimeRange.mjdStop);
	fprintf(out, "# ant D.O.Y. dur(days) nRecChan (tsys, bandName)[nRecChan]\n");

	TA.setOutput(out);
	TA.setStation(stn);

	for(;;)
	{
		v = fgets(line, MaxLineLength-1, p);
		if(!v)
		{
			// End of pipe.  Really this shouldn't happen unless obsv'n ends at midnight
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}

		n = sscanf(line, "%*s%lf%lf%n", &lineTimeRange.mjdStart, &lineTimeRange.mjdStop, &pos);
		if(n < 2)
		{
			printf("Error: malformed line: %s\n", line);

			break;
		}

		if(lineTimeRange.mjdStart < scanTimeRange.mjdStart)
		{
			// We're not yet at the time of scan start
			++nSkip;
			continue;
		}
		else if(lineTimeRange.mjdStop > scanTimeRange.mjdStop)
		{
			TA.flush();
			
			do
			{
				map<string,VexInterval>::const_iterator it;

				if(scan)
				{
					++scanNum;
				}
				if(scanNum >= V.nScan())
				{
					break;
				}
				scan = V.getScan(scanNum);
				if(!scan)
				{
					printf("Error: scan %d does not exist\n", scanNum);

					return -1;
				}
				mode = V.getModeByDefName(scan->modeDefName);
				if(!mode)
				{
					printf("Error: mode %s does not exist\n", scan->modeDefName.c_str());

					continue;
				}
				setup = mode->getSetup(stn);
				if(!setup)
				{
					printf("Error: mode %s not implemented for station %s\n", scan->modeDefName.c_str(), stn.c_str());

					continue;
				}
				for(it = scan->stations.begin(); it != scan->stations.end(); ++it)
				{
					if(it->first == stn)
					{
						scanTimeRange = it->second;
					}
				}
			}
			while(lineTimeRange.mjdStop > scanTimeRange.mjdStop);
			if(scanNum >= V.nScan())
			{
				break;
			}

			TA.setup(*setup, stn);  // also flushes
			
			fprintf(out, "# Scan %s\n", scan->defName.c_str());

			// set up averaging slots
			nSlot = static_cast<int>(scanTimeRange.duration_seconds() / nominalTsysInterval);
			if(nSlot == 0)
			{
				nSlot = 1;
			}
			slotDeltaT = scanTimeRange.duration() / nSlot;
			slotTimeRange.setTimeRange(scanTimeRange.mjdStart, scanTimeRange.mjdStart + slotDeltaT);
		}
		if(lineTimeRange.mjdStart >= scanTimeRange.mjdStart &&
		   lineTimeRange.mjdStop  <= scanTimeRange.mjdStop)
		{
			// data to consider
			double t = lineTimeRange.center();
			
			while(!slotTimeRange.contains(t))
			{
				TA.flush();
				slotTimeRange.shift(slotDeltaT);
			}

			TA.feed(lineTimeRange, line+pos);

			++nRecord;
		}
	}

	fclose(p);

	return nRecord;
}

void antennaSummary(const VexData &V, map<std::string,VexInterval> &as)
{
	for(unsigned int s = 0; s < V.nScan(); ++s)
	{
		const VexScan *scan = V.getScan(s);

		for(map<string,VexInterval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			const VexInterval &vi = it->second;

			if(as.count(it->first) == 0)
			{
				as[it->first] = VexInterval(vi);
			}
			else
			{
				if(vi.mjdStart < as[it->first].mjdStart)
				{
					as[it->first].mjdStart = vi.mjdStart;
				}
				if(vi.mjdStop > as[it->first].mjdStop)
				{
					as[it->first].mjdStop = vi.mjdStop;
				}
			}
		}
	}
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	int nWarn = 0;
	std::string fileList;
	map<string,VexInterval> as;
	const char *vexFilename = 0;
	const char *tsysFilename = 0;
	FILE *out;
	double nominalTsysInterval = defaultTsysInterval;
	int p;
	int verbose = 1;
	const char *tcalFilename;

	for(int a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-q") == 0 ||
			   strcmp(argv[a], "--quiet") == 0)
			{
				--verbose;
			}
			else
			{
				printf("Unknown command line option %s\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
		else if(vexFilename == 0)
		{
			vexFilename = argv[a];
		}
		else if(tsysFilename == 0)
		{
			tsysFilename = argv[a];
		}
		else
		{
			printf("Extra command line parameter : %s\n", argv[a]);

			return EXIT_FAILURE;
		}
	}

	if(tsysFilename == 0)
	{
		printf("Incomplete command line.  Run with -h for help.\n\n");

		return EXIT_FAILURE;
	}

	tcalFilename = getenv("TCAL_FILE");

	if(tcalFilename == 0)
	{
		printf("Environment variable TCAL_FILE must exist\n");

		return EXIT_FAILURE;
	}


	loadTcals(tcalFilename);

	P = new CorrParams();
	P->defaultSetup();
	P->minSubarraySize = 1;
	P->vexFile = vexFilename;
	V = loadVexFile(*P, &nWarn);

	printf("Warnings: %d\n", nWarn);

	out = fopen(tsysFilename, "w");
	if(!out)
	{
		printf("Cannot open %s for write\n", tsysFilename);

		return EXIT_FAILURE;
	}

	fprintf(out, "# TSYS file created by %s ver. %s\n", program, version);

	antennaSummary(*V, as);

	cout.precision(13);

	// Loop over antennas first
	map<string,VexInterval>::const_iterator it;
	for(it = as.begin(); it != as.end(); ++it)
	{
		// it->first is the station name
		// it->second is the VexInterval for that station's involvement

		std::string stn = it->first;
		Lower(stn);

		p = cout.precision();
		cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop << endl;
		cout.precision(p);

		fileList = genFileList(defaultSwitchedPowerPath, stn.c_str(), it->second);
		printf("FL=%s\n", fileList.c_str());

		processStation(out, *V, it->first, fileList, it->second, nominalTsysInterval, verbose);
	}

	fclose(out);

	delete V;

	return EXIT_SUCCESS;
}
