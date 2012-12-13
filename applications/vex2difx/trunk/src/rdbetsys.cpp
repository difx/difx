/***************************************************************************
 *   Copyright (C) 2011-2012 by Walter Brisken                             *
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
#include <difxio/difx_tcal.h>
#include "vextables.h"
#include "vexload.h"
#include "util.h"

const char program[] = "rdbetsys";
const char version[] = "1.1";
const char author[]  = "Walter Brisken";
const char verdate[] = "20121213";

const char defaultSwitchedPowerPath[] = "/home/vlba/metadata/switchedpower";
const double defaultTsysInterval = 15.0;	// Seconds
const int MaxFilenameLength = 256;
const char *defaultRxName = "0cm";

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
	printf("  --interval <t>\n");
	printf("  -i <t>      use averaging interval of <t> seconds [%f]\n\n", defaultTsysInterval);
	printf("  --scan\n");
	printf("  -s          average over whole scans\n\n");
	printf("Note that env. var. TCAL_PATH must be set to point to TCal data\n\n");
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
		snprintf(match, MaxFilenameLength, "%s/%s_%d_%03d_*.switched_power.xz", switchedPowerPath, stn, year, doy);

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



class TsysAverager
{
public:
	TsysAverager() : freq(0.0), bw(0.0), tCal(0.0), pOn(0.0), pOff(0.0), n(0), pol(' '), rxName(defaultRxName), nGood(0), nBad(0) {}
	void reset() { pOn = pOff = 0.0; n = 0; nGood = 0; nBad = 0; }
	double Tsys();
	void set(const VexChannel &vc);
	void print() const;
	double freq;	// signed, in MHz
	double bw;	// MHz
	double tCal;
	double pOn;
	double pOff;
	int n;
	char pol;
	const char *rxName;
	long long nGood, nBad;
};

double TsysAverager::Tsys()
{
	if(n > 0)
	{
		++nGood;
		
		return 0.5*tCal*(pOn + pOff)/(pOn - pOff);
	}
	else
	{
		++nBad;

		return 999.0;
	}
}

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
	long long nGood, nBad;
public:
	TsysAccumulator();
	~TsysAccumulator();
	void setOutput(FILE *outFile);
	void setStation(const std::string &stnName);
	void setup(const VexSetup &vexSetup, DifxTcal *T, double mjd, const string &str);
	void flush();
	void feed(const VexInterval &lineTimeRange, const char *data);
	long long getnGood() { return nGood; };
	long long getnBad() { return nBad; };
};

TsysAccumulator::TsysAccumulator() : nAccum(0), out(0), nGood(0), nBad(0)
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

void setRxNames(std::vector<TsysAverager> &averagers)
{
	bool has4cm = false;
	bool has6cm = false;
	bool has13cm = false;
	bool has20cm = false;

	for(std::vector<TsysAverager>::iterator ta = averagers.begin(); ta != averagers.end(); ++ta)
	{
		double freq = fabs(ta->freq + ta->bw/2.0);

		ta->rxName = defaultVLBAReceiverStrict(freq);
		if(strcmp(ta->rxName, "4cm") == 0)
		{
			has4cm = true;
		}
		else if(strcmp(ta->rxName, "6cm") == 0)
		{
			has6cm = true;
		}
		else if(strcmp(ta->rxName, "13cm") == 0)
		{
			has13cm = true;
		}
		else if(strcmp(ta->rxName, "20cm") == 0)
		{
			has20cm = true;
		}

	}
	if(has4cm && has6cm)
	{
		fprintf(stderr, "Warning: both 4cm and 6cm used at the same time!\n");
	}
	if(has13cm && has20cm)
	{
		fprintf(stderr, "Warning: both 13cm and 20cm used at the same time!\n");
	}
	for(std::vector<TsysAverager>::iterator ta = averagers.begin(); ta != averagers.end(); ++ta)
	{
		double freq = fabs(ta->freq + ta->bw/2.0);

		if(strcmp(ta->rxName, "?") == 0)
		{
			if(has4cm && freq > 7500.0 && freq < 10000.0)
			{
				ta->rxName = "4cm";
			}
			else if(has6cm && freq > 3500.0 && freq < 8500.0)
			{
				ta->rxName = "6cm";
			}
			else if(has13cm && freq > 1800.0 && freq < 4500.0)
			{
				ta->rxName = "13cm";
			}
			else if(has20cm && freq > 1000.0 && freq < 2200.0)
			{
				ta->rxName = "20cm";
			}
			else
			{
				ta->rxName = "";	/* Cannot be determined, or out of range */
			}
		}
	}
}

static void sanityCheckChannels(const VexSetup &vexSetup)
{
	unsigned int nRecChan = 0;
	
	for(std::vector<VexChannel>::const_iterator vc = vexSetup.channels.begin(); vc != vexSetup.channels.end(); ++vc)
	{
		if(vc->recordChan >= 0)
		{
			++nRecChan;
		}
	}
	if(nRecChan != vexSetup.nRecordChan)
	{
		cerr << "Developer Error: TsysAccumulator::sanityCheckChannels: vexSetup.nRecordChan=" << vexSetup.nRecordChan << " does not equal nRecChan=" << nRecChan << " as it should!" << endl;

		//exit(EXIT_FAILURE);
	}
}

void TsysAccumulator::setup(const VexSetup &vexSetup, DifxTcal *T, double mjd, const string &stn)
{
	std::vector<TsysAverager>::iterator ta;
	std::vector<VexChannel>::const_iterator vc;

	// write out the data!
	flush();
			
	for(ta = chans.begin(); ta != chans.end(); ++ta)
	{
		nGood += ta->nGood;
		nBad += ta->nBad;
		ta->reset();
	}

	// Sanity check the number of record channels
	sanityCheckChannels(vexSetup);

	chans.resize(vexSetup.nRecordChan);
	ta = chans.begin();
	for(vc = vexSetup.channels.begin(); vc != vexSetup.channels.end(); ++vc)
	{
		if(vc->recordChan >= 0)
		{
			ta->bw = vc->bbcBandwidth/1000000.0;
			ta->freq = vc->bbcFreq/1000000.0;
			if(vc->bbcSideBand == 'L')
			{
				/* the sign of ta->freq incorporates the sideband here */
				ta->freq = -ta->freq;
			}
			ta->pol = vexSetup.getIF(vc->ifName)->pol;

			++ta;
		}
	}
	setRxNames(chans);
	for(ta = chans.begin(); ta != chans.end(); ++ta)
	{
		ta->tCal = getDifxTcal(T, mjd, stn.c_str(), ta->rxName, ta->pol, fabs(ta->freq + ta->bw/2.0));
	}
}

void TsysAccumulator::flush()
{
	if(nAccum > 0)
	{
		double day;
		int doy, mjd;
		
		mjd = static_cast<int>(accumTimeRange.center());
		mjd2yearday(mjd, 0, &doy);
		day = accumTimeRange.center() - mjd + doy;

		fprintf(out, "%s %12.8f %10.8f %d", stn.c_str(), day, accumTimeRange.duration(), static_cast<int>(chans.size()));
		for(std::vector<TsysAverager>::iterator ta = chans.begin(); ta != chans.end(); ++ta)
		{
			fprintf(out, " %5.2f %s", ta->Tsys(), ta->rxName);
			nGood += ta->nGood;
			nBad += ta->nBad;
			ta->reset();
		}
		fprintf(out, "\n");

		nAccum = 0;
	}
}

void TsysAccumulator::feed(const VexInterval &lineTimeRange, const char *data)
{
	double pOn, pOff;
	double freq, bw;
	char pol[2];
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
		n = sscanf(data, "%lf%lf%1s%lf%lf%n", &freq, &bw, pol, &pOn, &pOff, &pos);
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

int processStation(FILE *out, const VexData &V, const string &stn, const string &fileList, const VexInterval &stnTimeRange, double nominalTsysInterval, DifxTcal *T, int verbose, long long *nGood, long long *nBad)
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

	command = "xzcat " + fileList + " 2> /dev/null";
	if(verbose > 2)
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
	// FIXME: it would be good to print filenames as the next line does, but with one file per line so line lengths stay confined.
	// fprintf(out, "# Files: %s\n", fileList.c_str());
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
					printf("Error: scan %u does not exist\n", scanNum);

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
				for(map<string,VexInterval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
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

			TA.setup(*setup, T, scanTimeRange.mjdStop, stn);  // also flushes
			
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
		if(scanTimeRange.contains(lineTimeRange))
		{
			// data to consider
			unsigned int n = 0;
			double t = lineTimeRange.center();
			VexInterval origSlot(slotTimeRange);
			
			if(t > slotTimeRange.mjdStart)
			{
				while(!slotTimeRange.contains(t))
				{
					TA.flush();
					slotTimeRange.shift(slotDeltaT);
					++n;

					if(n > nSlot)
					{
						cout << "Developer error handling line: " << line << endl;
						cout << "nSlot = " << nSlot << "  line=" << lineTimeRange << "  center=" << t << "  scan=" << scanTimeRange << "  origslot=" << origSlot << endl;
						break;
					}
				}

				if(n < nSlot)
				{
					TA.feed(lineTimeRange, line+pos);

					++nRecord;
				}
				else
				{
					cout << "Warning: lineTimeRange = " << lineTimeRange << " did not fit with scan " << scanTimeRange << endl;
				}
			}
		}
	}

	pclose(p);

	if(nGood)
	{
		*nGood = TA.getnGood();
	}

	if(nBad)
	{
		*nBad = TA.getnBad();
	}

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
	DifxTcal *T;
	int nWarn = 0;
	std::string fileList;
	map<string,VexInterval> as;
	const char *vexFilename = 0;
	const char *tsysFilename = 0;
	FILE *out;
	double nominalTsysInterval = defaultTsysInterval;
	int p, v;
	int verbose = 1;
	const char *tcalPath;
	int nZero = 0;
	string zeroStations;
	long long nGood, nBad;

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
			else if(strcmp(argv[a], "-i") == 0 ||
			   strcmp(argv[a], "--interval") == 0)
			{
				if(a+1 < argc)
				{
					nominalTsysInterval = atof(argv[++a]);
				}
				else
				{
					fprintf(stderr, "Interval parameter requires a numeric value\n");

					return EXIT_FAILURE;
				}
			}
			else if(strcmp(argv[a], "-s") == 0 ||
			  strcmp(argv[a], "--scan") == 0)
			{
				nominalTsysInterval = 86400.0;	/* set to the whole day; scan breaks are automatic */
			}
			else
			{
				fprintf(stderr, "Unknown command line option %s\n", argv[a]);

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
			fprintf(stderr, "Extra command line parameter : %s\n", argv[a]);

			return EXIT_FAILURE;
		}
	}

	if(tsysFilename == 0)
	{
		fprintf(stderr, "Incomplete command line.  Run with -h for help.\n\n");

		return EXIT_FAILURE;
	}

	tcalPath = getenv("TCAL_PATH");

	if(tcalPath == 0)
	{
		fprintf(stderr, "Environment variable TCAL_PATH must exist\n");

		return EXIT_FAILURE;
	}


	T = newDifxTcal();
	if(!T)
	{
		fprintf(stderr, "Error initializing Tcal system\n");

		return EXIT_FAILURE;
	}
	v = setDifxTcalVLBA(T, tcalPath);

	if(v != 0)
	{
		fprintf(stderr, "Error %d setting up Tcal system\n", v);

		deleteDifxTcal(T);

		return EXIT_FAILURE;
	}

	P = new CorrParams();
	P->defaultSetup();
	P->minSubarraySize = 1;
	P->vexFile = vexFilename;
	V = loadVexFile(*P, &nWarn);
	if(!V)
	{
		fprintf(stderr, "Error opening vex file %s\n", vexFilename);
		
		delete V;
		delete P;
		deleteDifxTcal(T);

		exit(EXIT_FAILURE);
	}

	if(verbose > 2)
	{
		cout << "Vex Data:" << endl;
		cout << *V << endl;
	}

	if(nWarn > 0)
	{
		fprintf(stderr, "Warning %d warnings in reading the vex file: %s\n", nWarn, vexFilename);
	}

	out = fopen(tsysFilename, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", tsysFilename);

		delete V;
		delete P;
		deleteDifxTcal(T);

		return EXIT_FAILURE;
	}

	fprintf(out, "# TSYS file created by %s ver. %s\n", program, version);

	antennaSummary(*V, as);

	cout.precision(13);

	// Loop over antennas first
	map<string,VexInterval>::const_iterator it;
	for(it = as.begin(); it != as.end(); ++it)
	{
		int nRecord;

		// it->first is the station name
		// it->second is the VexInterval for that station's involvement

		std::string stn = it->first;
		Lower(stn);

		if(verbose > 0)
		{
			p = cout.precision();
			cout << it->first << endl;
			cout << "  MJD time range: " << it->second.mjdStart << " to " << it->second.mjdStop << endl;
			cout.precision(p);
		}

		if(stn == "y")
		{
			cout << "  *** skipping this antenna.  Tsys from VLA comes from a different source ***" << endl;

			continue;
		}

		fileList = genFileList(defaultSwitchedPowerPath, stn.c_str(), it->second);
		if(verbose > 1)
		{
			printf("  File list: %s\n", fileList.c_str());
		}
		if(fileList.empty())
		{
			nRecord = 0;
			nGood = 0;
			nBad = 0;
		}
		else
		{
			nRecord = processStation(out, *V, it->first, fileList, it->second, nominalTsysInterval, T, verbose, &nGood, &nBad);
		}
		if(nRecord <= 0)
		{
			++nZero;
			if(!zeroStations.empty())
			{
				zeroStations += ",";
			}
			zeroStations += stn;
		}
		if(verbose > 0)
		{
			printf("  %d switched power records read\n", nRecord);
			printf("  %Ld good Tsys values created\n", nGood);
			printf("  %Ld bad (999) Tsys values created\n", nBad);
		}
	}

	Upper(zeroStations);

	if(nZero == 1)
	{
		printf("\n1 station (%s) had zero records written.  No Tsys information will be available for that antenna.\n", zeroStations.c_str());
	}
	else if(nZero > 1)
	{
		printf("\n%d stations (%s) had zero records written.  No Tsys information will be available for those antennas.\n", nZero, zeroStations.c_str());
	}

	fclose(out);

	delete V;
	delete P;

	deleteDifxTcal(T);

	return EXIT_SUCCESS;
}
