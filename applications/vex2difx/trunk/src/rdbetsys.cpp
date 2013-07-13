/***************************************************************************
 *   Copyright (C) 2011-2013 by Walter Brisken                             *
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
const char version[] = "1.4";
const char author[]  = "Walter Brisken";
const char verdate[] = "20130713";

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
	printf("  --antab\n");
	printf("  -a          produce output in ANTAB format\n\n");
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

std::string fileList2String(const std::list<std::string> &fileList)
{
	std::string str="";
	std::list<std::string>::const_iterator it;

	for(it = fileList.begin(); it != fileList.end(); ++it)
	{
		if(str.length() > 0)
		{
			str += " ";
		}
		str += *it;
	}

	return str;
}

void genFileList(std::list<std::string> &fileList, const char *switchedPowerPath, const char *stn, const VexInterval &timeRange)
{
	glob_t globbuf;

	fileList.clear();

	for(int mjd = static_cast<int>(timeRange.mjdStart); mjd < timeRange.mjdStop; ++mjd)
	{
		int year, doy;
		char match[MaxFilenameLength];
		int rv;

		mjd2yearday(mjd, &year, &doy);

		// First try looking in the station/year file area
		snprintf(match, MaxFilenameLength, "%s/%s/%d/%s_%d_%03d_*.switched_power.xz", switchedPowerPath, stn, year, stn, year, doy);

		rv = glob(match, 0, 0, &globbuf);
		if(rv == 0)
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
						fileList.push_back(globbuf.gl_pathv[i]);
					}

				}
			}
			globfree(&globbuf);
		}

		else if(rv == GLOB_NOMATCH)
		{
			// Then try looking in the central file area
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
							fileList.push_back(globbuf.gl_pathv[i]);
						}

					}
				}
				globfree(&globbuf);
			}
		}
	}
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
	std::string rxName;
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
	bool doAntab;
	std::string stn;
	std::vector<TsysAverager> chans;
	long long nGood, nBad, nSkip;
public:
	TsysAccumulator();
	~TsysAccumulator();
	void setOutput(FILE *outFile, bool doAntabFlag);
	void setStation(const std::string &stnName);
	void setup(const VexSetup &vexSetup, DifxTcal *T, double mjd, const std::string &str);
	void flush();
	void feed(const VexInterval &lineTimeRange, const char *data);
	long long getnGood() { return nGood; };
	long long getnBad() { return nBad; };
	long long getnSkip() { return nSkip; };
};

TsysAccumulator::TsysAccumulator() : nAccum(0), out(0), nGood(0), nBad(0), nSkip(0)
{
}

TsysAccumulator::~TsysAccumulator()
{
	flush();
}

void TsysAccumulator::setOutput(FILE *outFile, bool doAntabFlag)
{
	out = outFile;
	doAntab = doAntabFlag;
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

	// Here we only change receiver names that are not yet set

	for(std::vector<TsysAverager>::iterator ta = averagers.begin(); ta != averagers.end(); ++ta)
	{
		double freq = fabs(ta->freq + ta->bw/2.0);


		if(ta->rxName.empty())
		{
			ta->rxName = defaultVLBAReceiverStrict(freq);
		}
		if(ta->rxName == "4cm")
		{
			has4cm = true;
		}
		else if(ta->rxName == "6cm")
		{
			has6cm = true;
		}
		else if(ta->rxName == "13cm")
		{
			has13cm = true;
		}
		else if(ta->rxName == "20cm")
		{
			has20cm = true;
		}

	}
	if(has4cm && has6cm)
	{
		std::cerr << "Warning: both 4cm and 6cm used at the same time!" << std::endl;
	}
	if(has13cm && has20cm)
	{
		std::cerr << "Warning: both 13cm and 20cm used at the same time!" << std::endl;
	}
	for(std::vector<TsysAverager>::iterator ta = averagers.begin(); ta != averagers.end(); ++ta)
	{
		double freq = fabs(ta->freq + ta->bw/2.0);

		if(ta->rxName == "?")
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
	if(nRecChan != vexSetup.nRecordChan && nRecChan > 0 && vexSetup.nRecordChan > 0)
	{
		std::cerr << "Developer Error: TsysAccumulator::sanityCheckChannels: vexSetup.nRecordChan=" << vexSetup.nRecordChan << " does not equal nRecChan=" << nRecChan << " as it should!" << std::endl;

		exit(EXIT_FAILURE);
	}
}

void TsysAccumulator::setup(const VexSetup &vexSetup, DifxTcal *T, double mjd, const std::string &stn)
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

	chans.resize(vexSetup.channels.size());
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
			ta->rxName = vexSetup.getIF(vc->ifName)->bandName().c_str();

			++ta;
		}
	}
	setRxNames(chans);
	for(ta = chans.begin(); ta != chans.end(); ++ta)
	{
		ta->tCal = getDifxTcal(T, mjd, stn.c_str(), ta->rxName.c_str(), ta->pol, fabs(ta->freq + ta->bw/2.0));
	}
}

void TsysAccumulator::flush()
{
	if(nAccum > 0)
	{
		double day;
		int doy, mjd;
		bool doSkip = true;
		
		mjd = static_cast<int>(accumTimeRange.center());
		mjd2yearday(mjd, 0, &doy);
		day = accumTimeRange.center() - mjd + doy;

		for(std::vector<TsysAverager>::iterator ta = chans.begin(); ta != chans.end(); ++ta)
		{
			if(ta->tCal > 0.0)
			{
				doSkip = false;
			}
		}
		if(!doSkip)
		{
			if(doAntab)
			{
				int hour;
				double minute;
				double hourf = 24.0*(day - doy);
				
				hour = static_cast<int>(hourf);
				minute = 60.0*(hourf - hour);

				fprintf(out, "%d %02d:%06.3f", doy, hour, minute);
			}
			else
			{
				fprintf(out, "%s %12.8f %10.8f %d", stn.c_str(), day, accumTimeRange.duration(), static_cast<int>(chans.size()));
			}
		}
		for(std::vector<TsysAverager>::iterator ta = chans.begin(); ta != chans.end(); ++ta)
		{
			if(doSkip)
			{
				++nSkip;
			}
			else
			{
				if(doAntab)
				{
					fprintf(out, " %6.2f", ta->Tsys());
				}
				else
				{
					fprintf(out, " %5.2f %s", ta->Tsys(), ta->rxName.c_str());
				}
				nGood += ta->nGood;
				nBad += ta->nBad;
			}
			ta->reset();
		}
		if(!doSkip)
		{
			fprintf(out, "\n");
		}

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

static int processStation(FILE *out, const VexData &V, const std::string &stn, const std::list<std::string> &fileList, const VexInterval &stnTimeRange, double nominalTsysInterval, DifxTcal *T, int verbose, long long *nGood, long long *nBad, long long *nLine, long long *nTimeError, long long *nSkip, bool doAntab)
{
	const int MaxLineLength = 32768;	// make sure it is large enough!
	char line[MaxLineLength];
	FILE *p;
	std::list<std::string>::const_iterator flit;
	std::string command;
	int nRecord = 0;
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

	command = "xzcat " + fileList2String(fileList) + " 2> /dev/null";
	if(verbose > 2)
	{
		std::cout << "opening pipe: " << command << std::endl;
	}
	p = popen(command.c_str(), "r");
	if(!p)
	{
		std::cerr << "cannot open pipe: " << command << std::endl;

		return -1;
	}

	if(doAntab)
	{
		fprintf(out, "\n! Antenna: %s\n", stn.c_str());
		fprintf(out, "! Time range (MJD): %14.8f %14.8f\n", stnTimeRange.mjdStart, stnTimeRange.mjdStop);
		fprintf(out, "! Files searched for swiched power data include:\n");
		for(flit = fileList.begin(); flit != fileList.end(); ++flit)
		{
			fprintf(out, "!   %s\n", flit->c_str());
		}
		fprintf(out, "TSYS  timeoff = 0.0  %s  FT = 1.0 /\n", stn.c_str());
	}
	else
	{
		fprintf(out, "# %s %14.8f %14.8f\n", stn.c_str(), stnTimeRange.mjdStart, stnTimeRange.mjdStop);
		fprintf(out, "# Files searched for swiched power data include:\n");
		for(flit = fileList.begin(); flit != fileList.end(); ++flit)
		{
			fprintf(out, "#   %s\n", flit->c_str());
		}
		fprintf(out, "# ant D.O.Y. dur(days) nRecChan (tsys, bandName)[nRecChan]\n");
	}

	TA.setOutput(out, doAntab);
	TA.setStation(stn);
	*nLine = 0;
	*nTimeError = 0;

	for(;;)
	{
		v = fgets(line, MaxLineLength-1, p);
		if(!v)
		{
			// End of pipe.  Really this shouldn't happen unless obsv'n ends at midnight
			break;
		}
		++(*nLine);
		if(line[0] == '#')
		{
			continue;
		}

		n = sscanf(line, "%*s%lf%lf%n", &lineTimeRange.mjdStart, &lineTimeRange.mjdStop, &pos);
		if(n < 2)
		{
			std::cout << "Error: malformed line: " << line << std::endl;

			break;
		}
		if(lineTimeRange.mjdStart < V.obsStart() - 2.0 || lineTimeRange.mjdStop > V.obsStop() + 2.0)
		{
			++(*nTimeError);

			continue;
		}

		if(lineTimeRange.mjdStart < scanTimeRange.mjdStart)
		{
			// We're not yet at the time of scan start
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
					std::cerr << "Error: scan " << scanNum << "does not exist" << std::endl;

					return -1;
				}
				mode = V.getModeByDefName(scan->modeDefName);
				if(!mode)
				{
					std::cerr << "Error: mode " << scan->modeDefName << "does not exist" << std::endl;

					continue;
				}
				setup = mode->getSetup(stn);
				if(!setup)
				{
					std::cerr << "Error: mode " << scan->modeDefName << " not implemented for station " << stn << std::endl;

					continue;
				}
				for(std::map<std::string,VexInterval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
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
			
			if(doAntab)
			{
				fprintf(out, "! Scan %s\n", scan->defName.c_str());
			}
			else
			{
				fprintf(out, "# Scan %s\n", scan->defName.c_str());
			}
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
						std::cout << "Developer error handling line: " << line << std::endl;
						std::cout << "nSlot = " << nSlot << "  line=" << lineTimeRange << "  center=" << t << "  scan=" << scanTimeRange << "  origslot=" << origSlot << std::endl;
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
					std::cout << "Warning: lineTimeRange = " << lineTimeRange << " did not fit with scan " << scanTimeRange << std::endl;
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

	if(nSkip)
	{
		*nSkip = TA.getnSkip();
	}

	return nRecord;
}

void antennaSummary(const VexData &V, std::map<std::string,VexInterval> &as)
{
	for(unsigned int s = 0; s < V.nScan(); ++s)
	{
		const VexScan *scan = V.getScan(s);

		for(std::map<std::string,VexInterval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
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
	bool doAntab = false;
	std::list<std::string> fileList;
	std::map<std::string,VexInterval> as;
	const char *vexFilename = 0;
	const char *tsysFilename = 0;
	FILE *out;
	double nominalTsysInterval = defaultTsysInterval;
	int p, v;
	int verbose = 1;
	const char *tcalPath;
	int nZero = 0;
	std::string zeroStations;
	long long nGood=0, nBad=0, nLine=0, nTimeError=0, nSkip=0;

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
			else if(strcmp(argv[a], "-a") == 0 ||
			   strcasecmp(argv[a], "--antab") == 0)
			{
				doAntab = true;
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
					std::cerr << "Interval parameter requires a numeric value" << std::endl;

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
				std::cerr << "Unknown command line option: " << argv[a] << std::endl;

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
			std::cerr << "Extra command line parameter: " << argv[a] << std::endl;

			return EXIT_FAILURE;
		}
	}

	if(tsysFilename == 0)
	{
		std::cerr << "Incomplete command line.  Run with -h for help." << std::endl;

		return EXIT_FAILURE;
	}

	tcalPath = getenv("TCAL_PATH");

	if(tcalPath == 0)
	{
		std::cerr << "Environment variable TCAL_PATH must exist" << std::endl;

		return EXIT_FAILURE;
	}


	T = newDifxTcal();
	if(!T)
	{
		std::cerr << "Error initializing Tcal system" << std::endl;

		return EXIT_FAILURE;
	}
	v = setDifxTcalVLBA(T, tcalPath);

	if(v != 0)
	{
		std::cerr << "Error " << v << " setting up Tcal system" << std::endl;

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
		std::cerr << "Error opening vex file: " << vexFilename << std::endl;
		
		delete V;
		delete P;
		deleteDifxTcal(T);

		exit(EXIT_FAILURE);
	}

	if(verbose > 2)
	{
		std::cout << "Vex Data:" << std::endl;
		std::cout << *V << std::endl;
	}

	if(nWarn > 0)
	{
		std::cerr << "Warning " << nWarn << " warnings in reading the vex file: " << vexFilename << std::endl;
	}

	out = fopen(tsysFilename, "w");
	if(!out)
	{
		std::cerr << "Error: cannot open " << tsysFilename << "for write." << std::endl;

		delete V;
		delete P;
		deleteDifxTcal(T);

		return EXIT_FAILURE;
	}

	if(doAntab)
	{
		fprintf(out, "! Produced by: %s ver. %s\n\n", program, version);
		fprintf(out, "! The file contains Tsys values from the VLBA/GBT wideband system.\n\n");
		fprintf(out, "! Warning to user! The Tsys values in this file are listed in\n");
		fprintf(out, "! in the order channels are defined in the .vex file.  This means\n");
		fprintf(out, "! that you may need to add INDEX lines to this file to make it\n");
		fprintf(out, "! meaningful.  The INDEX value may vary with scan and by antenna!\n");
	}
	else
	{
		fprintf(out, "# TSYS file created by %s ver. %s\n", program, version);
	}

	antennaSummary(*V, as);

	std::cout.precision(13);

	// Loop over antennas first
	std::map<std::string,VexInterval>::const_iterator it;
	for(it = as.begin(); it != as.end(); ++it)
	{
		int nRecord;

		// it->first is the station name
		// it->second is the VexInterval for that station's involvement

		std::string stn = it->first;
		Lower(stn);

		if(verbose > 0)
		{
			p = std::cout.precision();
			std::cout << it->first << std::endl;
			std::cout << "  MJD time range: " << it->second.mjdStart << " to " << it->second.mjdStop << std::endl;
			std::cout.precision(p);
		}

		if(stn == "y")
		{
			std::cout << "  *** skipping this antenna.  Tsys from VLA comes from a different source ***" << std::endl;

			continue;
		}

		genFileList(fileList, defaultSwitchedPowerPath, stn.c_str(), it->second);
		if(verbose > 1)
		{
			std::cout << "  File list: " << fileList2String(fileList) << std::endl;
		}
		if(fileList.empty())
		{
			nLine = 0;
			nRecord = 0;
			nGood = 0;
			nBad = 0;
			nSkip = 0;
			nTimeError = 0;
		}
		else
		{
			nRecord = processStation(out, *V, it->first, fileList, it->second, nominalTsysInterval, T, verbose, &nGood, &nBad, &nLine, &nTimeError, &nSkip, doAntab);
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
			std::cout << nLine << " lines read" << std::endl;
			std::cout << nRecord << " switched power records read" << std::endl;
			std::cout << nGood << " good Tsys values created" << std::endl;
			std::cout << nBad << " bad (999) Tsys values created" << std::endl;
			std::cout << nSkip << " Tsys records skipped (probably because no Tcal)" << std::endl;
			std::cout << nTimeError << " time errors encountered" << std::endl;
		}
	}

	Upper(zeroStations);

	if(nZero > 0)
	{
		if(nZero == 1)
		{
			std::cout << "1 station";
		}
		else if(nZero > 1)
		{
			std::cout << nZero << " stations";
		}
		std::cout << " (" << zeroStations << ") had zero records written.  No Tsys information will be available for those antennas." << std::endl;
	}

	fclose(out);

	delete V;
	delete P;

	deleteDifxTcal(T);

	return EXIT_SUCCESS;
}
