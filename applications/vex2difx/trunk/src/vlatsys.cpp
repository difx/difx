/***************************************************************************
 *   Copyright (C) 2013 by Walter Brisken                                  *
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
 * $Id: $
 * $HeadURL: $
 * $LastChangedRevision: $
 * $Author: $
 * $LastChangedDate: $
 *
 *==========================================================================*/

#include <sstream>
#include <utility>
#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <glob.h>
#include <string>
#include <vector>
#include <regex.h>
#include "vextables.h"
#include "vexload.h"
#include "util.h"

const char program[] = "vlatsys";
const char version[] = "0.2";
const char author[]  = "Walter Brisken";
const char verdate[] = "20131120";

const int MaxFilenameLength = 256;
const char *defaultRxName = "0cm";

static void usage(const char *pgm)
{
	printf("\n%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("Usage: %s [options] <vex file> <VLAMP file 1> [<VLAMP file 2> ... ]\n\n", pgm);
	printf("where options can include:\n\n");
	printf("  --help\n");
	printf("  -h          print help info and quit\n\n");
	printf("  --verbose\n");
	printf("  -v          be more verbose in operation\n\n");
	printf("  --quiet\n");
	printf("  -q          be less verbose in operation\n\n");
	printf("  --mjd\n");
	printf("  -m          datestamp in MJD, not DOY (default)\n\n");
	printf("  --doy\n");
	printf("  -d          datestamp in DOY, not MJD\n\n");
	printf("Multiple VLAMP files can be supplied.  The output tsys file will\n");
	printf("be sorted in time order.  The output gain file will have all gain\n");
	printf("entries concatenated.\n\n");
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

double doy2mjd(int yr, double doy)
{
	int yr1 = yr - 1;

	return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
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


class TsysAverager
{
public:
	TsysAverager() : freq(0.0), tsys(0.0), n(0), pol(' '), rxName(defaultRxName), nGood(0), nBad(0) {}
	void reset() { tsys = 0.0; n = 0; nGood = 0; nBad = 0; }
	double Tsys();
	void set(const VexChannel &vc);
	void print() const;
	double freq;	// signed, in MHz
	double tsys;
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
		
		return tsys/n;
	}
	else
	{
		++nBad;

		return 999.0;
	}
}

void TsysAverager::print() const
{
	std::cout << "freq=" << freq << " pol=" << pol << std::endl;
}

class TsysAccumulator
{
private:
	VexInterval accumTimeRange;
	int nAccum;
	std::vector<std::pair<double, std::string> > *out;
	bool doMJD;
	std::string stn;
	std::vector<TsysAverager> chans;
	long long nGood, nBad, nSkip;
public:
	TsysAccumulator();
	~TsysAccumulator();
	void setOutput(std::vector<std::pair<double, std::string> > *output,  bool doMJDFlag);
	void setStation(const std::string &stnName);
	void setup(const VexSetup &vexSetup, const std::string &str);
	void feed(double mjd, int nChan, const double *tsys, const double *freq, const char *pol);
	long long getnGood() { return nGood; };
	long long getnBad() { return nBad; };
	long long getnSkip() { return nSkip; };
};

TsysAccumulator::TsysAccumulator() : nAccum(0), out(0), nGood(0), nBad(0), nSkip(0)
{
}

TsysAccumulator::~TsysAccumulator()
{
}

void TsysAccumulator::setOutput(std::vector<std::pair<double, std::string> > *output, bool doMJDFlag)
{
	out = output;
	doMJD = doMJDFlag;
}

void TsysAccumulator::setStation(const std::string &stnName)
{
	stn = stnName;
	Upper(stn);
}

void setRxNames(std::vector<TsysAverager> &averagers)
{
	for(std::vector<TsysAverager>::iterator ta = averagers.begin(); ta != averagers.end(); ++ta)
	{
		double freq = fabs(ta->freq);
		ta->rxName = defaultVLBAReceiverStrict(freq);
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

void TsysAccumulator::setup(const VexSetup &vexSetup, const std::string &stn)
{
	std::vector<TsysAverager>::iterator ta;
	std::vector<VexChannel>::const_iterator vc;

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
			ta->freq = vc->bbcFreq/1000000.0;
			if(vc->bbcSideBand == 'L')
			{
				/* the sign of ta->freq incorporates the sideband here */
				ta->freq = -ta->freq;
			}
			ta->freq += vc->bbcBandwidth/2000000.0;	// freq refers to center of band
			ta->pol = vexSetup.getIF(vc->ifName)->pol;
			ta->rxName = vexSetup.getIF(vc->ifName)->bandName().c_str();

			++ta;
		}
	}
	setRxNames(chans);
}

void TsysAccumulator::feed(double mjd, int nChan, const double *tsys, const double *freq, const char *pol)
{
	double day;
	int doy;
	std::stringstream ss;
	
	mjd2yearday(int(mjd), 0, &doy);
	day = doy + mjd - int(mjd);

	for(int i = 0; i < nChan; ++i)
	{
		for(std::vector<TsysAverager>::iterator ta = chans.begin(); ta != chans.end(); ++ta)
		{
			if(ta->pol == pol[i] && fabs(ta->freq - freq[i]) < 0.005)
			{
				ta->tsys += tsys[i];
				++ta->n;
			}
		}
	}


	ss.precision(14);
	ss << stn << " " << (doMJD ? mjd : day) << " 0.0 " << chans.size();

	ss.precision(6);
	for(std::vector<TsysAverager>::iterator ta = chans.begin(); ta != chans.end(); ++ta)
	{
		ss << " " << ta->Tsys() << " " << ta->rxName;

		nGood += ta->nGood;
		nBad += ta->nBad;
		
		ta->reset();
	}

	out->push_back(std::pair<double,std::string>(mjd, ss.str()));
}

static int processStation(std::vector<std::pair<double,std::string> > *tsysOutput, std::vector<std::string> *gainOutput, const VexData &V, const std::string &stn, const std::string &antabFilename, const VexInterval &stnTimeRange, int verbose, long long *nGood, long long *nBad, long long *nLine, long long *nTimeError, long long *nSkip, bool doMJD)
{
	const int MaxChannels = 64;
	const int MaxLineLength = 32768;	// make sure it is large enough!
	const int MaxAntennaNameLength = 8;
	char line[MaxLineLength];
	FILE *in;				// antab file descriptor
	std::string command;
	int nRecord = 0;
	char *v;
	int rv;
	VexInterval lineTimeRange;
	VexInterval scanTimeRange;
	VexInterval slotTimeRange;
	unsigned int scanNum = 0;
	const VexScan *scan = 0;
	TsysAccumulator TA;
	const VexMode *mode = 0;
	const VexSetup *setup = 0;
	regex_t chanLineMatch, tsysLineMatch, dataLineMatch;
	regmatch_t matchPtr[6];
	int nChan;	// number of channels with Tsys in antab file
	double freq[MaxChannels];
	double tsys[MaxChannels];
	char pol[MaxChannels];
	double timeoffset, ft;
	char antenna[MaxAntennaNameLength+1];
	int year0, doy0;	// at start of experiment

	mjd2yearday(int(V.getExper()->mjdStart), &year0, &doy0);

	in = fopen(antabFilename.c_str(), "r");
	if(!in)
	{
		std::cerr << "cannot open for read file: " << antabFilename << std::endl;

		return -1;
	}

	TA.setOutput(tsysOutput, doMJD);
	TA.setStation(stn);

	// look for lines like:   !   1 RCP U  21916.00MHz   2.95 
	rv = regcomp(&chanLineMatch, "([0-9]+)\\s+([RL])CP\\s+([UL])\\s+([0-9]*\\.?[0-9]*)\\s*([KMG]?)Hz", REG_EXTENDED | REG_ICASE);
	if(rv != 0)
	{
		printf("Developer error: regcomp(1) returned %d\n", rv);

		exit(EXIT_FAILURE);
	}

	// look for lines like:   TSYS  timeoff = 0.0  Y   FT = 1.0/
	rv = regcomp(&tsysLineMatch, "TSYS\\s+(timeoff\\s*=\\s*([0-9]*\\.?[0-9]*)\\s+)?([A-Z]+)(\\s+FT\\s+=\\s+([0-9]*\\.?[0-9]*))?\\s*/", REG_EXTENDED | REG_ICASE);
	if(rv != 0)
	{
		printf("Developer error: regcomp(2) returned %d\n", rv);

		exit(EXIT_FAILURE);
	}

	// look for lines like:   047 20:39.458  37.0  36.3  36.5  36.3 ! 26-26
	rv = regcomp(&dataLineMatch, "([0-9]+)\\s+([0-9]+):([0-9.]+)\\s+([^!]+)", REG_EXTENDED | REG_ICASE);
	if(rv != 0)
	{
		printf("Developer error: regcomp(3) returned %d\n", rv);

		exit(EXIT_FAILURE);
	}

	nChan = 0;

	for(;;)
	{
		v = fgets(line, MaxLineLength-1, in);
		if(!v)
		{
			// End of pipe.  Really this shouldn't happen unless obsv'n ends at midnight
			break;
		}
		++(*nLine);

		// is it a TSYS line?
		rv = regexec(&tsysLineMatch, line, 6, matchPtr, 0);
		if(rv == 0)
		{
			int l;

			if(matchPtr[5].rm_so < 0)
			{
				ft = 1.0;
			}
			else
			{
				line[matchPtr[5].rm_eo] = 0;
				ft = atof(line + matchPtr[5].rm_so);
			}

			l = matchPtr[3].rm_eo - matchPtr[3].rm_so;
			if(l > MaxAntennaNameLength)
			{
				antenna[0] = 0;
			}
			else
			{
				strncpy(antenna, line+matchPtr[3].rm_so, l);
				antenna[l] = 0;
			}

			if(matchPtr[2].rm_so < 0)
			{
				timeoffset = 0.0;
			}
			else
			{
				line[matchPtr[2].rm_eo] = 0;
				timeoffset = atof(line + matchPtr[2].rm_so);
			}
		}

		// is it a channel def line?
		rv = regexec(&chanLineMatch, line, 6, matchPtr, 0);
		if(rv == 0)
		{
			char prefix;
			double f;
			char p;

			if(matchPtr[5].rm_so == matchPtr[5].rm_eo)
			{
				prefix = '.';	// must be in Hz
			}
			else
			{
				prefix = toupper(line[matchPtr[5].rm_so]);
			}
			line[matchPtr[4].rm_eo] = 0;
			f = atof(line + matchPtr[4].rm_so);
			switch(prefix)
			{
			case '.':	// Hz
				f /= 1000000.0;
				break;
			case 'K':	// kHz
				f /= 1000.0;
				break;
			case 'G':	// GHz
				f /= 1000.0;
			}
			if(toupper(line[matchPtr[3].rm_so]) == 'L')
			{
				f = -f;
			}
			p = toupper(line[matchPtr[2].rm_so]);
			line[matchPtr[1].rm_eo] = 0;
			nChan = atoi(line + matchPtr[1].rm_so);
			
			pol[nChan-1] = p;
			freq[nChan-1] = f;

			continue;
		}

		if(line[0] == '/')
		{
			for(;;)
			{
				v = fgets(line, MaxLineLength-1, in);
				if(!v)
				{
					break;
				}
				gainOutput->push_back(line);
				++(*nLine);
			}
		
			break;
		}
		
		// is it a data line?
		rv = regexec(&dataLineMatch, line, 5, matchPtr, 0);
		if(rv == 0)
		{
			double mjd, doy;
			int year;
			int i, n;
			const char *p;

			line[matchPtr[1].rm_eo] = 0;
			doy = atoi(line + matchPtr[1].rm_so);

			line[matchPtr[2].rm_eo] = 0;
			doy += atoi(line + matchPtr[2].rm_so) / 24.0;

			line[matchPtr[3].rm_eo] = 0;
			doy += atof(line + matchPtr[3].rm_so) / 1440.0;

			if(doy < doy0 - 180)
			{
				year = year0 + 1;
			}
			else if(doy > doy0 + 180)
			{
				year = year0 - 1;
			}
			else
			{
				year = year0;
			}

			mjd = doy2mjd(year, doy);

			mjd += timeoffset/86400.0;

			p = line + matchPtr[4].rm_so;

			if(mjd < V.obsStart() - 2.0 || mjd > V.obsStop() + 2.0)
			{
				++(*nTimeError);

				continue;
			}

			if(mjd < scanTimeRange.mjdStart)
			{
				// We're not yet at the time of scan start

				continue;
			}

			for(i = 0; i < nChan; ++i)
			{
				if(sscanf(p, "%lf%n", tsys + i, &n) != 1)
				{
					break;
				}

				tsys[i] *= ft;

				p += n;
			}

			if(i == nChan)	// all of the channels were decoded
			{
				// Feed data
				if(mjd > scanTimeRange.mjdStop)
				{
					// End of a scan reached.  Find next scan
					do
					{
						if(scan)
						{
							++scanNum;
						}
						if(scanNum >= V.nScan())
						{
							continue;
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
					while(mjd > scanTimeRange.mjdStop);
					if(scanNum >= V.nScan())
					{
						continue;
					}

					TA.setup(*setup, stn);
				}
				if(scanTimeRange.contains(mjd))
				{
					TA.feed(mjd, nChan, tsys, freq, pol);
				}
			}

			continue;
		}

		/* check for end /  */
	
	}

	regfree(&chanLineMatch);
	regfree(&tsysLineMatch);
	regfree(&dataLineMatch);

	fclose(in);

	if(nGood)
	{
		*nGood += TA.getnGood();
	}

	if(nBad)
	{
		*nBad += TA.getnBad();
	}

	if(nSkip)
	{
		*nSkip += TA.getnSkip();
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
	int nWarn = 0;
	bool doMJD = true;
	std::map<std::string,VexInterval> as;
	const char *vexFilename = 0;	// input vex file
	std::vector<std::string> antabFiles;	// input Tsys files
	std::vector<std::pair<double, std::string> > tsysOutput;	// store output tsys in a pair so we can sort on record MJD easily
	std::vector<std::string> gainOutput;
	int p;
	int verbose = 1;

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
			else if(strcmp(argv[a], "-m") == 0 ||
			   strcasecmp(argv[a], "--mjd") == 0)
			{
				doMJD = true;
			}
			else if(strcmp(argv[a], "-d") == 0 ||
			   strcasecmp(argv[a], "--doy") == 0)
			{
				doMJD = false;
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
		else
		{
			antabFiles.push_back(argv[a]);
		}
	}

	if(antabFiles.empty())
	{
		std::cerr << "Incomplete command line.  Run with -h for help." << std::endl;

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

	antennaSummary(*V, as);

	std::cout.precision(13);

	// Loop over antennas first
	std::map<std::string,VexInterval>::const_iterator it;
	for(it = as.begin(); it != as.end(); ++it)
	{
		std::string filePrefix;
		std::string outFilename;
		long long nGood, nBad, nLine, nTimeError, nSkip;
		int nRecord;
		FILE *out;				// first the .tsys file, then maybe the .gain file

		// it->first is the station name
		// it->second is the VexInterval for that station's involvement

		std::string stn = it->first;
		Lower(stn);

		if(stn != "y")
		{
			continue;
		}

		if(verbose > 0)
		{
			p = std::cout.precision();
			std::cout << it->first << std::endl;
			std::cout << "  MJD time range: " << it->second.mjdStart << " to " << it->second.mjdStop << std::endl;
			std::cout.precision(p);
		}

		filePrefix = V->getExper()->name + "." + stn;
		Lower(filePrefix);

		nRecord = 0;
		nGood = nBad = nLine = nTimeError = nSkip = 0;
		for(std::vector<std::string>::const_iterator af = antabFiles.begin(); af != antabFiles.end(); ++af)
		{
			nRecord += processStation(&tsysOutput, &gainOutput, *V, it->first, *af, it->second, verbose, &nGood, &nBad, &nLine, &nTimeError, &nSkip, doMJD);
		}


		// put tsys in time order
		std::sort(tsysOutput.begin(), tsysOutput.end() );


		// Write Tsys file
		outFilename = filePrefix + ".tsys";
		out = fopen(outFilename.c_str(), "w");
		if(!out)
		{
			std::cerr << "cannot open for write file: " << outFilename << std::endl;

			break;
		}
		printf("Writing TS: %s\n", outFilename.c_str());
		fprintf(out, "# TSYS file created by %s ver. %s\n", program, version);
		fprintf(out, "# %s %14.8f %14.8f\n", stn.c_str(), it->second.mjdStart, it->second.mjdStop);
		fprintf(out, "# ant D.O.Y. dur(days) nRecChan (tsys, bandName)[nRecChan]\n");

		for(std::vector<std::pair<double, std::string> >::const_iterator ol = tsysOutput.begin(); ol != tsysOutput.end(); ++ol)
		{
			fprintf(out, "%s\n", ol->second.c_str());
		}
		fclose(out);


		// Write Gain file
		outFilename = filePrefix + ".gain";
		out = fopen(outFilename.c_str(), "w");
		if(!out)
		{
			std::cerr << "cannot open for write file: " << outFilename << std::endl;

			break;
		}
		printf("Writing GN: %s\n", outFilename.c_str());
		fprintf(out, "# Gain file created by %s ver. %s\n", program, version);
		for(std::vector<std::string>::const_iterator ol = gainOutput.begin(); ol != gainOutput.end(); ++ol)
		{
			fprintf(out, "%s", ol->c_str());
		}
		fclose(out);


		// Summarize the findings
		std::cout << nLine << " lines read" << std::endl;
		std::cout << nRecord << " switched power records read" << std::endl;
		std::cout << nGood << " good Tsys values created" << std::endl;
		std::cout << nBad << " bad (999) Tsys values created" << std::endl;
		std::cout << nSkip << " Tsys records skipped (probably because no Tcal)" << std::endl;
		std::cout << nTimeError << " time errors encountered" << std::endl;
	}

	delete V;
	delete P;

	return EXIT_SUCCESS;
}
