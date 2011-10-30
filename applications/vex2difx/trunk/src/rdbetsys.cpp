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

const char defaultSwitchedPowerPath[] = "/home/wbrisken/bd152i0";
const double defaultTsysInterval = 20.0;	// Seconds
const int MaxFilenameLength = 256;

void mjd2yearday(int mjd, int *year, int *doy)
{
	time_t t;
	struct tm tt;

	t = (mjd-40587LL)*86400LL;
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

	for(int i = 0; f[i] && n < 5; i++)
	{
		if(f[i] == '_' || f[i] == '.')
		{
			f[i] = 0;
			if(start >= 0)
			{
				num[n] = atoi(f+start);
				n++;
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

	for(int mjd = static_cast<int>(timeRange.mjdStart); mjd < timeRange.mjdStop; mjd++)
	{
		int year, doy;
		char match[MaxFilenameLength];

		mjd2yearday(mjd, &year, &doy);
		snprintf(match, MaxFilenameLength, "%s/%s_%d_%03d_*.switched_power.gz", switchedPowerPath, stn, year, doy);
		printf("Matching against: %s\n", match);

		if(glob(match, 0, 0, &globbuf) == 0)
		{
			if(globbuf.gl_pathc > 0)
			{
				VexInterval fileRange;

				fileRange.mjdStop = filename2fracday(globbuf.gl_pathv[0]) + mjd;
				for(unsigned int i = 0; i < globbuf.gl_pathc; i++)
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

					printf(" -> %s %f %f\n", globbuf.gl_pathv[i], fileRange.mjdStart, fileRange.mjdStop);
					if(timeRange.overlap(fileRange))
					{
						printf("*\n");
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
	TsysAverager() : pOn(0.0), pOff(0.0), n(0), receiver("0cm") {}
	void reset() { pOn = pOff = 0.0; n = 0; }
	double ratio() const { return (n > 0) ? (0.5*(pOn + pOff)/(pOn - pOff)) : 999.0; }
	void set(const VexChannel &vc);
	void print() const;
	double freq;	// signed, in MHz
	double bw;	// MHz
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
	void setup(const VexSetup &vexSetup);
	void flush();
	void feed(const VexInterval &lineTimeRange, const char *data);
};

TsysAccumulator::TsysAccumulator() : nAccum(0), out(0)
{
}

TsysAccumulator::~TsysAccumulator()
{
	std::cout << "destructor " << stn << std::endl;
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

void TsysAccumulator::setup(const VexSetup &vexSetup)
{
	std::vector<TsysAverager>::iterator ta;
	std::vector<VexChannel>::const_iterator vc;
	const VexFormat &format = vexSetup.format;
	flush();

	chans.resize(format.channels.size());
	for(vc = format.channels.begin(), ta = chans.begin(); vc != format.channels.end(); vc++, ta++)
	{
		ta->reset();
		ta->freq = vc->bbcFreq;
		if(vc->bbcSideBand == 'L')
		{
			ta->freq = -ta->freq;
		}
		ta->bw = vc->bbcBandwidth;
		ta->pol = vexSetup.getIF(vc->ifname)->pol;

		ta->print();
	}

	std::cout << "New Setup" << endl;
}

void TsysAccumulator::flush()
{
	int doy;
	int mjd;
	double day;
	std::vector<TsysAverager>::const_iterator ta;

	if(nAccum > 0)
	{
		mjd = static_cast<int>(accumTimeRange.center());
		mjd2yearday(mjd, 0, &doy);

		day = accumTimeRange.center() - mjd + doy;

		fprintf(out, "%s %12.8f %10.8f %i", stn.c_str(), day, accumTimeRange.duration(), chans.size());
		for(ta = chans.begin(); ta != chans.end(); ta++)
		{
			fprintf(out, " %5.2f %s", ta->ratio(), ta->receiver.c_str());
		}
		fprintf(out, "\n");
		nAccum = 0;
	}
}

void TsysAccumulator::feed(const VexInterval &lineTimeRange, const char *data)
{
	if(nAccum == 0)
	{
		accumTimeRange = lineTimeRange;
	}
	else
	{
		accumTimeRange.logicalOr(lineTimeRange);
	}
	nAccum++;
}

int processStation(FILE *out, const VexData &V, const string &stn, const string &fileList, const VexInterval stnTimeRange, double nominalTsysInterval)
{
	const int MaxLineLength = 32768;	// make sure it is large enough!
	char line[MaxLineLength];
	int verbose = 1;
	FILE *p;
	string command;
	int nRecord = 0;
	int nSkip = 0;
	int pos, n;
	char *v;
	VexInterval lineTimeRange;
	VexInterval scanTimeRange;
	VexInterval slotTimeRange;
	int scanNum = -1;
	const VexScan *scan;
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
			nSkip++;
			continue;
		}
		else if(lineTimeRange.mjdStop > scanTimeRange.mjdStop)
		{
			cout << "nRecord=" << nRecord << "  nSkip=" << nSkip << endl;
			TA.flush();
			
			do
			{
				map<string,VexInterval>::const_iterator it;

				scanNum++;
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
				for(it = scan->stations.begin(); it != scan->stations.end(); it++)
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

			TA.setup(*setup);  // also flushes
			
			fprintf(out, "# Scan %s\n", scan->defName.c_str());

			// set up averaging slots
			nSlot = static_cast<int>(scanTimeRange.duration_seconds() / nominalTsysInterval);
			if(nSlot == 0)
			{
				nSlot = 1;
			}
			slotDeltaT = scanTimeRange.duration() / nSlot;
			slotTimeRange.setTimeRange(scanTimeRange.mjdStart, scanTimeRange.mjdStart + slotDeltaT);

			std::cout << "New Scan: " << scanTimeRange << "  nSlot=" << nSlot << "  slotDeltaT=" << slotDeltaT << std::endl;
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

			nRecord++;
		}
	}

	fclose(p);

	return nRecord;
}

void antennaSummary(const VexData &V, map<std::string,VexInterval> &as)
{
	for(unsigned int s = 0; s < V.nScan(); s++)
	{
		const VexScan *scan = V.getScan(s);
		map<string,VexInterval>::const_iterator it;

		for(it = scan->stations.begin(); it != scan->stations.end(); it++)
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
	char vexFilename[MaxFilenameLength];
	char tsysFilename[MaxFilenameLength];
	FILE *out;
	double nominalTsysInterval = defaultTsysInterval;
	int p;

	if(argc != 3)
	{
		printf("Usage: %s vexFile tsysFile\n", program);

		return EXIT_FAILURE;
	}

	strcpy(vexFilename, argv[1]);
	strcpy(tsysFilename, argv[2]);


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
	for(it = as.begin(); it != as.end(); it++)
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

		processStation(out, *V, it->first, fileList, it->second, nominalTsysInterval);
	}

	fclose(out);

	delete V;

	return EXIT_SUCCESS;
}
