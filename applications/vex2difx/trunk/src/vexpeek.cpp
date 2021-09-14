/***************************************************************************
 *   Copyright (C) 2009-2021 by Walter Brisken and Jan Wagner              *
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

#include <vector>
#include <set>
#include <map>
#include <sstream>
#include <difxio/difx_input.h>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <vexdatamodel.h>
#include "testvex.h"

const std::string program("vexpeek");
const std::string version("0.16");
const std::string verdate("20210914");
const std::string author("Walter Brisken");

void usage(const char *pgm)
{
	std::cout << std::endl;
	std::cout << program << " ver. " << version << "  " << author << " " << verdate << std::endl;
	std::cout << std::endl;
	std::cout << "A program to print essential information from a vex file." << std::endl;
	std::cout << std::endl;
	std::cout << "Usage: " << pgm << " <vex filename> [options]" << std::endl;
	std::cout << std::endl;
	std::cout << "options can include:" << std::endl;
	std::cout << "  -h or --help : print help info and quit" << std::endl;
	std::cout << "  -v or --verbose : print entire vextables structure of vexfile" << std::endl;
	std::cout << "  -f or --format : add data format to output" << std::endl;
	std::cout << "  -t or --doTime : add detailed time data to some output" << std::endl;
	std::cout << "  -b or --bands : print list of band codes" << std::endl;
	std::cout << "  -s or --scans : print list of scans and their stations" << std::endl;
	std::cout << "        --scans2 : print list of scans with bands and times" << std::endl;
	std::cout << "                 - include twice to see nChan, nBit and bandwidth as well" << std::endl;
	std::cout << "  --scans=<ant> : print list of scans for antenna <ant>" << std::endl;
	std::cout << "  -r or --sources : print list of sources and their coordinates" << std::endl;
	std::cout << "  -u or --diskusage : print disk usage (GB)" << std::endl;
	std::cout << "  -m or --modules : print disk modules used (from TAPELOG_OBS)" << std::endl;
	std::cout << "  -c or --coords : print station coordinates" << std::endl;
	std::cout << "  -a or --all : print summary, bands, scans, and modules" << std::endl;
	std::cout << std::endl;
	std::cout << "  -B, -S, -M -R and/or -C can be used to add one of these sections to the output." << std::endl;
	std::cout << std::endl;
}

double totalDiskUsageGB(const VexData *V, const std::string &antName, double* rate = NULL)
{
	double GB = 0.0;

	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);
		if(!scan)
		{
			continue;
		}

		const Interval *I = scan->getAntennaInterval(antName);
		if(!I)
		{
			continue;
		}

		const VexMode *M = V->getModeByDefName(scan->modeDefName);
		if(!M)
		{
			continue;
		}

		const VexSetup *S = M->getSetup(antName);
		if(!S)
		{
			continue;
		}

		if (rate != NULL)
		{
			*rate = S->dataRateMbps();
		}

		GB += S->dataRateMbps()*I->duration_seconds()/8000.0;
	}

	return GB;
}

void antennaSummary(const VexData *V, int doFormat, int doUsage)
{
	std::map<std::string,Interval> as;
	std::map<std::string,std::string> af;

	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);

		for(std::map<std::string,Interval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			const Interval &vi = it->second;

			if(as.count(it->first) == 0)
			{
				as[it->first] = Interval(vi);

				// get format
				const VexMode *M = V->getModeByDefName(scan->modeDefName);
				if(M)
				{
					const VexSetup *S = M->getSetup(it->first);
					if(S)
					{
						af[it->first] = VexStream::DataFormatNames[S->streams[0].format];
					}
				}
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

	int p = std::cout.precision();

	std::cout.precision(13);

	for(std::map<std::string,Interval>::const_iterator it = as.begin(); it != as.end(); ++it)
	{
		std::cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop;
		if(doFormat)
		{
			std::cout << " " << af[it->first];
		}
		if(doUsage)
		{
			int p = std::cout.precision();
			double drate=0.0, usage;
			usage = totalDiskUsageGB(V, it->first, &drate);
			std::cout.precision(3);
			std::cout << std::fixed << " " << usage << " " << (int)drate;
			std::cout.precision(p);
		}
		std::cout << std::endl;
	}

	std::cout.precision(p);
}

void moduleSummary(VexData *V)
{
	std::map<std::string,Interval> as;

	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);

		for(std::map<std::string,Interval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			const Interval &vi = it->second;

			if(as.count(it->first) == 0)
			{
				as[it->first] = Interval(vi);
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

	int p = std::cout.precision();

	std::cout.precision(13);

	for(std::map<std::string,Interval>::const_iterator it = as.begin(); it != as.end(); ++it)
	{
		std::vector<VexBasebandData> *vsns;

		vsns = V->getVSNs(it->first);
		for(std::vector<VexBasebandData>::const_iterator vi = vsns->begin(); vi != vsns->end(); ++vi)
		{
			std::cout << it->first << " " << vi->filename << " " << vi->mjdStart << " " << vi->mjdStop << std::endl;
		}
	}

	std::cout.precision(p);
}

void scanList(const VexData *V)
{
	std::vector<std::string> allStations;
	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);
		std::cout << std::left << std::setw(8) << scan->defName << " ";
		std::cout << std::left << std::setw(12) << scan->sourceDefName << " ";
		std::cout << std::left << std::setw(12) << scan->modeDefName << "   ";

		std::vector<std::string> currStations;
		for(std::map<std::string,Interval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			currStations.push_back(it->first);
			if(std::find(allStations.begin(), allStations.end(), it->first) == allStations.end())
			{
				allStations.push_back(it->first);
			}
		}
		for(std::vector<std::string>::iterator it = allStations.begin(); it != allStations.end(); ++it)
		{
			std::string ant = "--";
			if(std::find(currStations.begin(), currStations.end(), *it) != currStations.end())
			{
				ant = *it;
			}
			std::cout << std::setw(3) << ant << " ";
		}
		std::cout << std::endl;
	}
}

void scanListWithTimes(const VexData *V)
{
	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);
		std::cout << std::left << std::setw(8) << scan->defName << " ";
		std::cout << std::left << std::setw(12) << scan->sourceDefName << " ";
		std::cout << std::left << std::setw(12) << scan->modeDefName << "   ";

		std::cout.precision(14);
		for(std::map<std::string,Interval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			std::cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop << "  ";
		}
		std::cout << std::endl;
	}
}

void scanListByAntenna(const VexData *V, const char *ant)
{
	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);

		std::cout.precision(14);
		for(std::map<std::string,Interval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			if(strcasecmp(ant, it->first.c_str()) == 0)
			{
				std::cout << std::left << std::setw(8) << scan->defName << " ";
				std::cout << std::left << std::setw(12) << scan->sourceDefName << " ";
				std::cout << std::left << std::setw(12) << scan->modeDefName << "   ";
				std::cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop;
				std::cout << std::endl;
			}
		}
	}
}

// print: ScanName SourceName Band NAntenna startMJD stopMJD
void scan2List(const VexData *V, int level)
{
	std::set<char> bands;

	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);
		const VexMode *M = V->getModeByDefName(scan->modeDefName);
		std::cout << std::left << std::setw(8) << scan->defName << " ";
		std::cout << std::left << std::setw(12) << scan->sourceDefName << " ";

		bands.clear();
		for(std::map<std::string,VexSetup>::const_iterator s = M->setups.begin(); s != M->setups.end(); ++s)
		{
			for(std::vector<VexChannel>::const_iterator v = s->second.channels.begin(); v != s->second.channels.end(); ++v)
			{
				bands.insert(v->bandCode());
			}
		}

		for(std::set<char>::const_iterator b=bands.begin(); b != bands.end(); ++b)
		{
			std::cout << *b;
		}

		std::cout << " " << scan->stations.size() << " ";

		std::cout.precision(14);
		std::cout << scan->mjdStart << " " << scan->mjdStop;

		if(level > 1)
		{
			std::cout << " " << M->zRecordChan() << " " << M->zBits() << " " << (M->zBandwidth()/1000000.0);
		}

		std::cout << std::endl;
	}
}

void sourceList(const VexData *V)
{
	for(unsigned int s = 0; s < V->nSource(); ++s)
	{
		const VexSource *source = V->getSource(s);
		std::cout << std::left << std::setw(VexSource::MAX_SRCNAME_LENGTH) << source->defName << " ";
		std::cout.precision(12);
		std::cout << std::left << std::setw(16) << source->ra << " " << std::left << std::setw(17) << source->dec;
		if(!source->sourceType1.empty())
		{
			std::cout << " " << source->sourceType1;
			if(!source->sourceType2.empty())
			{
				std::cout << " " << source->sourceType2;
			}
		}
		if(!source->sourceNames.empty())
		{
			std::cout << " :";
			for(std::vector<std::string>::const_iterator it = source->sourceNames.begin(); it != source->sourceNames.end(); ++it)
			{
				std::cout << " " << *it;
			}
		}

		std::cout << std::endl;
	}
}

void bandList(const VexData *V)
{
	int nMode = V->nMode();
	std::set<char> bands;

	for(int m = 0; m < nMode; ++m)
	{
		const VexMode *M = V->getMode(m);
		for(std::map<std::string,VexSetup>::const_iterator s = M->setups.begin(); s != M->setups.end(); ++s)
		{
			for(std::vector<VexChannel>::const_iterator v = s->second.channels.begin(); v != s->second.channels.end(); ++v)
			{
				bands.insert(v->bandCode());
			}
		}
	}

	for(std::set<char>::const_iterator b=bands.begin(); b != bands.end(); ++b)
	{
		std::cout << *b << " ";
	}
	std::cout << std::endl;
}

void antCoords(const VexData *V)
{
	const double secPerYear = 86400.0*365.25;
	int nAntenna = V->nAntenna();
	int p = std::cout.precision();
	std::cout.precision(6);
	std::cout << std::fixed;

	for(int a = 0; a < nAntenna; ++a)
	{
		const VexAntenna *A = V->getAntenna(a);
		std::cout << A->name << " " << A->x << " " << A->y << " " << A->z << "  " << (A->dx*secPerYear) << " " << (A->dy*secPerYear) << " " << (A->dz*secPerYear) << "  " << A->posEpoch << std::endl;
	}

	std::cout.precision(p);
	std::cout << std::scientific;
}

int main(int argc, char **argv)
{
	VexData *V;
	int v;
	int doSummary = 1;
	unsigned int nWarn = 0;
	int verbose = 0;
	int doBandList = 0;
	int doScanList = 0;
	int doScan2List = 0;
	int doSourceList = 0;
	int doFormat = 0;
	int doUsage = 0;
	int doModules = 0;
	int doTime = 0;
	int doCoords = 0;
	int a;
	const char *fileName = 0;
	const char *scanAnt = 0;

	for(a = 1; a < argc; ++a)
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
		else if(strcmp(argv[a], "-f") == 0 ||
		        strcmp(argv[a], "--format") == 0)
		{
			++doFormat;
		}
		else if(strcmp(argv[a], "-u") == 0 ||
			strcmp(argv[a], "--diskusage") == 0)
		{
			++doUsage;
		}
		else if(strcmp(argv[a], "-t") == 0 ||
			strcmp(argv[a], "--doTime") == 0)
		{
			++doTime;
		}
		else if(strcmp(argv[a], "-b") == 0 ||
		        strcmp(argv[a], "--bands") == 0)
		{
			++doBandList;
			doSummary = 0;
		}
		else if(strcmp(argv[a], "-s") == 0 ||
		        strcmp(argv[a], "--scans") == 0)
		{
			++doScanList;
			doSummary = 0;
		}
		else if(strcmp(argv[a], "--scans2") == 0)
		{
			++doScan2List;
			doSummary = 0;
		}
		else if(strcmp(argv[a], "-r") == 0 ||
		        strcmp(argv[a], "--sources") == 0)
		{
			++doSourceList;
			doSummary = 0;
		}
		else if(strcmp(argv[a], "-m") == 0 ||
			strcmp(argv[a], "--modules") == 0)
		{
			++doModules;
			doSummary = 0;
		}
		else if(strcmp(argv[a], "-c") == 0 ||
			strcmp(argv[a], "--coords") == 0)
		{
			++doCoords;
			doSummary = 0;
		}
		else if(strcmp(argv[a], "-B") == 0 ||
		        strcmp(argv[a], "--Bands") == 0)
		{
			++doBandList;
		}
		else if(strncmp(argv[a], "--scans=", 8) == 0 && argv[a][8] != 0)
		{
			++doScanList;
			scanAnt = argv[a]+8;
		}
		else if(strcmp(argv[a], "-S") == 0 ||
		        strcmp(argv[a], "--Scans") == 0)
		{
			++doScanList;
		}
		else if(strcmp(argv[a], "-R") == 0 ||
		        strcmp(argv[a], "--Sources") == 0)
		{
			++doSourceList;
		}
		else if(strcmp(argv[a], "-M") == 0 ||
			strcmp(argv[a], "--Modules") == 0)
		{
			++doModules;
		}
		else if(strcmp(argv[a], "-C") == 0 ||
			strcmp(argv[a], "--Coords") == 0)
		{
			++doCoords;
		}
		else if(strcmp(argv[a], "-a") == 0 ||
		        strcmp(argv[a], "--all") == 0)
		{
			++doBandList;
			++doScanList;
			++doSourceList;
			++doModules;
			++doCoords;
		}
		else if(argv[a][0] == '-')
		{
			printf("Unknown option %s .  Run with -h for help.\n\n", argv[a]);

			return EXIT_FAILURE;
		}
		else if(fileName != 0)
		{
			printf("Error: only one file name can be provided.\n\n");

			return EXIT_FAILURE;
		}
		else
		{
			fileName = argv[a];
		}
	}

	if(fileName == 0)
	{
		printf("No file name provided.  Run with -h for help.\n\n");

		return EXIT_FAILURE;
	}

	v = testVex(fileName);
	if(v != 0)
	{
		std::cout << "Error code " << v << std::endl;

		return 1;
	}

	V = loadVexFile(std::string(fileName), &nWarn);

	if(verbose)
	{
		std::cout << *V << std::endl;
		std::cout << std::endl;
	}

	if(doModules || doTime)
	{
		int p = std::cout.precision();

		std::cout.precision(13);
		std::cout << V->getExper()->name << " " << V->obsStart() << " " << V->obsStop() << std::endl;
		std::cout.precision(p);
	}
	if(doSummary)
	{
		if(!doModules && !doTime)
		{
			std::cout << V->getExper()->name << std::endl;
		}
		antennaSummary(V, doFormat, doUsage);
	}
	if(doBandList)
	{
		bandList(V);
	}
	if(doSourceList)
	{
		sourceList(V);
	}
	if(doScanList)
	{
		if(scanAnt != 0)
		{
			scanListByAntenna(V, scanAnt);
		}
		else if(doTime)
		{
			scanListWithTimes(V);
		}
		else
		{
			scanList(V);
		}
	}
	if(doScan2List)
	{
		scan2List(V, doScan2List);
	}
	if(doModules)
	{
		moduleSummary(V);
	}
	if(doCoords)
	{
		antCoords(V);
	}

	delete V;

	return 0;
}
