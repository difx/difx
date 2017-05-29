/***************************************************************************
 *   Copyright (C) 2009-2015 by Walter Brisken                             *
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

const std::string program("vexpeek");
const std::string version("0.7");
const std::string verdate("20130925");
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
	std::cout << "  -b or --bands : print list of band codes" << std::endl;
	std::cout << "  -s or --scans : print list of scans and their stations" << std::endl;
	std::cout << "  -u or --diskusage : print disk usage (GB)" << std::endl;
	std::cout << std::endl;
}

double totalDiskUsageGB(const VexData *V, const std::string &antName)
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
			std::cout.precision(3);
			std::cout << " " << totalDiskUsageGB(V, it->first);
			std::cout.precision(p);
		}
		std::cout << std::endl;
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
		std::cout << std::left << std::setw(10) << scan->sourceDefName << " ";
		std::cout << std::left << std::setw(10) << scan->modeDefName << "   ";

		std::vector<std::string> currStations;
		for(std::map<std::string,Interval>::const_iterator it = scan->stations.begin(); it != scan->stations.end(); ++it)
		{
			currStations.push_back(it->first);
			if (std::find(allStations.begin(), allStations.end(), it->first) == allStations.end())
			{
				allStations.push_back(it->first);
			}
		}
		for (std::vector<std::string>::iterator it = allStations.begin(); it != allStations.end(); ++it)
		{
			std::string ant = "--";
			if (std::find(currStations.begin(), currStations.end(), *it) != currStations.end())
			{
				ant = *it;
			}
			std::cout << std::setw(3) << ant << " ";
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

int testVex(const std::string &vexFile)
{
	const int MaxLineLength=128;
	std::ifstream is;
	char s[MaxLineLength];

	is.open(vexFile.c_str());
	if(is.fail())
	{
		std::cerr << "Error: vex2difx cannot open " << vexFile << std::endl;

		return -1;
	}

	is.getline(s, MaxLineLength);
	if(is.eof())
	{
		std::cerr << "Error: unexpected end of file: " << vexFile << std::endl;

		return -2;
	}

	if(strncmp(s, "$EXPER ", 7) == 0)
	{
		std::cerr << "Error: " << vexFile << " looks like a sked input file and is not vex formatted." << std::endl;

		return -3;
	}

	if(strncmp(s, "VEX", 3) != 0)
	{
		std::cerr << "Error: " << vexFile << " is not a vex file." << std::endl;

		return -4;
	}

	return 0;
}

int main(int argc, char **argv)
{
	VexData *V;
	int v;
	int nWarn = 0;
	int verbose = 0;
	int doBandList = 0;
	int doScanList = 0;
	int doFormat = 0;
	int doUsage = 0;
	int a;
	const char *fileName = 0;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-v") == 0 ||
		   strcmp(argv[a], "--verbose") == 0)
		{
			++verbose;
		}
		else if(strcmp(argv[a], "-b") == 0 ||
		        strcmp(argv[a], "--bands") == 0)
		{
			++doBandList;
		}
		else if(strcmp(argv[a], "-s") == 0 ||
		        strcmp(argv[a], "--scans") == 0)
		{
			++doScanList;
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
		else if(strcmp(argv[a], "-h") == 0 ||
		        strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
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

	if(doBandList)
	{
		bandList(V);
	}
	else if(doScanList)
	{
		scanList(V);
	}
	else if(verbose)
	{
		std::cout << *V << std::endl;
		std::cout << std::endl;
	}
	else
	{
		std::cout << V->getExper()->name << std::endl;

		antennaSummary(V, doFormat, doUsage);
	}

	delete V;

	return 0;
}
