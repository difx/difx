/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken                             *
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
#include "vextables.h"
#include "vexload.h"

const std::string program("vexpeek");
const std::string version("0.4");
const std::string verdate("20121219");
const std::string author("Walter Brisken");

void usage(const char *pgm)
{
	std::cout << std::endl;
	std::cout << program << " ver. " << version << "  " << author << " " << verdate << std::endl;
	std::cout << std::endl;
	std::cout << "A program to print essential information from a vex file." << std::endl;
	std::cout << std::endl;
	std::cout << "Usage: " << pgm << " <vex filename> [-v]" << std::endl;
	std::cout << std::endl;
	std::cout << "Options:" << std::endl;
	std::cout << "  -v or --verbose : print entire vextables structure of vexfile" << std::endl;
	std::cout << "  -b or --bands : print list of band codes" << std::endl;
	std::cout << std::endl;
}

void antennaSummary(const VexData *V)
{
	std::map<std::string,VexInterval> as;

	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);

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

	int p = std::cout.precision();

	std::cout.precision(13);

	for(std::map<std::string,VexInterval>::const_iterator it = as.begin(); it != as.end(); ++it)
	{
		std::cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop << std::endl;
	}

	std::cout.precision(p);
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
	CorrParams *P;
	int v;
	int nWarn = 0;

	if(argc < 2)
	{
		usage(argv[0]);

		return 1;
	}

	v = testVex(argv[1]);
	if(v != 0)
	{
		std::cout << "Error code " << v << std::endl;

		return 1;
	}

	P = new CorrParams();
	P->defaultSetup();
	P->minSubarraySize = 1;
	P->vexFile = argv[1];

	V = loadVexFile(*P, &nWarn);

	if(argc > 2 && (strcmp(argv[2], "-v") == 0 || strcmp(argv[2], "--verbose") == 0) )
	{
		std::cout << *V << std::endl;
		std::cout << std::endl;
	}
	else if(argc > 2 && (strcmp(argv[2], "-b") == 0 || strcmp(argv[2], "--bands") == 0) )
	{
		bandList(V);
	}
	else
	{
		std::cout << V->getExper()->name << std::endl;

		antennaSummary(V);
	}

	delete V;

	return 0;
}
