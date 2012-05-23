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
#include <sstream>
#include <difxio/difx_input.h>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include "vextables.h"
#include "vexload.h"

const string program("vexpeek");
const string version("0.3");
const string verdate("20120523");
const string author("Walter Brisken");

void usage(const char *pgm)
{
	cout << endl;
	cout << program << " ver. " << version << "  " << author << " " << verdate << endl;
	cout << endl;
	cout << "A program to print essential information from a vex file." << endl;
	cout << endl;
	cout << "Usage: " << pgm << " <vex filename> [-v]" << endl;
	cout << endl;
	cout << "Options:" << endl;
	cout << "  -v or --verbose : print entire vextables structure of vexfile" << endl;
	cout << "  -b or --bands : print list of band codes" << endl;
	cout << endl;
}

void antennaSummary(const VexData *V)
{
	map<string,VexInterval> as;

	for(unsigned int s = 0; s < V->nScan(); ++s)
	{
		const VexScan *scan = V->getScan(s);

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

	int p = cout.precision();

	cout.precision(13);

	for(map<string,VexInterval>::const_iterator it = as.begin(); it != as.end(); ++it)
	{
		cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop << endl;
	}

	cout.precision(p);
}

void bandList(const VexData *V)
{
	int nMode = V->nMode();
	set<char> bands;

	for(int m = 0; m < nMode; ++m)
	{
		const VexMode *M = V->getMode(m);
		for(map<string,VexSetup>::const_iterator s = M->setups.begin(); s != M->setups.end(); ++s)
		{
			for(vector<VexChannel>::const_iterator v = s->second.channels.begin(); v != s->second.channels.end(); ++v)
			{
				bands.insert(v->bandCode());
			}
		}
	}

	for(set<char>::const_iterator b=bands.begin(); b != bands.end(); ++b)
	{
		cout << *b << " ";
	}
	cout << endl;
}

int testVex(const string &vexFile)
{
	const int MaxLineLength=128;
	ifstream is;
	char s[MaxLineLength];

	is.open(vexFile.c_str());
	if(is.fail())
	{
		cerr << "Error: vex2difx cannot open " << vexFile << endl;

		return -1;
	}

	is.getline(s, MaxLineLength);
	if(is.eof())
	{
		cerr << "Error: unexpected end of file: " << vexFile << endl;

		return -2;
	}

	if(strncmp(s, "$EXPER ", 7) == 0)
	{
		cerr << "Error: " << vexFile << " looks like a sked input file and is not vex formatted." << endl;

		return -3;
	}

	if(strncmp(s, "VEX", 3) != 0)
	{
		cerr << "Error: " << vexFile << " is not a vex file." << endl;

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
		cout << "Error code " << v << endl;

		return 1;
	}

	P = new CorrParams();
	P->defaultSetup();
	P->minSubarraySize = 1;
	P->vexFile = argv[1];

	V = loadVexFile(*P, &nWarn);

	if(argc > 2 && (strcmp(argv[2], "-v") == 0 || strcmp(argv[2], "--verbose") == 0) )
	{
		cout << *V << endl;
		cout << endl;
	}
	else if(argc > 2 && (strcmp(argv[2], "-b") == 0 || strcmp(argv[2], "--bands") == 0) )
	{
		bandList(V);
	}
	else
	{
		cout << V->getExper()->name << endl;

		antennaSummary(V);
	}

	delete V;

	return 0;
}
