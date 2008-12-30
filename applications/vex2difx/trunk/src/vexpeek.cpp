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

const string program("vexpeep");
const string version("0.1");
const string verdate("20081208");
const string author("Walter Brisken");

void antennaSummary(const VexData *V)
{
	int s;
	map<string,VexInterval> as;

	for(s = 0; s < V->nScan(); s++)
	{
		const VexScan *scan = V->getScan(s);
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

	int p = cout.precision();

	cout.precision(13);

	map<string,VexInterval>::const_iterator it;
	for(it = as.begin(); it != as.end(); it++)
	{
		cout << it->first << " " << it->second.mjdStart << " " << it->second.mjdStop << endl;
	}

	cout.precision(p);
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	 
	if(argc < 2)
	{
		cout << endl;
		cout << program << " ver. " << version << "  " << author << " " << verdate << endl;
		cout << endl;
		cout << "Usage: " << argv[0] << " <vex filename>" << endl;
		cout << endl;
		return 0;
	}

	P = new CorrParams();
	P->defaultSetup();
	P->vexFile = argv[1];

	V = loadVexFile(*P);

	cout << V->getExper()->name << endl;

	antennaSummary(V);


	delete V;

	return 0;
}
