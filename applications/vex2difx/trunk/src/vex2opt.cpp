/***************************************************************************
 *   Copyright (C) 2009-2012 by Walter Brisken, Adam Deller, Matthias Bark *
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
 * $Id: vex2script.cpp 4560 2012-05-17 17:49:14Z MatthiasBark $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/vex2script.cpp $
 * $LastChangedRevision: 4560 $
 * $Author: MatthiasBark $
 * $LastChangedDate: 2012-05-17 11:49:14 -0600 (Thu, 17 May 2012) $
 *
 *==========================================================================*/

#include <fstream>
#include <cstring>
#include <cstdlib>
#include "optsources.h"
#include "optresources.h"
#include "optscans.h"
#include "corrparams.h"
#include "vexload.h"

using namespace std;

const string program("vex2opt");
const string version("0.2");
const string verdate("20130108");
const string author("Matthias Bark");

static void usage(int argc, char **argv)
{
	cout << endl;
	cout << program << " version " << version << "  " << author << " " << verdate << endl;
	cout << endl;
	cout << "Usage:  %s <vexfile> [options]" << endl;
	cout << endl;
	cout << "The first argument must be the name of a valid vex file." << endl;
	cout << endl;
	cout << "The optional arguments can be:" << endl;
	cout << endl;
}

bool isVLBA(const string& antName)
{
	const string VLBAantennas[] = 
		{"BR", "FD", "HN", "KP", "LA", "MK", "NL", "OV", "PT", "SC", ""};	// terminate list with "" !

	for(unsigned int i = 0; VLBAantennas[i] != ""; ++i)
	{
		if(VLBAantennas[i] == antName)
		{
			return true;
		}
	}

	return false;
}

bool isEVLA(const string& ant)
{
	if(ant == "Y" || ant == "Y27" || ant == "Y1")
	{
		return true;
	}

	return false;
}

bool isGBT(const string& ant)
{
	if(ant == "GB")
	{
		return true;
	}
	
	return false;
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	const VexAntenna *A;
	int nAntenna, nScan, atchar, lastchar;
	optsources pySources;
	optscans pyScans;
	optresources pyResources;
	int nWarn = 0;

	if(argc < 2 || strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)
	{
		usage(argc, argv);

		return EXIT_SUCCESS;
	}

	P = new CorrParams();
	P->vexFile = string(argv[1]);
	P->defaultSetup();
	P->minSubarraySize = 1;

	V = loadVexFile(*P, &nWarn);

	if(V == NULL)
	{
		cerr << "ERROR loading vex file! File may not exist!" << endl;

		exit(EXIT_FAILURE);
	}

	nAntenna = V->nAntenna();
	nScan = V->nScan();

	cout << "nAntenna = " << nAntenna << "  nScan = " << nScan << endl;

	for(int a = 0; a < nAntenna; ++a)
	{
		A = V->getAntenna(a);

		if( isEVLA(A->name) ) {
			pyScans.open(A->name, V);
			pyScans.writeComment(string("File written by ") + program + string(" version ") + version + string(" vintage ") + verdate);
			pyScans.writeScans(V);
			pyScans.close();

			pySources.open(A->name, V);
			pySources.writeComment(string("File written by ") + program + string(" version ") + version + string(" vintage ") + verdate);
			pySources.writeSourceTable(V);
			pySources.close();

			pyResources.open(A->name, V);
			pyResources.writeComment(string("File written by ") + program + string(" version ") + version + string(" vintage ") + verdate);
			pyResources.writeHeader(V);
			pyResources.writeLoifTable(V);
			pyResources.close();
		} else {
			cerr << "Not EVLA antenna - skip!" << endl;
		}
	}
	
	return EXIT_SUCCESS;
}
