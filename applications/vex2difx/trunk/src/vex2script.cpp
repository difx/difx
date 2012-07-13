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
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <fstream>
#include <cstring>
#include <cstdlib>
#include "pystream.h"
#include "corrparams.h"
#include "vexload.h"

const string program("vex2script");
const string version("0.6");
const string verdate("20120517");
const string author("Walter Brisken, Adam Deller, Matthias Bark");

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
        cout << "  --phasingsources=source1,source2... (for EVLA)" << endl;
        cout << "  --dbepersonality=[path/]filename (for VLBA)" << endl;
	cout << "  --mark5a  Set up DBE as PFB, but don't record" << endl;
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
	pystream py;
	pystream::ScriptType sType;
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

	for(int count = 2; count < argc; ++count)
	{
		//this is an interim measure to set the phasing sources for EVLA
		if(strncmp(argv[count], "--phasingsources=", 17) == 0)
		{
			atchar = 17;
			lastchar = 17;
			while(argv[count][atchar] != '\0')
			{
				if(argv[count][atchar] == ',')
				{
					argv[count][atchar] = '\0';
					py.addPhasingSource(string(argv[count]+lastchar));
					++atchar;
					lastchar = atchar;
				}
				++atchar;
			}
			py.addPhasingSource(string(argv[count]+lastchar));
		}
                else if(strncmp(argv[count], "--dbepersonality=", 17) == 0)
		{
			atchar = 17;
			lastchar = 17;
			while(argv[count][atchar] != '\0')
			{
				++atchar;
			}
			py.setDBEPersonality(string(argv[count]+lastchar));
                }
		else if(strcasecmp(argv[count], "--mark5A") == 0)
		{
			py.setMark5A(true);
			py.setDBEPersonalityType(pystream::RDBE_PFB);
			py.setRecorderType(pystream::RECORDER_NONE);
		}
                else
		{
			cout << "Ignoring argument " << argv[count] << endl;
		}
	}	

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
		if(isEVLA(A->name))
		{
			cout << "Skipping VLA antenna" << endl;
			continue;

			cout << "VLA antenna " << a << " = " << A->name << endl;
			sType = pystream::SCRIPT_EVLA;
		}
		else if(isGBT(A->name))
		{
			cout << "GBT antenna " << a << " = " << A->name << endl;
			sType = pystream::SCRIPT_GBT;
		}
		else if(isVLBA(A->name))
		{
			cout << "VLBA antenna " << a << " = " << A->name << endl;
			sType = pystream::SCRIPT_VLBA;
		}
		else 
		{
			cout << "Skipping unknown antenna " << A->name << endl;
			continue;
		}

		py.open(A->name, V, sType);

		py.writeHeader(V);
		py.writeComment(string("File written by ") + program + string(" version ") + version + string(" vintage ") + verdate);
		py.writeDbeInit(V);
		py.writeRecorderInit(V);
		if(sType == pystream::SCRIPT_GBT)
		{
			py.writeScansGBT(V);
		}
		else
		{
			py.writeLoifTable(V);
			py.writeSourceTable(V);
			py.writeScans(V);
		}

		py.close();
	}
	

	return EXIT_SUCCESS;
}
