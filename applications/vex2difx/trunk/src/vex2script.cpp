/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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
#include "pystream.h"
#include "corrparams.h"
#include "vexload.h"

const string program("vex2script");
const string version("0.1");
const string verdate("20091108");
const string author("Walter Brisken");

int usage(int argc, char **argv)
{
	cout << endl;
	cout << program << " version " << version << "  " << author << " " << verdate << endl;
	cout << endl;

	return 0;
}

bool isVLBA(const string& ant)
{
	const string VLBAantennas[10] = 
		{"BR", "FD", "HN", "KP", "LA", "MK", "NL", "OV", "PT", "SC"};

	for(unsigned int i = 0; i < 10; i++)
	{
		if(VLBAantennas[i] == ant)
		{
			return true;
		}
	}

	return false;
}

int main(int argc, char **argv)
{
	VexData *V;
	CorrParams *P;
	const VexAntenna *A;
	int nAntenna, nScan;
	pystream py;


	if(argc < 2)
	{
		return usage(argc, argv);
	}

	P = new CorrParams();
	P->vexFile = string(argv[1]);
	P->defaultSetup();

	V = loadVexFile(*P);

	nAntenna = V->nAntenna();
	nScan = V->nScan();

	cout << "nAntenna = " << nAntenna << "  nScan = " << nScan << endl;

	//nAntenna = 1;	// FIXME -- removeme

	for(int a = 0; a < nAntenna; a++)
	{
		A = V->getAntenna(a);
		if(!isVLBA(A->name))
		{
			cout << "Skipping non VLBA antenna " << A->name << endl;
			continue;
		}
		cout << "Antenna " << a << " = " << A->name << endl;
		py.open(A->name, V);

		py.writeHeader(V);
		py.writeRecorderInit(V);
		py.writeDbeInit(V);
		py.writeLoifTable(V);
		py.writeSourceTable(V);
		py.writeScans(V);

		py.close();
	}
	

	return 0;
}
