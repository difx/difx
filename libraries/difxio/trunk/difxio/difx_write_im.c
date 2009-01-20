/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdlib.h>
#include <string.h>
#include "difxio/difx_write.h"

int writeDifxIM(const DifxInput *D, const char *filename)
{
	FILE *out;
	DifxScan *scan;
	int a, s, p;
	int refAnt, order;
	const char *name;

	if(!D)
	{
		return -1;
	}

	if(D->nJob != 1)
	{
		fprintf(stderr, "writeDifxIM: nJob = %d (not 1)\n", 
			D->nJob);
		return -1;
	}

	if(D->nAntenna < 1 || !D->job || D->nScan < 0)
	{
		return 0;
	}

	out = fopen(filename, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", filename);
		return -1;
	}

	writeDifxLine(out, "CALC SERVER", D->job->calcServer);
	writeDifxLineInt(out, "CALC PROGRAM", D->job->calcProgram);
	writeDifxLineInt(out, "CALC VERSION", D->job->calcVersion);

	writeDifxDateLines(out, D->job->mjdStart);
	
	writeDifxLineInt(out, "POLYNOMIAL ORDER", D->job->polyOrder);
	writeDifxLineInt(out, "INTERVAL (SECS)", D->job->polyInterval);
	writeDifxLine(out, "ABERRATION CORR", 
		aberCorrStrings[D->job->aberCorr]);

	writeDifxAntennaArray(out, D->nAntenna, D->antenna, 0, 0, 0, 0, 0);

	writeDifxLineInt(out, "NUM SCANS", D->nScan);

	for(s = 0; s < D->nScan; s++)
	{
		scan = D->scan + s;

		if(scan->configId >= 0 && scan->configId < 1<<12)
		{
			DifxConfig *config;
			config = D->config + scan->configId;
			name = config->name;
		}
		else
		{
			name = scan->name;
		}

		writeDifxLine1(out, "SCAN %d SRC NAME", s, name);
		
		for(refAnt = 0; refAnt < scan->nAntenna; refAnt++)
		{
			if(scan->im[refAnt])
			{
				break;
			}
		}
		if(refAnt == scan->nAntenna)
		{
			writeDifxLineInt1(out, "SCAN %d NUM POLY", s, 0);
			continue;
		}

		writeDifxLineInt1(out, "SCAN %d NUM POLY", s, scan->nPoly);
		
		for(p = 0; p < scan->nPoly; p++)
		{
			writeDifxLineInt2(out, "SCAN %d POLY %d MJD",
				s, p, scan->im[refAnt][p].mjd);
			writeDifxLineInt2(out, "SCAN %d POLY %d SEC",
				s, p, scan->im[refAnt][p].sec);
			for(a = 0; a < scan->nAntenna; a++)
			{
				if(scan->im[a] == 0)
				{
					continue;
				}
				order = scan->im[a][p].order;
				writeDifxLineArray1(out, "ANT %d DELAY (us)", 
					a, scan->im[a][p].delay, order+1);
				writeDifxLineArray1(out, "ANT %d DRY (us)", 
					a, scan->im[a][p].dry, order+1);
				writeDifxLineArray1(out, "ANT %d WET (us)", 
					a, scan->im[a][p].wet, order+1);
				writeDifxLineArray1(out, "ANT %d U (m)",
					a, scan->im[a][p].u, order+1);
				writeDifxLineArray1(out, "ANT %d V (m)",
					a, scan->im[a][p].v, order+1);
				writeDifxLineArray1(out, "ANT %d W (m)",
					a, scan->im[a][p].w, order+1);
			}
		}
	}

	fclose(out);

	return 0;
}
