/***************************************************************************
 *   Copyright (C) 2008-2015 by Walter Brisken                             *
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

int writeDifxIM(const DifxInput *D)
{
	FILE *out;
	DifxScan *scan;
	int a, s, p, i;
	int refAnt, order;

	if(!D)
	{
		return -1;
	}

	if(D->nJob != 1)
	{
		fprintf(stderr, "writeDifxIM: nJob = %d (not 1)\n", D->nJob);

		return -1;
	}

	if(D->nAntenna < 1 || !D->job || D->nScan < 0)
	{
		return 0;
	}

	out = fopen(D->job->imFile, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", D->job->imFile);

		return -1;
	}

	writeDifxLine(out, "CALC SERVER", D->job->calcServer);
	writeDifxLineInt(out, "CALC PROGRAM", D->job->calcProgram);
	writeDifxLineInt(out, "CALC VERSION", D->job->calcVersion);

	if(D->fracSecondStartTime > 0)
	{
		writeDifxDateLines(out, truncSeconds(D->job->mjdStart));
	}
	else
	{
		writeDifxDateLines(out, roundSeconds(D->job->mjdStart));
	}
	
	writeDifxLineInt(out, "POLYNOMIAL ORDER", D->job->polyOrder);
	writeDifxLineInt(out, "INTERVAL (SECS)", D->job->polyInterval);
	writeDifxLine(out, "ABERRATION CORR", aberCorrStrings[D->job->aberCorr]);

	writeDifxAntennaArray(out, D->nAntenna, D->antenna, 0, 0, 0, 0, 0, 0);

	writeDifxLineInt(out, "NUM SCANS", D->nScan);

	for(s = 0; s < D->nScan; ++s)
	{
		scan = D->scan + s;

		writeDifxLine1(out, "SCAN %d POINTING SRC", s, D->source[scan->pointingCentreSrc].name);
		writeDifxLineInt1(out, "SCAN %d NUM PHS CTRS", s, scan->nPhaseCentres);
		for(i = 0; i < scan->nPhaseCentres; ++i)
		{
			writeDifxLine2(out, "SCAN %d PHS CTR %d SRC", s, i, D->source[scan->phsCentreSrcs[i]].name);
		}

		if(!scan->im)
		{
			writeDifxLineInt1(out, "SCAN %d NUM POLY", s, 0);
			fprintf(stderr, "Warning: Scan %d being written with no delay model information\n", s);
			continue;
		}
		
		for(refAnt = 0; refAnt < scan->nAntenna; ++refAnt)
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
		if(scan->imLM)
		{
			writeDifxLineDouble1(out, "SCAN %d DELTA LM (rad)", s, "%10.8f", scan->imLM[refAnt][0][0].delta);
		}
		else if(scan->imXYZ)
		{
			writeDifxLineDouble1(out, "SCAN %d DELTA XYZ (m)", s, "%5.3f", scan->imXYZ[refAnt][0][0].delta);
		}
		
		for(p = 0; p < scan->nPoly; ++p)
		{
			writeDifxLineInt2(out, "SCAN %d POLY %d MJD", s, p, scan->im[refAnt][0][p].mjd);
			writeDifxLineInt2(out, "SCAN %d POLY %d SEC", s, p, scan->im[refAnt][0][p].sec);
			for(i = 0; i < scan->nPhaseCentres+1; ++i)
			{
				for(a = 0; a < scan->nAntenna; ++a)
				{
					if(scan->im[a] == 0)
					{
						continue;
					}
					order = scan->im[a][i][p].order;
					writeDifxLineArray2(out, "SRC %d ANT %d DELAY (us)", i, a, scan->im[a][i][p].delay, order+1);
					writeDifxLineArray2(out, "SRC %d ANT %d DRY (us)", i, a, scan->im[a][i][p].dry, order+1);
					writeDifxLineArray2(out, "SRC %d ANT %d WET (us)", i, a, scan->im[a][i][p].wet, order+1);
					writeDifxLineArray2(out, "SRC %d ANT %d AZ", i, a, scan->im[a][i][p].az, order+1);
					if(scan->im[a][i][p].elcorr[0] != 0.0 || scan->im[a][i][p].elcorr[1] != 0.0)
					{
						writeDifxLineArray2(out, "SRC %d ANT %d EL CORR", i, a, scan->im[a][i][p].elcorr, order+1);
					}
					writeDifxLineArray2(out, "SRC %d ANT %d EL GEOM", i, a, scan->im[a][i][p].elgeom, order+1);
					if(scan->im[a][i][p].parangle[0] != 0.0 || scan->im[a][i][p].parangle[1] != 0.0)
					{
						writeDifxLineArray2(out, "SRC %d ANT %d PAR ANGLE", i, a, scan->im[a][i][p].parangle, order+1);
					}
					writeDifxLineArray2(out, "SRC %d ANT %d U (m)", i, a, scan->im[a][i][p].u, order+1);
					writeDifxLineArray2(out, "SRC %d ANT %d V (m)", i, a, scan->im[a][i][p].v, order+1);
					writeDifxLineArray2(out, "SRC %d ANT %d W (m)", i, a, scan->im[a][i][p].w, order+1);
					if(scan->imLM)
					{
						writeDifxLineArray2(out, "SRC %d ANT %d dDELAYdL", i, a, scan->imLM[a][i][p].dDelay_dl, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d dDELAYdM", i, a, scan->imLM[a][i][p].dDelay_dm, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdLdL", i, a, scan->imLM[a][i][p].d2Delay_dldl, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdLdM", i, a, scan->imLM[a][i][p].d2Delay_dldm, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdMdM", i, a, scan->imLM[a][i][p].d2Delay_dmdm, order+1);
					}
					else if(scan->imXYZ)
					{
						writeDifxLineArray2(out, "SRC %d ANT %d dDELAYdX", i, a, scan->imXYZ[a][i][p].dDelay_dX, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d dDELAYdY", i, a, scan->imXYZ[a][i][p].dDelay_dY, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d dDELAYdZ", i, a, scan->imXYZ[a][i][p].dDelay_dZ, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdXdX", i, a, scan->imXYZ[a][i][p].d2Delay_dXdX, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdXdY", i, a, scan->imXYZ[a][i][p].d2Delay_dXdY, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdXdZ", i, a, scan->imXYZ[a][i][p].d2Delay_dXdZ, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdYdY", i, a, scan->imXYZ[a][i][p].d2Delay_dYdY, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdYdZ", i, a, scan->imXYZ[a][i][p].d2Delay_dYdZ, order+1);
						writeDifxLineArray2(out, "SRC %d ANT %d d2DELAYdZdZ", i, a, scan->imXYZ[a][i][p].d2Delay_dZdZ, order+1);
					}

				}
			}
		}
	}

	fclose(out);

	return 0;
}
