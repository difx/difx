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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"


DifxPolyModel ***newDifxPolyModelArray(int nAntenna, int nSrcs, int nPoly)
{
	DifxPolyModel ***dpm;
	int a;

	dpm = (DifxPolyModel ***)calloc(nAntenna, sizeof(DifxPolyModel **));
	for(a = 0; a < nAntenna; ++a)
	{
		int s;

		dpm[a] = (DifxPolyModel **)calloc(nSrcs, sizeof(DifxPolyModel *));
		for(s = 0; s < nSrcs; ++s)
		{
			dpm[a][s] = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));
		}
	}

	return dpm;
}

DifxPolyModel *dupDifxPolyModelColumn(const DifxPolyModel *src, int nPoly)
{
	DifxPolyModel *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModel *)calloc(nPoly, sizeof(DifxPolyModel));

	memcpy(dest, src, nPoly*sizeof(DifxPolyModel));

	return dest;
}

void deleteDifxPolyModelArray(DifxPolyModel ***dpm, int nAntenna, int nSrcs)
{
	if(dpm)
	{
		int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(dpm[a])
			{
				int s;

				for(s = 0; s < nSrcs; ++s)
				{
					free(dpm[a][s]);
				}
				free(dpm[a]);
			}
		}
		free(dpm);
	}
}

void fprintDifxPolyModel(FILE *fp, const DifxPolyModel *dpm)
{
	fprintf(fp, "    DifxPolyModel : %p\n", dpm);
	if(dpm)
	{
		fprintf(fp, "        mjd, sec = %d, %d\n", dpm->mjd, dpm->sec);
		fprintf(fp, "        delay = %22.15e %22.15e %22.15e\n", dpm->delay[0], dpm->delay[1], dpm->delay[2]);
	}
}

void printDifxPolyModel(const DifxPolyModel *dpm)
{
	fprintDifxPolyModel(stdout, dpm);
}



DifxPolyModelLMExtension ***newDifxPolyModelLMExtensionArray(int nAntenna, int nSrcs, int nPoly)
{
	DifxPolyModelLMExtension ***lme;
	int a;

	lme = (DifxPolyModelLMExtension ***)calloc(nAntenna, sizeof(DifxPolyModelLMExtension **));
	for(a = 0; a < nAntenna; ++a)
	{
		int s;

		lme[a] = (DifxPolyModelLMExtension **)calloc(nSrcs, sizeof(DifxPolyModelLMExtension *));
		for(s = 0; s < nSrcs; ++s)
		{
			lme[a][s] = (DifxPolyModelLMExtension *)calloc(nPoly, sizeof(DifxPolyModelLMExtension));
		}
	}

	return lme;
}

DifxPolyModelLMExtension *dupDifxPolyModelLMExtensionColumn(const DifxPolyModelLMExtension *src, int nPoly)
{
	DifxPolyModelLMExtension *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModelLMExtension *)calloc(nPoly, sizeof(DifxPolyModelLMExtension));

	memcpy(dest, src, nPoly*sizeof(DifxPolyModelLMExtension));

	return dest;
}

void deleteDifxPolyModelLMExtensionArray(DifxPolyModelLMExtension ***lme, int nAntenna, int nSrcs)
{
	if(lme)
	{
		int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(lme[a])
			{
				int s;

				for(s = 0; s < nSrcs; ++s)
				{
					free(lme[a][s]);
				}
				free(lme[a]);
			}
		}
		free(lme);
	}
}



DifxPolyModelXYZExtension ***newDifxPolyModelXYZExtensionArray(int nAntenna, int nSrcs, int nPoly)
{
	DifxPolyModelXYZExtension ***xyze;
	int a;

	xyze = (DifxPolyModelXYZExtension ***)calloc(nAntenna, sizeof(DifxPolyModelXYZExtension **));
	for(a = 0; a < nAntenna; ++a)
	{
		int s;

		xyze[a] = (DifxPolyModelXYZExtension **)calloc(nSrcs, sizeof(DifxPolyModelXYZExtension *));
		for(s = 0; s < nSrcs; ++s)
		{
			xyze[a][s] = (DifxPolyModelXYZExtension *)calloc(nPoly, sizeof(DifxPolyModelXYZExtension));
		}
	}

	return xyze;
}

DifxPolyModelXYZExtension *dupDifxPolyModelXYZExtensionColumn(const DifxPolyModelXYZExtension *src, int nPoly)
{
	DifxPolyModelXYZExtension *dest;

	if(src == 0)
	{
		return 0;
	}

	dest = (DifxPolyModelXYZExtension *)calloc(nPoly, sizeof(DifxPolyModelXYZExtension));

	memcpy(dest, src, nPoly*sizeof(DifxPolyModelXYZExtension));

	return dest;
}

void deleteDifxPolyModelXYZExtensionArray(DifxPolyModelXYZExtension ***xyze, int nAntenna, int nSrcs)
{
	if(xyze)
	{
		int a;

		for(a = 0; a < nAntenna; ++a)
		{
			if(xyze[a])
			{
				int s;

				for(s = 0; s < nSrcs; ++s)
				{
					free(xyze[a][s]);
				}
				free(xyze[a]);
			}
		}
		free(xyze);
	}
}
