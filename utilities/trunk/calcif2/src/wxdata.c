/***************************************************************************
 *   Copyright (C) 2019 by Walter Brisken                                  *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "wxdata.h"

int loadWeatherForAntenna(WXData *wxData, const char *filename)
{
	const int MaxLineLength = 1024;
	char line[MaxLineLength];
	int nLine = 0;
	FILE *in;

	wxData->nRow = 0;

	in = fopen(filename, "r");
	if(!in)
	{
		return 0;
	}

	for(;;)
	{
		char *rv;
		int n;
		double mjd, temp, pressure, dewpoint, windspeed, winddir, rain, windgust;

		rv = fgets(line, MaxLineLength-1, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}

		n = sscanf(line, "%*s%lf%lf%lf%lf%lf%lf%lf%lf", &mjd, &temp, &pressure, &dewpoint, &windspeed, &winddir, &rain, &windgust);
		if(n == 8)
		{
			++nLine;
		}
	}

	if(nLine > 0)
	{
		rewind(in);

		wxData->row = (WXDataRow *)malloc(nLine*sizeof(WXDataRow));

		for(;;)
		{
			char *rv;
			int n;

			rv = fgets(line, MaxLineLength-1, in);
			if(!rv)
			{
				break;
			}
			if(line[0] == '#')
			{
				continue;
			}

			n = sscanf(line, "%*s%lf%lf%lf%lf%lf%lf%lf%lf", 
				&(wxData->row[wxData->nRow].mjd), 
				&(wxData->row[wxData->nRow].temp),
				&(wxData->row[wxData->nRow].pressure),
				&(wxData->row[wxData->nRow].dewpoint),
				&(wxData->row[wxData->nRow].windspeed),
				&(wxData->row[wxData->nRow].winddir),
				&(wxData->row[wxData->nRow].rain), 
				&(wxData->row[wxData->nRow].windgust));
			if(n == 8)
			{
				++wxData->nRow;
			}
		}
	}

	fclose(in);

	return wxData->nRow;
}

/* resultant WXData array is indexed by the antenna table from D */
WXData *loadWeatherForProject(DifxInput *D)
{
	int nAnt, a;
	WXData *wx;
	int n = 0;
	
	nAnt = D->nAntenna;

	wx = (WXData *)malloc(nAnt*sizeof(WXData));

	for(a = 0; a < nAnt; ++a)
	{
		char filename[PATH_MAX+1];
		int i;

		snprintf(filename, PATH_MAX, "%s%s.%s.weather", D->job->obsCode, D->job->obsSession, D->antenna[a].name);
		for(i = 0; filename[i]; ++i)
		{
			if(filename[i] >= 'A' && filename[i] <= 'Z')
			{
				filename[i] += ('a' - 'A');
			}
		}

		n += loadWeatherForAntenna(wx + a, filename);
	}

	if(n == 0)
	{
		deleteWXDataArray(wx, nAnt);
		wx = 0;
	}

	return wx;
}

void deleteWXDataArray(WXData *wxData, int n)
{
	if(wxData)
	{
		int a;

		for(a = 0; a < n; ++a)
		{
			free(wxData[a].row);
		}

		free(wxData);
	}
}

/* interpolates weather records and places result in output */
/* Return value 0 on success, negative on no action */
int interpolateWXData(WXDataRow *output, const WXData *wxData, double mjd)
{
	const double MaxWeatherInterpolate = 0.1;	/* [days] don't attempt to interpolate or extrapolate more than this amount */
	int n;

	if(!wxData)
	{
		return -1;
	}

	n = wxData->nRow;

	if(mjd < wxData->row[0].mjd)
	{
		if(wxData->row[0].mjd - mjd > MaxWeatherInterpolate)
		{
			return -2;
		}
		else
		{
			memcpy(output, wxData->row + 0, sizeof(WXDataRow));
		}
	}
	else if(mjd > wxData->row[n-1].mjd)
	{
		if(mjd - wxData->row[n-1].mjd > MaxWeatherInterpolate)
		{
			return -3;
		}
		else
		{
			memcpy(output, wxData->row + (n-1), sizeof(WXDataRow));
		}
	}
	else	/* find bracketing rows */
	{
		int r0, r1;	/* indices of row before and row after */
		double w0, w1;	/* weight for each of the rows */

		for(r0 = 0; r0 < n; ++r0)
		{
			if(wxData->row[r0].mjd <= mjd && mjd <= wxData->row[r0+1].mjd)
			{
				break;
			}
		}

		if(r0 >= n-1)
		{
			/* This should never happen! */

			return -4;
		}

		r1 = r0+1;

		if(wxData->row[r1].mjd - wxData->row[r0].mjd > 2*MaxWeatherInterpolate)
		{
			if(mjd - wxData->row[r0].mjd < MaxWeatherInterpolate)
			{
				w0 = 1.0;
				w1 = 0.0;
			}
			else if(wxData->row[r1].mjd - mjd < MaxWeatherInterpolate)
			{
				w0 = 0.0;
				w1 = 1.0;
			}
			else
			{
				/* Too far from any measurement */

				return -5;
			}
		}
		else
		{
			w0 = (wxData->row[r1].mjd - mjd)/(wxData->row[r1].mjd - wxData->row[r0].mjd);
			w1 = 1.0 - w0;
		}

		output->mjd       = w0*wxData->row[r0].mjd       + w1*wxData->row[r1].mjd;
		output->temp      = w0*wxData->row[r0].temp      + w1*wxData->row[r1].temp;
		output->pressure  = w0*wxData->row[r0].pressure  + w1*wxData->row[r1].pressure;
		output->dewpoint  = w0*wxData->row[r0].dewpoint  + w1*wxData->row[r1].dewpoint;
		output->windspeed = w0*wxData->row[r0].windspeed + w1*wxData->row[r1].windspeed;
		output->winddir   = w0*wxData->row[r0].winddir   + w1*wxData->row[r1].winddir;
		output->rain      = w0*wxData->row[r0].rain      + w1*wxData->row[r1].rain;
		output->windgust  = w0*wxData->row[r0].windgust  + w1*wxData->row[r1].windgust;
	}

	return 0;
}
