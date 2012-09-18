/***************************************************************************
 *   Copyright (C) 2012 by Walter Brisken                                  *
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
// $Id: difx_write_input.c 2905 2010-12-18 20:15:28Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/trunk/difxio/difx_write_input.c $
// $LastChangedRevision: 2905 $
// $Author: WalterBrisken $
// $LastChangedDate: 2010-12-18 13:15:28 -0700 (Sat, 18 Dec 2010) $
//
//============================================================================

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "difxio/difx_write.h"

int writeDifxPolyco(const DifxPolyco *polyco, int appendFile)
{
	int v = 0;
	const int MaxTimeLen = 64;
	char datetime[MaxTimeLen];
	struct tm t;
	time_t unixtime;
	const int unix_mjd0 = 40587;  /* mjd at beginning of unix time */
	FILE *out;
	int c;

	if(polyco == 0)
	{
		return -1;
	}

	unixtime = (polyco->mjd - unix_mjd0)*86400;
	gmtime_r(&unixtime, &t);
	strftime(datetime, MaxTimeLen, "%d-%b-%y  %H%M%S.00", &t);

	out = fopen(polyco->fileName, appendFile ? "a" : "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for %s\n", polyco->fileName, appendFile ? "append" : "write");

		return -1;
	}

	fprintf(out, "PSR+NAME   %s   %14.8f     %f 0.0 0.0\n", datetime, polyco->mjd, polyco->dm);
	fprintf(out, "%14.12f %14.10f 0 %d %d %f", polyco->p0, polyco->f0, polyco->nBlk, polyco->nCoef, polyco->refFreq);
	for(c = 0; c < polyco->nCoef; ++c)
	{
		if(c % 3 == 0)
		{
			fprintf(out, "\n");
		}
		fprintf(out, "  %20e", polyco->coef[c]);
	}
	fprintf(out, "\n");

	fclose(out);

	return v;
}

int writeDifxPulsar(const DifxPulsar *pulsar)
{
	FILE *out;
	int pp, b;
	int v = 0;

	if(pulsar == 0)
	{
		return -1;
	}

	out = fopen(pulsar->fileName, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", pulsar->fileName);

		return -1;
	}

	writeDifxLineInt(out, "NUM POLYCO FILES", pulsar->nPolyco);

	for(pp = 0; pp < pulsar->nPolyco; ++pp)
	{
		const DifxPolyco *polyco = pulsar->polyco + pp;
		int appendFile = 0;	/* set to 1 if appending rather than starting fresh is desired */

		if(pp > 0)
		{
			int j;

			for(j = 0; j < pp; ++j)
			{
				if(strcmp(polyco->fileName, pulsar->polyco[j].fileName) == 0)
				{
					appendFile = 1;
					break;
				}
			}
		}

		writeDifxLine1(out, "POLYCO FILE %d", pp, polyco->fileName);
	
		v = writeDifxPolyco(polyco, appendFile);
		if(v < 0)
		{
			break;
		}
	}

	if(v >= 0)
	{
		writeDifxLineInt(out, "NUM PULSAR BINS", pulsar->nBin);
		writeDifxLine(out, "SCRUNCH OUTPUT", pulsar->scrunch ? "TRUE" : "FALSE");
		for(b = 0; b < pulsar->nBin; ++b)
		{
			writeDifxLineDouble1(out, "BIN PHASE END %d", b, "%f", pulsar->binEnd[b]);
			writeDifxLineDouble1(out, "BIN WEIGHT %d", b, "%f", pulsar->binWeight[b]);
		}
	}

	fclose(out);

	return v;
}

/* writes both bin config and polyco files */
/* note that this should not be automatically called from writeDifxInput as there are
 * cases where writing these files is not desired */
int writeDifxPulsarFiles(const DifxInput *D)
{
	int dp;

	if(!D)
	{
		return -1;
	}

	if(D->nPulsar < 0)
	{
		return 0;
	}

	for(dp = 0; dp < D->nPulsar; ++dp)
	{
		int v;

		v = writeDifxPulsar(D->pulsar + dp);
		if(v < 0)
		{
			return -1;
		}
	}

	return D->nPulsar;
}
