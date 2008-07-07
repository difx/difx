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

#include <stdlib.h>
#include <string.h>
#include "difxio/difx_write.h"

int writeDifxRate(const DifxInput *D, const char *filename)
{
	FILE *out;
	DifxScan *scan;
	DifxSource *source;
	int a, s, i;
	int l;
	char value[1024], temp[64];

	if(!D)
	{
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

	/* FIXME : calc stuff */

	/* FIXME : date/time */

	sprintf(value, "%8.6f", D->job->modelInc);
	writeDifxLine(out, "INCREMENT (SECS)", value);

	writeDifxLineInt(out, "NUM TELESCOPES", D->nAntenna);

	for(a = 0; a < D->nAntenna; a++)
	{
		writeDifxLine1(out, "TELESCOPE %d NAME", a, D->antenna[a].name);
		writeDifxLine1(out, "TELESCOPE %d MOUNT", a, 
			D->antenna[a].mount);
		sprintf(value, "%6.4f", D->antenna[a].offset[1]);
		writeDifxLine1(out, "TELESCOPE %d OFFSET (m)", a, value);
		sprintf(value, "%6.4f", D->antenna[a].X);
		writedifxline1(out, "TELESCOPE %d X (m)", a, value);
		sprintf(value, "%6.4f", D->antenna[a].Y);
		writeDifxLine1(out, "TELESCOPE %d Y (m)", a, value);
		sprintf(value, "%6.4f", D->antenna[a].Z);
		writeDifxLine1(out, "TELESCOPE %d Z (m)", a, value);
	}

	writeDifxLineInt(out, "NUM SCANS", D->nScan);

	for(s = 0; s < D->nScan; s++)
	{
		scan = D->scan + s;
		source = D->source + scan->sourceId;
		writeDifxLineInt1(out, "SCAN %d POINTS", s, scan->nPoint);
		writeDifxLineInt1(out, "SCAN %d START PT", s,
			scan->startPoint);
		writeDifxLine1(out, "SCAN %d SRC NAME", s, scan->name);
		sprintf(value, "%12.10f", source->ra);
		writeDifxLine1(out, "SCAN %d RA", s, value);
		sprintf(value, "%12.10f", source->dec);
		writeDifxLine1(out, "SCAN %d DEC", s, value);
		for(i = -1; i <= scan->nPoint+1; i++)
		{
			value[0] = 0;
			l = 0;
			for(a = 0; a < D->nAntenna; a++)
			{
				l += sprintf(value+l, 
					"%12.10f\t%14.12f\t%14.12f\t", 
					scan->model[a][i].dt,
					scan->model[a][i].dry,
					scan->model[a][i].wet);
			}
			writeDifxLine1(out, "RELATIVE INC %d", i, value);
		}
	}

	fclose(out);

	return 0;
}
