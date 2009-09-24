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


static int writeCommonSettings(FILE *out, const DifxInput *D, 
	const char *filebase)
{
	char value[256];
	int secs;
	double dsecs;

	fprintf(out, "# COMMON SETTINGS ##!\n");
	sprintf(value, "%s.calc", filebase);
	writeDifxLine(out, "CALC FILENAME", value);
	sprintf(value, "%s.threads", filebase);
	writeDifxLine(out, "CORE CONF FILENAME", value);
	secs = (D->mjdStop - D->mjdStart)*86400.0 + 0.5;
	writeDifxLineInt(out, "EXECUTE TIME (SEC)", secs);
	writeDifxLineInt(out, "START MJD", (int)(D->mjdStart));
	if(D->fracSecondStartTime > 0)
	{
		dsecs = (D->mjdStart - (int)(D->mjdStart))*86400.0;
		writeDifxLineDouble(out, "START SECONDS", "%8.6f", dsecs);
	}
	else
	{
		secs = (D->mjdStart - (int)(D->mjdStart))*86400.0 + 0.5;
		writeDifxLineInt(out, "START SECONDS", secs);
	}
	writeDifxLineInt(out, "ACTIVE DATASTREAMS", D->nDatastream);
	writeDifxLineInt(out, "ACTIVE BASELINES", D->nBaseline);
	writeDifxLineInt(out, "VIS BUFFER LENGTH", D->visBufferLength);
	writeDifxLine(out, "OUTPUT FORMAT", "SWIN");
	sprintf(value, "%s.difx", filebase);
	writeDifxLine(out, "OUTPUT FILENAME", value);
	fprintf(out, "\n");

	return 0;
}

static int writeConfigurations(FILE *out, const DifxInput *D)
{
	fprintf(out, "# CONFIGURATIONS ###!\n");
	writeDifxConfigArray(out, D->nConfig, D->config, D->pulsar);
	fprintf(out, "\n");

	return 0;
}

static int writeRuleTable(FILE *out, const DifxInput *D)
{
	fprintf(out, "# RULES ############!\n");
	writeDifxRuleArray(out, D);
	fprintf(out, "\n");

	return 0;
}

static int writeFreqTable(FILE *out, const DifxInput *D)
{
	fprintf(out, "# FREQ TABLE #######!\n");
	writeDifxFreqArray(out, D->nFreq, D->freq);
	fprintf(out, "\n");

	return 0;
}

static int writeTelescopeTable(FILE *out, const DifxInput *D)
{
	fprintf(out, "# TELESCOPE TABLE ##!\n");
	writeDifxAntennaArray(out, D->nAntenna, D->antenna, 0, 0, 0, 1, 0);
	fprintf(out, "\n");

	return 0;
}

static int writeDatastreamTable(FILE *out, const DifxInput *D)
{
	int i;

	fprintf(out, "# DATASTREAM TABLE #!\n");
	writeDifxLineInt(out, "DATASTREAM ENTRIES", D->nDatastream);
	writeDifxLineInt(out, "DATA BUFFER FACTOR", D->dataBufferFactor);
	writeDifxLineInt(out, "NUM DATA SEGMENTS", D->nDataSegments);
	for(i = 0; i < D->nDatastream; i++)
	{
		writeDifxDatastream(out, D->datastream+i);
	}
	fprintf(out, "\n");

	return 0;
}

static int writeBaselineTable(FILE *out, const DifxInput *D)
{

	fprintf(out, "# BASELINE TABLE ###!\n");
	writeDifxBaselineArray(out, D->nBaseline, D->baseline);
	fprintf(out, "\n");

	return 0;
}

static int writeNetworkTable(FILE *out, const DifxInput *D)
{
        const DifxAntenna *da;
        int a;

        /* first determine if we need such a table */
        for(a = 0; a < D->nAntenna; a++)
        {
                da = D->antenna + a;
                if(da->windowSize != 0)
                {
                        break;
                }
        }
        if(a == D->nAntenna)
        {
                /* no network table needed */
                return 0;
        }

        fprintf(out, "# NETWORK TABLE ####!\n");

        for(a = 0; a < D->nAntenna; a++)
        {
                da = D->antenna + a;
                writeDifxLineInt1(out, "PORT NUM %d", a, da->networkPort);
                writeDifxLineInt1(out, "TCP WINDOW (KB) %d", a, da->windowSize);
        }

        fprintf(out, "\n");

        return 0;
}

static int writeDataTable(FILE *out, const DifxInput *D)
{
	int i, j;
 	const DifxAntenna *da;
	int type;	/* 0=None 1=VSN 2=Files */

	fprintf(out, "# DATA TABLE #######!\n");

	for(i = 0; i < D->nAntenna; i++)
	{
		if(D->datastream[i].antennaId >= 0)
		{
			da = D->antenna + D->datastream[i].antennaId;
			
			type = 0;
			if(strcmp(da->vsn, "File") == 0)
			{
				type = 2;
			}
			else if(strlen(da->vsn) == 8)
			{
				type = 1;
			}
			else if(strcasecmp(da->vsn, "None") == 0)
			{
				type = 0;
			}
			else if(strlen(da->vsn) < 8 && da->nFile > 0)
			{
				type = 2;
			}


			if(type == 1)
			{
				writeDifxLineInt1(out, "D/STREAM %d FILES", i, 1);
				writeDifxLine1(out, "FILE %d/0", i, da->vsn);
			}
			else if(type == 2)
			{
				if(da->file == 0)
				{
					fprintf(stderr, "Error: difxio:writeDataTable da->file = 0\n");
					return -1;
				}
				writeDifxLineInt1(out, "D/STREAM %d FILES", i, da->nFile);
				for(j = 0; j < da->nFile; j++)
				{
					if(da->file[j])
					{
						writeDifxLine2(out, "FILE %d/%d", i, j, da->file[j]);
					}
					else
					{
						writeDifxLine2(out, "FILE %d/%d", i, j, "Null");
					}
				}
			}
			else
			{
				writeDifxLineInt1(out, "D/STREAM %d FILES", i, 0);
			}
		}
		else
		{
			writeDifxLineInt1(out, "D/STREAM %d FILES", i, 0);
		}
	}

	fprintf(out, "\n");

	return 0;
}

int writeDifxInput(const DifxInput *D, const char *filename)
{
	FILE *out;
	char filebase[256];
	int i, l;

	strcpy(filebase, filename);
	l = strlen(filebase);
	for(i = l-1; i > 0; i--)
	{
		if(filebase[i] == '.')
		{
			filebase[i] = 0;
			break;
		}
	}

	if(!D)
	{
		return -1;
	}

	if(!D->job)
	{
		fprintf(stderr, "Error: writeDifxInput: D->job == 0!\n");
		return -1;
	}

	out = fopen(filename, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", filename);
		return -1;
	}

	//printf("About to start writing input file\n");
	writeCommonSettings(out, D, filebase);
	writeConfigurations(out, D);
	writeRuleTable(out, D);
	writeFreqTable(out, D);
	writeTelescopeTable(out, D);
	writeDatastreamTable(out, D);
	writeBaselineTable(out, D);
	writeDataTable(out, D);
	writeNetworkTable(out, D);

	fclose(out);

	return 0;
}

