/***************************************************************************
 *   Copyright (C) 2008-2014 by Walter Brisken & Adam Deller               *
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


static int writeCommonSettings(FILE *out, const DifxInput *D)
{
	int secs;
	double dsecs;

	fprintf(out, "# COMMON SETTINGS ##!\n");
	writeDifxLine(out, "CALC FILENAME", D->job->calcFile);
	writeDifxLine(out, "CORE CONF FILENAME", D->job->threadsFile);
	dsecs = (D->mjdStop - D->mjdStart)*86400.0;
	if(dsecs<1)  // Very short job
	{
		writeDifxLineDouble(out, "EXECUTE TIME (SEC)", "%.3f", dsecs);
	} 
	else 
	{
		if(D->fracSecondStartTime > 0)
		{
			secs = (D->mjdStop - D->mjdStart)*86400.0 + 0.5;
		}
		else
		{
			secs = (roundSeconds(D->mjdStop) - roundSeconds(D->mjdStart))*86400.0 + 0.0001;
		}
		writeDifxLineInt(out, "EXECUTE TIME (SEC)", secs);
	}

	writeDifxLineInt(out, "START MJD", (int)(D->mjdStart));
	if(D->fracSecondStartTime > 0)
	{
		dsecs = (D->mjdStart - (int)(D->mjdStart))*86400.0;
		writeDifxLineDouble(out, "START SECONDS", "%8.6f", dsecs);
	}
	else
	{
		//round to nearest second - consistent with what is done in write_calc
		secs = (D->mjdStart - (int)(D->mjdStart))*86400.0 + 0.5;
		writeDifxLineInt(out, "START SECONDS", secs);
	}
	writeDifxLineInt(out, "ACTIVE DATASTREAMS", D->nDatastream);
	writeDifxLineInt(out, "ACTIVE BASELINES", D->nBaseline);
	writeDifxLineInt(out, "VIS BUFFER LENGTH", D->visBufferLength);
	if (D->outputFormat==OutputFormatDIFX)
	{
		writeDifxLine(out, "OUTPUT FORMAT", "SWIN");
	}
	else
	{
		writeDifxLine(out, "OUTPUT FORMAT", "ASCII");
	}
	writeDifxLine(out, "OUTPUT FILENAME", D->job->outputFile);
	fprintf(out, "\n");

	return 0;
}

static int writeConfigurations(FILE *out, const DifxInput *D)
{
	fprintf(out, "# CONFIGURATIONS ###!\n");
	writeDifxConfigArray(out, D->nConfig, D->config, D->pulsar, D->phasedarray);
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
        const DifxDatastream *ds;
        int i;

        /* first determine if we need such a table */
        for(i = 0; i < D->nDatastream; i++)
        {
                ds = D->datastream + i;
		if(ds->dataSource == DataSourceNetwork)
		{
			break;
		}
        }
        if(i == D->nDatastream)
        {
                /* no network table needed */
                return 0;
        }

        fprintf(out, "# NETWORK TABLE ####!\n");

        for(i = 0; i < D->nDatastream; i++)
        {
                ds = D->datastream + i;
		if(ds->networkPort[0])
		{
                	writeDifxLine1(out, "PORT NUM %d", i, ds->networkPort);
		}
		else
		{
			writeDifxLineInt1(out, "PORT NUM %d", i, 0);
		}
                writeDifxLineInt1(out, "TCP WINDOW (KB) %d", i, ds->windowSize);
        }

        fprintf(out, "\n");

        return 0;
}

static int writeDataTable(FILE *out, const DifxInput *D)
{
	int i, j;
	const DifxDatastream *ds;

	fprintf(out, "# DATA TABLE #######!\n");

	for(i = 0; i < D->nDatastream; i++)
	{
		ds = D->datastream + i;

		if(ds->nFile > 0)
		{
			if(ds->file == 0)
			{
				fprintf(stderr, "Error: difxio:writeDataTable ds->file = 0\n");
				return -1;
			}
			writeDifxLineInt1(out, "D/STREAM %d FILES", i, ds->nFile);
			for(j = 0; j < ds->nFile; j++)
			{
				if(ds->file[j])
				{
					writeDifxLine2(out, "FILE %d/%d", i, j, ds->file[j]);
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

	fprintf(out, "\n");

	return 0;
}

int writeDifxInput(const DifxInput *D)
{
	FILE *out;

	if(!D)
	{
		return -1;
	}

	if(!D->job)
	{
		fprintf(stderr, "Error: writeDifxInput: D->job == 0!\n");

		return -1;
	}

	if(D->nJob != 1)
	{
		fprintf(stderr, "writeDifxInput: nJob = %d (not 1)\n", 
			D->nJob);
		return -1;
	}

	if(D->job->inputFile[0] == 0)
	{
		fprintf(stderr, "Developer error: writeDifxInput: D->job->inputFile is null\n");

		return -1;
	}

	out = fopen(D->job->inputFile, "w");
	if(!out)
	{
		fprintf(stderr, "Cannot open %s for write\n", D->job->inputFile);

		return -1;
	}

	writeCommonSettings(out, D);
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

