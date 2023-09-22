/***************************************************************************
 *   Copyright (C) 2010-2017 by Walter Brisken                             *
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

void DifxInputAllocThreads(DifxInput *D, int nCore)
{
	if(!D)
	{
		fprintf(stderr, "Error: DifxInputAllocThreads: D=0 nCore=%d\n", nCore);
		
		return;
	}
	if(D->nThread)
	{
		free(D->nThread);
		D->nThread = 0;
		D->nCore = 0;
	}
	if(nCore > 0)
	{
		D->nThread = (int *)calloc(nCore, sizeof(int));
		if(D->nThread == 0)
		{
			fprintf(stderr, "Alloc error: DifxInputAllocThreads: nCore=%d\n", nCore);

			return;
		}
		D->nCore = nCore;
	}
}

void DifxInputSetThreads(DifxInput *D, int nThread)
{
	if(!D)
	{
		return;
	}
	if(D->nCore > 0 && D->nThread)
	{
		int i;

		for(i = 0; i < D->nCore; ++i)
		{
			D->nThread[i] = nThread;
		}
	}
}

int DifxInputLoadThreads(DifxInput *D)
{
	const int MaxLineLen = 100;
	FILE *in;
	char line[MaxLineLen+1];
	char *rv;
	int nCore, i, n;

	if (!D)
	{
		return 0;
	}

	if(D->job->threadsFile[0] == 0)
	{
		return 0;
	}

	DifxInputAllocThreads(D, 0);

	in = fopen(D->job->threadsFile, "r");
	if(!in)
	{
		/* This could be normal, so don't raise a fuss */
		
		return 0;
	}

	rv = fgets(line, MaxLineLen, in);
	if(!rv)
	{
		fprintf(stderr, "Line %d of %s : unexpected EOF\n", 1, D->job->threadsFile);
		fclose(in);

		return -1;
	}

	if(strncmp(line, "NUMBER OF CORES:    ", 20) != 0 || strlen(line) < 21)
	{
		fprintf(stderr, "Line %d of %s : format error\n", 1, D->job->threadsFile);
		fclose(in);
		
		return -2;
	}

	n = sscanf(line+20, "%d", &nCore);
	if(n != 1)
	{
		fprintf(stderr, "Line %d of %s : format error\n", 1, D->job->threadsFile);
		fclose(in);
		
		return -2;
	}

	DifxInputAllocThreads(D, nCore);
	
	for(i = 0; i < nCore; ++i)
	{
		rv = fgets(line, MaxLineLen, in);
		if(!rv)
		{
			fprintf(stderr, "Line %d of %s : unexpected EOF\n", i+2, D->job->threadsFile);
			fclose(in);
			DifxInputAllocThreads(D, 0);

			return -1;
		}
		n = sscanf(line, "%d", &D->nThread[i]);
		if(n != 1)
		{
			fprintf(stderr, "Line %d of %s : format error\n", i+2, D->job->threadsFile);
			fclose(in);
			DifxInputAllocThreads(D, 0);

			return -2;
		}
	}

	fclose(in);

	return 0;
}

int DifxInputWriteThreads(const DifxInput *D)
{
	FILE *out;
	int i;

	if(!D)
	{
		return 0;
	}

	if(D->nJob != 1)
	{
		fprintf(stderr, "writeDifxThreads: nJob = %d (not 1)\n", D->nJob);

		return -1;
	}

	if(D->job->threadsFile[0] == 0)
	{
		return 0;
	}

	if(D->nCore == 0 || D->nThread == 0)
	{
		return 0;
	}

	out = fopen(D->job->threadsFile, "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", D->job->threadsFile);

		return -1;
	}

	fprintf(out, "NUMBER OF CORES:    %d\n", D->nCore);
	for(i = 0; i < D->nCore; ++i)
	{
		fprintf(out, "%d\n", D->nThread[i]);
	}

	fclose(out);

	return 0;
}
