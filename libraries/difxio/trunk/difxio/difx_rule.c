/***************************************************************************
 *   Copyright (C) 2009-2012 by Adam Deller & Walter Brisken               *
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
// $Id: $
// $HeadURL: $
// $LastChangedRevision: $
// $Author: $
// $LastChangedDate: $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/difx_input.h"
#include "difxio/difx_write.h"

DifxRule *newDifxRuleArray(int nRule)
{
        DifxRule *dr;
	int r;

        dr = (DifxRule *)calloc(nRule, sizeof(DifxRule));
        for(r = 0; r < nRule; ++r)
        {
		dr[r].qual = -1;
		dr[r].mjdStart = -1.0;
		dr[r].mjdStop = -1.0;
        }

        return dr;
}

void copyDifxRule(DifxRule *dest, DifxRule *src)
{
	snprintf(dest->configName, DIFXIO_NAME_LENGTH,    "%s", src->configName);
	DifxStringArrayclear(&dest->sourceName);
	DifxStringArrayappend(&dest->sourceName, &src->sourceName);
	DifxStringArrayclear(&dest->scanId);
	DifxStringArrayappend(&dest->scanId, &src->scanId);
	snprintf(dest->calCode,    DIFXIO_CALCODE_LENGTH, "%s", src->calCode);
	dest->qual     = src->qual;
	dest->mjdStart = src->mjdStart;
	dest->mjdStop  = src->mjdStop;
}

void deleteDifxRuleArray(DifxRule *dr)
{
	if(dr)
	{
		free(dr);
	}
}

void fprintDifxRule(FILE *fp, const DifxRule *dr)
{
	fprintf(fp, "  Difx Rule for config %s : %p\n", dr->configName, dr);
	fprintf(fp, "    source  = ");
	if(dr->sourceName.n > 0)
	{
		int i;

		for(i = 0; i < dr->sourceName.n; ++i)
		{
			if(i > 0)
			{
				fprintf(fp, ", ");
			}
			fprintf(fp, "%s", dr->sourceName.str[i]);
		}
	}
	fprintf(fp, "\n");
	fprintf(fp, "    scanId  = ");
	if(dr->scanId.n > 0)
	{
		int i;

		for(i = 0; i < dr->scanId.n; ++i)
		{
			if(i > 0)
			{
				fprintf(fp, ", ");
			}
			fprintf(fp, "%s", dr->scanId.str[i]);
		}
	}
	fprintf(fp, "\n");
	fprintf(fp, "    calCode = %s\n", dr->calCode);
	fprintf(fp, "    qual    = %d\n", dr->qual);
	fprintf(fp, "    mjdStart= %f\n", dr->mjdStart);
	fprintf(fp, "    mjdStop = %f\n", dr->mjdStop);
}

void printDifxRule(const DifxRule *dr)
{
        fprintDifxRule(stdout, dr);
}

int writeDifxRuleArray(FILE *out, const DifxInput *D)
{
	int n; //number of lines written
	int i;

	writeDifxLineInt(out, "NUM RULES", D->nRule);
	n = 1;
	for(i = 0; i < D->nRule; ++i)
	{
		DifxRule * dr;
		
		dr = D->rule + i;
		if(dr->sourceName.n > 0)
		{
			writeDifxLineStringArray1(out, "RULE %d SOURCE", i, &dr->sourceName);
			n = n+1;
		}
		if(dr->scanId.n > 0)
		{
			writeDifxLineStringArray1(out, "RULE %d SCAN ID", i, &dr->scanId);
			n = n+1;
		}
		if(strcmp(dr->calCode, "") != 0)
		{
			writeDifxLine1(out, "RULE %d CALCODE", i, dr->calCode);
			n = n+1;
		}
		if(dr->qual >= 0)
		{
			writeDifxLineInt1(out, "RULE %d QUAL", i, dr->qual);
			n = n+1;
		}
		if(dr->mjdStart > 0.0)
		{
			writeDifxLineDouble1(out, "RULE %d MJD START", 
					     i, "%15.8f", dr->mjdStart);
			n = n+1;
		}
		if(dr->mjdStop > 0.0)
		{
			writeDifxLineDouble1(out, "RULE %d MJD STOP",
					     i, "%15.8f", dr->mjdStop);
			n = n+1;
		}
		writeDifxLine1(out, "RULE %d CONFIG NAME", i, dr->configName);
		n = n+1;
	}
	
	return n;
}

int ruleAppliesToScanSource(const DifxRule *dr, const DifxScan *ds, const DifxSource *src)
{
#warning "FIXME: need to eventually handle lists of calCodes and quals"
	if((dr->sourceName.n > 0 && DifxStringArraycontains(&dr->sourceName, src->name) == 0) ||
	   (dr->scanId.n > 0  && DifxStringArraycontains(&dr->scanId, ds->identifier) == 0) ||
	   (strcmp(dr->calCode, "") != 0 && strcmp(src->calCode, dr->calCode) != 0) ||
	   (dr->qual >= 0 && src->qual != dr->qual) ||
	   (dr->mjdStart > 0.0 && ds->mjdStart < dr->mjdStart) || 
	   (dr->mjdStop > 0.0 && ds->mjdEnd > dr->mjdStop))
	{
		return 0;
	}

	return 1;
}

int simplifyDifxRules(DifxInput *D)
{
	int r;
	int numdeleted = 0;

	for(r = 0; r < D->nRule; ++r)
	{
		DifxRule *dr;
		int c;
		int used = 0;

		dr = D->rule + r;
		for(c = 0; c < D->nConfig; ++c)
		{
			DifxConfig *dc;
			
			dc = D->config+c;
			if(strcmp(dc->name, dr->configName) == 0)
			{
				used = 1;
			}
		}
		if(!used)
		{
			++numdeleted;
		}
		if(numdeleted > 0 && r+1 > numdeleted && used)
		{
			copyDifxRule(D->rule + r - numdeleted, dr);
		}
	}
	D->nRule -= numdeleted;

	if(numdeleted > 0)
	{
		if(D->nRule == 0)
		{
			D->nRule = 1;
			--numdeleted;
			
			deleteDifxRuleArray(D->rule);
			D->rule = newDifxRuleArray(D->nRule);

			strcpy(D->rule[0].configName, D->config[0].name);

			if(D->nConfig != 1)
			{
				fprintf(stderr, "Developer error: simplifyDifxRules: nRule = 0 and nConfig = %d\n", D->nConfig);

				exit(EXIT_FAILURE);
			}
		}
		else
		{
			D->rule = realloc(D->rule, D->nRule*sizeof(DifxRule));
		}
	}
	
	return numdeleted;
}

