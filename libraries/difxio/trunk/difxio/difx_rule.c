/***************************************************************************
 *   Copyright (C) 2009 by Adam Deller                                     *
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
        for(r = 0; r < nRule; r++)
        {
		dr[r].configName[0] = 0;
                dr[r].sourcename[0] = 0;
                dr[r].scanId[0] = 0;
		dr[r].calCode[0] = 0;
		dr[r].qual = -1;
		dr[r].mjdStart = -1.0;
		dr[r].mjdStop = -1.0;
        }

        return dr;
}

void copyDifxRule(DifxRule * dest, DifxRule * src)
{
	strcpy(dest->configName, src->configName);
	strcpy(dest->sourcename, src->sourcename);
	strcpy(dest->scanId,     src->scanId);
	strcpy(dest->calCode,    src->calCode);
	dest->qual     = src->qual;
	dest->mjdStart = src->mjdStart;
	dest->mjdStop  = src->mjdStop;
}

void deleteDifxRuleArray(DifxRule *dr)
{
	free(dr);
}

void fprintDifxRule(FILE *fp, const DifxRule *dr)
{
	fprintf(fp, "  Difx Rule for config %s : %p\n", dr->configName, dr);
	fprintf(fp, "    source  = %s\n", dr->sourcename);
	fprintf(fp, "    scanId  = %s\n", dr->scanId);
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
	DifxRule * dr;

	writeDifxLineInt(out, "NUM RULES", D->nRule);
	n = 1;
	for(i=0;i<D->nRule;i++)
	{
		dr = D->rule + i;
		if(strcmp(dr->sourcename, "") != 0) {
			writeDifxLine1(out, "RULE %d SOURCE", i, dr->sourcename);
			n = n+1;
		}
		if(strcmp(dr->scanId, "") != 0) {
			writeDifxLine1(out, "RULE %d SCAN ID", i, dr->scanId);
			n = n+1;
		}
		if(strcmp(dr->calCode, "") != 0) {
			writeDifxLine1(out, "RULE %d CALCODE", i, dr->calCode);
			n = n+1;
		}
		if(dr->qual >= 0) {
			writeDifxLineInt1(out, "RULE %d QUAL", i, dr->qual);
			n = n+1;
		}
		if(dr->mjdStart > 0.0) {
			writeDifxLineDouble1(out, "RULE %d MJD START", 
					     i, "%15.8f", dr->mjdStart);
			n = n+1;
		}
		if(dr->mjdStop > 0.0) {
			writeDifxLineDouble1(out, "RULE %d MJD STOP",
					     i, "%15.8f", dr->mjdStop);
			n = n+1;
		}
		writeDifxLine1(out, "RULE %d CONFIG NAME", i, dr->configName);
		n = n+1;
	}
	return n;
}

int ruleAppliesToScanSource(const DifxRule * dr, const DifxScan * ds, const DifxSource * src)
{
	if((strcmp(dr->sourcename, "") != 0 && strcmp(src->name, dr->sourcename) != 0) ||
	   (strcmp(dr->scanId, "") != 0  && strcmp(ds->identifier, dr->scanId) != 0) ||
	   (strcmp(dr->calCode, "") != 0 && strcmp(src->calCode, dr->calCode) != 0) ||
	   (dr->qual >= 0 && src->qual != dr->qual) ||
	   (dr->mjdStart > 0.0 && ds->mjdStart < dr->mjdStart) || 
	   (dr->mjdStop > 0.0 && ds->mjdEnd > ds->mjdEnd))
	{
		return 0;
	}
	return 1;
}

int simplifyDifxRules(DifxInput *D)
{
	int r, c, used, numdeleted;
	DifxRule * dr;
	DifxConfig * dc;

	numdeleted = 0;
	for(r=0;r<D->nRule;r++)
	{
		dr = D->rule + r;
		used = 0;
		for(c=0;c<D->nConfig;c++)
		{
			dc = D->config+c;
			if(strcmp(dc->name, dr->configName) == 0)
			{
				used = 1;
			}
		}
		if(!used)
		{
			numdeleted++;
		}
		if(numdeleted > 0 && r+1 > numdeleted)
		{
			copyDifxRule(D->rule + r - numdeleted, dr);
		}
	}
	D->nRule -= numdeleted;
	if(numdeleted > 0)
	{
		D->rule = realloc(D->rule, D->nRule*sizeof(DifxRule));
		if(D->rule == 0)
		{
			fprintf(stderr, "Error reallocating DifxRule array!\n");
			exit(1);
		}
	}
	return numdeleted;
}

