/***************************************************************************
 *   Copyright (C) 2008-2016 by Walter Brisken                             *
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
#include "difxio/difx_write.h"

const char eopMergeModeNames[][MAX_EOP_MERGE_MODE_STRING_LENGTH] =
{
	"strict",
	"relaxed",
	"loose",
	"illegal"
};

enum EOPMergeMode stringToEOPMergeMode(const char *str)
{
	enum EOPMergeMode e;

	for(e = 0; e < NumEOPMergeModes; ++e)
	{
		if(strcasecmp(str, eopMergeModeNames[e]) == 0)
		{
			break;
		}
	}

	return e;
}

DifxEOP *newDifxEOPArray(int nEOP)
{
	DifxEOP *de;

	if(nEOP == 0)
	{
		return 0;
	}

	de = (DifxEOP *)calloc(nEOP, sizeof(DifxEOP));

	return de;
}

void deleteDifxEOPArray(DifxEOP *de)
{
	if(de)
	{
		free(de);
	}
}

void fprintDifxEOP(FILE *fp, const DifxEOP *de)
{
	if(!de)
	{
		fprintf(fp, "  DifxEOP [] : %p\n", de);

		return;
	}
	fprintf(fp, "  DifxEOP [%d] : %p\n", (int)(de->mjd + 0.5), de);
	fprintf(fp, "    TAI - UTC = %d sec\n", de->tai_utc);
	fprintf(fp, "    UT1 - UTC = %9.6f sec\n", de->ut1_utc);
	fprintf(fp, "    Pole X, Y = %8.6f, %8.6f arcsec\n", de->xPole, de->yPole);
}

void printDifxEOP(const DifxEOP *de)
{
	fprintDifxEOP(stdout, de);
}

void fprintDifxEOPSummary(FILE *fp, const DifxEOP *de)
{
	fprintf(fp, "  EOP mjd=%5d  TAI-UTC=%d  UT1-UTC=%9.6f  X=%8.6f\"  Y=%8.6f\"\n",
		(int)(de->mjd + 0.5), de->tai_utc, de->ut1_utc, de->xPole, de->yPole);
}

void printDifxEOPSummary(const DifxEOP *de)
{
	fprintDifxEOPSummary(stdout, de);
}

void copyDifxEOP(DifxEOP *dest, const DifxEOP *src)
{
	if(dest != src)
	{
		if(dest == 0 || src == 0)
		{
			fprintf(stderr, "Error: copyDifxEOP: src=%p dest=%p but both must be non-null\n", src, dest);

			exit(EXIT_FAILURE);
		}
		*dest = *src;
	}
}

int isSameDifxEOP(const DifxEOP *de1, const DifxEOP *de2)
{
	if(de1->mjd == de2->mjd &&
	   de1->tai_utc == de2->tai_utc &&
	   de1->ut1_utc == de2->ut1_utc &&
	   de1->xPole == de2->xPole &&
	   de1->yPole == de2->yPole)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/* Note this function returns the number of merged EOP entries on the call 
 * stack : nde */
DifxEOP *mergeDifxEOPArrays(const DifxEOP *de1, int nde1, const DifxEOP *de2, int nde2, int *nde, const DifxMergeOptions *mergeOptions)
{
	DifxEOP *de;
	int mjdMin=-1, mjdMax=-1;
	int i, i1=0, i2=0;
	int src;

	/* First determine how many output eop rows.  It cannot be more than
	 * the sum of lengths of the two lists, but is likely shorter.  It
	 * also cannot be longer than mjdMax - mjdMin + 1 */

	if(nde1 > 0) for(i = 0; i < nde1; i++)
	{
		if(de1[i].mjd < mjdMin || mjdMin == -1)
		{
			mjdMin = de1[i].mjd;
		}
		if(de1[i].mjd > mjdMax || mjdMax == -1)
		{
			mjdMax = de1[i].mjd;
		}
	}
	if(nde2 > 0) for(i = 0; i < nde2; i++)
	{
		if(de2[i].mjd < mjdMin || mjdMin == -1)
		{
			mjdMin = de2[i].mjd;
		}
		if(de2[i].mjd > mjdMax || mjdMax == -1)
		{
			mjdMax = de2[i].mjd;
		}
	}

	if(mjdMax - mjdMin + 1 < nde1 + nde2)
	{
		*nde = mjdMax - mjdMin + 1;
	}
	else
	{
		*nde = nde1 + nde2;
	}

	de = newDifxEOPArray(*nde);

	/* now merge sort the EOPs, omitting duplicates */

	i = 0;
	for(;;)
	{
		if(i1 >= nde1)
		{
			i1 = -1;
		}
		if(i2 >= nde2)
		{
			i2 = -1;
		}
		if(i1 < 0 && i2 < 0)
		{
			break;
		}

		/* determine which EOPArray to take from */
		if(i1 == -1)
		{
			src = 2;
		}
		else if(i2 == -1)
		{
			src = 1;
		}
		else if(de1[i1].mjd <= de2[i2].mjd)
		{
			/* Two EOPs from the same day.  Make sure they are equal in value.  Then */
			/* arbitrarily choose to take from the first array. */
			/* If they are different, return a null pointer and set nde to 0, */
			/* indicating failure to merge.  */

			if(isSameDifxEOP(de1 + i1, de2 + i2) == 0)
			{
				/* OOPS! EOPs have differing values.  cannot merge! */
				*nde = 0;
				deleteDifxEOPArray(de);

				return 0;
			}

			src = 1;

			if(de1[i1].mjd == de2[i2].mjd)
			{
				i2++;
			}
		}
		else
		{
			src = 2;
		}

		/* do the copy and increments */
		if(src == 1)
		{
			copyDifxEOP(de + i, de1 + i1);
			i++;
			i1++;
		}
		else
		{
			copyDifxEOP(de + i, de2 + i2);
			i++;
			i2++;
		}
	}

	/* in case the output count was an overestimate: */
	*nde = i;

	return de;
}

int areDifxEOPsCompatible(const DifxEOP *de1, int nde1, const DifxEOP *de2, int nde2, const DifxMergeOptions *mergeOptions)
{
	DifxEOP *de;
	int nde;

	if(nde1 == 0 && nde2 == 0)
	{
		return 1;
	}

	if(mergeOptions->eopMergeMode == EOPMergeModeLoose)
	{
		return 1;
	}

	/* test mergability by actually trying.  Then delete the outcome if it is good. */
	de = mergeDifxEOPArrays(de1, nde1, de2, nde2, &nde, mergeOptions);
	if(!de)
	{
		/* merging fails! */
		return 0;
	}

	deleteDifxEOPArray(de);

	if(mergeOptions->eopMergeMode == EOPMergeModeRelaxed)
	{
		return 1;
	}
	else if(mergeOptions->eopMergeMode == EOPMergeModeStrict)
	{
		if(nde == nde1 && nde == nde2)
		{
			return 1;
		}
		else
		{
			return 0;
		}
	}

	return 0;
}

/* returns number of lines written */
int writeDifxEOPArray(FILE *out, int nEOP, const DifxEOP *de)
{
	int i = 0;

	writeDifxLineInt(out, "NUM EOPS", nEOP);
	for(i = 0; i < nEOP; i++)
	{
		writeDifxLineInt1(out, "EOP %d TIME (mjd)", i, de[i].mjd);
		writeDifxLineInt1(out, "EOP %d TAI_UTC (sec)", i, de[i].tai_utc);
		writeDifxLineDouble1(out, "EOP %d UT1_UTC (sec)", i, "%19.16f", de[i].ut1_utc);
		writeDifxLineDouble1(out, "EOP %d XPOLE (arcsec)", i, "%19.16f", de[i].xPole);
		writeDifxLineDouble1(out, "EOP %d YPOLE (arcsec)", i, "%19.16f", de[i].yPole);
	}

	return 5*nEOP + 1;
}
