/***************************************************************************
 *   Copyright (C) 2008-2019 by Walter Brisken                             *
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

#include <time.h>
#include "difxio/difx_write.h"

double truncSeconds(double mjd)
{
	int intmjd, intsec;

	intmjd = mjd;
	intsec = (mjd - intmjd)*86400.0;

	return intmjd + intsec/86400.0;
}

double roundSeconds(double mjd)
{
	int intmjd, intsec;

	intmjd = mjd;
	intsec = ((mjd - intmjd)*86400.0 + 0.5);

	return intmjd + intsec/86400.0;
}

static int mjd2date(long mjd, int *pYear, int *pMonth, int *pDay)
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * This function returns ERROR if the requested MJD precedes UNIX day 0 (MJD 40587)
 */
{
	const int MJD_UNIX0 = 40587;	/* MJD of start of UNIX clock */
	struct tm tm;
	time_t t;

	if(mjd < MJD_UNIX0)
	{
		return -1;
	}

	t = 86400LL*(mjd - MJD_UNIX0);
	gmtime_r(&t, &tm);

	if(pYear)
	{
		*pYear = tm.tm_year + 1900;
	}
	if(pMonth)
	{
		*pMonth = tm.tm_mon + 1;
	}
	if(pDay)
	{
		*pDay = tm.tm_mday;
	}

	return 0;
}

/* Just write the key; intended for call only by other write commands here */
static int writeDifxLineKey(FILE *out, const char *key)
{
	char line[MAX_DIFX_KEY_LEN+2];
	int i;

	for(i = 0; i < MAX_DIFX_KEY_LEN && key[i]; ++i)
	{
		line[i] = key[i];
	}
	line[i] = ':';
	++i;
	while(i < 20)
	{
		line[i] = ' ';
		++i;
	}
	
	line[i] = 0;
	++i;
	fprintf(out, "%s", line);

	return 0;
}

int writeDifxLine(FILE *out, const char *key, const char *value)
{
	writeDifxLineKey(out, key);
	fprintf(out, "%s\n", value);

	return 0;
}

int writeDifxLine1(FILE *out, const char *key, int i1, const char *value)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLine2(FILE *out, const char *key, int i1, int i2, const char *value)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1, i2);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLine3(FILE *out, const char *key, int i1, int i2, int i3, const char *value)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1, i2, i3);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLineBoolean(FILE *out, const char *key, int value)
{
	if(value)
	{
		return writeDifxLine(out, key, "TRUE");
	}
	else
	{
		return writeDifxLine(out, key, "FALSE");
	}
}

int writeDifxLineBoolean1(FILE *out, const char *key, int i1, int value)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1);
	
	if(value)
	{
		return writeDifxLine(out, k, "TRUE");
	}
	else
	{
		return writeDifxLine(out, k, "FALSE");
	}
}

int writeDifxLineInt(FILE *out, const char *key, int value)
{
	const int numLength = 32;
	char v[numLength];

	snprintf(v, numLength, "%d", value);

	return writeDifxLine(out, key, v);
}

int writeDifxLineInt1(FILE *out, const char *key, int i1, int value)
{
	const int numLength = 32;
	char v[numLength];
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(v, numLength, "%d", value);

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1);
	
	return writeDifxLine(out, k, v);
}

int writeDifxLineInt2(FILE *out, const char *key, int i1, int i2, int value)
{
	const int numLength = 32;
	char v[numLength];
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(v, numLength, "%d", value);

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1, i2);
	
	return writeDifxLine(out, k, v);
}

int writeDifxLineHex(FILE *out, const char *key, unsigned long value)
{
	const int numLength = 32;
	char v[numLength];

	snprintf(v, numLength, "0x%lX", value);

	return writeDifxLine(out, key, v);
}

int writeDifxLineDouble(FILE *out, const char *key, const char *format, double value)
{
	const int numLength = 32;
	char v[numLength];

	if(!format)
	{
		format = "%f";
	}
	if(!format[0])
	{
		format = "%f";
	}

	snprintf(v, numLength, format, value);

	return writeDifxLine(out, key, v);
}

int writeDifxLineDouble1(FILE *out, const char *key, int i1, const char *format, double value)
{
	const int numLength = 32;
	char v[numLength];
	char k[MAX_DIFX_KEY_LEN+1];

	if(!format)
	{
		format = "%f";
	}
	if(!format[0])
	{
		format = "%f";
	}

	snprintf(v, numLength, format, value);

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1);
	
	return writeDifxLine(out, k, v);
}

int writeDifxLineDouble2(FILE *out, const char *key, int i1, int i2, const char *format, double value)
{
	const int numLength = 32;
	char v[numLength];
	char k[MAX_DIFX_KEY_LEN+1];

	if(!format)
	{
		format = "%f";
	}
	if(!format[0])
	{
		format = "%f";
	}

	snprintf(v, numLength, format, value);

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1, i2);
	
	return writeDifxLine(out, k, v);
}

int writeDifxLineArray(FILE *out, const char *key, const double *array, int n)
{
	int i;

	writeDifxLineKey(out, key);

	for(i = 0; i < n; ++i)
	{
		fprintf(out, "%24.16e%c", array[i], (i < n-1) ? '\t' : '\n');
	}

	return 0;
}

int writeDifxLineArray1(FILE *out, const char *key, int i1, 
	const double *array, int n)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1);
	
	return writeDifxLineArray(out, k, array, n);
}

int writeDifxLineArray2(FILE *out, const char *key, int i1, int i2, const double *array, int n)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1, i2);
	
	return writeDifxLineArray(out, k, array, n);
}

int writeDifxLineStringArray(FILE *out, const char *key, const DifxStringArray *sa)
{
	int i;

	if(!sa)
	{
		return -1;
	}

	writeDifxLineKey(out, key);

	if(sa->n > 0)
	{
		for(i = 0; i < sa->n; ++i)
		{
			fprintf(out, "%s%c", sa->str[i], (i < sa->n-1) ? ',' : '\n');
		}
	}
	else
	{
		fprintf(out, "\n");
	}

	return 0;
}

int writeDifxLineStringArray1(FILE *out, const char *key, int i1, const DifxStringArray *sa)
{
	char k[MAX_DIFX_KEY_LEN+1];

	snprintf(k, MAX_DIFX_KEY_LEN+1, key, i1);

	return writeDifxLineStringArray(out, k, sa);
}

int writeDifxDateLines(FILE *out, double mjd)
{
	int yr=0, mo=0, da=0, hr, mi, se;
	int mjdint;
	double mjdfrac;
	
	mjd += 1.0/100000000.0;
	mjdint = (int)mjd;
	mjd2date(mjdint, &yr, &mo, &da);
	mjdfrac = (mjd-mjdint)*24.0;
	hr = mjdfrac;
	mjdfrac -= hr;
	mjdfrac *= 60.0;
	mi = mjdfrac;
	mjdfrac -= mi;
	mjdfrac *= 60.0;
	se = mjdfrac;

	writeDifxLineInt(out, "START YEAR", yr);
	writeDifxLineInt(out, "START MONTH", mo);
	writeDifxLineInt(out, "START DAY", da);
	writeDifxLineInt(out, "START HOUR", hr);
	writeDifxLineInt(out, "START MINUTE", mi);
	writeDifxLineInt(out, "START SECOND", se);

	return 0;
}

