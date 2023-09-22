/***************************************************************************
 *   Copyright (C) 2007-2012 by Walter Brisken                             *
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

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxio/parsedifx.h"

/* utility to provide read-only access to DiFX style text parameter files */

DifxParameters *newDifxParameters()
{
	DifxParameters *dp;
	int alloc_rows;

	/* initial size of array, will grow if needed */
	alloc_rows = 100;

	dp = (DifxParameters *)calloc(1, sizeof(DifxParameters));
	dp->alloc_rows = alloc_rows;
	dp->rows = (DifxRow *)calloc(alloc_rows, sizeof(DifxRow));

	return dp;
}

static void copyrow(DifxRow *dest, const DifxRow *src)
{
	dest->line  = src->line;
	dest->key   = src->key;
	dest->value = src->value;
}

/* double number of allocated rows */
void growDifxParameters(DifxParameters *dp)
{
	DifxRow *newrows;
	int alloc_rows;
	int i;

	alloc_rows = dp->alloc_rows;

	newrows = (DifxRow *)calloc(2*alloc_rows, sizeof(DifxRow));
	for(i = 0; i < alloc_rows; ++i)
	{
		copyrow(newrows + i, dp->rows + i);
	}
	for(i = alloc_rows; i < 2*alloc_rows; ++i)
	{
		DifxRow *row;

		row = newrows + i;
		row->line = 0;
		row->key = 0;
		row->value = 0;
	}
	free(dp->rows);
	dp->rows = newrows;
	dp->alloc_rows *= 2;
}

void deleteDifxParameters(DifxParameters *dp)
{
	if(dp)
	{
		if(dp->alloc_rows > 0)
		{
			int i;

			for(i = 0; i < dp->alloc_rows; ++i)
			{
				DifxRow *row;
				
				row = dp->rows + i;
				if(row->line)
				{
					free(row->line);
				}
				if(row->key)
				{
					free(row->key);
				}
				if(row->value)
				{
					free(row->value);
				}
			}
			free(dp->rows);
		}
		free(dp);
	}
}

static void parserow(DifxRow *row)
{
	int colon = -1;
	int firstprintable = -1;
	int lastprintable = -1;
	int i;
	char *line;

	line = row->line;

	for(i = 0; line[i]; ++i)
	{
		if(line[i] == ':')
		{
			colon = i;
			break;
		}
		if(line[i] > ' ')
		{
			lastprintable = i;
		}
	}

	if(lastprintable < 0)
	{
		return;
	}

	row->key = (char *)calloc(lastprintable+2, 1);
	snprintf(row->key, lastprintable+2, "%s", line);
	
	if(colon < 0)
	{
		return;
	}

	/* determine extent of printable characters of "value" */

	for(i = colon+1; line[i]; ++i)
	{
		if(line[i] > ' ')
		{
			if(firstprintable < 0)
			{
				firstprintable = i;
			}
			lastprintable = i;
		}
	}

	if(firstprintable >= 0)
	{
		row->value = (char *)calloc(lastprintable-firstprintable+2, 1);
		snprintf(row->value, lastprintable-firstprintable+2, "%s", line+firstprintable);
	}
	else
	{
		row->value = (char *)calloc(2, sizeof(char));
	}
}

void resetDifxParameters(DifxParameters *dp)
{
	int i;

	if(!dp)
	{
		fprintf(stderr, "resetDifxParameters : dp = 0\n");

		return;
	}

	if(dp->num_rows == 0)
	{
		return;
	}

	for(i = 0; i < dp->num_rows; ++i)
	{
		DifxRow *row;
		
		row = &dp->rows[i];
		if(row->line)
		{
			free(row->line);
			row->line = 0;
		}
		if(row->key)
		{
			free(row->key);
			row->key = 0;
		}
		if(row->value)
		{
			free(row->value);
			row->value = 0;
		}
	}

	dp->num_rows = 0;
}

int DifxParametersaddrow(DifxParameters *dp, const char *line)
{
	DifxRow *row;

	if(!dp)
	{
		fprintf(stderr, "DifxParametersaddrow : dp = 0\n");

		return -1;
	}

	if(dp->num_rows >= dp->alloc_rows)
	{
		growDifxParameters(dp);
	}

	row = &dp->rows[dp->num_rows];
	++dp->num_rows;

	row->line = strdup(line);

	parserow(row);

	return dp->num_rows;
}

DifxParameters *newDifxParametersfromfile(const char *filename)
{
	DifxParameters *dp;
	char line[MAX_DIFX_INPUT_LINE_LENGTH+1];
	FILE *in;

	in = fopen(filename, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open %s for read\n", filename);

		return 0;
	}

	dp = newDifxParameters();

	for(;;)
	{
		char *ptr;
		
		ptr = fgets(line, MAX_DIFX_INPUT_LINE_LENGTH, in);
		if(ptr == 0)
		{
			break;
		}
		DifxParametersaddrow(dp, line);
	}
	
	fclose(in);

	return dp;
}

void printDifxParameters(const DifxParameters *dp)
{
	int i;
	
	if(!dp)
	{
		fprintf(stderr, "printDifxParameters : dp = 0\n");
		
		return;
	}
	
	printf("DifxParameters : nrow = %d\n", dp->num_rows);

	for(i = 0; i < dp->num_rows; ++i)
	{
		const DifxRow *row;
		
		row = dp->rows + i;
		if(row->value)
		{
			printf("%d\t<%s> = <%s>\n", i, row->key, row->value);
		}
		else if(row->key)
		{
			printf("%d\t<%s>\n", i, row->key);
		}
		else
		{
			printf("%d\n", i);
		}
	}
}

int DifxParametersfind_limited(const DifxParameters *dp, int start_row, int max_rows, const char *key)
{
	int i;
	int max_r;

	max_r = start_row + max_rows;
	if(max_r > dp->num_rows)
	{
		max_r = dp->num_rows;
	}

	if(!dp)
	{
		fprintf(stderr, "DifxParametersgetstring : dp = 0\n");

		return -1;
	}

	if(!key)
	{
		fprintf(stderr, "DifxParametersgetstring : key = 0\n");
		
		return -1;
	}

	if(start_row < 0 || start_row >= dp->num_rows)
	{
		return -1;
	}

	for(i = start_row; i < max_r; ++i)
	{
		if(dp->rows[i].key == 0)
		{
			continue;
		}
		if(strcmp(key, dp->rows[i].key) == 0)
		{
			return i;
		}
	}

	return -1;
}

int DifxParametersfind(const DifxParameters *dp, int start_row, const char *key)
{
	int i;

	if(!dp)
	{
		fprintf(stderr, "DifxParametersgetstring : dp = 0\n");

		return -1;
	}

	if(!key)
	{
		fprintf(stderr, "DifxParametersgetstring : key = 0\n");
		
		return -1;
	}

	if(start_row < 0 || start_row >= dp->num_rows)
	{
		return -1;
	}

	for(i = start_row; i < dp->num_rows; ++i)
	{
		if(dp->rows[i].key == 0)
		{
			continue;
		}
		if(strcmp(key, dp->rows[i].key) == 0)
		{
			return i;
		}
	}

	return -1;
}

int DifxParametersfind1_limited(const DifxParameters *dp, int start_row, int max_rows, const char *key, int index1)
{
	char newkey[MAX_DIFX_KEY_LEN+1];

	snprintf(newkey, MAX_DIFX_KEY_LEN+1, key, index1);

	return DifxParametersfind_limited(dp, start_row, max_rows, newkey);
}

int DifxParametersfind2_limited(const DifxParameters *dp, int start_row, int max_rows, const char *key, int index1, int index2)
{
	char newkey[MAX_DIFX_KEY_LEN+1];

	snprintf(newkey, MAX_DIFX_KEY_LEN+1, key, index1, index2);

	return DifxParametersfind_limited(dp, start_row, max_rows, newkey);
}

int DifxParametersfind1(const DifxParameters *dp, int start_row, const char *key, int index1)
{
	char newkey[MAX_DIFX_KEY_LEN+1];

	snprintf(newkey, MAX_DIFX_KEY_LEN+1, key, index1);

	return DifxParametersfind(dp, start_row, newkey);
}

int DifxParametersfind2(const DifxParameters *dp, int start_row, const char *key, int index1, int index2)
{
	char newkey[MAX_DIFX_KEY_LEN+1];

	snprintf(newkey, MAX_DIFX_KEY_LEN+1, key, index1, index2);

	return DifxParametersfind(dp, start_row, newkey);
}

const char *DifxParametersvalue(const DifxParameters *dp, int row)
{
	if(!dp)
	{
		fprintf(stderr, "DifxParametersvalue : dp = 0\n");

		return 0;
	}
	
	if(row < 0 || row >= dp->num_rows)
	{
		return 0;
	}

	return dp->rows[row].value;
}

/* return number of found symbols */
int DifxParametersbatchfind(const DifxParameters *dp, int start, const char keys[][MAX_DIFX_KEY_LEN], int n, int rows[])
{
	int i;
	int s;

	s = start;
	for(i = 0; i < n; ++i)
	{
		rows[i] = DifxParametersfind(dp, s, keys[i]);
		if(rows[i] < 0)
		{
			fprintf(stderr, "Parameter not found: %s\n", keys[i]);

			return i;
		}
		s = rows[i] + 1;
	}

	return n;
}

/* return number of found symbols */
int DifxParametersbatchfind1(const DifxParameters *dp, int start, const char keys[][MAX_DIFX_KEY_LEN], int index1, int n, int rows[])
{
	int i;
	int s;

	s = start;
	for(i = 0; i < n; ++i)
	{
		rows[i] = DifxParametersfind1(dp, s, keys[i], index1);
		if(rows[i] < 0)
		{
			fprintf(stderr, "Parameter not found: ");
			fprintf(stderr, keys[i], index1);
			fprintf(stderr, "\n");

			return i;
		}
		s = rows[i] + 1;
	}

	return n;
}

/* return number of found symbols */
int DifxParametersbatchfind2(const DifxParameters *dp, int start, const char keys[][MAX_DIFX_KEY_LEN], int index1, int index2, int n, int rows[])
{
	int i;
	int s;

	s = start;
	for(i = 0; i < n; ++i)
	{
		rows[i] = DifxParametersfind2(dp, s, keys[i], index1, index2);
		if(rows[i] < 0)
		{
			fprintf(stderr, "Parameter not found: ");
			fprintf(stderr, keys[i], index1, index2);
			fprintf(stderr, "\n");

			return i;
		}
		s = rows[i] + 1;
	}

	return n;
}

void DifxStringArrayinit(DifxStringArray *sa)
{
	sa->n = 0;
	sa->nAlloc = 0;
	sa->str = 0;
}

int DifxStringArrayadd(DifxStringArray *sa, const char *str, int max)
{
	if(!sa)
	{
		return -1;
	}

	if(sa->nAlloc == 0)
	{
		sa->nAlloc = 16;
		sa->n = 0;
		sa->str = (char **)calloc(sa->nAlloc, sizeof(char *));
	}
	else if(sa->nAlloc <= sa->n)
	{
		sa->nAlloc *= 2;
		sa->str = (char **)realloc(sa->str, sa->nAlloc*sizeof(char *));
	}

	if(str)
	{
		if(max > 0)
		{
			sa->str[sa->n] = strndup(str, max);
		}
		else
		{
			sa->str[sa->n] = strdup(str);
		}
	}
	else
	{
		sa->str[sa->n] = strdup("");
	}

	++sa->n;

	return sa->n;
}

int DifxStringArrayaddlist(DifxStringArray *sa, const char *str)
{
	int start = 0;
	int i;

	if(!sa)
	{
		return -1;
	}

	for(i = 0; ; ++i)
	{
		if(str[i] == 0 || str[i] == ',')
		{
			if(i > start)
			{
				int a, b;
				
				for(a = start; a < i; ++a)
				{
					if(str[a] > ' ')
					{
						break;
					}
				}
				for(b = i-1; b >= start; --b)
				{
					if(str[b] > ' ')
					{
						break;
					}
				}
				if(b >= a)
				{
					DifxStringArrayadd(sa, str + a, b - a + 1);
				}
			}

			start = i + 1;
		}

		if(str[i] == 0)
		{
			/* done */

			break;
		}
	}

	return sa->n;
}

int DifxStringArrayappend(DifxStringArray *dest, const DifxStringArray *src)
{
	if(src->n > 0)
	{
		int i;
		
		for(i = 0; i < src->n; ++i)
		{
			DifxStringArrayadd(dest, src->str[i], 0);
		}
	}

	return src->n;
}

int DifxStringArraycontains(const DifxStringArray *sa, const char *str)
{
	int i;

	if(!sa)
	{
		return 0;
	}
	if(sa->n <= 0)
	{
		return 0;
	}
	for(i = 0; i < sa->n; ++i)
	{
		if(strcmp(sa->str[i], str) == 0)
		{
			return 1;
		}
	}

	return 0;
}

void DifxStringArrayprint(const DifxStringArray *sa)
{
	printf("DifxStringArray [%p]\n", sa);
	if(sa)
	{
		printf("  n=%d\n", sa->n);
		if(sa->n > 0)
		{
			int i;
			
			for(i = 0; i < sa->n; ++i)
			{
				printf("  str[%d]=%s\n", i, sa->str[i]);
			}
		}
	}
}

void DifxStringArrayclear(DifxStringArray *sa)
{
	if(!sa)
	{
		return;
	}

	if(sa->nAlloc > 0)
	{
		int i;

		for(i = 0; i < sa->n; ++i)
		{
			if(sa->str[i])
			{
				free(sa->str[i]);
			}
		}
		free(sa->str);
		sa->nAlloc = 0;
		sa->n = 0;
		sa->str = 0;
	}
}
