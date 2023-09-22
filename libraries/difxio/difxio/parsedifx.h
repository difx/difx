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

#ifndef __PARSEDIFX__
#define __PARSEDIFX__

/* the following is the maximum length a difx .input (or .calc, ...) line can be.
 * the large value is dictacted by need to support a nearly limitless number of
 * scans or sources in a RULE table
 */
#define	MAX_DIFX_INPUT_LINE_LENGTH	8192

#define MAX_DIFX_KEY_LEN		32

#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
	char *line;	/* entire, unmodified line of text */
	char *key;	/* before colon, trailing whitespace removed */
			/* also can hold comment */
	char *value;	/* after colon, leading/trailing whitespace removed */
			/* NULL pointer if no colon on row */
} DifxRow;

typedef struct
{
	int num_rows;	/* number of rows populated */
	int alloc_rows;	/* number of rows allocated; not to be updated by users */
	DifxRow *rows;	/* pointer to allocated row structures */
} DifxParameters;

typedef struct
{
	char **str;
	int n;

	int nAlloc;	/* number of rows allocated; not to be updated by users */
} DifxStringArray;



/* Allocate new structure with given number of rows */
DifxParameters *newDifxParameters();

/* Free from memory existing structure */
void deleteDifxParameters(DifxParameters *dp);

/* Load difx file, allocating memory as needed, return pointer to struct */
DifxParameters *newDifxParametersfromfile(const char *filename);

/* Delete all the parameters */
void resetDifxParameters(DifxParameters *dp);

/* Mainly used internally -- used to double the number of allocated rows */
void growDifxParameters(DifxParameters *dp);

/* Add another line of text to structure and parse at same time */
int DifxParametersaddrow(DifxParameters *dp, const char *line);

/* Initialize a string array; not needed if the structure starts as binary zero */
void DifxStringArrayinit(DifxStringArray *sa);

/* Add string to array, using strndup */
int DifxStringArrayadd(DifxStringArray *sa, const char *str, int max);

/* Take a comma separated string list and parcel out into a string array */
int DifxStringArrayaddlist(DifxStringArray *sa, const char *str);

/* Erase allocated memory of a DifxStringArray */
void DifxStringArrayclear(DifxStringArray *sa);

/* Print to stdout the list of strings */
void DifxStringArrayprint(const DifxStringArray *sa);

/* Copy all members of src to dest, without first deleting contents of dest */
int DifxStringArrayappend(DifxStringArray *dest, const DifxStringArray *src);

/* Check for membership of DifxStringArray: return 1 if true */
int DifxStringArraycontains(const DifxStringArray *sa, const char *str);

/* Print contents of structure to stdout */
void printDifxParameters(const DifxParameters *dp);

/* Return index of first match to "key" starting from start_row */
int DifxParametersfind(const DifxParameters *dp, int start_row, const char *key);

/* Same as above, but allow key with a particular index value.  for example
 * to match "TELESCOPE 1 INDEX" set key = "TELESCOPE %d INDEX" and index = 1 */
int DifxParametersfind1(const DifxParameters *dp, int start_row, const char *key, int index1);

/* Same as above, but allow two indexes */
int DifxParametersfind2(const DifxParameters *dp, int start_row, const char *key, int index1, int index2);

/* the _limited versions of these limits the range of row to search */
int DifxParametersfind_limited(const DifxParameters *dp, int start_row, int max_rows, const char *key);
int DifxParametersfind1_limited(const DifxParameters *dp, int start_row, int max_rows, const char *key, int index1);
int DifxParametersfind2_limited(const DifxParameters *dp, int start_row, int max_rows, const char *key, int index1, int index2);

/* Safely get the value of a row */
const char *DifxParametersvalue(const DifxParameters *dp, int row);

/* find row indicies for several keys */
int DifxParametersbatchfind(const DifxParameters *dp, int start, const char keys[][MAX_DIFX_KEY_LEN], int n, int rows[]);
int DifxParametersbatchfind1(const DifxParameters *dp, int start, const char keys[][MAX_DIFX_KEY_LEN], int index1, int n, int rows[]);
int DifxParametersbatchfind2(const DifxParameters *dp, int start, const char keys[][MAX_DIFX_KEY_LEN], int index1, int index2, int n, int rows[]);

#ifdef __cplusplus
}
#endif


#endif
