/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/applications/difx2fits/trunk/src/fitsTS.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include "difx2fits.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glob.h>
#include "util.h"
#define  UTIL_DEBUG 0

int glob2(const char *label, const char *pattern, int flags, int errfunc(const char *epath, int eerrno), glob_t *pglob)
{
	int v;

	v = glob(pattern, flags, errfunc, pglob);
	if(v != 0)
	{
		if(v == GLOB_NOSPACE)
		{
			fprintf(stderr, "Error: %s: No space!\n", label);

			exit(EXIT_FAILURE);
		}
		else if(v == GLOB_ABORTED)
		{
			fprintf(stderr, "Error: %s: Read error!\n", label);

			exit(EXIT_FAILURE);
		}
		else if(v == GLOB_NOMATCH)
		{
			/* no matching files, so quietly leave function */
		}
		else
		{
			fprintf(stderr, "Error: %s: unknown glob() failure! %d\n", label, v);

			exit(EXIT_FAILURE);
		}
	}

	return v;
}

/* look for files that match given filename modulo case.  Put result in
 * filename that was passed to this function.
 *
 * If multiple matches are found, use one at random :( and squawk
 *
 * Return: number of files that matched.
 */
int globcase(const char *label, const char *match, char *fileName)
{
	int i;
	int n = 0;
	glob_t globbuf;

	glob2(label, match, 0, 0, &globbuf);

	if(globbuf.gl_pathc == 0)
	{
		globfree(&globbuf);

		return 0;
	}
	for(i = 0; i < globbuf.gl_pathc; ++i)
	{
		if(strcasecmp(fileName, globbuf.gl_pathv[i]) == 0)
		{
			if(n == 0)
			{
				strcpy(fileName, globbuf.gl_pathv[i]);
			}
			else if(n == 1)
			{
				fprintf(stderr, "\nError: multiple filenames matching %s differing only in case were found.\n", fileName);

				exit(EXIT_FAILURE);
			}
			++n;
		}
	}

	globfree(&globbuf);

	return n;
}

/*
*  Auxiulliary routine for sorting strings
*/
static int
       cmpstringp(const void *p1, const void *p2)
       {
           /* The actual arguments to this function are "pointers to
              pointers to char", but strcmp(3) arguments are "pointers
              to char", hence the following cast plus dereference */

           return strcmp(* (char * const *) p1, * (char * const *) p2);
       }

/* 
*  Sort the contents of pglob buffer chronologically assuming 
*  it contains PCAL filenames created by DiFX
*/
int glob2_sort_pcal_files( glob_t *pglob)
{
	char mjd_str[14];
	int  i, j, ret, ind;
	char *str;
	char **strarr;
	char date_prefix[14];
	char *pattern = ".difx/PCAL";

	if ( pglob->gl_pathc < 2 ){
	     return 0;
	}

        strarr = (char **)malloc ( pglob->gl_pathc * sizeof(char*));
        
	for(i = 0; i < pglob->gl_pathc; ++i)
        {
/*
* ------------- Extract date from the PCAL file name and put it in date_prefix
*/
		str = strstr(pglob->gl_pathv[i], pattern );
		ind = str - pglob->gl_pathv[i] + strlen(pattern);
		strncpy ( date_prefix, pglob->gl_pathv[i] + ind+1, sizeof(date_prefix)-1 );
		date_prefix[sizeof(date_prefix)-1] = '\0';

/*
* ------------- Prepend date_prefix for the file name and put result to strarr
*/
		strarr[i] = malloc ( DIFXIO_FILENAME_LENGTH+1 );
		strncpy ( strarr[i], date_prefix, DIFXIO_FILENAME_LENGTH );
		strncat ( strarr[i], pglob->gl_pathv[i], DIFXIO_FILENAME_LENGTH );
        }

/*
* ----- Sort strarr alphabetically which is equivalent to chronological sorting
*/
        qsort ( strarr, pglob->gl_pathc, sizeof (char **), cmpstringp);

/*
* ----- copy strarr to pglob with data_prefix removed
*/
	for(i = 0; i < pglob->gl_pathc; ++i){
		strncpy ( pglob->gl_pathv[i], strarr[i] + strlen(date_prefix), strlen(pglob->gl_pathv[i])+1 );
                free ( strarr[i] );
        }
        free ( strarr );
        
	return 0;
}

/* 
*  Generage a cross reference table for chronological sorting of jobs
*  associated with a given antenna with index antennaId.
*  In order to achieve this, the code searches for phase calibration
*  files created by DiFX for a given job and extracts the date from the 
*  file name. If there is more than phcase calibration file for 
*  a given job and given station, the pcal file for the earliest epoch is picked.
*/
int sortjobpcal ( const DifxInput *D, int antennaId, int *jobxref )
{
   int    ind, i, j, v;
   glob_t globBuffer;
   char   date_prefix[14];
   char   **strarr;
   char   *str;
   char   globPattern[DIFXIO_FILENAME_LENGTH];
   char   *pattern = ".difx/PCAL";
   char   *filler = "99999_999999_";

   if ( UTIL_DEBUG == 1 ){
        printf ( "\n%s line %4d i= %d  D->nJob %d \n", __FILE__, __LINE__, D->nJob ); fflush(stdout);
   }
   if ( D->nJob < 1 ){
        return 0;
   }
   if ( D->nJob == 1 ){
        jobxref[0] = 0;
        return 0;
   }
   strarr = (char **)malloc ( D->nJob * sizeof(char*));
   for ( i = 0; i < D->nJob; ++i){
         v = snprintf ( globPattern, DIFXIO_FILENAME_LENGTH, "%s/PCAL*%s", D->job[i].outputFile, D->antenna[antennaId].name);
         v = glob2(__FUNCTION__, globPattern, 0, 0, &globBuffer);
	 if ( v == 0 ){
/*
* ----------- Sort PCAL files chronologically
*/
              v = glob2_sort_pcal_files( &globBuffer);
/*
* ----------- Extract date from the PCAL file name and put it in date_prefix
*/
	      str = strstr(globBuffer.gl_pathv[0], pattern );
	      ind = str -  globBuffer.gl_pathv[0] + strlen(pattern);
	      strncpy ( date_prefix, globBuffer.gl_pathv[0] + ind+1, sizeof(date_prefix)-1 );
	      date_prefix[sizeof(date_prefix)-1] = '\0';

/*
* ----------- Prepend date_prefix for the file name and put result to strarr
*/
	      strarr[i] = malloc ( DIFXIO_FILENAME_LENGTH+1 );
	      strncpy ( strarr[i], date_prefix, DIFXIO_FILENAME_LENGTH );
	      strncat ( strarr[i], globBuffer.gl_pathv[0], DIFXIO_FILENAME_LENGTH );
         } else {
/*
* ----------- Given station did not observe for this job. Put place holder
* ----------- to put that that job to the end of the list after sorting
*/
	      strarr[i] = malloc ( DIFXIO_FILENAME_LENGTH+1 );
	      strncpy ( strarr[i], filler, DIFXIO_FILENAME_LENGTH );
         }
         if ( UTIL_DEBUG == 1 ){
              printf ( "%s line %4d  i= %d  strarr[i]= %s\n", __FILE__, __LINE__, i, strarr[i] ); fflush(stdout);
         }
   }
/*
* ----- Sort strarr arrayt alphabetically which is equivalent to chronological sorting
*/
   qsort ( strarr, D->nJob, sizeof (char **), cmpstringp );
/*
* ----- Collect cross references
*/
   for ( i = 0; i < D->nJob; ++i){
         if ( strstr(strarr[i], filler) == NULL) {
              for ( j = 0; j < D->nJob; ++j){
                    if ( strstr(strarr[i], D->job[j].outputFile) != NULL) {
                         jobxref[i] = j;
                    }
               }
         } else {
               jobxref[i] = -1;
         }
         if ( UTIL_DEBUG == 1 ){
              printf ( "%s line %4d i= %d  st= %s ref[%3d]= %3d \n", __FILE__, __LINE__, i, strarr[i], i, jobxref[i] ); fflush(stdout);
         }
         free( strarr[i] ); 
   }
   free( strarr ); 
}
