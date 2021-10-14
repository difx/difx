/************************************************************************/
/*									*/
/* Recursive routine which descends directory trees, searching each	*/
/* directory for valid correlator data filenames and adding them to	*/
/* the "files" fstruct array.  It is part of a general package for	*/
/* producing lists of data files from user-supplied command-line	*/
/* specifications, which may include parent directories of data areas.	*/
/*									*/
/*	Inputs:		directory		name of directory	*/
/*			type			0 to 4, -1 for any	*/
/*									*/
/*	In/Out		nalloc			number of elements	*/
/*						allocated in "files"	*/
/*			filenum			number of entries 	*/
/*						filled in to date	*/
/*			depth			recursion depth		*/
/*									*/
/*	Output:		files			array of data files	*/
/*									*/
/* Created 23 September 1992 by CJL					*/
/* Generic library version 15 January 1993 by CJL			*/
/* Upgraded to mk4 file types August 4 1995 by CJL			*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "fstruct.h"
#include "mk4_util.h"

#define MAXDEPTH 2			/* Any deeper and you risk some idiot */
					/* going to the main data area and typing */
					/* "*.*" for his data file specification, */
					/* which would generate a monster list of */
					/* all data on the disk */
int
extract_filenames (char *directory, int type, fstruct **files, int *nalloc, int *filenum, int *depth)
    {
    DIR *dp, *opendir();
    struct dirent *ds, *readdir();
    char temp[384], fulnam[384];
    /* char *ptr; */
    struct stat file_status;
    extern char progname[];

    *depth += 1;
					/* Open directory ... step 1 */
    if ((dp = opendir (directory)) == NULL)
	{
	sprintf (temp, "extract_filenames: directory name %s", directory);
	perror(temp);
	return (1);
	}
					/* Now actually read all filenames in */
					/* directory and examine each one in turn */
    while ((ds = readdir(dp)) != NULL)
	{
	if (*filenum >= MAXFILES)
            {
            msg ("Too many files, processing only first %d", 2, MAXFILES);
            break;
            }
        if (*filenum >= (*nalloc)-1)
            {
            *nalloc += 1000;
            *files = (fstruct *) realloc (*files, (*nalloc) * sizeof(fstruct));
            if (*files == NULL)
                {
                msg ("Unable to allocate space for file list", 2);
		closedir (dp);
                return (1);
                }
	    }
					/* These directory entries must be ignored */
	if ((strcmp (ds->d_name, ".") == 0) || (strcmp (ds->d_name, "..") == 0)) 
	    continue;
					/* Construct full pathname and stat it */
	sprintf (fulnam, "%s/%s", directory, ds->d_name);
	if (stat (fulnam, &file_status) != 0) 
	    {
	    perror (progname);
	    continue;
	    }
					/* This is a subdirectory.  If we have not */
					/* gone too deep already, make a recursive */
					/* call to this routine */
        if (file_status.st_mode & S_IFDIR)
	    {
	    if (*depth < MAXDEPTH)
        	if (extract_filenames (fulnam, type,
					 files, nalloc, filenum, depth) != 0)
		    {
		    closedir (dp);
		    return (1);
		    }
	    }

					/* This is a regular file ... just need to */
					/* check if it is a valid correlator data */
					/* file name, and add an appropriate entry */
					/* to the files structure array */
        else
	    {
	    				/* Is this a valid filename? */
	    if (check_name (ds->d_name, (*files)+(*filenum)) != 0) continue;
                                        /* Is it of the desired type? */
            if (((*files)[*filenum].type != type) && (type != -1)) continue;
                                        /* fill in filename */
            (*files)[*filenum].name = (char *) malloc (strlen(fulnam) + 1);
            if ((*files)[*filenum].name == NULL)
                {
                msg ("Unable to allocate space for file names", 2);
		closedir (dp);
                return (1);
                }
            strcpy ((*files)[*filenum].name, fulnam);
                                        /* OK, this one is accepted */
            *filenum += 1;
	    }
	}

    closedir (dp);
    *depth -= 1;
    return (0);
    }
	
