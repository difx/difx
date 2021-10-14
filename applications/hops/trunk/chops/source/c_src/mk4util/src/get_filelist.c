/************************************************************************/
/*                                                                      */
/* Generalized routine for extracting a command-line list of data file  */
/* names, with some parsing for the various name elements.  Input is a  */
/* unix-standard argc, argv pair, and a type argument to give a         */
/* simple filtering capability for the different file types.  Output    */
/* is a dynamically allocated array (allocated inside this package),    */
/* filled with the file names and parsed information.                   */
/* The most important feature is the ability to descend into multiple   */
/* subdirectory levels and search there for data files, via the         */
/* recursive routine extract_filenames()                                */
/*                                                                      */
/*      Inputs:         argc, argv              command line arguments  */
/*                                              in standard form        */
/*                      type                    0 to 4, -1 for any      */
/*                                                                      */
/*      Output:         files                   fstruct array           */
/*                                                                      */
/* Created December 23 1992 by CJL                                      */
/* Upgraded to mk4 file types August 4 1995 by CJL                      */
/* Searches datadir if not found locally Mar 7 gbc                      */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "fstruct.h"
#include "mk4_util.h"

int
get_filelist (int argc, char **argv, int type, fstruct **files)
    {
    char c;
    int i, j, filenum, depth, lastslash, nalloc;
    struct stat file_status;
    char *fname, *ddargv;
    static int first = TRUE;
    extern char datadir[];

    filenum = 0;
                                        /* Make some space to start with */
    if (first)
        {
        *files = (fstruct *) calloc (1000, sizeof(fstruct));
        if (*files == NULL)
            {
            msg ("Unable to allocate space for file list", 2);
            return (1);
            }
        nalloc = 1000;
        first = FALSE;
        }
                                        /* Loop over all args, looking for */
                                        /* correlator data files */
    for (i = 0; i < argc; i++)
        {
                                        /* Expand as needed.  check_name() */
                                        /* takes care of initialization */
        if (filenum >= MAXFILES)
            {
            msg ("Too many files, processing only first %d", 2, MAXFILES);
            break;
            }
        if (filenum >= nalloc-2)        /* Leaves space for termination */
            {
            nalloc += 1000;
            *files = (fstruct *) realloc (*files, nalloc*sizeof(fstruct));
            if (*files == NULL)
                {
                msg ("Unable to allocate space for file list", 2);
                return (1);
                }
            }
#ifdef HAVE_CONFIG_H
                                        /* Make sure this is a file/dir */
        if (stat(argv[i], &file_status) != 0)
	    {
	    msg ("Cannot find %s locally.", 1, argv[i]);
	    ddargv = malloc(strlen(argv[i]) + strlen(datadir) + 3);
	    if (ddargv == NULL)
		{
		msg ("Unable to allocate space for datadir/file", 2);
		continue;
		}
	    sprintf(ddargv, "%s/%s", datadir, argv[i]);
	    msg ("Trying %s in datadir.", 1, ddargv);
	    if (stat(ddargv, &file_status) == 0) argv[i] = ddargv;
	    else continue;		/* that didn't work, either */
	    msg ("Examining %s", 3, argv[i]);
#else /* HAVE_CONFIG_H */
        if (stat(argv[i], &file_status) != 0) continue
#endif /* HAVE_CONFIG_H */
	    }

                                        /* Make list of data file names */
					/* requested. extract_filename() is */
					/* recursive, so it will descend */
					/* directory levels until it finds */
                                        /* the correlator files. */
        depth = 0;
        if (file_status.st_mode & S_IFDIR) 
            {
            if (extract_filenames (argv[i], type, 
                        files, &nalloc, &filenum, &depth) != 0)
                return (1);
            }
                                        /* An ordinary file */
        else 
            {
                                        /* Locate last / so we can deal with */
                                        /* just the filename itself */
            j = 0;
            lastslash = 0;
            while ((c = argv[i][j++]) != '\0')
                if (c == '/') lastslash = j;
            fname = argv[i] + lastslash;
                                        /* Is this a valid filename? */
            if (check_name (fname, (*files)+filenum) != 0) continue;
                                        /* Is it of the desired type? */
            if (((*files)[filenum].type != type) && (type != -1)) continue;
                                        /* fill in filename */
            (*files)[filenum].name = (char *) malloc (strlen(argv[i]) + 1);
            if ((*files)[filenum].name == NULL)
                {
                msg ("Unable to allocate space for file names", 2);
                return (1);
                }
            strcpy ((*files)[filenum].name, argv[i]);
                                        /* OK, this one is accepted */
            filenum++;
            }
        }
                                        /* Terminate the file list */
    (*files)[filenum].order = -1;
                                        /* If this is complete list, sort it */
    if (type == -1)
        {
        if (sort_names (*files, filenum) < 0)
            {
            msg ("Error sorting input file list", 2);
            return (1);
            }
        }
                                        /* Necessary because if sort_names */
                                        /* is not called, all entries have */
                                        /* a -1 order value */
    else
        for (i=0; i<filenum; i++) (*files)[i].order = 0;

    return (0);
    }
