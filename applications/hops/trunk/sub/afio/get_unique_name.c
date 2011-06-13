/************************************************************************/
/*									*/
/* Figures out a valid correlator data file name from the partial	*/
/* specification that is the best you can do from A-file information	*/
/* prior to version 2 (when scan seconds information was inserted).	*/
/* This routine is intended as a partner to the *name() routines in	*/
/* the afio library, but will work standalone.  It has no effect on 	*/
/* inputs with a full filename specification, simply copying it to the	*/
/* output.								*/
/*									*/
/*	Inputs:		input		full or partial filename, with	*/
/*					asterisk in second field of	*/
/*					scantime directory name, as	*/
/*					placed there by rootname() etc.	*/
/*									*/
/*	Output:		output		full filename - may be same	*/
/*					string as input.		*/
/*		Note: input and output are relative pathnames, starting	*/
/*			at the experiment directory.  The data area	*/
/*			directory is specified in the datadir extern	*/
/*			(see $UTIL/environment())			*/
/*									*/
/*			return value	0 = success			*/
/*									*/
/* Created March 5 1992 by CJL						*/
/* Modified for AFIO library use October 18 1993 by CJL			*/
/*									*/
/************************************************************************/
#include <string.h>
#include <sys/types.h>
#include <stdio.h>
#include <dirent.h>
#include "mk4_afio.h"
#include "mk4_util.h"

int
get_unique_name(char *input, char *output)
    {
    DIR *dpexp, *dpscan, *opendir();
    struct dirent *dsexp, *dsscan, *readdir();
    char directory[200], rel_dir[200], scandir[200], tempout[256], *scanname, *fname;
    extern char datadir[];
    int i, j, slen, match;

    for (i = 0; input[i] != '*'; i++) 	/* Loop until asterisk */
	{
	output[i] = input[i];
	rel_dir[i] = input[i];
	if (rel_dir[i] == '\0') return (0);	/* name was complete already */
	}
    input[i] = '\0';			/* replace asterisk with null */
    rel_dir[i] = '\0';
    for (j = i; j > 0; j--)		/* search backwards for slash */
	{
	if (rel_dir[j] == '/')
	    {
	    rel_dir[j] = '\0';	/* This isolates experiment dir and */
	    break;			/* partially formed scantime */
	    }
	}
    if (j == 0) return (-1);		/* somehow a relative path got in */
    scanname = rel_dir + j + 1;
    fname = input + i + 2;	/* input+i = asterisk, jump over '/' */
				/* Prepend data area to relative directory path */
    sprintf (directory, "%s/%s", datadir, rel_dir);

				/* OK, now we have the experiment directory, and */
				/* the scantime subdirectory name missing the */
				/* second information. We also have the filename */
				/* and we need to search all matching scantime */
				/* directories for matching files.  There had better */
				/* be only one complete match, or there is an error */
    if ((dpexp = opendir (directory)) == NULL)
	{
/*	perror();      This gives seg fault on HP ????!!!! */
	msg ("Error opening experiment directory %s", 2, directory);
	return (-1);
	}

    match = 0;
    slen = strlen (scanname);
    while ((dsexp = readdir(dpexp)) != NULL)
	{				/* If scantime matches, go in for a look */
	if (strncmp (scanname, dsexp->d_name, slen) != 0) continue;
	if (strlen (dsexp->d_name) != slen + 2) continue;	/* omits other files */
	sprintf (scandir, "%s/%s",directory, dsexp->d_name);

	if ((dpscan = opendir (scandir)) == NULL)
	    {				/* Probable but not fatal error */
/*	    perror(); */
	    msg ("Error opening scantime directory %s", 2, dsexp->d_name);
	    continue;
	    }
	while ((dsscan = readdir(dpscan)) != NULL)	/* loop over file entries */
	    {
	    if (strcmp (fname, dsscan->d_name) == 0)
		{
		match++;			/* multiple matches bad news */
		if (match > 1)
		    {
		    msg ("Ambiguous file specification %s/%s*/%s", 2,
				directory, scanname, fname);
		    closedir (dpscan);
		    closedir (dpexp);
		    return (-1);
		    }				/* This is it, switch to relative */
		sprintf (tempout, "%s/%s/%s", rel_dir, dsexp->d_name, fname);
		}
	    }
	closedir (dpscan);
	}

    if (match == 0) 			/* Hmmm, never did find it */
	{
	msg ("No such file %s/%s*/%s", 2, directory, scanname, fname);
	closedir (dpexp);
	return (-1);
	}
    else strcpy (output, tempout);

    closedir (dpexp);

    return (0);
    }
