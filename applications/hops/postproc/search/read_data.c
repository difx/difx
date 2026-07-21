/************************************************************************/
/*									*/
/* This routine is responsible for reading data into the internal	*/
/* memory of search.  The routine also takes care of			*/
/* automatically expanding the data array if it gets full, using a 	*/
/* standard UNIX memory allocation package				*/
/*									*/
/*	Inputs:		data		Data array			*/
/*			filename	Name of file to read		*/
/*									*/
/*	Output:		return value	0 for success, -1 for failure	*/
/*									*/
/* Created October 5 1995 by CJL, borrowed from average and simplified	*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include "search.h"

int
read_data (avg_data** data, char* filename, int* navg)
    {
    extern int space;
    static int first = TRUE;
    FILE *fp;
    struct stat statbuf;
    int nkb, mixed_type, other_type, nbadtype, nfail, version, type, pret;
    char line[512];
					/* Open up input file */
    if (strcmp (filename, "stdin") == 0) fp = stdin;
    else if (stat (filename, &statbuf) != 0) 
	{
	if (errno == ENOENT) msg ("File '%s' does not exist", 3, filename);
	else msg ("Problem accessing file '%s'", 3, filename);
	return (-1);
	}

    else if ((fp = fopen (filename,"r")) == NULL) 
	{
	msg ("Problem opening '%s'", 3, filename);
	return (-1);
	}
					/* Read through entire file */
    msg ("Reading data from file '%s' ...", 1, filename);
    nbadtype = nfail = 0;
    mixed_type = FALSE;
    while (fgets (line, 511, fp) != NULL) 
	{
					/* Ignore comment lines */
        if (afile_comment(line)) continue;
					/* What type of line is this? */
					/* Handle case of old format */
	aline_id (line, &version, &type);
					/* Better be 0, 1, 2, 3 or 4 */
	if (type != 2)
	    {
	    msg ("Found line other than type-2, skipping", 2);
	    continue;
	    }
					/* call parser */
	other_type = FALSE;
	pret = parse_fsumm (line, &((*data)[*navg].fdata));

					/* Count parsing failures */
	if (pret != 0)
	    {
	    msg ("line parse failed, pret = %d", -1, pret);
	    nfail++;
	    continue;
	    }
/*	if ((*data)[*navg].fdata.datatype[0] != 'J')
	    {
	    msg ("line is not incoherently scan-averaged data", 1);
	    nfail++;
	    continue;
	    } */
					/* Valid record, expand array if necessary */
	*navg += 1;
	if (*navg == space)
	    {
	    space += 100;
	    *data = (avg_data *) realloc(*data, space*sizeof(avg_data));
	    if (*data == NULL)
		{
		perror("realloc");
		msg("Fatal error allocating memory for data - Abort!", 3);
		exit(1);
		}
	    nkb = (space*sizeof(avg_data))/1024;
	    printf ("Expanded array memory to %d Kb ...\r", nkb); fflush (stdout);
	    }

	}				/* End of main read loop */

    nkb = (space*sizeof(avg_data))/1024;
    msg ("Total space occupied by data arrays = %d Kb", 1, nkb);

    if (nfail > 0)
	msg ("Warning, %d lines failed to parse in '%s' (ignored)", 2, 
			nfail, filename);

    if (strcmp (filename, "stdin") != 0) fclose(fp);
    return(0);
    }
