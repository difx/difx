/************************************************************************/
/*									*/
/* This routine is responsible for reading data into the internal	*/
/* memory of aedit.  It takes a filename and reads through it, storing	*/
/* only those lines which are consistent with the current input		*/
/* settings.  This filtering is done in as efficient a manner as	*/
/* possible, by checking only those inputs which are not in their	*/
/* default "pass anything" state, and by breaking out of the checking	*/
/* operation at the first mismatch.  The routine also takes care of	*/
/* automatically expanding the data array if it gets full, using a 	*/
/* standard UNIX memory allocation package				*/
/*									*/
/*	Inputs:		data		Data array			*/
/*			filename	Name of file to read		*/
/*									*/
/*	Output:		return value	0 for success, -1 for failure	*/
/*									*/
/* Created April 9 1989 by CJL						*/
/* Modified February 1 1993 by CJL -- Use AFIO library			*/
/*									*/
/************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "aedata.h"
#include "aedit.h"

#define QUICK 0
#define SLOW 1

int
read_data (data, filename)
char *filename;
esum *data;
    {
    extern struct inputs inp;
    extern int rscan, cscan, fscan, tscan, qscan;
    extern int rspace, cspace, fspace, tspace, qspace;
    extern int up_to_date, fsortstat[], csortstat[], rsortstat[],
	tsortstat[], qsortstat[];
    FILE *fp;
    struct stat statbuf;
    int i, nfilt, bad, nkbr, nkbc, nkbf, nkbt, nkbq, 
				year_ok, pret, frac, ck[12];
    int nadded[5], expand, type;
    char line[512];

    year_ok = TRUE;
    
    if(stat(filename,&statbuf) != 0) 
	{
	if(errno == ENOENT) msg("File '%s' does not exist",2,filename);
	else msg("Problem accessing file '%s'",2,filename);
	return(-1);
	}

    if((fp=fopen(filename,"r")) == NULL) 
	{
	msg("Problem opening '%s'",2,filename);
	return(-1);
	}

    active_filter();

    msg("Reading data from file '%s' ...",2,filename);

    for (i=0; i<5; i++) nadded[i] = 0;
    while(fgets(line,511,fp) != NULL) 
	{
	if (line[0] == '*') continue;		/* Ignore comment lines */

						/* What type of line is this? */
	if (isdigit (line[0])) sscanf (line, "%*d %*s %d", &type);
	else sscanf (line, "%*s %d", &type);
	type %= 50;				/* In case of old format */
	if ((type < 0) || (type > 4))		/* Better be 0, 1, 2, 3 or 4 */
	    {
	    msg ("Found line with incomprehensible format, stopping.",2);
	    break;
	    }
						/* call proper parser */
						/* Remember to clear elements of */
						/* aedit array structures */
	switch (type)
	    {
	    case 0:
		pret = parse_rsumm (line, &((data->rdata)[rscan].data));
		aeclr_root (&(data->rdata[rscan]));
		break;

	    case 1:
		pret = parse_csumm (line, &((data->cdata)[cscan].data));
		aeclr_corel (&(data->cdata[cscan]));
		break;

	    case 2:
		pret = parse_fsumm (line, &((data->fdata)[fscan].data));
		aeclr_fringe (&(data->fdata[fscan]));
		break;

	    case 3:
		pret = parse_tsumm (line, &((data->tdata)[tscan].data));
		aeclr_triangle (&(data->tdata[tscan]));
		break;

	    case 4:
/*		pret = parse_qsumm (line, &((data->qdata)[qscan].data));  */
		aeclr_quad (&(data->qdata[qscan]));
	    }

	bad = FALSE;
	if(pret >= 0) 			/* Line checks out this far */
	    {
	    if(pret == 1) year_ok = FALSE;	/* Some bad scan year data in here */

						/* Filter the data */
	    if (type == 0) bad |= rfilter(&((data->rdata)[rscan]),QUICK);
	    if (type == 1) bad |= cfilter(&((data->cdata)[cscan]),QUICK);
	    if (type == 2) bad |= ffilter(&((data->fdata)[fscan]),QUICK);
	    if (type == 3) bad |= tfilter(&((data->tdata)[tscan]),QUICK);
	    if (type == 4) bad |= qfilter(&((data->qdata)[qscan]),QUICK);

	    if(! bad) 			/* OK, this ends up in memory */
		{
		nadded[type]++;
		if (type == 0) rscan++;
		else if (type == 1) cscan++;
		else if (type == 2) fscan++;
					/* These do memory allocation internally */
		else if (type == 3) tarray_index (data);
		else if (type == 4) qarray_index (data);
		}
	    }

	expand = FALSE;		/* Expand memory as necessary */
	if(rscan == rspace)
	    {
	    expand = TRUE;
	    rspace += 100;
	    data->rdata = 
		    (rootarray *) realloc(data->rdata, rspace*sizeof(rootarray));
	    }
	if(cscan == cspace)
	    {
	    expand = TRUE;
	    cspace += 100;
	    data->cdata = 
		    (corelarray *) realloc(data->cdata, cspace*sizeof(corelarray));
	    }
	if(fscan == fspace)
	    {
	    expand = TRUE;
	    fspace += 100;
	    data->fdata = 
		    (fringearray *) realloc(data->fdata, fspace*sizeof(fringearray));
	    }

	if (expand)			/* Check for error and notify user */
	    {
	    if ((data->rdata == NULL) || (data->cdata == NULL)
			|| (data->fdata == NULL))
		{
		perror("realloc");
		msg("Fatal error allocating memory for data - Abort!",3);
		exit(1);
		}
	    nkbr = (rspace*sizeof(rootarray))/1024;
	    nkbc = (cspace*sizeof(corelarray))/1024;
	    nkbf = (fspace*sizeof(fringearray))/1024;
	    nkbt = (tspace*sizeof(trianglearray))/1024;
	    nkbq = (qspace*sizeof(quadarray))/1024;
	    printf ("Expanded array memory to %d Kb ...\r",
			nkbr + nkbc + nkbf + nkbt + nkbq); fflush (stdout);
	    }

	}				/* End of main read loop */

    if(nadded[0] + nadded[1] + nadded[2] + nadded[3] + nadded[4] > 0) 
	{
	up_to_date = FALSE;		/* For data summary use */
	nkbr = (rspace*sizeof(rootarray))/1024;
	nkbc = (cspace*sizeof(corelarray))/1024;
	nkbf = (fspace*sizeof(fringearray))/1024;
	nkbt = (tspace*sizeof(trianglearray))/1024;
	nkbq = (qspace*sizeof(quadarray))/1024;
	msg ("Total space now occupied by data = %d Kb", 2,
			nkbr + nkbc + nkbf + nkbt + nkbq);
					/* Throw away sorting explicitly */
	rsortstat[0] = csortstat[0] = fsortstat[0] = 
				tsortstat[0] = qsortstat[0] = 0;
	}

    if (nadded[0] > 0)		/* Report any new data */
	msg("Read %d  root records from %s, total now %d", 2,
		nadded[0], filename, rscan);
    if (nadded[1] > 0)
	msg("Read %d corel records from %s, total now %d", 2,
		nadded[1], filename, cscan);
    if (nadded[2] > 0)
	msg("Read %d fringe records from %s, total now %d", 2,
		nadded[2], filename, fscan);
    if (nadded[3] > 0)
	msg("Read %d closure triangle records from %s, total now %d", 2,
		nadded[3], filename, tscan);
    if (nadded[4] > 0)
	msg("Read %d closure quad records from %s, total now %d", 2,
		nadded[4], filename, qscan);

    if(! year_ok) 
	{
	msg("WARNING: file contained scans with missing scan year information",2);
	msg("Use 'SETYEAR' command on subset(s) of data to restore",2);
	}
    fclose(fp);
    return(0);
    }
