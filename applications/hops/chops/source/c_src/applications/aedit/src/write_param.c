/************************************************************************/
/*									*/
/* This is the routine that writes the data (possibly edited) out to a	*/
/* file of the user's choice.						*/
/*									*/
/*	Inputs:		data		Pointer to main data structure	*/
/*			filename	Regular A-format output file	*/
/*									*/
/* Created April 5 1989 by CJL						*/
/* Modified April 23 1991 by CJL -- fix bug for snr >= 1000		*/
/* Modified March 11 1992 by CJL -- permit writing of types 0, 1 and 2	*/
/*				    relegated actual writing to three	*/
/*				    specialized subroutines		*/
/* Modified February 1 1993 by CJL -- use AFIO library			*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>
#include "aedata.h"
#include "usearray.h"
#include "aedit.h"

int write_param (esum *data, char *filename, char *precision)
    {
    struct stat statbuf;
    char param_buf[500], buf[50];
    int ret, i, index, np, last_id, last_index, ndigits;
    FILE *fp;
    time_t now;
    fringearray *fdata;
    struct udat *type;
    extern int fflag, fscan;
    extern int fsortstat[];
    extern struct usearray user_param;
					/* Sanity checks */
    if(fscan == 0)
	{
	msg ("No data!", 2);
	return (-1);
	}
					/* Is there a precision argument? */
    ndigits = 6;
    if (strlen (precision) > 0)
	{
	if (sscanf (precision, "%d%s", &ndigits, buf) != 1)
	    {
	    msg ("Invalid precision argument '%s', ignoring", 2, precision);
	    ndigits = 6;
	    }
	else if ((ndigits < 1) || (ndigits > 15))
	    {
	    msg ("Precision '%s' must be in range 1-15, ignoring", 2, precision);
	    ndigits = 6;
	    }
	}
					/* Check out requested filename */
    if(stat(filename,&statbuf) == 0) 
	{
	if(statbuf.st_size != 0)
	    if(! confirm("File exists and contains data.  Proceed?")) return(-1);
	}
    else if(errno != ENOENT) 
	{
	msg("problem %d with output file %s",2,errno,filename);
	return(-1);
	}
					/* Open file and insert header info */
    if((fp = fopen(filename,"w")) == NULL) 
	{
	msg("Could not open file for output",2);
	return(-1);
	}

    now = time (NULL);
    fprintf(fp,"* This file processed by AEDIT, %s", ctime (&now));
    fprintf(fp,"* First part of each line is UNIX FILENAME, SOURCE, #FREQS\n");
    fprintf(fp,"* Parameters listed are:");
					/* Construct list, condense arrays */
    last_id = -1;
    last_index = 0;
    param_buf[0] = '\0';
    for (i=0; i<user_param.nparms; i++)
	{
	type = user_param.type + i;
					/* This takes care of arrays */
	if (type->parameter_id == last_id)
	    {
	    last_index = type->parameter_index;
	    continue;
	    }
	if (last_index > 0) 
	    {
	    sprintf (buf, "(1-%d)", last_index);
	    strcat (param_buf, buf);
	    last_index = 0;
	    }
	sprintf (buf, ", %s", type->parameter_name);
	strcat (param_buf, buf);
	last_id = type->parameter_id;
	}
    if (last_index > 0) 
	{
	sprintf (buf, "(1-%d)", last_index);
	strcat (param_buf, buf);
	}
    param_buf[0] = ' ';	/* Remove comma */
    fprintf (fp, "%s\n", param_buf);
					/* Write out unflagged records */
    ret = 0;
    np = 0;
    for (i=0; i<fscan; i++)
	{
	if (fsortstat[0] > 0) index = data->fdata[i].order;
	else index = i;
	fdata = data->fdata + index;
	if (fdata->flag != 0) continue;
	if (fdata->param_ptr < 0) continue;

	if (write_prmline (fdata, ndigits, fp) != 0)
	    {
	    msg ("Warning: wrote only %d parameter records", 2, np);
	    ret = -1;
	    break;
	    }
	else np++;
	}
    if ((ret != -1) && (np > 0))
	msg ("Wrote %d user parameter records successfully", 2, np);

    fclose(fp);
    return(ret);
    }
