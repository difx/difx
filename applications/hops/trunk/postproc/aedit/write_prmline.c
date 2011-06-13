/************************************************************************/
/*									*/
/* Dumps minimal identification information plus all user-defined and	*/
/* extracted parameters out to the named stream, with simple formatting	*/
/*									*/
/*	Inputs:		fdata		One datum			*/
/*			ndigits		Requested precision of params	*/
/*			fp		output stream (already open)	*/
/*									*/
/*	Output:		Next line in file				*/
/*			return value	0=OK, !=0 failure		*/
/*									*/
/* Created 15 December 1993 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "usearray.h"

int
write_prmline (fdata, ndigits, fp)
fringearray *fdata;
int ndigits;
FILE *fp;
    {
    char filename[40], buf[500], vbuf[50], format[10];
    int i, syear, sday, shour, smin, ssec;
    double value;
    fringesum *data;
    extern struct usearray user_param;
					/* Convenience pointer */
    data = &(fdata->data);

    int_to_time (data->time_tag,&syear,&sday,&shour,&smin,&ssec);

					/* Just so user knows what data these */
					/* parameters refer to */
    if (data->version > 1)
	sprintf (filename, "%d/%03d-%02d%02d%02d/%2s.%c.%d.%s",
	    data->expt_no,
	    sday,shour,smin,ssec,
	    data->baseline,
	    data->freq_code,
	    data->extent_no,
	    data->root_id);
    else sprintf (filename, "%d/%03d-%02d%02d\?\?/%2s.%c.%d.%s",
	    data->expt_no,
	    sday,shour,smin,
	    data->baseline,
	    data->freq_code,
	    data->extent_no,
	    data->root_id);
    sprintf(buf,"%-33s %8s %2d:",
	filename,
	data->source,
	data->no_freq);

					/* Set up requested format */
    sprintf (format, " %%%d.%dg", ndigits+2, ndigits);
					/* Loop over all parameters, formatting */
					/* each f.p. value into a string and */
					/* appending to the line */
    for (i=0; i<user_param.nparms; i++)
	{
	value = user_param.parameter[i][fdata->param_ptr];
	sprintf (vbuf, format, value);
	strcat (buf, vbuf);
	}
    strcat (buf, "\n");

    if (fputs (buf,fp) == EOF)
        {
        msg ("Error writing to output file", 2);
        return (-1);
        }

    return(0);
    }
