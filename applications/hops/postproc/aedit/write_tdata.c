/************************************************************************/
/*									*/
/* This is the routine that writes the triangle data (possibly edited) 	*/
/* out to a file of the user's choice.					*/
/*									*/
/*	Inputs:		data		Pointer to main data structure	*/
/*			filename	Regular A-format output file	*/
/*									*/
/* Created August 18 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "mk4_afio.h"
#include "aedata.h"
#include "aedit.h"
#include "summary.h"

int write_tdata (esum *data, char *filename)
    {
    extern struct inputs inp;
    extern int tflag, tscan, output_version;
    extern int tsortstat[];
    extern struct datasumm tsumm;
    struct stat statbuf;
    int ret, i, index, nt, version, nver;
    FILE *fp;
					/* Sanity checks */
    if(tscan == 0)
	{
	msg ("No data!", 2);
	return (-1);
	}
					/* Figure out version for header */
    if (summ_data (data, VERSION) != 0)
	{
	msg ("Error getting version numbers of data", 2);
	return (-1);
	}
    nver = 0;
    for (i=0; i<=MAXVERSION; i++) if (tsumm.version[i] > 0) nver++;
    if (nver > 1) version = 0;
    else version = data->tdata[0].data.version;
    if (output_version != 0) version = output_version;
					/* Does the user know what he/she */
					/* is doing? */
    if(tflag == 0)
	if(! confirm("Data not edited yet.  Proceed?")) return(-1);
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

    ret = afile_header (version, 3, fp);

    nt = 0;
    for (i=0; i<tscan; i++)
	{
	if (tsortstat[0] > 0) index = data->tdata[i].order;
	else index = i;
	if (data->tdata[index].flag != 0) continue;
					/* Version override if needed */
	version = data->tdata[index].data.version;
	if (output_version > 0) data->tdata[index].data.version = output_version;
	ret = write_tsumm (&(data->tdata[index].data), fp);
	data->tdata[index].data.version = version;
	if (ret != 0)
	    {
	    msg ("Warning: wrote only %d triangle records", 2, nt);
	    ret = -1;
	    break;
	    }
	else nt++;
	}
    if ((ret != -1) && (nt > 0))
	msg ("Wrote %d triangle records successfully", 2, nt);

    fclose(fp);
    return(ret);
    }
