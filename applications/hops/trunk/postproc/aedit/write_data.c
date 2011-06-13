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
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "aedata.h"
#include "aedit.h"
#include "summary.h"
#include "sort.h"

int
write_data (data, filename)
esum *data;
char *filename;
    {
    extern struct inputs inp;
    extern int fflag, cflag, rflag, fscan, cscan, rscan, output_version;
    extern int data_version;
    extern int fsortstat[], csortstat[], rsortstat[];
    struct stat statbuf;
    int ret, i, index, nf, nc, nr, version;
    FILE *fp;
					/* Sanity checks */
    if((fscan + cscan + rscan) == 0)
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
    version = data_version;
    if (output_version != 0) version = output_version;
					/* Does the user know what he/she */
					/* is doing? */
    if((fflag + cflag + rflag) == 0)
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
					/* Write a type-2 header */
    if (afile_header (version, 2, fp) != 0)
	msg ("Error writing header to '%s'", 2, filename);

					/* Family sort is a special case */
    if ((fsortstat[fsortstat[0]] == S_FAMILY) && 
		(csortstat[csortstat[0]] == S_FAMILY) &&
		(rsortstat[rsortstat[0]] == S_FAMILY))
	ret = write_families (data, fp);
					/* Normal mode, do each type as a block */
					/* Write out fringe records first */
    else
	{
	ret = 0;
	nf = nc = nr = 0;
	for (i=0; i<fscan; i++)
	    {
	    if (fsortstat[0] > 0) index = data->fdata[i].order;
	    else index = i;
	    if (data->fdata[index].flag != 0) continue;
					/* Version override if needed */
	    version = data->fdata[index].data.version;
	    if (output_version > 0) data->fdata[index].data.version = output_version;
	    ret = write_fsumm (&(data->fdata[index].data), fp);
	    data->fdata[index].data.version = version;
	    if (ret != 0)
		{
		msg ("Warning: wrote only %d fringe records", 2, nf);
		ret = -1;
		break;
		}
	    else nf++;
	    }
	if ((ret != -1) && (nf > 0))
	    msg ("Wrote %d fringe records successfully", 2, nf);
					/* Now the corel records */
	for (i=0; i<cscan; i++)
            {
	    if (csortstat[0] > 0) index = data->cdata[i].order;
	    else index = i;
            if (data->cdata[index].flag != 0) continue;
					/* Version override if needed */
	    version = data->cdata[index].data.version;
	    if (output_version > 0) data->cdata[index].data.version = output_version;
            ret = write_csumm (&(data->cdata[index].data), fp);
	    data->cdata[index].data.version = version;
	    if (ret != 0)
        	{
        	msg ("Warning: wrote only %d corel records", 2, nc);
        	ret = -2;
        	break;
        	}
            else nc++;
            }
	if ((ret != -2) && (nc > 0)) 
	    msg ("Wrote %d corel records successfully", 2, nc);
					/* Finally, the root records */
	for (i=0; i<rscan; i++)
            {
	    if (rsortstat[0] > 0) index = data->rdata[i].order;
	    else index = i;
            if (data->rdata[index].flag != 0) continue;
					/* Version override if needed */
	    version = data->rdata[index].data.version;
	    if (output_version > 0) data->rdata[index].data.version = output_version;
            ret = write_rsumm (&(data->rdata[index].data), fp);
	    data->rdata[index].data.version = version;
	    if (ret != 0)
        	{
        	msg ("Warning: wrote only %d root records", 2, nr);
        	ret = -3;
        	break;
        	}
            else nr++;
            }
	if ((ret != -3) && (nr > 0))
	    msg ("Wrote %d root records successfully", 2, nr);
	}

    fclose(fp);
    return(ret);
    }
