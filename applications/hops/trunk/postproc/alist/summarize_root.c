/************************************************************************/
/*									*/
/* This routine takes a filled-in root data structure, and uses the	*/
/* information therein to fill in an empty rootsum structure		*/
/*									*/
/*	Inputs:		root		data_root structure (full)	*/
/*									*/
/*	Output:		rsumm		rootsum structure		*/
/*									*/
/* Created 25 September 1992 by CJL					*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "data.h"
#include "adata.h"

int
summarize_root (root, rsumm)
struct data_root *root;
rootsum *rsumm;
    {
    char stations[40]; 
    char *fname;
    int i, j, len, size, nstat, year, day, hour, min, sec;
    struct type_1000 *r;
    struct stat file_status;
    extern int output_version;

    clear_rsumm (rsumm);
    rsumm->version = output_version;

    fname = root->filename;
    r = root->t1000;			/* This just simplifies appearance */
    strcpy (rsumm->fname, "unix ");
    rsumm->expt_no = r->expt_no;
    rsumm->extent_no = 0;
					/* Get file size -- nasty, but gotta do it */
    if (stat (fname, &file_status) != 0)
	{
	msg ("What's this?  Can't stat %s in summarize_root!", 2, fname);
	return (1);
	}
    size = file_status.st_size;
    if ((size % 256) != 0) msg ("Warning, %s not multiple of 256 bytes", 2, fname);
    rsumm->size = size/256;

    rsumm->corel_vers = 'a' - 1 + (r->version_no & 0177);
    len = strlen (fname);		/* Strip off root id from filename */
    strcpy (rsumm->root_id, fname + len - 6);

    rsumm->archiv = r->expt_no;		/* In accordance with unix archive */
					/* strategy.  Can distinguish this number */
					/* from HP1000 archive number by filename */
					/* field (rsumm->fname) */
    strncpy (rsumm->source, r->source, 8);
    rsumm->source[8] = '\0';
					/* Times must be converted to seconds since */
					/* Jan 1 1980 for the afio library */
    year = r->procdate_yyddd / 1000 + 70;
    day = r->procdate_yyddd % 1000;
					/* Seconds/4 is encoded in bits 12-15 */
    hour = (r->procdate_hhmm & 07777) / 100;
    min = (r->procdate_hhmm & 07777) % 100;
    sec = (r->procdate_hhmm >> 12) * 4;
    rsumm->procdate = time_to_int (year, day, hour, min, sec);

    rsumm->time_tag = time_to_int (r->utc_start.year,
				   r->utc_start.day_of_year,
				   r->utc_start.hour,
				   r->utc_start.minute, 
				   r->utc_start.second);
    rsumm->ssec = r->utc_start.second;
					/* Now must loop through type-1000 */
					/* records to construct station list */
    nstat = 0;
    stations[nstat] = '\0';
    for (i=0; i<3; i++)
	{
	if (nstat > 20) break;
	for (j=0; j<8; j++)
	    {
					/* If this baseline contained in root, */
					/* add its stations to the list */
	    if (r[i].barray[j].t_2000_no < 0) continue;
	    if (strchr (stations, r[i].barray[j].baseline_id[0]) == NULL)
		{
		stations[nstat++] = r[i].barray[j].baseline_id[0];
		stations[nstat] = '\0';
		}
	    if (strchr (stations, r[i].barray[j].baseline_id[1]) == NULL)
		{
		stations[nstat++] = r[i].barray[j].baseline_id[1];
		stations[nstat] = '\0';
		}
	    }
	}				/* This should never happen */
    if (strlen(stations) >= 20)
	{
	msg ("Error, more than 20 stations in a root file!", 2);
	return (1);
	}
    else strcpy (rsumm->stations, stations);

    return (0);
    }
