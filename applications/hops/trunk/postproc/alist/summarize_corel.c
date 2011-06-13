/************************************************************************/
/*									*/
/* This routine takes a corel file name and its corresponding filled	*/
/* root structure, and fills in the corelsum structure for output in	*/
/* A-file format.  We need to avoid opening up the corel data files	*/
/* themselves, as they are huge ... all the necessary information is	*/
/* contained in the baseline-specific parts of the root.		*/
/*									*/
/*	Inputs:		fname		Name of input corel file	*/
/*			root		data_root structure (full)	*/
/*									*/
/*	Output:		csumm		corelsum structure		*/
/*									*/
/* Created 1 October 1992 by CJL					*/
/*									*/
/************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include "data.h"
#include "adata.h"

int
summarize_corel (fname, root, csumm)
char *fname;
struct data_root *root;
corelsum *csumm;
    {
    char *rootname;
    char c, baseline[3];
    int i, j, len, size, nstat, year, day, hour, min, lastslash, err;
    int basenum;
    struct type_1000 *r;
    struct binfo *blinf;
    struct root_baseline *rbase;
    struct crossref *xr;
    struct stat file_status;
    extern int output_version;

    clear_csumm (csumm);
    csumm->version = output_version;
					/* Use the input filename to figure out */
					/* the baseline, extent number, and */
					/* root id code.  We do some cross checking */
					/* later to guard against screwups */
    i = 0;
    lastslash = 0;
    while ((c = fname[i++]) != NULL)
        if (c == '/') lastslash = i;
    if (sscanf (fname+lastslash, "%2s.%hd.%6s", baseline, 
				&(csumm->extent_no), csumm->root_id) != 3)
	{
	msg ("Could not decode filename %s", 2, fname+lastslash);
	return (1);
	}
    else strncpy (csumm->baseline, baseline, 2);

    rootname = root->filename;
    r = root->t1000;			/* This just simplifies appearance */
    strcpy (csumm->fname, "unix ");
    csumm->expt_no = r->expt_no;
					/* Get file size -- nasty, but gotta do it */
    if (stat (fname, &file_status) != 0)
	{
	msg ("What's this?  Can't stat %s in summarize_corel!", 2, fname);
	return (1);
	}
    size = file_status.st_size;
    if ((size % 256) != 0) msg ("Warning, %s not multiple of 256 bytes", 2, fname);
    csumm->size = size/256;
					/* Bits 8-15 contain IEEE/HP info */
    csumm->corel_vers = 'a' - 1 + (r->version_no & 0177);

    csumm->archiv = r->expt_no;		/* In accordance with unix archive */
					/* strategy.  Can distinguish this number */
					/* from HP1000 archive number by filename */
					/* field (rsumm->fname) */
    strncpy (csumm->source, r->source, 8);
    csumm->source[8] = '\0';
					/* Times must be converted to seconds since */
					/* Jan 1 1980 for the afio library */
    csumm->time_tag = time_to_int (r->utc_start.year,
				   r->utc_start.day_of_year,
				   r->utc_start.hour,
				   r->utc_start.minute, 
				   r->utc_start.second);
    csumm->ssec = r->utc_start.second;
    csumm->startsec = r->utc_start.second;
					/* Frequency codes in last 2 chars of this */
    strncpy (csumm->freqs, r->name_freq + 6, 2);
    csumm->freqs[2] = '\0';
					/* Now we must locate this baseline in the */
					/* root, to extract more detailed info */
					/* Take the opportunity to check that the */
					/* filename (which we used above) tallies */
					/* with the information in the root */
    for (i=0; i<10; i++)
	{
	for (j=0; j<8; j++)
	    {
	    blinf = r[i].barray + j;
	    if (strncmp (blinf->baseline_id, csumm->baseline, 2) == 0)
		{
		basenum = 8 * i + j + 1;
		if (blinf->bduration > 0) csumm->sduration = blinf->bduration;
		else csumm->sduration = r->duration;
		i = 10;
		break;
		}
	    }
	}
					/* Check that baseline id matches */
    rbase = root->base + basenum;
    err = FALSE;
    if (rbase->t2000[0].baseline_no != basenum) err = TRUE;
    else if (strncmp (csumm->baseline, rbase->t2000[0].baseline_id, 2) != 0) err = TRUE;
					/* Should open up target file and cross */
					/* check index numbers etc. here, but we */
					/* are lazy! */
    if (err)
	{
	msg ("Inconsistency in root for file %s!", 2, fname);
	return (1);
	}
					/* Is this right?  Are they the right */
					/* quantities?  Are the signs right? */
					/* Who knows ... the HP1000 equivalent is */
					/* (a) buried in several obscure programs, */
					/* and (b) undocumented in any standard place */
    csumm->refclock_err = rbase->t2200.ref_tsync;
    csumm->clock_diff = rbase->t2200.ref_tsync - rbase->t2200.rem_tsync;

					/* Find a type 2000 record and a cross */
					/* reference table entry that corresponds */
					/* to the extent number ... necessary */
					/* because there can be multiple extents */
					/* for a single baseline # in a single root */
    err = TRUE;
    for (i=0; i<MAX_T2000; i++)
	{
	for (j=0; j<6; j++)
	    {
	    xr = rbase->t2000[i].xref + j;
	    if (csumm->extent_no == xr->extent_no)
		{
		err = FALSE;
		i = MAX_T2000;
		break;
		}
	    }
	}
					/* Not found!!! */
    if (err)
	{
	msg ("Could find no reference to extent %d in root %s!", 2, 
					csumm->extent_no, rootname);
	return (1);
	}
					/* Processing time for this extent */
    year = xr->procdate_yyddd / 1000 + 70;
    day = xr->procdate_yyddd % 1000;
    hour = xr->procdate_hhmm / 100;
    min = xr->procdate_hhmm % 100;
    csumm->procdate = time_to_int (year, day, hour, min, 0);

    csumm->refdrive = xr->ref_tdrive;
    csumm->remdrive = xr->rem_tdrive;
    csumm->quality = xr->corel_code[0];
    if (csumm->quality == '\0') csumm->quality = ' ';
    csumm->status = xr->post_mortem;

/**************************************************************************/
/*									  */
/* Still missing:  corstart, corstop and eqts.  They are generated on the */
/* HP1000 system by the online system, placed in the processing log file, */
/* and merged into the A-file by VMERG.  These fields will be ignored	  */
/* under UNIX, and replaced with zeros here.				  */
/*									  */
/**************************************************************************/

    csumm->corstart = 0;
    csumm->corstop = 0;
    csumm->eqts = 0;

    return (0);
    }
