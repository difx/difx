/************************************************************************/
/*                                                                      */
/* This routine empties the structure whose pointer is passed           */
/* to it.  For use when reading in data, to avoid confusing leftover    */
/* data.  The reading routines do not always overwrite all fields in    */
/* the structure.                                                       */
/*                                                                      */
/*      Inputs:         dsumm           structure pointer               */
/*                                                                      */
/*      Output:         *dsumm          modified                        */
/*                                                                      */
/* Created 31 March 1989 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "adata.h"
#include "mk4_afio.h"

void
clear_csumm(corelsum *dsumm)
    {
    dsumm->version = CURRENT_VERSION;
    strcpy (dsumm->fname, "00000");
    dsumm->expt_no = 0;
    dsumm->extent_no = 0;
    dsumm->size = 0;
    dsumm->corel_vers = ' ';
    dsumm->procdate = 0;
    dsumm->time_tag = 0;
    dsumm->ssec = -1;
    dsumm->source[0] = '\0';
    dsumm->baseline[0] = '\0';
    dsumm->quality = ' ';
    dsumm->startsec = 0;
    dsumm->sduration = 0;
    dsumm->lags = 0;
    dsumm->corstart = 0;
    dsumm->corstop = 0;
    dsumm->refdrive = 0;
    dsumm->remdrive = 0;
    dsumm->eqts = 0;
    dsumm->freqs[0] = '\0';
    dsumm->refclock_err = 0.0;
    dsumm->clock_diff = 0.0;
    dsumm->root_id[0] = '\0';
    dsumm->status = 0;
    dsumm->archiv = 0;
    dsumm->scan_id[0] = '\0';
    }
