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
/* Modified to support version 2 A-file format CJL Jan 25 1994          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "adata.h"
#include "mk4_afio.h"

void
clear_fsumm(fringesum *dsumm)
    {
                                        /* default to latest version */
    dsumm->version = CURRENT_VERSION;
    strcpy (dsumm->fname, "00000");
    dsumm->expt_no = 0;
    dsumm->extent_no = 0;
    dsumm->duration = 0;
    dsumm->length = 0;
    dsumm->offset = 0;
    dsumm->corel_vers = '?';
    dsumm->procdate = 0;
    dsumm->time_tag = 0;
    dsumm->scan_offset = 0;
    dsumm->ssec = -1;
    dsumm->source[0] = '\0';
    memset(dsumm->baseline, 0, sizeof(dsumm->baseline));
    dsumm->quality = '?';
    dsumm->freq_code = '?';
    dsumm->mode = '?';
    dsumm->no_freq = 0;
    strcpy (dsumm->reftape, "????????");
    strcpy (dsumm->remtape, "????????");
    dsumm->archiv = 0;
    dsumm->lags = 0;
    dsumm->amp = 0.0;
    dsumm->snr = 0.0;
    dsumm->resid_phas = 0.0;
    dsumm->phase_snr = 0.0;
    dsumm->sbdelay = 0.0;
    dsumm->mbdelay = 0.0;
    dsumm->delay_rate = 0.0;
    dsumm->esdesp = 0;
    dsumm->epoch[0] = 0;
    dsumm->epoch[1] = 0;
    dsumm->total_phas = 0.0;
    dsumm->total_rate = 0.0;
    dsumm->total_mbdelay = 0.0;
    dsumm->total_sbresid = 0.0;
    dsumm->ambiguity = 0.0;
    dsumm->pcals[0] = 0;
    dsumm->pcals[1] = 0;
    dsumm->pcals[2] = 0;
    dsumm->pcals[3] = 0;
    dsumm->root_id[0] = '\0';
                                        /* New for version 2 and higher A-files */
    dsumm->ref_freq = 0.0;
    strcpy (dsumm->datatype, "??");
    dsumm->ref_elev = 0.0;
    dsumm->rem_elev = 0.0;
    dsumm->ref_az = 0.0;
    dsumm->rem_az = 0.0;
    dsumm->u = 0.0;
    dsumm->v = 0.0;
    dsumm->srch_cotime = -1;
    dsumm->noloss_cotime = -1;
    dsumm->parents[0] = 0;
    dsumm->parents[1] = 0;
    dsumm->parents[2] = 0;
    dsumm->parents[3] = 0;
    dsumm->scan_id[0] = '\0';
    strcpy (dsumm->polarization, "??");
    dsumm->errcode = '?';
    }
