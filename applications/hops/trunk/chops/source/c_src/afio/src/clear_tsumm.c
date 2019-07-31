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
/* Created 1 August 1994 by CJL                                         */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "adata.h"
#include "mk4_afio.h"

void
clear_tsumm(trianglesum *dsumm)
    {
                                        /* default to latest version */
    dsumm->version = CURRENT_VERSION;
    dsumm->expt_no = 0;
    dsumm->time_tag = 0;
    dsumm->scan_offset = 0;
    dsumm->source[0] = '\0';
    dsumm->freq_code = '?';
    dsumm->mode = '?';
    dsumm->lags = 0;
    dsumm->triangle[0] = '\0';
    dsumm->root_id[0][0] = '\0';
    dsumm->root_id[1][0] = '\0';
    dsumm->root_id[2][0] = '\0';
    dsumm->extent_no[0] = 0;
    dsumm->extent_no[1] = 0;
    dsumm->extent_no[2] = 0;
    dsumm->length[0] = 0;
    dsumm->length[1] = 0;
    dsumm->length[2] = 0;
    dsumm->duration = 0;
    dsumm->offset = 0;
    dsumm->scan_quality = '?';
    dsumm->data_quality = '?';
    dsumm->esdesp = 0;
    dsumm->bis_amp = 0.0;
    dsumm->bis_snr = 0.0;
    dsumm->bis_phas = 0.0;
    strcpy (dsumm->datatype, "??");
    dsumm->csbdelay = 0.0;
    dsumm->cmbdelay = 0.0;
    dsumm->cdelay_rate = 0.0;
    dsumm->elevation[0] = 0.0;
    dsumm->elevation[1] = 0.0;
    dsumm->elevation[2] = 0.0;
    dsumm->azimuth[0] = 0.0;
    dsumm->azimuth[1] = 0.0;
    dsumm->azimuth[2] = 0.0;
    dsumm->epoch[0] = 0;
    dsumm->epoch[1] = 0;
    dsumm->ref_freq = 0.0;
    dsumm->cotime = 0;
    dsumm->scan_id[0] = '\0';
    }
