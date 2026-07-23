/************************************************************************/
/*                                                                      */
/* This is the routine that writes the triangle data out to the stream  */
/* passed as fp.  Each call writes 1 line.                              */
/*                                                                      */
/*      Inputs:         data            Filled in trianglesum structure */
/*                      fp              Regular A-format output file    */
/*                                                                      */
/*  Created 1 August 1994 by CJL                                        */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include "adata.h"
#include "mk4_afio.h"
#include "mk4_util.h"

char *tformat_v2 = "%1d %4d 3 %2d %03d-%02d%02d%02d %-8s %c%c %s %-20s %-11s %-14s\
 %c %c %06d %9.2f %7.2f %5.1f %6.3f %8.5f %6.4f %7.3f %-14s %-11s %02d%02d %8.2f\n";

char *tformat_v3 = "%1d %4d 3 %2d %03d-%02d%02d%02d %-8s %c%c %s %-20s %-11s %-14s\
 %3d %3d %c %c %06d %9.2f %7.2f %5.1f %2s %6.3f %8.5f %6.4f %7.3f %-14s %-11s\
 %02d%02d %8.2f\n";

char *tformat_v4 = "%1d %4d 3 %2d %03d-%02d%02d%02d %3d %-8s %c%c %4d %s %-20s %-11s %-14s\
 %3d %3d %c %c %06d %9.2f %7.2f %5.1f %2s %6.3f %8.5f %6.4f %7.3f %-14s %-11s\
 %02d%02d %8.2f %3d\n";

char *tformat_v5 = "%1d %4d 3 %s %4d %03d-%02d%02d%02d %3d %-8s %c%c %4d %s %-20s %-11s %-14s\
 %3d %3d %c %c %06d %9.2f %7.2f %5.1f %2s %6.3f %8.5f %6.4f %7.3f %-14s %-11s\
 %02d%02d %8.2f %3d\n";

char *tformat_v6 = "%1d %4d 3 %8s %4d %03d-%02d%02d%02d %3d %32s\
 %c%c %4d %3s %20s %11s %14s\
 %3d %3d  %c  %c %06d\
 %10.3f %8.3f %+8.3f %2s %7.4f\
 %8.5f %6.4f %9.5f %14s %11s\
  %02d%02d %10.3f %7d\n";

int
write_tsumm(trianglesum *data, FILE *fp)
    {
    static char buf[512], roots[21], extents[30], lengths[30];
    static char elevations[30], azimuths[30];
    int syear, sday, shour, smin, ssec;

                                        /* Decode time tag */
    int_to_time (data->time_tag, &syear, &sday, &shour, &smin, &ssec);
    if (syear >= 100) syear -= 100;

                                        /* Construct various strings */
    strcpy (roots, data->root_id[0]);
    strcat (roots, ",");
    if (strcmp (data->root_id[0], data->root_id[1]) != 0) 
        strcat (roots, data->root_id[1]);
    strcat (roots, ",");
    if (strcmp (data->root_id[1], data->root_id[2]) != 0) 
        strcat (roots, data->root_id[2]);
    sprintf (extents, "%d,%d,%d", data->extent_no[0], data->extent_no[1],
                                data->extent_no[2]);
    sprintf (lengths, "%d,%d,%d", data->length[0], data->length[1],
                                data->length[2]);
    sprintf (elevations, "%.1f,%.1f,%.1f", data->elevation[0], 
                        data->elevation[1], data->elevation[2]);
    sprintf (azimuths, "%d,%d,%d", (int)data->azimuth[0], 
                        (int)data->azimuth[1], (int)data->azimuth[2]);

    switch (data->version)
        {
                                        /* Version 2 */
        case 2:
                                        /* Construct complete line */
            sprintf (buf, tformat_v2,
                data->version,
                data->expt_no,
                syear, sday, shour, smin, ssec,
                data->source,
                data->freq_code,
                data->mode,
                data->triangle,
                roots,
                extents,
                lengths,
                data->scan_quality,
                data->data_quality,
                data->esdesp,
                data->bis_amp,
                data->bis_snr,
                data->bis_phas,
                data->csbdelay,
                data->cmbdelay,
                data->ambiguity,
                data->cdelay_rate,
                elevations,
                azimuths,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq);
            break;
                                        /* Version 3 */
        case 3:
                                        /* Construct complete line */
            sprintf (buf, tformat_v3,
                data->version,
                data->expt_no,
                syear, sday, shour, smin, ssec,
                data->source,
                data->freq_code,
                data->mode,
                data->triangle,
                roots,
                extents,
                lengths,
                data->duration,
                data->offset,
                data->scan_quality,
                data->data_quality,
                data->esdesp,
                data->bis_amp,
                data->bis_snr,
                data->bis_phas,
                data->datatype,
                data->csbdelay,
                data->cmbdelay,
                data->ambiguity,
                data->cdelay_rate,
                elevations,
                azimuths,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq);
            break;

                                        /* Version 4 */
        case 4:
                                        /* Construct complete line */
            sprintf (buf, tformat_v4,
                data->version,
                data->expt_no,
                syear, sday, shour, smin, ssec,
                data->scan_offset,
                data->source,
                data->freq_code,
                data->mode,
                data->lags,
                data->triangle,
                roots,
                extents,
                lengths,
                data->duration,
                data->offset,
                data->scan_quality,
                data->data_quality,
                data->esdesp,
                data->bis_amp,
                data->bis_snr,
                data->bis_phas,
                data->datatype,
                data->csbdelay,
                data->cmbdelay,
                data->ambiguity,
                data->cdelay_rate,
                elevations,
                azimuths,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->cotime);
            break;

                                        /* Version 5 */
        case 5:
                                        /* Construct complete line */
            sprintf (buf, tformat_v5,
                data->version,
                data->expt_no,
                data->scan_id,
                syear, sday, shour, smin, ssec,
                data->scan_offset,
                data->source,
                data->freq_code,
                data->mode,
                data->lags,
                data->triangle,
                roots,
                extents,
                lengths,
                data->duration,
                data->offset,
                data->scan_quality,
                data->data_quality,
                data->esdesp,
                data->bis_amp,
                data->bis_snr,
                data->bis_phas,
                data->datatype,
                data->csbdelay,
                data->cmbdelay,
                data->ambiguity,
                data->cdelay_rate,
                elevations,
                azimuths,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->cotime);
            break;

                                        /* Version 6 */
        case 6:
                                        /* Construct complete line */
            if (syear < 50) syear += 2000;  /* y2k */
            sprintf (buf, tformat_v6,
                data->version,
                data->expt_no,
                data->scan_id,
                syear, sday, shour, smin, ssec,
                data->scan_offset,
                data->source,
                data->freq_code,
                data->mode,
                data->lags,
                data->triangle,
                roots,
                extents,
                lengths,
                data->duration,
                data->offset,
                data->scan_quality,
                data->data_quality,
                data->esdesp,
                data->bis_amp,
                data->bis_snr,
                data->bis_phas,
                data->datatype,
                data->csbdelay,
                data->cmbdelay,
                data->ambiguity,
                data->cdelay_rate,
                elevations,
                azimuths,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->cotime);
            break;

        default:
            msg ("Unsupported version number in write_tsumm() (%d)",
                2, data->version);
            return (-1);
        }
                                        /* Write it out */
    if (fputs (buf, fp) == EOF) 
        {
        msg("Error writing to output file from write_tsumm()",2);
        return(-1);
        }

    return(0);
    }
