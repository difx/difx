/************************************************************************/
/*                                                                      */
/* This is the routine that writes the type-2 data out to the stream    */
/* passed as fp.  Each call writes 1 line.                              */
/* Differences in the                                                   */
/* way C and Fortran format numbers necessitate some fiddling with a    */
/* couple of characters in the output of sprintf.  Essentially, C will  */
/* expand a field width to accomodate the number, where Fortran will    */
/* not (it prints asterisks for out of range values).  Combine this     */
/* with the fact that C insists on a 0 before the decimal point for     */
/* floating point numbers less than 1, and the Fortran format F4.3      */
/* cannot be easily mimicked.  Affected are sbresid and ambiguity       */
/*                                                                      */
/*      Inputs:         data            Filled in fringesum structure   */
/*                      fp              Regular A-format output file    */
/*                                                                      */
/* Created as write_data() April 5 1989 by CJL                          */
/* Modified April 23 1991 by CJL -- fix bug for snr >= 1000             */
/* Modified March 11 1991 by CJL -- Separate type 0, 1, 2 output        */
/* Separated into afio library from aedit by CJL, 23 September 1992     */
/* This involved alteration to write only one line at a time.           */
/* Adjusted multiband delay format to remove leading zero and give      */
/* extra precision, CJL 9 September 1993                                */
/* Added support for version 2 CJL 26 January 1994 "no more columns!"   */
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

                                        /* Lots of stupid fiddling around */
                                        /* to cope with amplitude and SNR */
                                        /* overflows */
char *fformat2a = "<%-5s %1d %2d %4d %4d %2d%03dd%02d%02d %2d %03d-%02d%02d\
 %-8s %s%c %s %s %c%c%02d %6.2f %3d %5.1f%6.2f %s %s ";
char *fformat2b = "<%-5s %1d %2d %4d %4d %2d%03dd%02d%02d %2d %03d-%02d%02d\
 %-8s %s%c %s %s %c%c%02d %6.1f %3d %5.1f%6.2f %s %s ";
char *fformat2c = "<%-5s %1d %2d %4d %4d %2d%03dd%02d%02d %2d %03d-%02d%02d\
 %-8s %s%c %s %s %c%c%02d %5.1f %4d %5.1f%6.2f %s %s ";
char *fformat2d = "<%-5s %1d %2d %4d %4d %2d%03dd%02d%02d %2d %03d-%02d%02d\
 %-8s %s%c %s %s %c%c%02d %5.0f %4d %5.1f%6.2f %s %s ";
char *fformat2e = "%06d A%s %02d%02d %5.1f \
%11.8f %13.6f %5.3f%5.3f %3d %3d %3d %3d %3d %s\n";

char *fformat_v2 = "%1d %s 2 %2d %3d %4d %2d%03d%c%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s%c %c%c%02d %6.2f %#5.4g %5.1f %2s %6.3f %8.5f %6.4f %7.2f %4.1f %4.1f %5.1f %5.1f\
 %7.4g %7.4g %06d %02d%02d %8.2f %5.1f %11.8f %13.6f %5.3f %s\n";

char *fformat_v3 = "%1d %s 2 %2d %3d %3d %3d %4d %2d%03d%c%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s%c %c%c%02d %6.2f %#5.4g %5.1f %2s %6.3f %8.5f %6.4f %7.2f %4.1f %4.1f %5.1f %5.1f\
 %7.4g %7.4g %06d %02d%02d %8.2f %5.1f %11.8f %13.6f %5.3f %s\n";

char *fformat_v4 = "%1d %s 2 %2d %3d %3d %3d %4d %2d%03d%c%02d%02d%02d %2d\
 %03d-%02d%02d%02d %3d %-8s %s%c %c%c%02d %4d %6.2f %#5.4g %5.1f %#5.4g %2s %6.3f %8.5f\
 %6.4f %8.3f %4.1f %4.1f %5.1f %5.1f %7.4g %7.4g %06d %02d%02d %8.2f %5.1f %11.8f\
 %13.6f %5.3f %3d %3d %s\n";

char *fformat_v5 = "%1d %s 2 %2d %3d %3d %3d %4d %s %02d%03d-%02d%02d%02d %4d\
 %03d-%02d%02d%02d %3d %-8s %s %c%c %c%02d %2s %4d %6.2f %#5.4g %5.1f %#5.4g %2s %6.3f %8.5f\
 %6.4f %8.3f %4.1f %4.1f %5.1f %5.1f %7.4g %7.4g %06d %02d%02d %8.2f %5.1f %11.8f\
 %13.6f %5.3f %3d %3d\n";

int
write_fsumm(fringesum *data, FILE *fp)
    {
    char parents[20], buf[512], buf2[512], format[120], mbdstring[9], *mbdptr;
    char ratestring[10], archiv_str[10], c;
    int i, pyear, pday, phour, pmin, psec, syear, sday, shour, smin, ssec;
    int snr;


    parents[0] = '0';
    parents[1] = '\0';
    int_to_time(data->procdate,&pyear,&pday,&phour,&pmin,&psec);
    if (pyear >= 100) pyear -= 100;
    int_to_time(data->time_tag,&syear,&sday,&shour,&smin,&ssec);
    if (syear >= 100) syear -= 100;

    switch (data->version)
        {
                                        /* Version 1, HP-1000 and unix pre-'94 */
                                        /* Must retain column numbers for HP1000 */
                                        /* software */
        case 1:
                                        /* data->snr is now floating point */
            snr = (int)(data->snr + 0.5);
                                        /* SNR overflow */
            if (snr < 1000) 
                {
                strcpy (format, fformat2a);
                if (data->amp > 999.999) strcpy (format, fformat2b);
                }
            else 
                {
                strcpy (format, fformat2c);
                if (data->amp > 999.999) strcpy (format, fformat2d);
                }
                                        /* Rate overflow */
            sprintf (ratestring, "%6.2f", data->delay_rate);
            ratestring[6] = '\0';
                                        /* Adjust mbdelay format to get to */
                                        /* 10 picosec resolution if <= 1 microsec */
            sprintf (mbdstring, "%8.5f", data->mbdelay);
            if (mbdstring[1] == '0')
                {
                mbdstring[1] = mbdstring[0];
                mbdptr = mbdstring + 1;
                }
            else
                {
                mbdstring[7] = '\0';
                mbdptr = mbdstring;
                }
                                        /* A-file number, must support ARW's new */
                                        /* scheme for >9999 at Bonn */
            if (data->archiv > 9999)
                {
                c = (data->archiv / 1000) - 10 + 'A';
                sprintf (archiv_str, "%c%03d", c, data->archiv % 1000);
                }
            else sprintf (archiv_str, "%04d", data->archiv);

            sprintf(buf,format,
                data->fname,
                2,
                data->extent_no,
                data->length,
                data->expt_no,
                pyear,pday,phour,pmin,
                syear,sday,shour,smin,
                data->source,
                data->baseline,
                data->quality,
                data->reftape,
                data->remtape,
                data->freq_code,
                data->mode,
                data->no_freq,
                data->amp,
                snr,
                data->resid_phas,
                data->sbdelay,
                mbdptr,
                ratestring);

            sprintf(buf2,fformat2e,
                data->esdesp,
                archiv_str,
                data->epoch[0],
                data->epoch[1],
                data->total_phas,
                data->total_rate,
                data->total_mbdelay,
                fabs((double)data->total_sbresid),
                data->ambiguity,
                data->parents[0],
                data->pcals[0],
                data->pcals[1],
                data->pcals[2],
                data->pcals[3],
                data->root_id);

            strcat(buf,buf2);
            if (data->total_mbdelay > -1000000.0)
                {
                buf[174] = ' ';
                if(data->total_sbresid < 0.0) buf[169] = '-';
                }
            else
                {
                buf[175] = ' ';
                if(data->total_sbresid < 0.0) buf[170] = '-';
                }
            break;

        case 2:
                                        /* Version 2, unix only, Jan 94 on */
                                        /* Need only construct parent string */
            i = 0;
            while (data->parents[i] > 0)
                {
                if (i == 0) sprintf (parents, "%d", data->parents[i]);
                else
                    {
                    sprintf (buf2, ",%d", data->parents[i]);
                    strcat (parents, buf2);
                    }
                i++;
                }

            sprintf (buf, fformat_v2,
                data->version,
                data->root_id,
                data->extent_no,
                data->length,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->freq_code,
                data->mode,
                data->no_freq,
                data->amp,
                data->snr,
                data->resid_phas,
                data->datatype,
                data->sbdelay,
                data->mbdelay,
                data->ambiguity,
                data->delay_rate,
                data->ref_elev,
                data->rem_elev,
                data->ref_az,
                data->rem_az,
                data->u,
                data->v,
                data->esdesp,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->total_phas,
                data->total_rate,
                data->total_mbdelay,
                data->total_sbresid,
                parents);
            break;

        case 3:
                                        /* Version 3, unix only, March 95 on */
                                        /* Need only construct parent string */
            i = 0;
            while (data->parents[i] > 0)
                {
                if (i == 0) sprintf (parents, "%d", data->parents[i]);
                else
                    {
                    sprintf (buf2, ",%d", data->parents[i]);
                    strcat (parents, buf2);
                    }
                i++;
                }

            sprintf (buf, fformat_v3,
                data->version,
                data->root_id,
                data->extent_no,
                data->duration,
                data->length,
                data->offset,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->freq_code,
                data->mode,
                data->no_freq,
                data->amp,
                data->snr,
                data->resid_phas,
                data->datatype,
                data->sbdelay,
                data->mbdelay,
                data->ambiguity,
                data->delay_rate,
                data->ref_elev,
                data->rem_elev,
                data->ref_az,
                data->rem_az,
                data->u,
                data->v,
                data->esdesp,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->total_phas,
                data->total_rate,
                data->total_mbdelay,
                data->total_sbresid,
                parents);
            break;

        case 4:
                                        /* Version 4, unix only, November 95 on */
                                        /* Need only construct parent string */
            i = 0;
            while (data->parents[i] > 0)
                {
                if (i == 0) sprintf (parents, "%d", data->parents[i]);
                else
                    {
                    sprintf (buf2, ",%d", data->parents[i]);
                    strcat (parents, buf2);
                    }
                i++;
                }

            sprintf (buf, fformat_v4,
                data->version,
                data->root_id,
                data->extent_no,
                data->duration,
                data->length,
                data->offset,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->scan_offset,
                data->source,
                data->baseline,
                data->quality,
                data->freq_code,
                data->mode,
                data->no_freq,
                data->lags,
                data->amp,
                data->snr,
                data->resid_phas,
                data->phase_snr,
                data->datatype,
                data->sbdelay,
                data->mbdelay,
                data->ambiguity,
                data->delay_rate,
                data->ref_elev,
                data->rem_elev,
                data->ref_az,
                data->rem_az,
                data->u,
                data->v,
                data->esdesp,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->total_phas,
                data->total_rate,
                data->total_mbdelay,
                data->total_sbresid,
                data->srch_cotime,
                data->noloss_cotime,
                parents);
            break;

        case 5:
                                        /* Version 5, Mk4 only, September 99 on */
            if (pyear > 100) pyear %= 100;
            sprintf (buf, fformat_v5,
                data->version,
                data->root_id,
                data->extent_no,
                data->duration,
                data->length,
                data->offset,
                data->expt_no,
                data->scan_id,
                pyear, pday, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->scan_offset,
                data->source,
                data->baseline,
                data->quality,
                data->errcode,
                data->freq_code,
                data->no_freq,
                data->polarization,
                data->lags,
                data->amp,
                data->snr,
                data->resid_phas,
                data->phase_snr,
                data->datatype,
                data->sbdelay,
                data->mbdelay,
                data->ambiguity,
                data->delay_rate,
                data->ref_elev,
                data->rem_elev,
                data->ref_az,
                data->rem_az,
                data->u,
                data->v,
                data->esdesp,
                data->epoch[0],
                data->epoch[1],
                data->ref_freq,
                data->total_phas,
                data->total_rate,
                data->total_mbdelay,
                data->total_sbresid,
                data->srch_cotime,
                data->noloss_cotime);
            break;

        default:
            msg ("Unsupported version number in write_fsumm() (%d)", 2, data->version);
            return (-1);
        }

    if (fputs (buf, fp) == EOF) 
        {
        msg("Error writing to output file from write_fsumm()",2);
        return(-1);
        }

    return(0);
    }
