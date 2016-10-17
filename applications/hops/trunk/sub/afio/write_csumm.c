/************************************************************************/
/*                                                                      */
/* This is the routine that writes the type-1 data out to the stream    */
/* passed as fp.  Each call writes 1 line.                              */
/*                                                                      */
/*      Inputs:         data            Filled in corelsum structure    */
/*                      fp              Regular A-format output file    */
/*                                                                      */
/* Created as write_data() April 5 1989 by CJL                          */
/* Modified March 11 1991 by CJL -- Separate type 0, 1, 2 output        */
/* Separated into afio library from aedit by CJL, 23 September 1992     */
/* This involved alteration to write only one line at a time.           */
/* Added support for version 2 CJL 27 January 1994                      */
/* Added support for version 4 CJL 1 November 1995                      */
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

char *cformat = "<%-5s %1d %2d %4d %4d %2d%03dd%02d%02d %2d %03d-%02d%02d\
 %-8s %s%c";
char *extras = " %2d %4d %4d %4d %1d%1d%02d %-2s\
             %7.2f %7.2f        %06o";

char *cformat_v2 = "2 %s 1 %2d %3d %4d %2d%03d%c%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s%c %4d %d %d %-2s %7.2f %7.2f %06o\n";

char *cformat_v3 = "3 %s 1 %2d %3d         %4d %2d%03d%c%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s%c %4d %d %d %-2s %7.2f %7.2f %06o\n";

char *cformat_v4 = "4 %s 1 %2d %3d         %4d %2d%03d%c%02d%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s%c %4d %4d %d %d %-2s %7.2f %7.2f %06o\n";

char *cformat_v5 = "5 %s 1 %3d         %4d %s %2d%03d-%02d%02d%02d %4d %03d-%02d%02d%02d\
 %-8s %s%c %4d %4d %d %d %-2s %7.2f %7.2f %06o\n";

char *cformat_v6 = "6 %s 1 %3d         %4d %s %2d%03d-%02d%02d%02d %4d %03d-%02d%02d%02d\
 %-8s %s%c %4d %4d %d %d %-2s %7.2f %7.2f %06o\n";

int
write_csumm(corelsum *data, FILE *fp)
    {
    char c, buf[140], buf2[140];
    int i, pyear, pday, phour, pmin, psec, syear, sday, shour, smin, ssec;

    for (i=0; i<140; i++) buf[i] = ' ';
    buf[139] = '\0';

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
            sprintf(buf,cformat,
                data->fname,
                1,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear,pday,phour,pmin,
                syear,sday,shour,smin,
                data->source,
                data->baseline,
                data->quality);

            if (data->refdrive != 0)
                {
                sprintf (buf2, extras,
                    data->startsec,
                    data->sduration,
                    data->corstart,
                    data->corstop,
                    data->refdrive,
                    data->remdrive,
                    data->eqts,
                    data->freqs,
                    data->refclock_err,
                    data->clock_diff,
                    data->status);
                strcat (buf, buf2);
                }

            i = strlen (buf);
            buf[i] = ' ';

            if (strlen(data->root_id) == 6) strncpy (buf+112, data->root_id, 6);
                                /* A-file number, must support ARW's new */
                                /* scheme for >9999 at Bonn */
            if (data->archiv > 9999)
                {
                c = (data->archiv / 1000) - 10 + 'A';
                sprintf (buf+126, "A%c%03d\n", c, data->archiv % 1000);
                }
            else sprintf (buf+126, "A%04d\n", data->archiv);
            break;

        case 2:
                                        /* Version 2, unix only, Jan 94 on */
            sprintf (buf, cformat_v2,
                data->root_id,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->sduration,
                data->refdrive,
                data->remdrive,
                data->freqs,
                data->refclock_err,
                data->clock_diff,
                data->status);
            break;

        case 3:
                                        /* Version 3, same as version 2 */
            sprintf (buf, cformat_v3,
                data->root_id,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->sduration,
                data->refdrive,
                data->remdrive,
                data->freqs,
                data->refclock_err,
                data->clock_diff,
                data->status);
            break;

        case 4:
                                        /* Version 4, unix only, Nov 95 on */
            sprintf (buf, cformat_v4,
                data->root_id,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->sduration,
                data->lags,
                data->refdrive,
                data->remdrive,
                data->freqs,
                data->refclock_err,
                data->clock_diff,
                data->status);
            break;

        case 5:
                                        /* Version 5, Mk4 only, Sep 99 on */
            sprintf (buf, cformat_v4,
                data->root_id,
                data->size,
                data->expt_no,
                data->scan_id,
                pyear, pday, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->sduration,
                data->lags,
                data->refdrive,
                data->remdrive,
                data->freqs,
                data->refclock_err,
                data->clock_diff,
                data->status);
            break;

        case 6:
                                        /* Version 6, EHT era */
            sprintf (buf, cformat_v4,
                data->root_id,
                data->size,
                data->expt_no,
                data->scan_id,
                pyear, pday, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->source,
                data->baseline,
                data->quality,
                data->sduration,
                data->lags,
                data->refdrive,
                data->remdrive,
                data->freqs,
                data->refclock_err,
                data->clock_diff,
                data->status);
            break;

        default:
            msg ("Unsupported version number in write_csumm() (%d)",
                2, data->version);
            return (-1);
        }


    if(fputs(buf,fp) == EOF) 
        {
        msg("Error writing to output file",2);
        return(-1);
        }

    return(0);
    }
