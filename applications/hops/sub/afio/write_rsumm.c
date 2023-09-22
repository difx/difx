/************************************************************************/
/*                                                                      */
/* This is the routine that writes the type-0 data out to the stream    */
/* passed as fp.  Each call writes 1 line.                              */
/*                                                                      */
/*      Inputs:         data            filled-in rootsum structure     */
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
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include "adata.h"
#include "mk4_afio.h"
#include "mk4_util.h"

char *rformat = "<%-5s 0 %2d %4d %4d %2d%03dd%02d%02d %2d %03d-%02d%02d\
 %-8s %s";

char *rformat_v2 = "2 %s 0 %2d %3d %4d %2d%03d%c%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s\n";

char *rformat_v3 = "3 %s 0 %2d %3d         %4d %2d%03d%c%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s\n";

char *rformat_v4 = "4 %s 0 %2d %3d         %4d %2d%03d%c%02d%02d%02d %2d %03d-%02d%02d%02d\
 %-8s %s\n";

char *rformat_v5 = "5 %s 0 %3d         %4d %s %2d%03d-%02d%02d%02d %4d %03d-%02d%02d%02d\
 %-8s %s\n";

char *rformat_v6 = "6 %s 0 %3d         %4d %s %2d%03d-%02d%02d%02d %4d %03d-%02d%02d%02d\
 %-8s %s\n";

int
write_rsumm(rootsum *data, FILE *fp)
    {
    char c, buf[140];
    int i, n, pyear, pday, phour, pmin, psec, syear, sday, shour, smin, ssec;

    for (i=0; i<140; i++) buf[i] = ' ';

    int_to_time (data->procdate,&pyear,&pday,&phour,&pmin,&psec);
    if (pyear >= 100) pyear -= 100;
    int_to_time (data->time_tag,&syear,&sday,&shour,&smin,&ssec);
    if (syear >= 100) syear -= 100;

    switch (data->version)
        {
        case 1:

            sprintf(buf,rformat,
                data->fname,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear,pday,phour,pmin,
                syear,sday,shour,smin,
                data->source,
                data->stations);

            n = strlen (buf);
            buf[n] = ' ';

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
            sprintf (buf, rformat_v2,
                data->root_id,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin,
                syear, sday, shour, smin, ssec,
                data->source,
                data->stations);
            break;

        case 3:
            sprintf (buf, rformat_v3,
                data->root_id,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin,
                syear, sday, shour, smin, ssec,
                data->source,
                data->stations);
            break;

        case 4:
            sprintf (buf, rformat_v4,
                data->root_id,
                data->extent_no,
                data->size,
                data->expt_no,
                pyear, pday, data->corel_vers, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->source,
                data->stations);
            break;

        case 5:
            sprintf (buf, rformat_v5,
                data->root_id,
                data->size,
                data->expt_no,
                data->scan_id,
                pyear, pday, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->source,
                data->stations);
            break;

        case 6:
            sprintf (buf, rformat_v6,
                data->root_id,
                data->size,
                data->expt_no,
                data->scan_id,
                pyear, pday, phour, pmin, psec,
                syear, sday, shour, smin, ssec,
                data->source,
                data->stations);
            break;

        default:
            msg ("Unsupported version number in write_rsumm() (%d)",
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
