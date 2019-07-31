/************************************************************************/
/*                                                                      */
/* This routine loops through triangle data in memory, and accumulates  */
/* certain statistics.  This information can be printed (pr_summary.c)  */
/*                                                                      */
/*      Inputs:         tdata                                           */
/*                      mode                    defined in summary.h    */
/*                                                                      */
/*      Output:         filled "tsumm" structure                        */
/*                      return value            0 = OK, -1 = failure    */
/*                                                                      */
/* Created 9 April 1989 by CJL                                          */
/* Added mode support for data versions 1 February 1994 by CJL          */
/* Cloned for triangles 24 August 1994 by CJL                           */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int summ_triangle (trianglearray *tdata, int mode)
    {
    extern struct datasumm tsumm;
    extern int tscan, tflag;
    static char *qstring = "ABCDEFGH0123456789";
    int dtime;
    int i, j, k, l, match, first, found, ret;
    trianglesum *datum;

    first = TRUE;

    ret = 0;
    for (i=0; i<tscan; i++)             /* Loop over all triangle records */
        {
                                        /* Flagged, ignore */
        if (tdata[i].flag != 0) continue;
                                        /* Point to data section */
        datum = &(tdata[i].data);
                                        /* Version number count */
        tsumm.version[datum->version] += 1;
        if (mode == VERSION) continue;
                                        /* Check times */
        dtime = datum->time_tag;
        if(first)                       /* Initialize */
            {
            tsumm.begin = tsumm.end = dtime;
            first = FALSE;
            }
        if (dtime < tsumm.begin) tsumm.begin = dtime;
        if (dtime > tsumm.end) tsumm.end = dtime;

                                        /* Check frequencies */
        if (strchr (tsumm.frequencies,datum->freq_code) == NULL)
            {
            l = strlen (tsumm.frequencies);
            if (l < MAXBANDS)
                {
                tsumm.frequencies[l] = datum->freq_code;
                tsumm.frequencies[l+1] = '\0';
                }
            }
                                        /* Check stations */
        for (j=0; j<3; j++)
            {
            if (strlen (tsumm.stations) < MAXSTTOT)
                add_station (datum->triangle[j], tsumm.stations);
            }
                                        /* Check triangles.  Note that tricheck() */
                                        /* is sneaky.  If a triangle matches, but */
                                        /* the stations are in a different order, */
                                        /* it modifies the data record itself to */
                                        /* match the template station order. The */
                                        /* modified records fall through the other */
                                        /* summarization functions (update_fqex()) */
        for (j=0; j<tsumm.nbtq; j++)
            if (tricheck (datum, tsumm.btq + 4*j) == 0) break;
        if (j == tsumm.nbtq)
            {
            strcpy (tsumm.btq + 4*tsumm.nbtq, datum->triangle);
            tsumm.nbtq++;
            }
                                        /* Check experiment numbers */
        match = FALSE;
        for (j=0; j<tsumm.nexp; j++) 
            {
            if (tsumm.experiments[j] == datum->expt_no)
                {
                match = TRUE;
                break;
                }
            }
        if (! match && (tsumm.nexp < MAXEXPTS)) 
            tsumm.experiments[tsumm.nexp++] = datum->expt_no;
                                        /* Check SNR limits */
        if (datum->bis_snr > tsumm.snrmax) tsumm.snrmax = datum->bis_snr;
        if (datum->bis_snr < tsumm.snrmin) tsumm.snrmin = datum->bis_snr;

                                        /* Accumulate quality codes */
        k = 0;
        while (qstring[k])
            {
            if (qstring[k] == datum->scan_quality) break;
            k++;
            }
        tsumm.qcodes[k]++;
                                        /* This updates source list */
        if (tsumm.nsource < MAXSRC)
            tsumm.nsource += 
                update_sinfo (tsumm.source, datum->source, tsumm.nsource);
                                        /* Fill in freq/exp/source-specific summary */
        if (ret = update_fqex (datum, &tsumm, TRIANGLE)) break;

        }               /* End for loop */
    return (ret);
    }
