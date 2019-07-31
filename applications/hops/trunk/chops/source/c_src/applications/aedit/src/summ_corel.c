/************************************************************************/
/*                                                                      */
/* This routine loops through the corel data in memory, and accumulates */
/* certain statistics.  This information can be printed (pr_summary.c)  */
/*                                                                      */
/*      Inputs:         cdata                                           */
/*                      mode                    defined in summary.h    */
/*                                                                      */
/*      Output:         filled "csumm" structure                        */
/*                      return value            0 = OK, -1 = failure    */
/*                                                                      */
/* Created 9 April 1989 by CJL                                          */
/* Added mode support for data versions 1 February 1994 by CJL          */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int summ_corel (corelarray *cdata, int mode)
    {
    extern struct datasumm csumm;
    extern int cscan, cflag;
    int dtime, ptime;
    int i, j, l, match, first, found;
    corelsum *datum;

    first = TRUE;

    for (i=0; i<cscan; i++)             /* Loop over all corel records */
        {
                                        /* Flagged, ignore */
        if (cdata[i].flag != 0) continue;
                                        /* Point to data section */
        datum = &(cdata[i].data);
                                        /* Version number count */
        csumm.version[datum->version] += 1;
        if (mode == VERSION) continue;
                                        /* Check times */
        dtime = datum->time_tag;
        ptime = datum->procdate;
        if(first)                       /* Initialize */
            {
            csumm.begin = csumm.end = dtime;
            csumm.proc_begin = csumm.proc_end = ptime;
            first = FALSE;
            }
        if (dtime < csumm.begin) csumm.begin = dtime;
        if (dtime > csumm.end) csumm.end = dtime;
        if (ptime < csumm.proc_begin) csumm.proc_begin = ptime;
        if (ptime > csumm.proc_end) csumm.proc_end = ptime;

                                        /* Check stations */
        if (strlen (csumm.stations) < MAXSTTOT) 
                add_station (datum->baseline[0], csumm.stations);
        if (strlen (csumm.stations) < MAXSTTOT) 
                add_station (datum->baseline[1], csumm.stations);
                                        /* Check baselines */
        for (j=0; j<csumm.nbtq; j++)
            if (strcmp (datum->baseline, csumm.btq + 3*j) == 0) break;
        if (j == csumm.nbtq)
            {
            strcpy (csumm.btq + 3*csumm.nbtq, datum->baseline);
            csumm.nbtq++;
            }
                                        /* Check experiment numbers */
        match = FALSE;
        for (j=0; j<csumm.nexp; j++) 
            {
            if (csumm.experiments[j] == datum->expt_no)
                {
                match = TRUE;
                break;
                }
            }
        if (! match && (csumm.nexp < MAXEXPTS)) 
            csumm.experiments[csumm.nexp++] = datum->expt_no;

                                        /* Accumulate quality codes */
        switch (datum->quality)
            {
            case 'A':
                csumm.qcodes[0]++;
                break;
            case 'B':
                csumm.qcodes[1]++;
                break;
            case 'C':
                csumm.qcodes[2]++;
                break;
            case 'D':
                csumm.qcodes[3]++;
                break;
            case 'E':
                csumm.qcodes[4]++;
                break;
            case 'F':
                csumm.qcodes[5]++;
                break;
            case '0':
                csumm.qcodes[6]++;
                break;
            case '1':
                csumm.qcodes[7]++;
                break;
            case '2':
                csumm.qcodes[8]++;
                break;
            case '3':
                csumm.qcodes[9]++;
                break;
            case '4':
                csumm.qcodes[10]++;
                break;
            case '5':
                csumm.qcodes[11]++;
                break;
            case '6':
                csumm.qcodes[12]++;
                break;
            case '7':
                csumm.qcodes[13]++;
                break;
            case '8':
                csumm.qcodes[14]++;
                break;
            case '9':
                csumm.qcodes[15]++;
                break;
            default:
                csumm.qcodes[16]++;
            }

                                        /* This updates source list */
        if (csumm.nsource < MAXSRC)
            csumm.nsource += 
                update_sinfo (csumm.source, datum->source, csumm.nsource);

        }               /* End for loop */
    return(0);
    }
