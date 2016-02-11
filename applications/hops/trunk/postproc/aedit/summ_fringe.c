/************************************************************************/
/*                                                                      */
/* This routine loops through the fringe data in memory, and accumulates*/
/* certain statistics.  This information can be printed (pr_summary.c), */
/* or can be used to guide the plotting operations.                     */
/*                                                                      */
/*      Inputs:         data                                            */
/*                      mode            defined in summary.h            */
/*                                                                      */
/*      Output:         filled "fsumm" structure                        */
/*                      return value            0 = OK, -1 = failure    */
/*                                                                      */
/* Created 9 April 1989 by CJL                                          */
/* Added mode argument for speed in normal operation, March 13 1992 CJL */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"

int
summ_fringe (data, mode)
esum *data;
int mode;
    {
    extern struct datasumm fsumm;
    extern int fscan, fflag, tscan;
    int dtime, ptime;
    int i, j, k, l, match, first, ret;
    static char *qstring = "ABCDEFGH0123456789";
    fringearray *fdata;
    fringesum *datum;

    first = TRUE;
    ret = 0;
    fdata = data->fdata;
                                        /* Loop over fringe records */
    for (i=0; i<fscan; i++)
        {                               /* Skip if flagged */
        if (fdata[i].flag != 0) continue;
                                        /* Point at data section */
        datum = &(fdata[i].data);
                                        /* Version number count */
        fsumm.version[datum->version] += 1;
        if (mode == VERSION) continue;
                                        /* Check frequencies */
        if (strchr (fsumm.frequencies,datum->freq_code) == NULL) 
            {
            l = strlen (fsumm.frequencies);
            if (l < MAXBANDS) 
                {
                fsumm.frequencies[l] = datum->freq_code;
                fsumm.frequencies[l+1] = '\0';
                }
            }
                                        /* Check experiment numbers */
        match = FALSE;
        for (j=0; j<fsumm.nexp; j++) 
            {
            if (fsumm.experiments[j] == datum->expt_no) 
                {
                match = TRUE;
                break;
                }
            }
        if (! match && (fsumm.nexp < MAXEXPTS)) 
            fsumm.experiments[fsumm.nexp++] = datum->expt_no;

                                        /* This updates source list */
        if (fsumm.nsource < MAXSRC)
            fsumm.nsource += 
                update_sinfo (fsumm.source, datum->source, fsumm.nsource);

                                        /* Check times */
        dtime = datum->time_tag;
        ptime = datum->procdate;
        if(first)                       /* Initialize */
            {
            fsumm.begin = fsumm.end = dtime;
            fsumm.proc_begin = fsumm.proc_end = ptime;
            first = FALSE;
            }
        if (dtime < fsumm.begin) fsumm.begin = dtime;
        if (dtime > fsumm.end) fsumm.end = dtime;
        if (ptime < fsumm.proc_begin) fsumm.proc_begin = ptime;
        if (ptime > fsumm.proc_end) fsumm.proc_end = ptime;

                                        /* Check stations */
        if (strlen (fsumm.stations) < MAXSTTOT)
            add_station (datum->baseline[0], fsumm.stations);
        if (strlen (fsumm.stations) < MAXSTTOT)
            add_station (datum->baseline[1], fsumm.stations);
                                        /* Check baselines */
        for (j=0; j<fsumm.nbtq; j++)
            if (strcmp (datum->baseline, fsumm.btq + 3*j) == NULL) break;
        if (j == fsumm.nbtq)
            {
            strcpy (fsumm.btq + 3*fsumm.nbtq, datum->baseline);
            fsumm.nbtq++;
            }
                                        /* Check polarizations */
        if (strstr (fsumm.polarizations, datum->polarization) == NULL)
            {
            if (strlen (fsumm.polarizations) == 48)
                {
                msg ("Error, bad polarization information", 2);
                return (-1);
                }
            strcat (fsumm.polarizations, datum->polarization);
            strcat (fsumm.polarizations, " ");
            }
                                        /* Accumulate quality codes */
        k = 0;
        while (qstring[k])
            {
            if (qstring[k] == mk3_qf (datum)) break;
            k++;
            }
        fsumm.qcodes[k]++;
                                        /* Check SNR limits */
        if(datum->snr > fsumm.snrmax) fsumm.snrmax = datum->snr;
        if(datum->snr < fsumm.snrmin) fsumm.snrmin = datum->snr;

                                        /* Fill in freq/exp/source-specific summary */
        if (ret = update_fqex (datum, &fsumm, BASELINE)) break;
        }               /* End for loop */
                                        /* Calculate the closure quantities */
    if (mode == CLOSURE)
        {
        if (tscan > 0) 
            {
            msg ("Warning ... using closure data already in memory.", 2);
            msg ("Type 'clear close' first if you want to recalculate.", 2);
            }
        else
            {
            msg ("Calculating closure quantities ...", 2);
            calc_close (data);
            msg ("Computed %d triangle records occupying %d Kb of memory", 2,
                tscan, tscan*sizeof(trianglearray)/1024);
            }
        }
    return(ret);
    }
