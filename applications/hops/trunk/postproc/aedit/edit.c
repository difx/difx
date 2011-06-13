/************************************************************************/
/*                                                                      */
/* This is the routine responsible for flagging scans in memory.  There */
/* are three modes of operation.  First, the user of an interactive     */
/* graphics device can enable the cursor and "zap" the point which lies */
/* unambiguously nearest the cursor, or define a region of the plot in  */
/* which all points are to be zapped.  Second, various filters can be   */
/* applied to the data, controlled by the contents of the inp structure */
/* which the user can manipulate directly.  Third, there is a duplicate */
/* removal mode.  Duplicate scans are flagged, keeping only the highest */
/* snr, the best quality code, or the most recent processing date.      */
/* The edit operation manipulates various bits in the flag field of the */
/* *sum structures ... any set bit flags the scan, but many bits        */
/* can be set at once.  The meaning of the bits is defined in aedit.h   */
/*                                                                      */
/*      Inputs:         arg1, arg2      Strings determining the edit    */
/*                                      mode                            */
/*                                                                      */
/*      Output:         data            flag fields modified            */
/*                                                                      */
/* Created 4 April 1989 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "flags.h"

#define QUICK 0
#define SLOW 1

int
edit (data, arg1, arg2)
esum *data;
char *arg1, *arg2;
    {
    extern struct inputs inp;
    extern struct plot_info pdata[];
    extern int fflag, cflag, rflag, tflag, qflag;
    extern int fscan, cscan, rscan, tscan, qscan;
    extern int up_to_date, rmdup, plot_open, batch;
    int i, n, neditf, neditc, neditr, neditt, neditq, *nedit;
    int noflag, option, index, ret, frac, doall;
    char c;

    n = strlen(arg1);   /* Convert to lower case */
    for(i=0;i<n;i++) 
        {
        c = arg1[i];
        if(isupper(c)) c = tolower(c);
        arg1[i] = c;
        }

    neditf = neditc = neditr = neditt = neditq = 0; ret = 0;

    if(strncmp(arg1,"cursor",n) == 0) 
        {
        if(batch) 
            {
            msg("EDIT CURSOR disabled in batch mode", 2);
            return(-1);
            }
        if(! plot_open) 
            {
            msg("No active plot!", 2);
            return(-1);
            }
        if (pdata[0].plotby == QUAD_PLOT)
            {
            msg ("EDIT CURSOR not yet implemented for closure quads", 2);
            return (-1);
            }
                                        /* Set nedit pointer to correct variable */
        if (pdata[0].plotby == TRIANGLE_PLOT) nedit = &neditt;
        else nedit = &neditf;
                                        /* The '0' means edit mode */
        ret = cursor_select (data, nedit, 0);
        if (ret != -5) ret = 0;
        }

    else if (strncmp (arg1, "inputs", n) == 0) 
        {
        if((n = strlen(arg2)) == 0) doall = TRUE;
        else
            {
            for(i=0;i<n;i++)            /* Convert to lower case */
                {
                c = arg2[i];
                if(isupper(c)) c = tolower(c);
                arg2[i] = c;
                } 
            if (strncmp(arg2, "all", n) == 0) doall = TRUE;
            else if (strncmp(arg2, "unflagged", n) == 0) doall = FALSE;
            else
                {
                msg ("\tOptions:  \tEDIT INPUTS ALL", 2);
                msg ("\t\t\t            UNFLAGGED",2);
                msg ("\t\t\t(default is ALL)", 2);
                return (-1);
                }
            }
        active_filter();                        /* Prime filtering routine */
                                        /* Do all 5 types one by one */
        for(i=0;i<fscan;i++) 
            {
            if(data->fdata[i].flag == 0) noflag = TRUE;
            else noflag = FALSE;
            if ((!noflag) && (!doall)) continue;        /* Unflagged option */
                                                /* Apply filter */
            data->fdata[i].flag |= ffilter(&(data->fdata[i]),SLOW);
                                                /* Newly flagged point */
            if(noflag && (data->fdata[i].flag != 0)) neditf++;
            }
        for(i=0;i<cscan;i++) 
            {
            if(data->cdata[i].flag == 0) noflag = TRUE;
            else noflag = FALSE;
            if ((!noflag) && (!doall)) continue;        /* Unflagged option */
                                                /* Apply filter */
            data->cdata[i].flag |= cfilter(&(data->cdata[i]),SLOW);
                                                /* Newly flagged point */
            if(noflag && (data->cdata[i].flag != 0)) neditc++;
            }
        for(i=0;i<rscan;i++) 
            {
            if(data->rdata[i].flag == 0) noflag = TRUE;
            else noflag = FALSE;
            if ((!noflag) && (!doall)) continue;        /* Unflagged option */
                                                /* Apply filter */
            data->rdata[i].flag |= rfilter(&(data->rdata[i]),SLOW);
                                                /* Newly flagged point */
            if(noflag && (data->rdata[i].flag != 0)) neditr++;
            }
        for(i=0;i<tscan;i++) 
            {
            if(data->tdata[i].flag == 0) noflag = TRUE;
            else noflag = FALSE;
            if ((!noflag) && (!doall)) continue;        /* Unflagged option */
                                                /* Apply filter */
            data->tdata[i].flag |= tfilter(&(data->tdata[i]),SLOW);
                                                /* Newly flagged point */
            if(noflag && (data->tdata[i].flag != 0)) neditt++;
            }
        for(i=0;i<qscan;i++) 
            {
            if(data->qdata[i].flag == 0) noflag = TRUE;
            else noflag = FALSE;
            if ((!noflag) && (!doall)) continue;        /* Unflagged option */
                                                /* Apply filter */
            data->qdata[i].flag |= qfilter(&(data->qdata[i]),SLOW);
                                                /* Newly flagged point */
            if(noflag && (data->qdata[i].flag != 0)) neditq++;
            }
        }

    else if(strncmp(arg1,"duplicates",n) == 0) 
        {
        if((n=strlen(arg2)) == 0) 
            {
            msg("\tOptions:  \tEDIT DUPLICATES SNR", 2);
            msg("\t\t\t                QCODES", 2);
            msg("\t\t\t                PROCDATE", 2);
            return(-1);
            }
                                        /* Convert to lower case */
        for(i=0;i<n;i++) 
            {
            c = arg2[i];
            if(isupper(c)) c = tolower(c);
            arg2[i] = c;
            }

        if(strncmp(arg2,"snr",n) == 0) option = 1;
        else if(strncmp(arg2,"qcodes",n) == 0) option = 2;
        else if(strncmp(arg2,"procdate",n) == 0) option = 3;
        else 
            {
            msg("Unrecognized edit duplicate option %s", 2, arg2);
            return(-1);
            }
        if(fscan > 1000) msg("Working ...", 1);
        if((neditf = dup_flag(data,option)) < 0) 
            {
            msg("Error during duplicate flagging operation", 2);
            return(-1);
            }
        rmdup = TRUE;
        }

    else if (strncmp (arg1, "parents", n) == 0)
        edit_families (data, 0, &neditc, &neditr);
    else if (strncmp (arg1, "children", n) == 0)
        edit_families (data, 1, &neditf, &neditc);

    else if (strncmp (arg1, "close", n) == 0)
        {
        n = strlen (arg2);
        for(i=0;i<n;i++)                /* Convert to lower case */
            {
            c = arg2[i];
            if(isupper(c)) c = tolower(c);
            arg2[i] = c;
            } 
        if (n == 0)
            ret = edit_close (data, MODE_TRI | MODE_BASE, &neditf, &neditt);
        else if (strncmp (arg2, "triangles", n) == 0)
            ret = edit_close (data, MODE_TRI, &neditf, &neditt);
        else if (strncmp (arg2, "baselines", n) == 0)
            ret = edit_close (data, MODE_BASE, &neditf, &neditt);
        else
            {
            msg("\tOptions:  \tEDIT CLOSE", 2);
            msg("\t\t\t        EDIT CLOSE TRIANGLES", 2);
            msg("\t\t\t        EDIT CLOSE BASELINES", 2);
            return(-1);
            }
        if (ret != 0) return (-1);
        }
            

    else
        {
        msg("Unrecognized edit option %s", 2,arg1);
        return(-1);
        }

    fflag += neditf;
    cflag += neditc;
    rflag += neditr;
    tflag += neditt;
    qflag += neditq;
    if (rscan > 0)
        {
        msg("\tSummary of editing on type-0 (root) data:", 2);
        msg("\tNew edits\tFlag\tTotal\tRemaining", 2);
        msg("\t%d\t\t%d\t%d\t%d", 2,neditr,rflag,rscan,rscan-rflag);
        msg("", 2);
        }
    if (cscan > 0)
        {
        msg("\tSummary of editing on type-1 (corel) data:", 2);
        msg("\tNew edits\tFlag\tTotal\tRemaining", 2);
        msg("\t%d\t\t%d\t%d\t%d", 2,neditc,cflag,cscan,cscan-cflag);
        msg("", 2);
        }
    if (fscan > 0)
        {
        msg("\tSummary of editing on type-2 (fringe) data:", 2);
        msg("\tNew edits\tFlag\tTotal\tRemaining", 2);
        msg("\t%d\t\t%d\t%d\t%d", 2,neditf,fflag,fscan,fscan-fflag);
        msg("", 2);
        }
    if (tscan > 0)
        {
        msg("\tSummary of editing on type-3 (triangle) data:", 2);
        msg("\tNew edits\tFlag\tTotal\tRemaining", 2);
        msg("\t%d\t\t%d\t%d\t%d", 2,neditt,tflag,tscan,tscan-tflag);
        msg("", 2);
        }
    if (qscan > 0)
        {
        msg("\tSummary of editing on type-4 (quad) data:", 2);
        msg("\tNew edits\tFlag\tTotal\tRemaining", 2);
        msg("\t%d\t\t%d\t%d\t%d", 2,neditq,qflag,qscan,qscan-qflag);
        msg("", 2);
        }
    if(neditf + neditc + neditr + neditt + neditq > 0) up_to_date = FALSE;
    return(ret);
    }
