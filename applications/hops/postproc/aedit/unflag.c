/************************************************************************/
/*                                                                      */
/* This routine clears bits in the flag field according to the string   */
/* passed to it in arg1.  There is a subtlety when unflagging data that */
/* has been through an EDIT DUPLICATES, such that the internal          */
/* consistency regarding duplicates is destroyed.  This problem is      */
/* presently handled by warning message, since automatic re-editing     */
/* would be too computationally expensive                               */
/*                                                                      */
/*      Inputs:         arg1            string identifying bit to clear */
/*                                                                      */
/*      Output:         return value    0 for success                   */
/*                                                                      */
/* Created April 9 1989 by CJL                                          */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedata.h"
#include "flags.h"
#include "aedit.h"

#define FALSE 0
#define TRUE 1

int unflag (esum *data, char *arg1)
    {
    extern struct inputs inp;
    extern int fscan, fflag, up_to_date, rmdup;
    extern int cscan, cflag, rscan, rflag;
    extern int tscan, tflag, qscan, qflag;
    fringearray *fdata;
    corelarray *cdata;
    rootarray *rdata;
    trianglearray *tdata;
    quadarray *qdata;
    int nunflagf, nunflagc, nunflagr, nunflagt, nunflagq;
    int i, n, corrupt;
    char c;
    int mask;

    n = strlen(arg1);   /* Convert to lower case */
    for(i=0;i<n;i++) 
        {
        c = arg1[i];
        if(isupper(c)) c = tolower(c);
        arg1[i] = c;
        }

    if(strncmp(arg1,"all",n) == 0)                      mask = 0;
    else if(strncmp(arg1,"duplicates",n) == 0)          mask = ~(DUPLICATE);
    else if(strncmp(arg1,"cursor",n) == 0)              mask = ~(ZAPPED);
    else if(strncmp(arg1,"qcodes",n) == 0)              mask = ~(BADQF);
    else if(strncmp(arg1,"snr",n) == 0)                 mask = ~(BADSNR);
    else if(strncmp(arg1,"bsnr",n) == 0)                mask = ~(BADBSNR);
    else if(strncmp(arg1,"timerange",n) == 0)           mask = ~(BADTIME);
    else if(strncmp(arg1,"procrange",n) == 0)           mask = ~(BADPROC);
    else if(strncmp(arg1,"stations",n) == 0)            mask = ~(BADSTATION);
    else if(strncmp(arg1,"baselines",n) == 0)           mask = ~(BADBASELN);
    else if(strncmp(arg1,"triangles",n) == 0)           mask = ~(BADTRNGL);
    else if(strncmp(arg1,"quads",n) == 0)               mask = ~(BADQUAD);
    else if(strncmp(arg1,"experiment",n) == 0)          mask = ~(BADEXPT);
    else if(strncmp(arg1,"frequencies",n) == 0)         mask = ~(BADFREQA);
    else if(strncmp(arg1,"polarizations",n) == 0)       mask = ~(BADPOL);
    else if(strncmp(arg1,"sources",n) == 0)             mask = ~(BADSOURCE);
    else if(strncmp(arg1,"length",n) == 0)              mask = ~(BADLENGTH);
    else if(strncmp(arg1,"fraction",n) == 0)            mask = ~(BADFRAC);
    else if(strncmp(arg1,"nfreq",n) == 0)               mask = ~(BADNFREQ);
    else if(strncmp(arg1,"parameter",n) == 0)           mask = ~(BADPARAM);
    else if(strncmp(arg1,"type",n) == 0)                mask = ~(BADTYPE);
    else if(strncmp(arg1,"parents",n) == 0)             mask = ~(CHILDLESS);
    else if(strncmp(arg1,"children",n) == 0)            mask = ~(ORPHAN);
    else if(strncmp(arg1,"notriangle",n) == 0)          mask = ~(NO_TRIANGLE);
    else if(strncmp(arg1,"nobaseline",n) == 0)          mask = ~(NO_BASELINE);
    else
        {
        msg ("Unrecognized argument in 'unflag'", 2);
        return (-1);
        }

    fdata = data->fdata;
    cdata = data->cdata;
    rdata = data->rdata;
    tdata = data->tdata;
    qdata = data->qdata;

    nunflagf = nunflagc = nunflagr = nunflagt = nunflagq = 0;
    for(i=0;i<fscan;i++) 
        {
        if(fdata[i].flag != 0) 
            {
            fdata[i].flag &= mask;
            if(fdata[i].flag == 0) nunflagf++;
            }
        }
    for(i=0;i<cscan;i++) 
        {
        if(cdata[i].flag != 0) 
            {
            cdata[i].flag &= mask;
            if(cdata[i].flag == 0) nunflagc++;
            }
        }
    for(i=0;i<rscan;i++) 
        {
        if(rdata[i].flag != 0) 
            {
            rdata[i].flag &= mask;
            if(rdata[i].flag == 0) nunflagr++;
            }
        }
    for(i=0;i<tscan;i++) 
        {
        if(tdata[i].flag != 0) 
            {
            tdata[i].flag &= mask;
            if(tdata[i].flag == 0) nunflagt++;
            }
        }
    for(i=0;i<qscan;i++) 
        {
        if(qdata[i].flag != 0) 
            {
            qdata[i].flag &= mask;
            if(qdata[i].flag == 0) nunflagq++;
            }
        }
    fflag -= nunflagf;
    cflag -= nunflagc;
    rflag -= nunflagr;
    tflag -= nunflagt;
    qflag -= nunflagq;
                                        /* Check for screwups */
    corrupt = FALSE;
    if ((fflag < 0) || (cflag < 0) || (rflag < 0)
                || (tflag < 0) || (qflag < 0)) corrupt = TRUE;
    if (fflag < 0) fflag = 0;
    if (cflag < 0) cflag = 0;
    if (rflag < 0) rflag = 0;
    if (tflag < 0) tflag = 0;
    if (qflag < 0) qflag = 0;
    if (corrupt)
        {
        msg("Data corrupted!",2);
        return (-1);
        }
                                        /* Report what got unflagged */
    if (rscan > 0)
        {
        msg ("Type 0 (root) data:", 2);
        msg("\tRestored\tFlag\tTotal\tRemaining",2);
        msg("\t%d\t\t%d\t%d\t%d",2,nunflagr,rflag,rscan,rscan-rflag);
        if(nunflagr != 0) up_to_date = FALSE;
        }
    if (cscan > 0)
        {
        msg ("Type 1 (corel) data:", 2);
        msg("\tRestored\tFlag\tTotal\tRemaining",2);
        msg("\t%d\t\t%d\t%d\t%d",2,nunflagc,cflag,cscan,cscan-cflag);
        if(nunflagc != 0) up_to_date = FALSE;
        }
    if (fscan > 0)
        {
        msg ("Type 2 (fringe) data:", 2);
        msg("\tRestored\tFlag\tTotal\tRemaining",2);
        msg("\t%d\t\t%d\t%d\t%d",2,nunflagf,fflag,fscan,fscan-fflag);
        if(nunflagf != 0) up_to_date = FALSE;
        if((mask & DUPLICATE) != 0) rmdup = FALSE;
        else if(rmdup && nunflagf > 0) 
            {
            msg("\nWARNING: unflagging operation may have introduced new duplicates",2);
            msg("Recommended action is to unflag duplicates and rerun edit duplicates",2);
            }
        }
    if (tscan > 0)
        {
        msg ("Type 3 (triangle) data:", 2);
        msg("\tRestored\tFlag\tTotal\tRemaining",2);
        msg("\t%d\t\t%d\t%d\t%d",2,nunflagt,tflag,tscan,tscan-tflag);
        if(nunflagt != 0) up_to_date = FALSE;
        }
    if (qscan > 0)
        {
        msg ("Type 1 (quad) data:", 2);
        msg("\tRestored\tFlag\tTotal\tRemaining",2);
        msg("\t%d\t\t%d\t%d\t%d",2,nunflagq,qflag,qscan,qscan-qflag);
        if(nunflagq != 0) up_to_date = FALSE;
        }
    return(0);
    }
