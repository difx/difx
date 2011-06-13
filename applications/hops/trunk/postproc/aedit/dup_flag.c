/************************************************************************/
/*                                                                      */
/* This routine is responsible for locating and flagging duplicate      */
/* scans, such as often occur in MkIII VLBI.  The choice of which scan  */
/* to retain is based on the value of "option".  If more than one scan  */
/* ties for the lead, discrimination is done on the basis of the other  */
/* options.  If all options give equal merit to more than one scan, the */
/* first one is selected and all others are flagged.                    */
/*                                                                      */
/* Due to the quadratic behaviour of a dumb duplicate removal, large    */
/* datasets can make the job tedious.  To eliminate this problem in the */
/* neatest way, the data is first sorted so that the inner loop can     */
/* terminate as soon as a scan time mismatch is detected.  There is an  */
/* independent need for a sort capability in aedit, so a hash table     */
/* approach was deemed inferior and unnecessary.                        */
/*                                                                      */
/*      Inputs:         option          choose best snr, best qcode,    */
/*                                      or latest procdate (1,2,and 3)  */
/*                                      0 ==> just see if they exist    */
/*                                                                      */
/*      Output:         return value    Number of scans flagged         */
/*                                      for exist option, TRUE or FALSE */
/*                                                                      */
/* Created 11 April 1989 by CJL                                         */
/* Added sort modification 16 April 1990, CJL                           */
/* Added option to just check for existence of duplicates, not do       */
/* anything about them  March 7 1992 CJL                                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "aedata.h"
#include "flags.h"
#include "summary.h"

#define FALSE 0
#define TRUE 1

#define qflist "FDCBAGH0E123456789 "

int
dup_flag (data, option)
esum *data;
int option;
    {
    extern int fscan;
    extern int fsortstat[10];
    int i, j, ordi, ordj, ndup, nd, nbest, opt1, opt2, opt3, tmpopt,
        qf, qmax, time, tmax, ndflag, unsort, exist;
    int same[500], dlist[500];
    float snr, snrmax;
    fringearray *fdata;
    fringesum *datum;

                                        /* For conciseness */
    fdata = data->fdata;
                                        /* First sort by scan time */
                                        /* Remember whether to restore */
                                        /* original order after we are done */
                                        /* Scan sorting needs care for mixed */
                                        /* data format versions */
    summ_data (data, VERSION);
    if(fsortstat[fsortstat[0]] == 1) unsort = FALSE;
    else 
        {
        sorter(fdata, "timetag", 2);
        unsort = TRUE;
        }

    ndflag = 0;
    exist = FALSE;
                                        /* Loop through all the data */
    for(i=0;i<fscan;i++) 
        {
        ordi = fdata[i].order;
                                        /* skip flagged scans */
        if(fdata[ordi].flag != 0) continue;

        ndup = 0;
                                        /* Permanent copy */
        dlist[ndup] = ordi;
                                        /* Matches itself! */
        same[ndup++] = ordi;
                                        /* Check all unflagged scans for match */
        for(j=i+1;j<fscan;j++) 
            {
            ordj = fdata[j].order;
            if(dup_check(fdata,ordi,ordj) && (fdata[ordj].flag == 0)) 
                {
                dlist[ndup] = ordj;
                same[ndup++] = ordj;
                }
                                        /* Data are sorted */
            if(fdata[ordi].data.time_tag != fdata[ordj].data.time_tag) break;
            }
                                        /* Exist option, skip all the */
                                        /* fancy stuff below, we just */
                                        /* want yes or no here */
        if (option == 0)
            {
            if (ndup > 1) 
                {
                exist = TRUE;
                break;
                }
            else continue;
            }
                                        /* ndup will be corrupted later */
        nd = ndup;
        opt1 = opt2 = opt3 = FALSE;
        tmpopt = option;
                                        /* Start with all duplicates equal */
        nbest = ndup;
                                        /* Keep going until only 1 left */
        while(nbest > 1) 
            {
                                        /* Search only scans which are tied */
                                        /* for the lead at this point */
            ndup = nbest;
            switch(tmpopt) 
                {
                                        /* Keep highest snr */
                case 1:
                    opt1 = TRUE;
                    snrmax = 0.0; nbest = 0;
                    for(j=0;j<ndup;j++) 
                        {
                        snr = fdata[same[j]].data.snr;
                        if(snr > snrmax) 
                            {
                            snrmax = snr;
                            nbest = 0;
                            same[nbest++] = same[j];
                            }
                        else if(snr == snrmax) same[nbest++] = same[j];
                        }
                    if(nbest > 1) 
                        {
                        if(opt2 && opt3) nbest = 1; /* Enough - take 1st */
                        else if(opt2) tmpopt = 3;   /* All that's left to do */
                        else tmpopt = 2;            /* Do qf next */
                        }
                    break;
                                        /* Keep "best" quality code */
                case 2:
                    opt2 = TRUE;
                    qmax = 0; nbest = 0;
                    for(j=0;j<ndup;j++) 
                        {
                        for(qf=0;qf<19;qf++)    /* Find it */
                            {
                            datum = &(fdata[same[j]].data);
                            if(mk3_qf (datum) == qflist[qf]) break;
                            }
                        if(qf >= 18) qf = 0;
                        if(qf > qmax) 
                            {
                            qmax = qf;
                            nbest = 0;
                            same[nbest++] = same[j];
                            }
                        else if(qf == qmax) same[nbest++] = same[j];
                        }
                    if(nbest > 1) 
                        {
                        if(opt1 && opt3) nbest = 1; /* Enough - take 1st */
                        else if(opt1) tmpopt = 3;   /* All that's left to do */
                        else tmpopt = 1;            /* Do snr next */
                        }
                    break;
                                        /* Keep most recent processing */
                case 3:
                    opt3 = TRUE;
                    tmax = 0; nbest = 0;
                    for(j=0;j<ndup;j++) 
                        {
                        time = fdata[same[j]].data.procdate;
                        if(time > tmax) 
                            {
                            tmax = time;
                            nbest = 0;
                            same[nbest++] = same[j];
                            }
                        else if(time == tmax) same[nbest++] = same[j];
                        }
                    if(nbest > 1) 
                        {
                        if(opt1 && opt2) nbest = 1; /* Enough - take 1st */
                        else if(opt2) tmpopt = 1;   /* All that's left to do */
                        else tmpopt = 2;            /* Do qf next */
                        }
                    break;

                default:
                    msg("Bad option in dup_flag()", 2);
                }
            }                   /* End loop to id best scan */

                                        /* Loop through original duplicate */
                                        /* list, flagging all but the best */
        for(j=0;j<nd;j++) 
            {
            if(dlist[j] != same[0]) 
                {
                if(fdata[dlist[j]].flag == 0) ndflag++;
                fdata[dlist[j]].flag |= DUPLICATE;
                }
            }
        }               /* End main loop */

                                                /* Undo the last sort operation */
    if(unsort) 
        {
        for(i=0;i<fscan;i++) fdata[fdata[i].lastorder].order = i;
        fsortstat[0]--;
        }

    if (option == 0) return (exist);
    else return(ndflag);
    }
