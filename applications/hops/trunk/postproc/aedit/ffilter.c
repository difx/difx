/************************************************************************/
/*                                                                      */
/* This routine determines the status of a data point with respect to   */
/* the input filter settings.  It has two modes.  If the mode is QUICK, */
/* the routine returns a non-zero value as soon as a filter is not      */
/* passed, without checking additional filters.  If the mode is SLOW,   */
/* all filters are checked, and the return value has the appropriate    */
/* bit set for each filter not passed.  Efficiency is achieved by first */
/* calling active_filter(), which allows filters in their pass-all      */
/* state to be completely bypassed.                                     */
/*                                                                      */
/*      Inputs:         datum           one element of data array       */
/*                      mode            QUICK or SLOW                   */
/*                      inp structure   User input settings             */
/*                      fcheck,nfilt    Active filter information       */
/*                                      (external from active_filter()) */
/*                                                                      */
/*      Output:         return value    0 ==> passed all filters        */
/*                                                                      */
/* Created 30 April 1990 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "aedit.h"
#include "flags.h"
#include "usearray.h"

#define QUICK 0
#define SLOW 1

int
ffilter (fdatum, mode)
fringearray *fdatum;
int mode;
{
    extern struct inputs inp;
    extern int fcheck[12], nfilt;
    int i, ret, nf, frac, p0;
    float pval;
    fringesum *datum;
    extern struct usearray user_param;

    ret = 0;
    datum = &(fdatum->data);
    for (i=0;i<nfilt;i++) 
        {
        switch (fcheck[i]) {
            case F_TIMETAG:
                if (inp.begin == 0 && inp.end == 0) break;
                if (datum->time_tag < inp.begin || datum->time_tag > inp.end)
                                ret |= BADTIME;
                break;
            case F_PROCDATE:
                if (inp.proc_begin == 0 && inp.proc_end == 0) break;
                if (datum->procdate < inp.proc_begin || datum->procdate > inp.proc_end)
                                ret |= BADPROC;
                break;
            case F_STATION:
                if (strchr (inp.stations, datum->baseline[0]) == NULL ||
                        strchr (inp.stations, datum->baseline[1]) == NULL)
                                ret |= BADSTATION;
                break;
            case F_BASELINE:
                if (! smatch (inp.baselines, datum->baseline)) ret |= BADBASELN;
                break;
            case F_FREQUENCY:
                if (strchr (inp.frequencies, datum->freq_code) == NULL)
                                ret |= BADFREQ;
                break;
            case F_POLARIZATION:
                if (! smatch (inp.polarizations, datum->polarization)) ret |= BADPOL;
                break;
            case F_EXPERIMENT:
                if (inp.experiment != datum->expt_no) ret |= BADEXPT;
                break;
            case F_QCODE:
                if (strchr (inp.qcodes, mk3_qf (datum)) == NULL)
                                ret |= BADQF;
                break;
            case F_SOURCE:
                if (smatch (inp.sources, datum->source) != 2) ret |= BADSOURCE;
                break;
            case F_TYPE:
                if (strchr (inp.type, '2') == NULL) ret |= BADTYPE;
                break;
            case F_SNR:
                if (inp.snr[0] > datum->snr) ret |= BADSNR;
                if (inp.snr[1] < datum->snr) ret |= BADSNR;
                break;
            case F_LENGTH:
                if (inp.length > datum->length) ret |= BADLENGTH;
                break;
            case F_FRACTION:
                frac = datum->esdesp % 10;
                if ((inp.fraction > 0 && frac < inp.fraction) ||
                         (inp.fraction < 0 && frac > -(inp.fraction)))
                        ret |= BADFRAC;
                break;
            case F_NFREQ:
                nf = datum->no_freq;
                if ((nf < inp.nfreq[0]) || (nf > inp.nfreq[1]))
                        ret |= BADNFREQ;
                break;
            case F_PARAMETER:
                p0 = (int)(inp.parameter[0] + .01) - 1;
                                        /* Parameter filter must be out of date */
                if (p0 >= user_param.nparms)
                    {
                    inp.parameter[0] = 0.0;
                    break;
                    }
                                        /* No parameters for this datum */
                if (fdatum->param_ptr < 0) break;
                pval = user_param.parameter[p0][fdatum->param_ptr];
                if ((pval < inp.parameter[1]) || (pval > inp.parameter[2]))
                        ret |= BADPARAM;
                break;
            case F_TRIANGLE:
            case F_QUAD:
            case F_BSNR:
                break;

            default:
                msg ("error in ffilter.c", 2);
                return (ERROR);
            }                           /* End switch */

        if (mode == QUICK && ret != 0) return (ret);
        }
    return (ret);
}
