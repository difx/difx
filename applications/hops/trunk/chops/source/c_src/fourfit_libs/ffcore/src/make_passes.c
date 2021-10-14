/************************************************************************/
/*                                                                      */
/* Uses the control information in conjunction with the organized data  */
/* in order to set up new data arrays and control variables for         */
/* multiple passes through the fringe searching step.  This permits     */
/* all the fringe searching necessary for a single root/baseline to be  */
/* done with only one pass of data reading from disk, which makes       */
/* for efficient execution                                              */
/*                                                                      */
/* Created April 10 1992 by CJL                                         */
/* Overhauled for Mk4, April 23 1998 by CJL                             */
/* Increased length of source string, July 17 2001, CJL                 */
/* Generate multiple passes for multiple polar. prods.  rjc 2011.12.21  */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mk4_data.h"
#include "vex.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "control.h"

int
make_passes (
struct scan_struct *ovex,
struct freq_corel *corel,
struct type_param *param,
struct type_pass **pass,
int *npass)
    {
    char fgroups[10], baseline[3], source[32], group;
    struct freq_corel *fc;
    int i,j, k, sb, nsub, start_offset, stop_offset, nindices, usb, lsb, f,
        ngpt, n, m, ipass,igrp,
        scantime,f_c_index, pol, sbpol, ng, nsbind,     
        polprod_present[4];
                                        // table of last polarizations (for summing, etc.)
    int lptab[16] = {0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3};
    double pstart, cstart, cstop;
    static int pass_alloc = FALSE;
    struct type_pass *p;
    struct gptab_table
        {
        char fgr;
        int pol;
        int npols;
        } gptab[40];

                                        /* Integer format for scantime from */
                                        /* UTIL library */

    scantime = time_to_int (0, (int) ovex->start_time.day,
                               (int) ovex->start_time.hour,
                               (int) ovex->start_time.minute,
                               (int) ovex->start_time.second);

                                        /* Extract source */
    strncpy (source, ovex->src.source_name, 31);
    source[31] = '\0';
    i = 30;
    while (source[i] == ' ')            /* pad with nulls, rather than blanks */
        source[i--] = '\0';
                                        /* If control information excludes all */
                                        /* such data, no point reading it in */
                                        /* Generate list of all subgroups */
    fgroups[0] = '\0';
    ng = 0;
    for (i=0; i<MAXFREQ; i++)
        {                               /* Don't process empty freq. table entries */
//      if (corel[i].freq_code == ' ') 
        if (corel[i].frequency == 0.0) 
            continue;   
        group = corel[i].fgroup;
                                        /* Check if this subgroup even needed in */
                                        /* refringe mode */
        if ((param->rf_fglist != NULL) && (strchr (param->rf_fglist, group) == NULL)) 
            continue;

        if (skip_data (scantime, param->baseline, source, group))
            continue;
                                        /* Append only if not yet there */
        if (strchr (fgroups, group) == NULL) 
            {
            fgroups[ng++] = group;
            fgroups[ng] = '\0';
            }
        }
                                        // expand list of groups into (possible) multipasses
                                        // over used polarizations
                                        // for each freq group, figure out which
                                        // polarization products are present
    ngpt = 0;

    for (igrp=0; igrp<ng; igrp++)
        {
        if (param->pol)                 // non-zero param.pol is single pass
            {
            gptab[ngpt].fgr = fgroups[igrp];
            gptab[ngpt].npols = 0;
            if (param->pol == POL_IXY)
                {
                gptab[ngpt].npols = 4;
                gptab[ngpt++].pol = 3;  // combine all 4 pols in I mode
                }
            else                        // 1..15 is bit-encoded; count the set bits
                {
                for (j=0; j<4; j++)
                    if (param->pol & 1<<j) // if this is a requested pol product
                        gptab[ngpt].npols++;
                                        // for now, let pass pol be the last from the sum
                gptab[ngpt++].pol = lptab[param->pol];
                }
            }
        else                            // do one pass for each product present
            {
            for (j=0; j<4; j++)         // mark none used yet for this fgroup
                polprod_present[j] = FALSE;

            for (f=0; f<MAXFREQ; f++)   // search all frequency channels...
                                        // ...matching the current group
                if (corel[f].fgroup == fgroups[igrp])
                    for (j=0; j<4; j++)
                        if (polprod_present[j] == FALSE
                            && (corel[f].index[2*j] != 0 || corel[f].index[2*j+1] != 0))
                            {           // if either usb or lsb have data, add pass
                            gptab[ngpt].fgr = fgroups[igrp];
                            gptab[ngpt].npols = 1;
                            gptab[ngpt++].pol = j;
                            polprod_present[j] = TRUE;
                            }
            }
        }

                                        /* Loop over fgroups and polar products */
    *npass = 0;
    if (pass_alloc) 
        free (*pass);
                                        /* Make realloc() behave like malloc() */
    *pass = (struct type_pass *) malloc(1); 

    msg ("constructing %d passes over %d frequency groups", 0, ngpt, ng);
    for (ipass=0; ipass<ngpt; ipass++)
        {
                                        /* Allocate memory as required */
        *pass = (struct type_pass *)
            realloc (*pass, (*npass+1) * sizeof (struct type_pass));
        if (*pass == NULL)
            {
            msg ("Error allocating memory for passes", 2);
            pass_alloc = FALSE;
            return (1);
            }
        else 
            pass_alloc = TRUE;
                                        /* Convenience pointers */
        p = *pass + *npass;
        fc = p->pass_data;
                                        /* realloc() does not initialize */
        for (j=0; j<MAXFREQ; j++) 
            p->pass_data[j].data_alloc = FALSE;
        clear_pass (p);
        for (j=0; j<4; j++)
            p->pprods_present[j] = 0;

        p->pol = gptab[*npass].pol;
        p->npols = gptab[*npass].npols;
                                        /* Fill in cblock for this pass... */
                                        /* first have to fill in subgroup info */
        fc->fgroup = gptab[ipass].fgr;
        if (generate_cblock (ovex, p) != 0)
            {
            msg ("Failure generating cblock, skipping pass",2);
            continue;
            }
                                        /* Now sort out timerange for this pass */
                                        /* Note that cstart/cstop are true times */
                                        /* that correspond to the values in the */
                                        /* pass structure, which in Mk4 are the */
                                        /* same as appear in the param structure */

        cstart = (p->control.time_span[0] < 0)?      /* kludge: negative t means */
            param->start_nom - (double)p->control.time_span[0]: /* relative time */
            86400 *(ovex->start_time.day - 1)         /* else it's absolute time */
           + 3600 * ovex->start_time.hour
           + (double)p->control.time_span[0];
           
        cstop  = (p->control.time_span[1] < 0)?
            param->stop_nom + (double)p->control.time_span[1]:      /* relative */
            86400 *(ovex->start_time.day - 1)
           + 3600 * ovex->start_time.hour
           + (double)p->control.time_span[1];                        /* absolute */

        if (cstart < param->start)
            cstart = param->start;
        if (cstop > param->stop)
            cstop = param->stop;
                                        /* Actual start is beginning of 1st ap */
                                        /* .001 to avoid rejection of aps when */
                                        /* cstart/cstop right on ap boundary */
        pstart = param->start;
        msg ("pstart %10f cstart %10f cstop %10f", 0, pstart, cstart, cstop);
        if (pstart == cstart)
            start_offset = 0;
        else
            start_offset = (int)((cstart - pstart) 
                                / param->acc_period + .001);
        stop_offset = (int)((cstop - pstart) / param->acc_period + .001);
        p->num_ap = stop_offset - start_offset;
        p->ap_off = start_offset;
        msg ("num_ap %d ap_off %d", 0, p->num_ap, p->ap_off);
                                        /* Autocorrelation? */
        if ((param->cormode == AUTO_PER_LAG) || (param->cormode == AUTO_GLOBAL)) 
            p->autocorr = TRUE;
                                        /* No data, skip this pass */
        if (p->num_ap == 0) continue;
                                        /* True start/stop times */
        p->start = param->start + (start_offset * param->acc_period);
        p->stop = param->start + (stop_offset * param->acc_period);
        p->reftime = param->reftime;

        for (n=0; n<2; n++)  
            p->linpol[n] = 0;

        for (j=0; j<MAXFREQ; j++)     /* Loop over frequencies */
            {
                                        /* Pluck out freqs with matching fgroups */ 
                                        /* that are in frequency list, if it's there */
            if (corel[j].fgroup != gptab[ipass].fgr || corel[j].frequency == 0.0)
                continue;

                                        // assign single-letter freq code appropriate for
                                        // this freq & b-line, overwrite if so directed
            if (p->control.chid_rf[0] != 0.0)
                for (k=0; k<strlen(p->control.chid); k++)
                    {
                    if (fabs (corel[j].frequency - p->control.chid_rf[k]) < 0.01)
                        {
                        corel[j].freq_code = p->control.chid[k];
                        msg("corel freq code %c overwrite for freq %d",1, corel[j].freq_code,j);
                        break;
                        }
                    else
                        {
                        double diff = fabs(corel[j].frequency - p->control.chid_rf[k]);
                        if (diff < 0.1) msg("corel freq code %c miss: |%f - %f| = %f", 1,
                            p->control.chid[k], corel[j].frequency, p->control.chid_rf[k], diff);
                        }
                    }
            else
                corel[j].freq_code = p->control.chid[j];



                                        /* Copy freq_corel struct into this pass */
            msg ("[make_passes] corel[j].freq_code %c",-3,corel[j].freq_code);
            fc->freq_code = corel[j].freq_code;
            f_c_index = j;
            for (k=0; k<strlen(p->control.chid); k++)
                if (corel[j].freq_code == p->control.chid[k])
                    f_c_index = k;
            fc->frequency = corel[j].frequency;
                                        // loop over ref & rem stations
            for (n=0; n<2; n++)  
                {
                fc->ch_idx[n] = -1;     // link to original ovex channel #
                for (m=0; m<MAX_CHAN; m++)
                    if (fabs(1e6*fc->frequency 
                             - (ovex->st+param->ov_bline[n])->channels[m].sky_frequency) < 1e-3
                        && (fc->ch_idx[n] < 0 || 
                            (ovex->st+param->ov_bline[n])->channels[m].net_sideband == 'U'))

                        fc->ch_idx[n] = m;
                if (fc->ch_idx[n] < 0)
                    {
                    fc->ch_idx[n] = 0;
                    msg ("couldn't find frequency %c, pcal may be wrong",2,fc->freq_code);
                    }

                strncpy (fc->ch_usb_lcp[n], corel[j].ch_usb_lcp[n], 8);
                strncpy (fc->ch_usb_rcp[n], corel[j].ch_usb_rcp[n], 8);
                strncpy (fc->ch_lsb_lcp[n], corel[j].ch_lsb_lcp[n], 8);
                strncpy (fc->ch_lsb_rcp[n], corel[j].ch_lsb_rcp[n], 8);

                for (k=0; k<16; k++)
                    {
                    fc->trk_lcp[n][k] = corel[j].trk_lcp[n][k];
                    fc->trk_rcp[n][k] = corel[j].trk_rcp[n][k];
                    msg ("trk_lcp[%d][%d] %d  _rcp %d", -3,
                         n,k,fc->trk_lcp[n][k], fc->trk_rcp[n][k]);
                    fc->mean_lcp_trk_err[n][k] = corel[j].mean_lcp_trk_err[n][k];
                    fc->mean_rcp_trk_err[n][k] = corel[j].mean_rcp_trk_err[n][k];
                    }
                for (k=0; k<MAX_PCF; k++)
                    fc->pc_freqs[n][k] = corel[j].pc_freqs[n][k];
                
                fc->bbc_lcp[n] = corel[j].bbc_lcp[n];
                fc->bbc_rcp[n] = corel[j].bbc_rcp[n];
                p->linpol[n] |= (ovex->st+param->ov_bline[n])->channels[j].polarization == 'X'
                             || (ovex->st+param->ov_bline[n])->channels[j].polarization == 'Y';
                }
            nindices = 0;
            usb = lsb = FALSE;
            for (sb = 0; sb < 2; sb++)
              {
              nsbind = 0;
              for (pol=0; pol<4; pol++)
                {                       /* skip sideband or index, if not desired */
                sbpol = sb + 2*pol;
                msg("sb=%d, p->a_s[fci]=%d, fci=%d, skip_index=%d", -1, 
                    sb, p->control.accept_sbs[f_c_index], f_c_index, 
                    skip_index (corel[j].index[sbpol], &(p->control)));
                
                if (((sb == 0) && ((p->control.accept_sbs[f_c_index] & USB) == 0))
                 || ((sb == 1) && ((p->control.accept_sbs[f_c_index] & LSB) == 0))
                 || (skip_index (corel[j].index[sbpol], &(p->control))))
                    continue;
                                        /* Presence of valid index number */
                                        /* causes corresponding data in ap */
                                        /* records to be included in fit */
                fc->index[sbpol] = corel[j].index[sbpol];
                                        /* Count valid indices */
                if (fc->index[sbpol] > 0) 
                    {
                    nindices++;
                    nsbind++;
                    }
                }
                                        /* Keep count of "channels" included */
                                        /* THIS NEEDS CLOSER EXAMINATION */
              if (nsbind > 0) 
                  p->channels++;
              }
                                        /* If no index numbers for this */
                                        /* frequency pass skip_index(), */
                                        /* do not insert freq at all */
            if (nindices == 0) 
                continue;
            fc->data = corel[j].data;
            fc++;
            p->nfreq++;
            msg ("Pass %d, freq = %f", -1, *npass + 1, corel[j].frequency);
            }                           /* Don't waste time with absent data */
        if (p->nfreq == 0) 
            continue;
                                        /* This pass OK, proceed */
        (*npass)++;
        }
    msg ("p->nfreq == %d  p->npols == %d", 1, p->nfreq, p->npols);

    return (0);
    }
