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
make_passes (ovex, corel, param, pass, npass)
struct scan_struct *ovex;
struct freq_corel *corel;
struct type_param *param;
struct type_pass **pass;
int *npass;
    {
    char fgroups[10], baseline[3], source[32], group;            
    struct freq_corel *fc;
    int i, j, k, sb, nsub, start_offset, stop_offset, nindices, usb, lsb, f, ngpt;
    int scantime,f_c_index, pol, sbpol, ng, nsbind;     
    int polprod_present[4];
                                        // table of last polarizations (for summing, etc.)
    int lptab[16] = {0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3};
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
        if (corel[i].freq_code == ' ') 
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

    for (i=0; i<ng; i++)
        {
        if (param->pol)                 // non-zero param.pol is single pass
            {
            gptab[ngpt].fgr = fgroups[i];
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
                if (corel[f].fgroup == fgroups[i])
                    for (j=0; j<4; j++)
                        if (polprod_present[j] == FALSE
                            && (corel[f].index[2*j] != 0 || corel[f].index[2*j+1] != 0))
                            {           // if either usb or lsb have data, add pass
                            gptab[ngpt].fgr = fgroups[i];
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
    for (i=0; i<ngpt; i++)
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

        p->pol = gptab[*npass].pol;
        p->npols = gptab[*npass].npols;
                                        /* Fill in cblock for this pass... */
                                        /* first have to fill in subgroup info */
        fc->fgroup = gptab[i].fgr;
        if (generate_cblock (ovex, param, p) != 0)
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

        for (j=0; j<MAXFREQ; j++)     /* Loop over frequencies */
            {
                                        /* Pluck out freqs with matching fgroups */ 
                                        /* that are in frequency list, if it's there */
            if (corel[j].fgroup != gptab[i].fgr || corel[j].freq_code == ' ')
                continue;
                                        /* Copy freq_corel struct into this pass */
            msg ("[make_passes] corel[j].freq_code %c",-3,corel[j].freq_code);
            fc->freq_code = corel[j].freq_code;
            f_c_index = fcode(corel[j].freq_code);   
            fc->frequency = corel[j].frequency;
            strncpy (fc->ch_usb_lcp[0], corel[j].ch_usb_lcp[0], 8);
            strncpy (fc->ch_usb_lcp[1], corel[j].ch_usb_lcp[1], 8);
            strncpy (fc->ch_usb_rcp[0], corel[j].ch_usb_rcp[0], 8);
            strncpy (fc->ch_usb_rcp[1], corel[j].ch_usb_rcp[1], 8);
            strncpy (fc->ch_lsb_lcp[0], corel[j].ch_lsb_lcp[0], 8);
            strncpy (fc->ch_lsb_lcp[1], corel[j].ch_lsb_lcp[1], 8);
            strncpy (fc->ch_lsb_rcp[0], corel[j].ch_lsb_rcp[0], 8);
            strncpy (fc->ch_lsb_rcp[1], corel[j].ch_lsb_rcp[1], 8);
            for (k=0; k<16; k++)
                {
                fc->trk_lcp[0][k] = corel[j].trk_lcp[0][k];
                fc->trk_lcp[1][k] = corel[j].trk_lcp[1][k];
                fc->trk_rcp[0][k] = corel[j].trk_rcp[0][k];
                fc->trk_rcp[1][k] = corel[j].trk_rcp[1][k];
                fc->mean_lcp_trk_err[0][k] = corel[j].mean_lcp_trk_err[0][k];
                fc->mean_lcp_trk_err[1][k] = corel[j].mean_lcp_trk_err[1][k];
                fc->mean_rcp_trk_err[0][k] = corel[j].mean_rcp_trk_err[0][k];
                fc->mean_rcp_trk_err[1][k] = corel[j].mean_rcp_trk_err[1][k];
                }
            for (k=0; k<MAX_PCF; k++)
                {
                fc->pc_freqs[0][k] = corel[j].pc_freqs[0][k];
                fc->pc_freqs[1][k] = corel[j].pc_freqs[1][k];
                }
            fc->bbc_lcp[0] = corel[j].bbc_lcp[0];
            fc->bbc_lcp[1] = corel[j].bbc_lcp[1];
            fc->bbc_rcp[0] = corel[j].bbc_rcp[0];
            fc->bbc_rcp[1] = corel[j].bbc_rcp[1];
            nindices = 0;
            usb = lsb = FALSE;
            for (sb = 0; sb < 2; sb++)
              {
              nsbind = 0;
              for (pol=0; pol<4; pol++)
                {                       /* skip sideband or index, if not desired */
                sbpol = sb + 2*pol;
                msg("sb=%d, p->cf[fci]=%d, fci=%d, skip_index=%d", -1, 
                    sb, p->control.frequency[f_c_index], f_c_index, 
                    skip_index (corel[j].index[sbpol], &(p->control)));
                
                if (((sb == 0) && ((p->control.frequency[f_c_index] & USB) == 0))
                 || ((sb == 1) && ((p->control.frequency[f_c_index] & LSB) == 0))
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
