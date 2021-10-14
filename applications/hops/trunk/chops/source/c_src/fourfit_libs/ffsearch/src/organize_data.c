/*******************************************************************************/
/*                                                                             */
/* This routine fills in the pointers to corel records in the main time vs     */
/* frequency array.  It figures out the time extrema, filters the data for     */
/* invalid time tags, and puts values for the sky frequencies in the param     */
/* arrays.  It then calls set_pointers() to unscramble the data records that   */
/* were read in arbitrary order.                                               */
/*                                                                             */
/* Original version  CJL August 5 1991 as set_data_pointers.c                  */
/* Redesigned for greater modularity and renamed by CJL, April 8 1992          */
/* Rewritten completely for Mk4 by CJL, April 7 1998                           */
/*                                                                             */
/*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"

int 
organize_data (
struct mk4_corel *cdata,
struct scan_struct *ovex,
struct ivex_struct *ivex,
struct mk4_sdata *sdata,
struct freq_corel *corel,
struct type_param *param,
struct type_status *status,
struct c_block *cb_head
)
    {
    //extern struct type_param param;
    extern int do_accounting;
    struct station_struct *stn1, *stn2;
    struct mk4_sdata *sd1, *sd2;
    int i;
    char st1, st2;
                                        /* Get the station structs */
    st1 = cdata->t100->baseline[0];
    st2 = cdata->t100->baseline[1];
    stn1 = stn2 = NULL;
    for (i=0; i<ovex->nst; i++)
        {
        if (st1 == ovex->st[i].mk4_site_id) 
            {
            stn1 = ovex->st + i;
            param->ov_bline[0] = i;
            }
        if (st2 == ovex->st[i].mk4_site_id)
            {
            stn2 = ovex->st + i;
            param->ov_bline[1] = i;
            }
        }
    if ((stn1 == NULL) || (stn2 == NULL))
        {
        if (stn1 == NULL) msg ("Could not find station '%c' in ovex", 2, st1);
        if (stn2 == NULL) msg ("Could not find station '%c' in ovex", 2, st2);
        return (-1);
        }
                                        /* Fill in as much of the param struct */
                                        /* as we can at this point */
    if (fill_param (ovex, ivex, stn1, stn2, cdata, param, cb_head) != 0)
        {
        msg ("Error filling param structure", 2);
        return (-1);
        }
                                        /* Find time extrema.  Also figures */
                                        /* out number of accumulation periods, */
                                        /* and the fourfit reference time */
                                        /* Results go into param structure */
    if (time_range (ovex, stn1, stn2, cdata, param) != 0)
        {
        msg ("Error in time_range(), file %s, baseline %c%c", 2,
                                ovex->filename, st1, st2);
        return (-1);
        }
                                        /* Set up the frequency axis of main */
                                        /* time/freq array, based on root and */
                                        /* cdata contents, then fill in pointers */
                                        /* in corel structure, to generate a nicely */
                                        /* ordered time vs frequency array */
                                        /* Failure (partial or complete) is */
                                        /* indicated by null ptrs in main array */
    if (set_pointers (stn1, stn2, cdata, param, corel) != 0)
        {
        msg ("set_pointers() fails", 2);
        return (-1);
        }
                                        /* Interpolate various quantities from */
                                        /* sdata files */
    for (i=0; i<MAXSTATIONS; i++)
        {
        if (sdata[i].id == NULL) continue;
        if (sdata[i].t300->id == st1) 
            {
            sd1 = sdata + i;
            msg ("Found station 1 sdata (%c)", 0, st1);
            }
        if (sdata[i].t300->id == st2) 
            {
            sd2 = sdata + i;
            msg ("Found station 2 sdata (%c)", 0, st2);
            }
        }
    if (do_accounting) account ("Organize data");
    if (stcount_interp (sd1, sd2, param, corel, status) != 0)
        {
        msg ("Error interpolating state count information.", 2);
        return (-1);
        }
    if (do_accounting) account ("STcount interp");
    if (pcal_interp (sd1, sd2, param, corel, cdata) != 0)
        {
        msg ("Error interpolating phasecal information.", 2);
        return (-1);
        }
    if (do_accounting) account ("PCal interp");
                                        /* Record the station unit numbers */
    param->su_number[0] = sd1->t300->SU_number;
    param->su_number[1] = sd2->t300->SU_number;
                                        // insert appropriate parallactic angles
                                        // FIXME - should evaluate at frt
    // there are 6 coefficients for the coefficients of
    // progressively higher powers of dt = now - start of scan
    // see compute_model.c for sample calculations.
    if (sd1->model[0].t303[0] != NULL)
        {
        param->par_angle[0] =
            sd1->model[0].t303[0]->parallactic_angle[0] / 180.0 * M_PI;
        param->elevation[0] =
            sd1->model[0].t303[0]->elevation[0] / 180.0 * M_PI;
        }
    else
        {
        param->par_angle[0] = 0.0;
        param->elevation[0] = 0.0;
        }

    if (sd2->model[0].t303[0] != NULL)
        {
        param->par_angle[1] =
            sd2->model[0].t303[0]->parallactic_angle[0] / 180.0 * M_PI;
        param->elevation[1] =
            sd2->model[0].t303[0]->elevation[0] / 180.0 * M_PI;
        }
    else
        {
        param->par_angle[1] = 0.0;
        param->elevation[1] = 0.0;
        }
  
    return(0);
    }
