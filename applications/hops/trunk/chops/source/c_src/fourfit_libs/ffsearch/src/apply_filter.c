/************************************************************************/
/*                                                                      */
/* Eventually will perform sophisticated filtering functions.  For now  */
/* simply applies basic checks for bad data, similar to FRNGE defaults  */
/*                                                                      */
/* Created 21 April 1992 by CJL                                         */
/* Modified 25 Aug. 1992 by RJC to use control structure                */
/* Stripped to virtual no-op for Mk4 pending further development        */
/*                      CJL, 11 May 1998                                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "control.h"
#include "filter.h"
#include "statistics.h"

struct type_filter filter;
struct type_statistics statistics;

int apply_filter (struct type_pass *pass)
    {
    struct type_120 *record;
    struct data_corel *datum;
    extern struct type_param param;
    float xperror, yperror, frq_x_perr[8], frq_y_perr[8], tot_x_perr, tot_y_perr;
    int totbits, pcalbits, totacc, flagbit;
    int pol, sbpol, frq, ap, sb, mk3a;

    msg ("Number of AP's: %d",-1,pass->num_ap);
                                        /* Initialize counts to 0 for each pass */
    filter.absent     = 0;
    filter.xperror    = filter.yperror     = 0;
    filter.ndiscard   = filter.ap_mid_miss = 0;
    filter.emasked    = 0;
    filter.suid[0]    = filter.suid[1]     = 0;
    filter.chid[0]    = filter.chid[1]     = 0;
    filter.chksum[0]  = filter.chksum[1]   = 0;
    filter.cfnum[0]   = filter.cfnum[1]    = 0;
    filter.badd[0]    = filter.badd[1]     = 0;
    filter.zero[0]    = filter.zero[1]     = 0;
    filter.link[0]    = filter.link[1]     = 0;
    filter.ib_sync[0] = filter.ib_sync[1]  = 0;

    tot_x_perr = tot_y_perr = 0.0;
    totacc = 0;
                                        /* Init statistics arrays */
    for (frq=0; frq<pass->nfreq; frq++)
        for (sb=0; sb<2; sb++)
            {
            statistics.frq_x_perror[frq][sb] = 0.0;
            statistics.frq_y_perror[frq][sb] = 0.0;
            statistics.frq_acc[frq][sb] = 0;
            }
    statistics.ercc = 0;
                                        /* Loop over all freqs, ap's, sbands */
                                        /* (i.e. all data for this pass) */
    for (frq = 0; frq < pass->nfreq; frq++)
        {
        msg ("%f: %d %d %d %d %d %d %d %d", -1, pass->pass_data[frq].frequency,
                pass->pass_data[frq].index[0], pass->pass_data[frq].index[1],
                pass->pass_data[frq].index[2], pass->pass_data[frq].index[3],
                pass->pass_data[frq].index[4], pass->pass_data[frq].index[5],
                pass->pass_data[frq].index[6], pass->pass_data[frq].index[7]);

        for (sb=0; sb<2; sb++)
            {
            frq_x_perr[sb] = 0.0;
            frq_y_perr[sb] = 0.0;
            }
        for (ap = pass->ap_off; ap < pass->ap_off + pass->num_ap; ap++)
            {
            datum = pass->pass_data[frq].data + ap;
                                        /* datum->flag has already been set according */
                                        /* to presence of data, by set_pointers() */
                                        /* The code below must unset the appropriate */
                                        /* flag bit in response to various filtering */
                                        /* conditions */
                                        /* Switched mode, if gate is off, set */
                                        /* flag null and skip this ap */
            if (pass->control.switched_mode)
                if (gate_off (pass, frq, ap)) 
                    {
                    datum->flag = 0;
                    continue;
                    }
                                        /* Loop over all sideband/pol combinations */
                                        /* for this AP */
            for (sb=0; sb<2; sb++)
                {
                for (pol=0; pol<4; pol++)
                    {
                    sbpol = sb + 2*pol;
                    flagbit = 1<<sbpol;
                                        /* Check data are supposed to be there */
                                        /* and if not, flag it */
                    if (pass->pass_data[frq].index[sbpol] == 0) 
                        {
                        datum->flag &= ~flagbit;
                        continue;
                        }
                    switch (pol)
                        {
                        case 0:
                            record = pass->pass_data[frq].data[ap].apdata_ll[sb];
                            break;
                        case 1:
                            record = pass->pass_data[frq].data[ap].apdata_rr[sb];
                            break;
                        case 2:
                            record = pass->pass_data[frq].data[ap].apdata_lr[sb];
                            break;
                        case 3:
                            record = pass->pass_data[frq].data[ap].apdata_rl[sb];
                            break;
                        default:
                            break;
                        }
                                        /* Sanity check - does the record exist? */
                    if (record == NULL)
                        {
                        datum->flag &= ~flagbit;
                        filter.absent++;
                        continue;
                        }
                                        /* trap for any flag bit set; should use
                                         * a parameter to mask the bits  rjc 2000.6.28 */
                    param.error_mask = E_MISSED_AP_MIDS;
                    if (record->status & param.error_mask)
                        {
                        datum->flag &= ~flagbit;
                        filter.emasked++;
                        filter.ndiscard++;
                        }
                                       /* update counters for various error conditions */

                    if (record->status & E_MISSED_CF_SERV)
                        filter.cf_service++;
                                       /* keep track of number of AP's w/ missed mids */
                    if (record->status & E_MISSED_AP_MIDS)
                        filter.ap_mid_miss++;
                                       
                    if (record->status & E_TAP_OVERRUN)
                        filter.tap_overrun++;
                                       
                    if (record->status & E_DLY_3_CARRY)
                        filter.triple_carries++;
                                       
                    if (record->status & E_REF_SU_ID)
                        filter.suid[0]++;
                                       
                    if (record->status & E_REM_SU_ID)
                        filter.suid[1]++;
                                       
                    if (record->status & E_REF_CHN_ID)
                        filter.chid[0]++;
                                       
                    if (record->status & E_REM_CHN_ID)
                        filter.chid[1]++;
                                        /* Keep track of header checksum errors */
                    if (record->status & E_REF_CHKSUM)
                        filter.chksum[0]++;
                                       
                    if (record->status & E_REM_CHKSUM)
                        filter.chksum[1]++;
                                       
                    if (record->status & E_REF_CFNUM)
                        filter.cfnum[0]++;
                                       
                    if (record->status & E_REM_CFNUM)
                        filter.cfnum[1]++;
                                       
                    if (record->status & E_REF_BADD)
                        filter.badd[0]++;
                                       
                    if (record->status & E_REM_BADD)
                        filter.badd[1]++;
                                       /* keep track of number of AP's w/ 0 headers */
                    if (record->status & E_REF_ZERO)
                        filter.zero[0]++;
                                       
                    if (record->status & E_REM_ZERO)
                        filter.zero[1]++;
                                       
                    if (record->status & E_REF_LINK)
                        filter.link[0]++;
                                       
                    if (record->status & E_REM_LINK)
                        filter.link[1]++;
                                       
                    if (record->status & E_REF_SYNC)
                        filter.ib_sync[0]++;
                                       
                    if (record->status & E_REM_SYNC)
                        filter.ib_sync[1]++;
                                       
                                        /* This AP passed all filtering, count it */
                    if (datum->flag & flagbit) totacc++;
                    }
                }
            }   /* End ap loop */
        }       /* End frq loop */

    msg ("Filtering: %d absent records", 1, filter.absent);
    msg ("Filtering: %d records deleted by error_mask", 1, filter.emasked);
    msg ("Filtering: %d cf service missed", 1, filter.cf_service);
    msg ("Filtering: %d middle CF of AP missing records", 1, filter.ap_mid_miss);
    msg ("Filtering: %d tap overruns", 1, filter.tap_overrun);
    msg ("Filtering: %d triple carries", 1, filter.triple_carries);
    msg ("Filtering: %d ref %d rem SU ID errors", 1, filter.suid[0], filter.suid[1]);
    msg ("Filtering: %d ref %d rem channel ID errors", 1, filter.chid[0], filter.chid[1]);
    msg ("Filtering: %d ref %d rem checksum errors", 1, filter.chksum[0], filter.chksum[1]);
    msg ("Filtering: %d ref %d rem cfnum errors", 1, filter.cfnum[0], filter.cfnum[1]);
    msg ("Filtering: %d ref %d rem badd headers", 1, filter.badd[0], filter.badd[1]);
    msg ("Filtering: %d ref %d rem zero headers", 1, filter.zero[0], filter.zero[1]);
    msg ("Filtering: %d ref %d rem link errors", 1, filter.link[0], filter.link[1]);
    msg ("Filtering: %d ref %d rem in_brd sync errors", 1, filter.ib_sync[0], filter.ib_sync[1]);
    msg ("Filtering: %d accepted records", 1, totacc);
                                        /* Calculate overall p.error rates */
    if (totacc == 0)
        {
        msg ("No data passed filtering! error.", 2);
        return (1);
        }
    statistics.xperror = tot_x_perr / totacc;
    statistics.yperror = tot_y_perr / totacc;
    return (0);
    }
