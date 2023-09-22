/************************************************************************/
/*                                                                      */
/* Given a station def and an unfilled station parameter structure,     */
/* this routine descends the VEX hierarchy as needed to extract all     */
/* the parameters pertaining to the station.                            */
/*                                                                      */
/*      Inputs:         deflist         List of defs for this station   */
/*                      ndef            Number of defs in list          */
/*                      refdate         scan time (for clock_parms())   */
/*                                                                      */
/*      Output:         st              The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created December 29, 1997 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define IFNOT(blockname)\
        if (strcmp (blist[deflist[i].blockno].name, blockname) != 0)

int
fill_station_parms (struct def_list *deflist,
                    int ndefs,
                    struct date *refdate,
                    struct station_struct *st)
    {
    int i, nchan;
    int site_present,
        freq_present,
        bbc_present,
        if_present,
        clock_present;
    extern struct block blist[];
                                        /* Do a sanity check */
    if (st->start_offset >=  st->stop_offset)
        {
        msg ("Station stop time invalid %d>=%d",
            2, st->start_offset, st->stop_offset);
        return (1);
        }
                                        /* Now time to start digging into the */
                                        /* list of defs assembled for this */
                                        /* station */
                                        /* SITE */
    site_present = FALSE;
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("SITE") continue;
        if (do_site (deflist + i, st) != 0) return (1);
        site_present = TRUE;
        }
    if (!site_present)
        {
        msg ("missing SITE section for station %c", 2, st->mk4_site_id);
        return (1);
        }
                                        /* To resolve links properly, it turns */
                                        /* out that we must do the $FREQ defs */
                                        /* before several others, so we do */
                                        /* them early */
    nchan = 0;
    freq_present = FALSE;
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("FREQ") continue;
        if (do_freq (deflist + i, st, &nchan) != 0) return (1);
        freq_present = TRUE;
        }
    if (!freq_present)
        {
        msg ("missing FREQ section for station %c", 2, st->mk4_site_id);
        return (1);
        }
                                        /* Next must do BBC and links to IF */
    bbc_present = FALSE;
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("BBC") continue;
        if (do_bbc (deflist + i, nchan, st) != 0) return (1);
        bbc_present = TRUE;
        }
    if (!bbc_present)
        {
        msg ("missing BBC section for station %c", 2, st->mk4_site_id);
        return (1);
        }
                                        /* Continue with channel-dependent ones */
                                        /* IF */
    if_present = FALSE;
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("IF") continue;
        if (do_if (deflist + i, nchan, st) != 0) return (1);
        if_present = TRUE;
        }
    if (!if_present)
        {
        msg ("missing IF section for station %c", 2, st->mk4_site_id);
        return (1);
        }
                                        /* PHASE_CAL_DETECT */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("PHASE_CAL_DETECT") continue;
        if (do_phase_cal_detect (deflist + i, nchan, st) != 0) 
            return (1);
        }
                                        /* TRACKS */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("TRACKS") continue;
        if (do_track (deflist + i, nchan, st) != 0) return (1);
        }
                                        /* Now can do them in any order */
                                        /* ANTENNA */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("ANTENNA") continue;
        if (do_antenna (deflist + i, st) != 0) return (1);
        }
                                        /* CLOCK */
    clock_present = FALSE;
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("CLOCK") continue;
        if (do_clock (deflist + i, refdate, st) != 0) return (1);
        clock_present = TRUE;
        }
    if (!clock_present)
        {
        msg ("missing CLOCK section for station %c", 2, st->mk4_site_id);
        return (1);
        }
                                        /* DAS */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("DAS") continue;
        if (do_das (deflist + i, st) != 0) return (1);
        }
                                        /* HEAD_POS */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("HEAD_POS") continue;
        if (do_head_pos (deflist + i, st) != 0) return (1);
        }
                                        /* PASS_ORDER */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("PASS_ORDER") continue;
        if (do_pass_order (deflist + i, st) != 0) return (1);
        }
                                        /* ROLL */
    for (i=0; i<ndefs; i++)
        {
        IFNOT ("ROLL") continue;
        if (do_roll (deflist + i, st) != 0) return (1);
        }
                                        /* TAPELOG_OBS */
                                        /* (Not used for initial ops) */
/*  for (i=0; i<ndefs; i++)
        {
        IFNOT ("TAPELOG_OBS") continue;
        if (do_tapelog_obs (deflist + i, st) != 0) return (1);
        } */

    return (0);
    }
