/************************************************************************/
/*                                                                      */
/* Given a clock def and a station parameter structure, this routine    */
/* extracts and stores all the relevant information                     */
/*                                                                      */
/*      Inputs:         dl              pointer to def_list struct      */
/*                      refdate         Scan reference time             */
/*                                                                      */
/*      Output:         stn             The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created January 9, 1998 by CJL                                       */
/* Rewritten for Haystack parser, 30 October 1998 by CJL                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_clock (struct def_list *dl,
          struct date *refdate,
          struct station_struct *stn)
    {
    int st, alltimes, clk_strt, this_clk_strt;
    int reftime, secs, start, stop;
    char *str;
    struct def *thisdef;
    struct date clock_date;
    struct param_val p_val;
    extern struct block blist[];
    extern struct statement *stlist;
    extern int ovex_ver;
                                        /* Def had better be there */
    thisdef = blist[dl->blockno].deflist + dl->defno;
    if (thisdef == NULL) return (1);
                                        /* Figure out the time range for */
                                        /* which we seek a clock entry */
    secs = rint ((double)refdate->second);
    reftime = time_to_int (refdate->year, refdate->day, refdate->hour,
                        refdate->minute, secs);
    start = reftime + stn->start_offset;
    stop = reftime + stn->stop_offset;
                                        /* Extract low level statements */
                                        /* in the def */
    alltimes = FALSE;
    clk_strt = 0;
    for (st=thisdef->start+1; st<thisdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "CLOCK", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("clock_early")
            {
            if (alltimes)
                {
                msg ("Internally inconsistent CLOCK def for station %s", 2,
                                stn->site_id);
                return (-1);
                }
                                        /* Look for start time of this clock */
                                        /* entry */
            if (p_val.dval[0].type != VAL_NONE)
                memcpy (&clock_date, &(p_val.dval[0].data.epochval),
                                sizeof (struct date));
            else 
                {
                if (clk_strt > 0)
                    {
                    msg ("Internally inconsistent CLOCK def for station %s",
                                        2, stn->site_id);
                    return (-1);
                    }
                alltimes = TRUE;
                }
                                        /* If we have a clock valid time, is it */
                                        /* the right one? */
            if (! alltimes)
                {
                secs = rint ((double)clock_date.second);
                this_clk_strt = time_to_int (clock_date.year,
                                                clock_date.day, clock_date.hour,
                                                clock_date.minute, secs);
                                        /* Too late, or before existing entry */
                if ((this_clk_strt > stop) || (this_clk_strt < clk_strt))
                    continue;
                else if (this_clk_strt > start)
                    {
                    msg ("Error, clock valid time in middle of scan", 2);
                    return (-1);
                    }
                else clk_strt = this_clk_strt;
                }
                                        /* Clock value mandatory */
            stn->clock_early = p_val.dval[1].data.realval;
                                        /* Other 2 subfields optional */
                                        /* rate and rate epoch go together */
            if (p_val.nval > 2)
                {
                if (p_val.nval != 4)
                    {
                    msg ("Clock rate and rate epoch go together", 2);
                    return (-1);
                    }
                memcpy (&(stn->clockrate_epoch), &(p_val.dval[2].data.epochval),
                                        sizeof (struct date));
                stn->clockrate = p_val.dval[3].data.realval;
                }
            }
        }
                                        /* Check that we got mandatory value */
    if (stn->clock_early == (float)F_UNDEFINED)
        {
        msg ("No valid clock_early statement for station '%c'.", 2, stn->mk4_site_id);
        return (-1);
        }

    msg ("Clock early value = %g for station %c", -1, 
                                stn->clock_early, stn->mk4_site_id);

    return (0);
    }
