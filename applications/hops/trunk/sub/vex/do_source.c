/************************************************************************/
/*                                                                      */
/* Fills in the source section of the scan structure, from a source def */
/*                                                                      */
/*      Inputs:         sourcedef       Pointer to the source def       */
/*                                                                      */
/*      Output:         src             Pointer to the source struct    */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 18 December 1997 by CJL                                      */
/* Rewritten for Haystack parser 29 October 1998 by CJL                 */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_source (struct def *sourcedef,
           struct source_struct *src)
    {
    int st;
    char *str, *srctyp;
    struct param_val p_val;
    extern struct statement *stlist;
    extern int ovex_ver;
                                        /* Loop over statements in def */
    for (st=sourcedef->start+1; st<sourcedef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "SOURCE", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }
        if ISNAME ("source_type")
            {
            srctyp = p_val.dval[0].data.strval;
            if (strcmp (srctyp, "quasar") == 0) src->source_type = QUASAR;
            else if (strcmp (srctyp, "star") == 0) src->source_type = STAR;
            else if (strcmp (srctyp, "earth_satellite") == 0)
                                                src->source_type = SATELLITE;
            else if (strcmp (srctyp, "dummy") == 0) src->source_type = DUMMY;
            else 
                {
                msg ("Source type '%s' unknown", 2, srctyp);
                return (-1);
                }
            if (p_val.nval == 2)
                {
                if (strcmp (p_val.dval[1].data.strval, "calibrator") == 0) 
                    src->calibrator = TRUE;
                else src->calibrator = FALSE;
                }
            }
        else if ISNAME ("IAU_name") 
            strcpy (src->iau_name, p_val.dval[0].data.strval);
        else if ISNAME ("source_name") 
            strcpy (src->source_name, p_val.dval[0].data.strval);
        else if ISNAME ("ra") 
            {
            src->position.ra_hrs = p_val.dval[0].data.raval.ra_hrs;
            src->position.ra_mins = p_val.dval[0].data.raval.ra_mins;
            src->position.ra_secs = p_val.dval[0].data.raval.ra_secs;
            }
        else if ISNAME ("dec") 
            {
            src->position.dec_degs = p_val.dval[0].data.decval.dec_degs;
            src->position.dec_mins = p_val.dval[0].data.decval.dec_mins;
            src->position.dec_secs = p_val.dval[0].data.decval.dec_secs;
            }
        else if ISNAME ("ref_coord_frame")
            {
            if (strcmp (p_val.dval[0].data.strval, "B1950") == 0)
                src->position_ref_frame = B1950;
            else if (strcmp (p_val.dval[0].data.strval, "J2000") == 0)
                src->position_ref_frame = J2000;
            else
                {
                msg ("Unrecognized reference frame specifier '%s'", 2,
                        p_val.dval[0].data.strval);
                return (-1);
                }
            }
        else if ISNAME ("ra_rate") src->ra_rate = p_val.dval[0].data.realval;
        else if ISNAME ("dec_rate") src->dec_rate = p_val.dval[0].data.realval;
        else if ISNAME ("source_position_epoch") 
            memcpy (&(src->position_epoch), &(p_val.dval[0].data.epochval), 
                                        sizeof (struct date));
        }

    return (0);
    }
