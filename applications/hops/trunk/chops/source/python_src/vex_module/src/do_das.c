/************************************************************************/
/*                                                                      */
/* Given a das def and a station parameter structure, this routine      */
/* extracts and stores all the relevant information                     */
/*                                                                      */
/*      Inputs:         defname         The das def in question         */
/*                      vex             The main vex file               */
/*                                                                      */
/*      Output:         st              The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created January 9, 1998 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
do_das (struct def_list *dl,
        struct station_struct *stn)
    {
    int st;
    char *str, *ptr;
    struct def *thisdef;
    struct param_val p_val;
    extern struct block blist[];
    extern struct statement *stlist;
    extern int ovex_ver;
                                        /* Def had better be there */
    thisdef = blist[dl->blockno].deflist + dl->defno;
    if (thisdef == NULL) return (1);
                                        /* Extract low level statements */
                                        /* in the def */
    for (st=thisdef->start+1; st<thisdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "DAS", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("record_density")
            stn->record_density = p_val.dval[0].data.realval;
        else if ISNAME ("recording_system_ID")
            stn->recorder_id = p_val.dval[0].data.intval;
        else if ISNAME ("record_transport_type")
            {
            ptr = p_val.dval[0].data.strval;
            if (strcmp (ptr, "Mark3A") == 0)      stn->recorder_type = MARK3A;
            else if (strcmp (ptr, "Mark4") == 0)  stn->recorder_type = MARK4;
            else if (strcmp (ptr, "VLBA") == 0)   stn->recorder_type = VLBA;
            else if (strcmp (ptr, "VLBAG") == 0)  stn->recorder_type = VLBAG;
            else if (strcmp (ptr, "S2") == 0)     stn->recorder_type = S2;
            else if (strcmp (ptr, "K4") == 0)     stn->recorder_type = K4;
            else if (strcmp (ptr, "Mark5A") == 0) stn->recorder_type = MARK5A;
            else if (strcmp (ptr, "Mark5B") == 0) stn->recorder_type = MARK5B;
            }
        else if ISNAME ("electronics_rack_type")
            {
            ptr = p_val.dval[0].data.strval;
            if (strcmp (ptr, "Mark3A") == 0)     stn->rack_type = MARK3A;
            else if (strcmp (ptr, "Mark4") == 0) stn->rack_type = MARK4;
            else if (strcmp (ptr, "VLBA") == 0)  stn->rack_type = VLBA;
            else if (strcmp (ptr, "VLBAG") == 0) stn->rack_type = VLBAG;
            else if (strcmp (ptr, "S2") == 0)    stn->rack_type = S2;
            else if (strcmp (ptr, "K4") == 0)    stn->rack_type = K4;
            }
        else if ISNAME ("tape_length")
            stn->tape_length = p_val.dval[0].data.realval;
        else if ISNAME ("tape_motion")
            {
            ptr = p_val.dval[0].data.strval;
            if (strcmp (ptr, "start_stop") == 0) stn->tape_motion = START_STOP;
            else if (strcmp (ptr, "adaptive") == 0) stn->tape_motion = ADAPTIVE;
            else if (strcmp (ptr, "continuous") == 0) stn->tape_motion = CONTINUOUS;

            if ((stn->tape_motion == START_STOP)
                        || (stn->tape_motion == ADAPTIVE))
                stn->early_start - p_val.dval[1].data.realval;
            }
        }

    return (0);
    }
