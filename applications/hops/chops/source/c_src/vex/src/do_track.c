/************************************************************************/
/*                                                                      */
/* Given a track def and a station parameter structure, this routine    */
/* extracts and stores all the relevant information                     */
/*                                                                      */
/*      Inputs:         dl              pointer to def_list struct      */
/*                      nchan           Number of st channels present   */
/*                                                                      */
/*      Output:         stn             The station struct to be filled */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created January 12, 1998 by CJL                                      */
/* Rewritten for Haystack parser, 30 October 1998 by CJL                */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)
#define SIGN 1
#define MAG 2

int
do_track (struct def_list *dl,
          int nchan,
          struct station_struct *stn)
    {
    int i, j, st, found, type, hdstk, index, track;
    char *str, *link, *ptr;
    struct def *thisdef;
    struct chan_struct *ch;
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
        if (parse_pval (str, "TRACKS", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            return (-1);
            }

        if ISNAME ("fanout_def")
            {
                                        /* Look for correct subpass */
            if (p_val.dval[0].data.strval[0] != stn->subpass) continue;
                                        /* Locate relevant channel(s) */
            link = p_val.dval[1].data.linkval;
            found = FALSE;
            for (i=0; i<nchan; i++)
                {
                ch = stn->channels + i;
                if (strcmp (link, ch->chan_id) != 0) continue;
                                        /* Is this sign or magnitude? */
                if (strcmp (p_val.dval[2].data.strval, "sign") == 0) type = SIGN;
                else if (strcmp (p_val.dval[2].data.strval, "mag") == 0) type = MAG;
                                        /* Headstack number */
                hdstk = p_val.dval[3].data.intval;
                if (type == SIGN) ch->sign_headstack = hdstk;
                else if (type == MAG) ch->mag_headstack = hdstk;
                                        /* track numbers */
                for (j=4; j<p_val.nval; j++)
                    {
                    index = j-4;
                    track = p_val.dval[j].data.intval;
                    if (type == SIGN) ch->sign_tracks[index] = track;
                    else if (type == MAG) ch->mag_tracks[index] = track;
                    }
                found = TRUE;
                }
            if (! found)
                {
                msg ("Warning: track for chan '%s', subpass %c, no such channel", 1,
                                        link, stn->subpass);
                }
            }
        else if ISNAME ("fanin_def")
            {
            msg ("Fan-in not supported by current MkIV software",2);
            return (-1);
            }
        else if ISNAME ("track_frame_format")
            {
            ptr = p_val.dval[0].data.strval;
            if (strcmp (ptr, "Mark3A") == 0) stn->track_format = MARK3A;
            else if (strcmp (ptr, "Mark4") == 0) stn->track_format = MARK4;
            else if (strcmp (ptr, "VLBA") == 0) stn->track_format = VLBA;
            }
        else if ISNAME ("data_modulation")
            {
            ptr = p_val.dval[0].data.strval;
            if (strcmp (ptr, "on") == 0) stn->modulation = ON;
            else if (strcmp (ptr, "off") == 0) stn->modulation = OFF;
            }
        else if ISNAME ("bits/sample")
            stn->bits_sample = p_val.dval[0].data.intval;
        else if ISNAME ("multiplex_ratio")
            stn->multiplex_ratio = p_val.dval[0].data.intval;
        }

    return (0);
    }
