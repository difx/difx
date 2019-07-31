/***********************************************************************
*                                                                      *
* criteria_match is a logical function that determines whether or not  *
* the pointed-to c_block has criteria (baseline, frequency group,      *
* source, and scan start time) that match the actual info. from the    *
* current pass.                                                        *
*                                                  rjc  92.9.2         *
* fixed source matching code                       rjc  2010.5.27      *
***********************************************************************/

#include <stdio.h>
#include "control.h"

int
criteria_match (struct c_block* cb_ptr, char base[2], char sour[31], char group, int time)
    {
    int i,match[4],all_match;


                                                       /* compare baselines */
    match[0] =  (base[0] == WILDCARD
             || cb_ptr->baseline[0] == WILDCARD
             || cb_ptr->baseline[0] == base[0])
             && (base[1] == WILDCARD
             ||  cb_ptr->baseline[1] == WILDCARD
             ||  cb_ptr->baseline[1] == base[1]);

                                                         /* compare sources */
    match[1] = TRUE;
    for (i=0; i<16; i++)            // any different non-wildcard chars ruin match
        if (sour[i] != WILDCARD 
            && sour[i] != ' '
            && sour[i] != '\0'
            && cb_ptr->source[i] != WILDCARD 
            && sour[i] != cb_ptr->source[i])
            match[1] = FALSE;

                                                 /* compare frequency group */
    match[2] =  group == WILDCARD
             || cb_ptr->f_group == WILDCARD
             || cb_ptr->f_group == group;


                                            /* and compare scan start times */
    match[3] =  (cb_ptr->scan[0] == NULLINT
             ||  cb_ptr->scan[0] <= time)
             && (cb_ptr->scan[1] == NULLINT
             ||  cb_ptr->scan[1] >= time);



    all_match = TRUE;                        /* innocent 'til proven guilty */

                                              /* mark false if any disagree */
    for (i=0; i<4; i++)       
        all_match &= (cb_ptr->knot[i] == FALSE && match[i] == TRUE )  ||
                     (cb_ptr->knot[i] == TRUE  && match[i] == FALSE);

    msg ("c_m: arguments: %c%c %c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c %c %d",-1,
             base[0],base[1],sour[0],sour[1],sour[2],sour[3],
             sour[4],sour[5],sour[6],sour[7],sour[8],
             sour[9],sour[10],sour[11],sour[12],sour[13],
             sour[14],sour[15],sour[16],sour[17],sour[18],
             sour[19],sour[20],sour[21],sour[22],sour[23],
             sour[24],sour[25],sour[26],sour[27],sour[28],
             sour[29],sour[30],sour[31], group,time);

    msg ("c_block params: %c%c %c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c %c %d..%d match=%d%d%d%d knot=%d%d%d%d %d", -1,
             cb_ptr->baseline[0], cb_ptr->baseline[1],
             cb_ptr->source[0], cb_ptr->source[1], cb_ptr->source[2], cb_ptr->source[3],
             cb_ptr->source[4], cb_ptr->source[5], cb_ptr->source[6], cb_ptr->source[7],
             cb_ptr->source[8], cb_ptr->source[9], cb_ptr->source[10], cb_ptr->source[11],
             cb_ptr->source[12], cb_ptr->source[13], cb_ptr->source[14], cb_ptr->source[15],
             cb_ptr->f_group,cb_ptr->scan[0], cb_ptr->scan[1],
             match[0],match[1],match[2],match[3],
             cb_ptr->knot[0],cb_ptr->knot[1],cb_ptr->knot[2],cb_ptr->knot[3],all_match);

    return (all_match);
    }
