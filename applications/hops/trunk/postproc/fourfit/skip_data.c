/*******************************************************************************
*                                                                              *
*  skip_data is a function that determines whether or not a particular scan, as*
*  specified by the baseline, source, group, and start time, is to be fourfit'd*
*  This is accomplished by searching through the chained list of control       *
*  blocks, looking for any specification that this scan should be skipped.     *
*                                                                              *
*  Inputs:     scantime       Start of scan as integer secs since 1980.0       *
*              baseline       String containing the 2 char baseline.           *
*              source         String containing the 1--32 char source name.    *
*              group          Single character containing freq. group          *
*                                                                              *
*  Outputs:    <none>                                                          *
*                                                                              *
*  Returns:    0 = FALSE: fourfit this scan                                    *
*              1 = TRUE:  skip this scan                                       *
*                                                                              *
*  Created 93.3.17 by rjc                                                      *
*                                                                              *
*******************************************************************************/
#include <string.h>
#include "control.h"

#define TRUE 1
#define FALSE 0

int skip_data (int scantime, char* baseline, char* source, char group)
   {
   extern struct c_block *cb_head;
   struct c_block *cb_ptr;

   int skipit;
   char base[2],sour[32];


                         /* copy source and baseline into fixed-length format */
   memcpy (base,baseline,2);
   memcpy (sour,"                                ",32);   /* pad with blanks */
   memcpy (sour,source,strlen(source));

   skipit = FALSE;                /* assume data wanted unless marked as skip */

                                  /* loop over all the chained control blocks
                                     looking for any that match this scan
                                     insofar as source, baseline, and start
                                     time are concerned                       */
   
   for (cb_ptr=cb_head; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
      if (criteria_match (cb_ptr,base,sour,group,scantime))
         if (cb_ptr->skip == TRUE)
            {
            msg ("skipping baseline %s", -1,baseline);
            skipit = TRUE;
            break;              /* mark to be ignored, and bail out of search */
            }
   
   return (skipit);
   }
