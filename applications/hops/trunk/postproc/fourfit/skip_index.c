/*******************************************************************************
*                                                                              *
*   skip_index will return true iff the input index number, ind,  is not       *
*              present in the c_block, but other indices are.                  *
*                                                                              *
*                                                      rjc  93.3.23            *
*******************************************************************************/
#include "control.h"
#include "mk4_sizes.h"

#define FALSE 0
#define TRUE 1


int skip_index (ind, cblock)
int ind;
struct c_block *cblock;
   {
   int skip_it,i;

   if (cblock->index[0] == NULLINT)
      skip_it = FALSE;                    /* no indices specified, don't skip */

   else
      {
      skip_it = TRUE;
      for (i=0; i<2*MAXFREQ; i++)

         if (cblock->index[i] == NULLINT)
	    break;                             /* quit looking at end of list */

         else if (cblock->index[i] == ind)
	    {
	    skip_it = FALSE;        /* if ind is there, we want to include it */
	    break;
	    }
      }
       
   return (skip_it);
   }
