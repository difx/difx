/************************************************************************/
/*                                                                      */
/* Takes the data in memory plus the image of a cc file in memory       */
/* and constructs a 2-D array of quality codes.  This is then displayed */
/* in an Xwindow PGPLOT window as a (possibly multi-page) colour-coded  */
/* matrix.  Interactive cursor operations are then invoked for data     */
/* perusal, editing, and fringe plot popups.                            */
/*                                                                      */
/*      Inputs:         sf (extern)     filled Sched structure          */
/*                                                                      */
/*      Output:                         return value 0 = OK, 1 = bad    */
/*                                                                      */
/* Created 16 Feb 1993 by CJL                                           */
/* Added fork for Mk4 functionality, 2 Feb 2001 by CJL                  */
/* removed mk3 code                    2010.6.8   rjc                   */
/************************************************************************/
#include <stdio.h>
#include "psplot.h"
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int psplot (esum *data)
    {
    int ret;
    char expno[5];
    static struct ps_array psarray;
    extern int rmdup, data_version;
    extern struct datasumm fsumm;

    if (! rmdup)
        {
        msg ("You must remove duplicates before attempting psplot", 2);
        return (1);
        }
    if (summ_data (data, STANDARD) != 0)
        {
        msg ("Error summarizing data for psplot", 2);
        return (1);
        }
    if (data_version == 0)
        {
        msg ("Your data are of mixed version numbers, cannot psplot", 2);
        return (1);
        }
    if (fsumm.nexp > 1)
        {
        msg ("Multiple experiments present ... please edit your", 2);
        msg ("dataset down to a single experiment before psplotting", 2);
        return (1);
        }
    
    ret = psplot4 (data);
    return (ret);
    }
