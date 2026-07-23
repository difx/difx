/************************************************************************/
/*                                                                      */
/* Places the data in time, baseline and segmentation time order,       */
/* ready for loop in main routine.                                      */
/*                                                                      */
/*      Inputs:         data            It would be tough without this  */
/*                      navg            number of records to sort       */
/*      Output:         data            Ready to boogie                 */
/*                                                                      */
/* Created October 5 1995 by CJL, borrowed from average and simplified  */
/* Hacked up April 26 2026 by GBC                                       */ 
/************************************************************************/
#include "cohfit.h"

int sort_data (avg_data *data, int navg)
    {
    int i;
                                        /* Initialize the sort order */
    for (i=0; i<navg; i++) data[i].order = i;
                                        /* First sort by segmentation time */
    for (i=0; i<navg; i++)
        data[i].keyval = data[i].fdata.duration;
    sorter (data, navg);

                                        /* Then sort by polarization */
    for (i=0; i<navg; i++)
            data[i].keyval = data[i].fdata.polarization[1]
                                        + 256 * data[i].fdata.polarization[0];
    sorter (data, navg);

                                        /* Then sort by baseline */
    for (i=0; i<navg; i++)
            data[i].keyval = data[i].fdata.baseline[1]
                                        + 256 * data[i].fdata.baseline[0];
    sorter (data, navg);


                                        /* Sort by time */
    for (i=0; i<navg; i++)
        data[i].keyval = data[i].fdata.time_tag;
    sorter (data, navg);
                                        /* Finally, sort by fgroup */
    for (i=0; i<navg; i++)
        data[i].keyval = data[i].fdata.freq_code;
    sorter (data, navg);

    return (0);
    }
/*
 * eof vim:nospell
 */
