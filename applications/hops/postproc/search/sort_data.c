/************************************************************************/
/*									*/
/* Places the data in time, baseline and delay/rate order,	 	*/
/* ready for loop in main routine.					*/
/*									*/
/*	Inputs:		data		It would be tough without this	*/
/*			navg		number of records to sort	*/
/*									*/
/*	Output:		data		Ready to boogie			*/
/*									*/
/* Created January 29 1996 by CJL, borrowed from average and simplified	*/
/*									*/
/************************************************************************/
#include <string.h>
#include "search.h"

int
sort_data (avg_data* data, int navg)
    {
    int i;
    char rc[7];
    extern void sorter (avg_data* data, int navg);
					/* Initialize the sort order */
    for (i=0; i<navg; i++) data[i].order = i;
return (0);
					/* First sort by rate */
    for (i=0; i<navg; i++)
	data[i].keyval = data[i].fdata.delay_rate;
    sorter (data, navg);
					/* Then sort by delay */
    for (i=0; i<navg; i++)
	data[i].keyval = data[i].fdata.mbdelay;
    sorter (data, navg);
					/* Sort by extent */
    for (i=0; i<navg; i++)
	data[i].keyval = data[i].fdata.extent_no;
    sorter (data, navg);
					/* Sort by rootcode */
    for (i=0; i<navg; i++)
	{
	strcpy (rc, data[i].fdata.root_id);
	data[i].keyval = (rc[0] - 'a') * 11881376
			+ (rc[0] - 'a') * 456976
			+ (rc[0] - 'a') * 17576
			+ (rc[0] - 'a') * 676
			+ (rc[0] - 'a') * 26
			+ (rc[0] - 'a');
	}
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

    return (0);
    }
