/************************************************************************/
/*                                                                      */
/* Places the data in scan and time order, ready for averaging          */
/*                                                                      */
/*      Inputs:         data            It would be tough without this  */
/*                      nseg            number of records to sort       */
/*                                                                      */
/*      Output:         data            Ready to boogie                 */
/*                                                                      */
/* Created Septermber 9 1994 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "average.h"
#include "mk4_util.h"

int
sort_data (seg_data *data,
           int nseg)
    {
    int i, j, nsrc;
    char *snames[MAXSRC], source[32];
    extern int datatype;

                                        /* Initialize the sort order */
    for (i=0; i<nseg; i++) data[i].order = i;
                                        /* sort by scantime first */
    for (i=0; i<nseg; i++)
        {
        if (datatype == 2)
            data[i].keyval = data[i].u.fdata.time_tag;
        else if (datatype == 3)
            data[i].keyval = data[i].u.tdata.time_tag;
        }
    sorter (data, nseg);
                                        /* Then sort by baseline/triangle */
    for (i=0; i<nseg; i++)
        {
        if (datatype == 2)
            data[i].keyval = data[i].u.fdata.baseline[1]
                                        + 256 * data[i].u.fdata.baseline[0];
        else if (datatype == 3)
                data[i].keyval = data[i].u.tdata.triangle[2]
                                        + 256 * data[i].u.tdata.triangle[1]
                                        + 65536 * data[i].u.tdata.triangle[0];
        }
    sorter (data, nseg);
                                        /* Sort by sourcename */
    nsrc = 0;
    for (i=0; i<nseg; i++)
        {
        if (datatype == 2) strcpy (source, data[i].u.fdata.source);
        else if (datatype == 3) strcpy (source, data[i].u.tdata.source);
                                        /* Get list of sources */
        for (j=0; j<nsrc; j++) if (strcmp (source, snames[j]) == 0) break;
        if (j == nsrc) snames[nsrc++] = source;
        if (nsrc >= MAXSRC) 
            {
            msg ("Too many sources to sort", 3);
            return (-1);
            }
                                        /* Don't need alphabetization, */
                                        /* just group sources together. */
        data[i].keyval = j;
        }
    sorter (data, nseg);
                                        /* Finally sort by segmentation time */
                                        /* if this is fringex baseline data */
    if ((datatype == 2) && ((data[0].u.fdata.datatype[0] == 'A') 
                || (data[0].u.fdata.datatype[0] == 'C')
                || (data[0].u.fdata.datatype[0] == 'O')))
        {
        for (i=0; i<nseg; i++)
            {
            if (datatype == 2)
                data[i].keyval = data[i].u.fdata.duration;
            else if (datatype == 3)
                data[i].keyval = data[i].u.tdata.duration;
            }
        }
    sorter (data, nseg);

    return (0);
    }
