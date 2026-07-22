/************************************************************************/
/*                                                                      */
/* This routine sorts data read by cohfit.  A shell                     */
/* sort is performed, but with no physical rearrangement of the         */
/* data array.  Instead, the "order" element of the data structure is   */
/* filled with the number of the scan which would occupy this slot in   */
/* the sorted order.                                                    */
/*                                                                      */
/* This algorithm was taken from RSORT, an HP1000 program by RJC, which */
/* in turn was based on Knuth's sorting and searching volume, p.85,     */
/* Algorithm D.  The implementation here has an added piece of code to  */
/* stabilize the sort, using the "lastorder" element of the data        */
/* structure to break ties in the comparison of data values.            */
/*                                                                      */
/* Which is to say, yet another sort routine headed for oblivion.       */
/* It is called ONLY from sort_data.c                                   */
/*                                                                      */
/*      Inputs:         data            Main data array, keyval filled  */
/*                      navg            number of records               */
/*      Output:         data            order, lastorder modified       */
/*                                                                      */
/* Created October 5 1995 by CJL, stolen from average (almost verbatim) */
/* Hacked up April 26 2026 by GBC                                       */
/************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include "cohfit.h"

/*
 * Regarding the lastorder variable.  This is not a simple copy of "order".
 * Rather it is the position in the array this record would have, not the
 * index of the record that would occupy this slot in the sorted array.
 *
 * Initially data is marked with data[i].order = i, for i in (0..navg].
 */

void sorter (avg_data *data, int navg)
    {
    int i, j, k, ival, jval, jptr, interval, npass;
    int lasti, lastj;

    for (i=0; i<navg; i++)
        data[data[i].order].lastorder = i;
                                        /* Find coarsest sort interval */
                                        /* and passes required */
    interval = 1; npass = 0;
    while ((interval = 3*interval + 1) <= navg) npass++;
    interval = interval/3;
    if (interval < 4) interval = 4;
    if (npass < 1) npass = 1;
                                        /* Actually do the sort */
    for (k=0; k<npass; k++) 
        {
                                        /* Progressively shorter intervals */
        interval /= 3;
        for (j=interval; j<navg; j++) 
            {
                                        /* All done with pointers in */
                                        /* order element of data array */
            jptr = data[j].order;
            jval = data[jptr].keyval;
            i = j - interval;
                                        /* Loop till we find slot for j'th */
            while (TRUE)
                {
                ival = data[data[i].order].keyval;
                                        /* Found it */
                if (jval > ival) break;
                                        /* Stabilize sort by breaking tie */
                else if (jval == ival)
                    {
                    lastj = data[jptr].lastorder;
                    lasti = data[data[i].order].lastorder;
                    if (lastj > lasti) break;
                    }

                                        /* Drop i'th record 1 interval */
                data[i+interval].order = data[i].order;
                i -= interval;
                                        /* Fall through ... loop finished */
                if (i < 0) break;
                }
                                        /* This is correct slot for j'th */
            data[i+interval].order = jptr;
            }
        }

    return;
    }
/*
 * eof vim:nospell
 */
