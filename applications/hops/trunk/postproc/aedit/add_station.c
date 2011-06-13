/************************************************************************/
/*									*/
/* Small utility routine to add a station letter code to alphabetized	*/
/* list.  Used extensively in the data summaries, which drive plotting,	*/
/* closure calculations, and some other functions.			*/
/*									*/
/*	Inputs:		station		Letter code of station to add	*/
/*			slist		Current list of stations	*/
/*									*/
/*	Output:		slist		Modified as appropriate		*/
/*									*/
/* Created November 4 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>

void
add_station (station, slist)
char station, *slist;
    {
    int i, l;
					/* Station already present */
    if (strchr (slist, station) != NULL) return;
					/* Insert new station by working */
					/* back from the end */
    l = strlen (slist);
    slist[l+1] = '\0';
    for (i=l; i>0; i--)
	{
	if (station > slist[i-1])
	    {
	    slist[i] = station;
	    break;
	    }
	slist[i] = slist[i-1];
	}
    if (i == 0) slist[0] = station;
    }
