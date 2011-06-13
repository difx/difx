/************************************************************************/
/*                                                                      */
/* Interprets the argument of the -i command line flag, placing the     */
/* interpreted results in the loops structure, ready for the looping    */
/* code in the main program.                                            */
/*                                                                      */
/*      Inputs:         arg             argument of -i flag             */
/*                      mode            for coh. time modes             */
/*                                                                      */
/*      Output:         loops           integ. time section set up      */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 17 October 1995 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "fringex.h"

int
parse_iflag (arg, mode, loops)
char *arg;
int *mode;
struct loops *loops;
    {
    char tlist[256];
    int i, n, nsecs, times[15];
                                        /* For "automatic" search regions, */
                                        /* actual int. time values must */
                                        /* wait until we read data files. */
                                        /* This fact is encoded as a negative */
                                        /* number of integration times, telling */
                                        /* subsequent code to fill in by */
                                        /* multiplying by acc. period */
    if (strcmp (arg, "all") == 0)
        {
        nsecs = 1;
        for (i=0; i<10; i++)
            {
            loops->nsecs[i] = (double)nsecs;
            nsecs *= 2;
            }
        loops->nnsec = -10;
        }
                                        /* Use coherence times in the A-file */
                                        /* supplied via the -r flag */
    else if (strcmp (arg, "search") == 0)
        {
        loops->nnsec = 1;
        loops->nsecs[0] = 0.0;
        *mode |= SEARCH;
        }
    else if (strcmp (arg, "noloss") == 0)
        {
        loops->nnsec = 1;
        loops->nsecs[0] = 0.0;
        *mode |= NOLOSS;
        }
                                        /* User-specified comma-separated list */
                                        /* Any times after the 15th are ignored */
    else if ((n = sscanf (arg, "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d", 
                times, times+1, times+2, times+3, times+4,
                times+5, times+6, times+7, times+8, times+9,
                times+10, times+11, times+12, times+13, times+14)) > 0)
        {
        for (i=0; i<n; i++)
            loops->nsecs[i] = (double)times[i];
        loops->nnsec = n;
        }
    else
        {
        msg ("Incomprehensible argument to -i flag, '%s'", 3, arg);
        return (1);
        }

    return (0);
    }
