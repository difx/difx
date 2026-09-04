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
#include <string.h>
#include "fringex.h"

int parse_iflag (char *arg, int *mode, struct loops *loops)
    {
    char tlist[256];
    int i, n, nsecs, times[MAXNSECS];
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
                                        /* User-specified comma-sep. list */
                                        /* Times up to 100 are used. */
    else if ((n = sscanf (arg,
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d," 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d," 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d," 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d," 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d," 
              "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d" ,
                times+0, times+1, times+2, times+3, times+4, times+5,
                times+6, times+7, times+8, times+9, times+10, times+11,
                times+12, times+13, times+14, times+15, times+16,

                times+17, times+18, times+19, times+20, times+21, times+22,
                times+23, times+24, times+25, times+26, times+27, times+28,
                times+29, times+30, times+31, times+32, times+33,

                times+34, times+35, times+36, times+37, times+38, times+39,
                times+40, times+41, times+42, times+43, times+44, times+45,
                times+46, times+47, times+48, times+49,

                times+50, times+51, times+52, times+53, times+54, times+55,
                times+56, times+57, times+58, times+59, times+60, times+61,
                times+62, times+63, times+64, times+65, times+66,

                times+67, times+68, times+69, times+70, times+71, times+72,
                times+73, times+74, times+75, times+76, times+77, times+78,
                times+79, times+80, times+81, times+82, times+83,

                times+84, times+85, times+86, times+87, times+88, times+89,
                times+90, times+91, times+92, times+93, times+94, times+95,
                times+96, times+97, times+98, times+99   )) > 0)
        {
        for (i=0; i<n; i++)
            loops->nsecs[i] = (double)times[i];
        loops->nnsec = n;
        msg ("Scanned %d segment times", 0, loops->nnsec);
        }
    else
        {
        msg ("Incomprehensible argument to -i flag, '%s'", 3, arg);
        return (1);
        }

    return (0);
    }

/*
 * eof
 */
