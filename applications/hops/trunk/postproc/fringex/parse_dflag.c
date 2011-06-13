/************************************************************************/
/*                                                                      */
/* Interprets the argument of the -d command line flag, placing the     */
/* interpreted results in the loops structure, ready for the looping    */
/* code in the main program.                                            */
/*                                                                      */
/*      Inputs:         arg             argument of -d flag             */
/*                                                                      */
/*      Output:         loops           rate and delay sections set up  */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 17 October 1995 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "fringex.h"

int
parse_dflag (arg, mode, loops)
char *arg;
int *mode;
struct loops *loops;
    {
    int i, ndel, nrate;
    double delstep, ratestep, highdel, lowdel, highrate, lowrate;
    double ndelstep, nratestep, delayoff, rateoff;
                                        /* For "automatic" search regions, */
                                        /* actual rate/delay values must */
                                        /* wait until we read data files. */
                                        /* This fact is encoded as a negative */
                                        /* number of rates and delays, telling */
                                        /* subsequent code to fill in with */
                                        /* Nyquist-sampled rate/delay intervals */
    if (sscanf (arg, "%dx%d", &nrate, &ndel) == 2)
        {
        loops->nrates = -nrate;
        loops->ndelays = -ndel;
        }
                                        /* Wide open windows encoded as zero */
                                        /* number of rate/delay cells */
    else if (strcmp (arg, "all") == 0)
        {
        loops->nrates = 0;
        loops->ndelays = 0;
        }
                                        /* Tell fringex to use the A-file */
                                        /* values of delay/rate */
                                        /* Only makes sense in c mode */
    else if (strcmp (arg, "srchpos") == 0)
        {
        *mode |= SRCHPOS;
        *mode |= CMODE;
        }
                                        /* Explicit user rate/delay values */
    else if (sscanf (arg, "%lf,%lf,%lf,%lf,%lf,%lf",
                                &lowrate, &highrate, &ratestep,
                                &lowdel, &highdel, &delstep) == 6)
        {
                                        /* Process delays first */
        ndelstep = (highdel - lowdel) / delstep;
        if (ndelstep < 0.0)
            {
            msg ("You must specify the delay range low-high, not high-low", 3);
            return (1);
            }
        else if (ndelstep < 1.0)
            {
            msg ("Delay step size larger than delay range requested", 3);
            return (1);
            }
        else
            {
            for (i=0; i<MAXDELAYS; i++)
                {
                loops->delays[i] = lowdel + i*delstep;
                if (loops->delays[i] >= highdel) break;
                }
            loops->ndelays = i+1;
            }
                                        /* Now do the same for rates */
        nratestep = (highrate - lowrate) / ratestep;
        if (nratestep < 0.0)
            {
            msg ("You must specify the rate range low-high, not high-low", 3);
            return (1);
            }
        else if (nratestep < 1.0)
            {
            msg ("Rate step size larger than rate range requested", 3);
            return (1);
            }
        else
            {
            for (i=0; i<MAXRATES; i++)
                {
                loops->rates[i] = lowrate + i*ratestep;
                if (loops->rates[i] >= highrate) break;
                }
            loops->nrates = i+1;
            }
        }
                                        /* Single rate/delay location specified */
    else if (sscanf (arg, "%lf,%lf", &rateoff, &delayoff) == 2)
        {
        loops->rates[0] = rateoff;
        loops->delays[0] = delayoff;
        loops->nrates = 1;
        loops->ndelays = 1;
        }
    else
        {
        msg ("Incomprehensible argument to -d flag, '%s'", 3, arg);
        return (1);
        }

    return (0);
    }
