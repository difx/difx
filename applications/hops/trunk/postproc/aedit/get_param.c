/************************************************************************/
/*                                                                      */
/* This routine extracts a parameter specification from the user, and   */
/* tries to get the requested parameters from the fringe files for all  */
/* unflagged type 2 lines in memory.  It will dynamically allocate      */
/* parameter array memory as needed.                                    */
/*                                                                      */
/*      Inputs:         data            To figure out where to get info */
/*                                                                      */
/*      Output:         user_param      Filled in array of user-defined */
/*                                      parameters (extern)             */
/*                      data            Pointers to user_param elements */
/*                                      added to fringe lines in data   */
/*                                                                      */
/* Created 11 August 1993 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "aedata.h"
#include "aedit.h"
#include "param_list.h"

int
get_param (data, arg1, arg2, remarg)
esum *data;
char *arg1, *arg2, *remarg;
    {
    int i, n, maxfreq;
    char args[256];
    extern int fscan, fflag;
    extern struct inputs inp;
    extern struct usearray user_param;
                                        /* Any data to extract info for? */
    if ((user_param.npoints = fscan - fflag) == 0)
        {
        msg ("No unflagged data present!", 2);
        return (0);
        }
                                        /* Figure out how many freqs needed */
    maxfreq = 0;
    for (i=0; i<fscan; i++)
        {
        if (data->fdata[i].flag != 0) continue;
        if ((n = data->fdata[i].data.no_freq) > maxfreq) maxfreq = n;
        }
                                        /* Get desired parameters from user */
    sprintf (args, "%s %s %s", arg1, arg2, remarg);
    if (get_param_list (&user_param, maxfreq, args) != 0)
        {
        msg ("Error getting user-defined list of parameters", 2);
        return (1);
        }
    if (user_param.nparms == 0)
        {
        msg ("No parameters specified", 2);
        return (0);
        }
                                        /* Clear out any old pointers in data */
    for (i=0; i<fscan; i++) 
        {
        data->fdata[i].param_ptr = -1;
        }
                                        /* Allocate space to hold parameters */
    if (allocate_parms (&user_param) != 0)
        {
        msg ("Error allocating memory for parameter array", 2);
        return (1);
        }
                                        /* Do actual extraction from disk files */
    if (extract_parms (data, &user_param) != 0)
        {
        msg ("Error getting parameters from fringe files", 2);
        return (1);
        }
                                        /* Parameter input filter now obsolete */
    inp.parameter[0] = 0.0;

    return (0);
    }
