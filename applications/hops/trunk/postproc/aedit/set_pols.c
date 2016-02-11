/************************************************************************/
/*                                                                      */
/* This routine sets up the polarizations input parameter, parsing the  */
/* user-typed input line and accepting only 2-character polarization    */
/* specifiers, namely RR, LL, LR and RL.                                */
/*                                                                      */
/*      Inputs:         arg1,arg2,remarg        Typed input strings     */
/*                                                                      */
/*      Output:         inp.polarizations       polarization input par  */
/*                                                                      */
/* Created 15 Feb 2001 by CJL                                           */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedit.h"

int
set_pols (arg1, arg2, remarg)
char *arg1, *arg2, *remarg;
    {
    extern struct inputs inp;
    char outbuf[150], buf[150];
    char *pol, *strtok();
    char pol_list[5] = {"LRXY"};

    outbuf[0] = '\0';

    sprintf(buf,"%s %s %s",arg1,arg2,remarg);

    pol = strtok(buf," ,");    /* Step through pols */
    while (pol != NULL) 
        {
        if (strchr (pol_list, pol[0]) == NULL
         || strchr (pol_list, pol[1]) == NULL)
            {
            msg("Bad polarization '%s'", 2, pol);
            return(-1);
            }
        strcat (outbuf, pol);    /* Build up pretty baseline string */
        strcat (outbuf," ");
        pol = strtok(NULL," ,");
        }
    inp.polarizations[0] = '\0';
    strcpy (inp.polarizations, outbuf);   /* Blank input line gives blank result */
    return (0);
    }
