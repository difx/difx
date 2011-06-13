/************************************************************************/
/*                                                                      */
/* This simple routine decodes a user typed string representing a start */
/* and end procdate.                                                    */
/*                                                                      */
/*      Inputs:         arg1,arg2       Times in format ddd-hhmm        */
/*                                                                      */
/*      Output:         inp.procrange   data selection parameter        */
/*                                                                      */
/* Created 5 April 1989 by CJL                                          */
/* Adapted for procdate 8 February 1994 by CJL                          */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "aedit.h"

int
set_procrange(arg1, arg2)
char *arg1, *arg2;
{
    extern struct inputs inp;
    int syear, sday, shour, smin, fyear, fday, fhour, fmin, sbad, fbad;

    sbad = fbad = FALSE;

    if(strlen(arg1) == 0) 
        {
        inp.proc_begin = 0;
        inp.proc_end = 0;
        return(0);
        }

    if(sscanf(arg1,"%2d%3d-%2d%2d",&syear,&sday,&shour,&smin) != 4) 
        {
        msg("Bad time format '%s', correct format is yyddd-hhmm",2,arg1);
        return(-1);
        }
    if (syear < 70) syear += 2000;
    if(sscanf(arg2,"%2d%3d-%2d%2d",&fyear,&fday,&fhour,&fmin) != 4) 
        {
        msg("Bad procdate format '%s', correct format is yyddd-hhmm",2,arg2);
        return(-1);
        }
    if (fyear < 70) fyear += 2000;

    if(sday>366 || sday<1 || shour>23 || shour<0 || smin>59 || smin<0) sbad = TRUE;
    if(fday>366 || fday<1 || fhour>23 || fhour<0 || fmin>59 || fmin<0) fbad = TRUE;
    if(sbad) msg("Impossible start procdate '%s'",2,arg1);
    if(fbad) msg("Impossible finish procdate '%s'",2,arg2);
    if(sbad || fbad) return(-1);

    inp.proc_begin = time_to_int (syear,sday,shour,smin,0);
    inp.proc_end = time_to_int (fyear,fday,fhour,fmin,0);
    if(inp.proc_begin > inp.proc_end) 
        {
        inp.proc_begin = 0;
        inp.proc_end = 0;
        msg("Start procdate must precede end procdate!",2);
        return(-1);
        }
    return(0);
}
