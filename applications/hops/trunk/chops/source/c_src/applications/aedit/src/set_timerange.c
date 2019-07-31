/************************************************************************/
/*                                                                      */
/* This simple routine decodes a user typed string representing a start */
/* and end time.                                                        */
/*                                                                      */
/*      Inputs:         arg1,arg2       Times in format ddd-hhmm        */
/*                                                                      */
/*      Output:         inp.timerange   data selection parameter        */
/*                                                                      */
/* Created 5 April 1989 by CJL                                          */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "aedit.h"

int set_timerange (char *arg1, char *arg2)
    {
    extern struct inputs inp;
    int syear, sday, shour, smin, ssec;
    int fyear, fday, fhour, fmin, fsec, sbad, fbad;
    
    sbad = fbad = FALSE;

    if(strlen(arg1) == 0) 
        {
        inp.begin = 0;
        inp.end = 0;
        return(0);
        }
    else if ((strlen (arg1) != 12) || (strlen (arg2) != 12))
        {
        msg("Bad time format '%s %s', correct format is yyddd-hhmmss", 
                                                                2, arg1, arg2);
        return(-1);
        }

    if(sscanf(arg1,"%2d%3d-%2d%2d%2d",&syear,&sday,&shour,&smin,&ssec) != 5) 
        {
        msg("Bad time format '%s', correct format is yyddd-hhmmss",2,arg1);
        return(-1);
        }
    if (syear < 80) syear += 2000;
    if(sscanf(arg2,"%2d%3d-%2d%2d%2d",&fyear,&fday,&fhour,&fmin,&fsec) != 5) 
        {
        msg("Bad time format '%s', correct format is yyddd-hhmmss",2,arg2);
        return(-1);
        }
    if (fyear < 80) fyear += 2000;
                                        /* Check for out of range numbers */
    if (sday>366 || sday<1 || shour>23 || shour<0 
                || smin>59 || smin<0 || ssec>59 || ssec<0) sbad = TRUE;
    if (fday>366 || fday<1 || fhour>23 || fhour<0 
                || fmin>59 || fmin<0 || fsec>59 || fsec<0) fbad = TRUE;
    if (sbad) msg("Impossible start time '%s'",2,arg1);
    if (fbad) msg("Impossible finish time '%s'",2,arg2);
    if (sbad || fbad) return(-1);

    inp.begin = time_to_int(syear,sday,shour,smin,ssec);
    inp.end = time_to_int(fyear,fday,fhour,fmin,fsec);
    if(inp.begin > inp.end) 
        {
        inp.begin = 0;
        inp.end = 0;
        msg("Start time must precede end time!",2);
        return(-1);
        }
    return(0);
    }
