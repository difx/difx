/************************************************************************/
/*                                                                      */
/* This routine receives the number of seconds since midnight on        */
/* Jan 1st 1980 and converts to yy,ddd,hh,mm,ss output integers.  This  */
/* is the way all the dates are encoded in aedit - makes for fast, easy */
/* checks on timeranges etc.                                            */
/*                                                                      */
/*      Inputs:         time             Seconds since Jan 1 1980       */
/*                                                                      */
/*      Output:         year             since 1900                     */
/*                      day,hour,min,sec self_explanatory               */
/*                                                                      */
/* Created 17 April 1989 by CJL                                         */
/* Moved to UTIL library and extended to use seconds, CJL 17 March 1993 */
/*                                                                      */
/************************************************************************/
#include "mk4_util.h"

void
int_to_time (int tim, int *year, int *day, int *hour, int *min, int *sec)
{
    int i, time;

    time = tim;
    *sec = time%60;
    time /= 60;
    *min = time%60;
    time /= 60;
    *hour = time%24;
    time /= 24;
    for(i=0;i<99;i++) 
        {               /* Can't think of anything neater */
        if(i%4 == 0 && time <= 366) break;
        else if(time <= 365) break;
        if(i%4 == 0) time -= 366;
        else time -= 365;
        }
    *year = i + 80;
    *day = time + 1;            /* Days are 1-relative */
}
