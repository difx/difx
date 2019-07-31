/************************************************************************/
/*                                                                      */
/* This routine returns the number of minutes since midnight on         */
/* Jan 1st 1980 represented by the yy,ddd,hh,mm input integers.  This   */
/* is the way all the dates are encoded in aedit - makes for fast, easy */
/* checks on timeranges etc.                                            */
/*                                                                      */
/*      Inputs:         year            e.g. 1982 or 82                 */
/*                                      year = 0 special value, meaning */
/*                                      use start of current year       */
/*                      day,hour,min    self-explanatory                */
/*                      (Now replaced by mk4 date struct)               */
/*                                                                      */
/*      Output:         return value    31 bits allows 68 yr range      */
/*                      (Now replaced by double precision seconds)      */
/*                                                                      */
/* Created 17 April 1989 by CJL                                         */
/* Moved to UTIL library and changed to return seconds, not minutes     */
/*                              CJL 17 March 1993                       */
/* Added option to calculate relative to beginning of current year      */
/*                              rjc 94.5.5                              */
/* Converted to return double precision to serve Mk4                    */
/*              Original integer version remains.  CJL 14 January 2000  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_typedefs.h"
#include "mk4_util.h"

double
time_to_double(struct date mk4_date)
{
    int year80, nleaps; 
    double secs_since_80;

    msg ("time_to_double - yr,day,hr,min,sec = %d %d %d %d %f", -2, mk4_date.year,
                    mk4_date.day,
                    mk4_date.hour,
                    mk4_date.minute,
                    mk4_date.second);

    if (mk4_date.year == 0)
        year80 = 0;
    else
        year80 = mk4_date.year % 100 - 80;
    if (year80 < 0) year80 += 100;
    nleaps = (year80 + 3) /4;   /* Only count if past the leap year */

    secs_since_80 = year80 * 31536000.0 
                    + (mk4_date.day+nleaps-1) * 86400.0
                    + mk4_date.hour * 3600.0 
                    + mk4_date.minute * 60.0 
                    + mk4_date.second;

    return (secs_since_80);
}
