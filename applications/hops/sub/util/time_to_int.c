/************************************************************************/
/*									*/
/* This routine returns the number of minutes since midnight on		*/
/* Jan 1st 1980 represented by the yy,ddd,hh,mm input integers.  This	*/
/* is the way all the dates are encoded in aedit - makes for fast, easy	*/
/* checks on timeranges etc.						*/
/*									*/
/*	Inputs:		year		e.g. 1982 or 82			*/
/*                                      year = 0 special value, meaning */
/*                                      use start of current year       */
/*			day,hour,min	self-explanatory		*/
/*									*/
/*	Output:		return value	31 bits allows 68 yr range	*/
/*									*/
/* Created 17 April 1989 by CJL						*/
/* Moved to UTIL library and changed to return seconds, not minutes	*/
/*                              CJL 17 March 1993			*/
/* Added option to calculate relative to beginning of current year      */
/*				rjc 94.5.5				*/
/************************************************************************/

int
time_to_int(int year, int day, int hour, int min, int sec)
{
    int year80, nleaps, secs_since_80;

    if (year == 0)
	year80 = 0;
    else
        year80 = year % 100 - 80;
    if (year80 < 0) year80 += 100;
    nleaps = (year80 + 3) /4;	/* Only count if past the leap year */
    secs_since_80 = 
	year80*31536000 + (day+nleaps-1)*86400 + hour*3600 + min*60 + sec;
    return (secs_since_80);
}
