/************************************************/
/*						*/
/*	day_of_datef -- Gets the day of the year*/
/*  from a datef file (basically, by adding	*/
/*  the length of months...)			*/
/*						*/
/*    day_of_datef(datef)-> short		*/
/*						*/
/*	Author: Tony Lower			*/
/* 		Date: July 8th, 1991		*/
/*	Last Edited: July 8th, 1991		*/
/*	Modified October 2 1991 to conform to	*/
/*	style conventions, CJL.			*/
/*						*/
/************************************************/

#include "general.h"
#include "mk4_util.h"

short 
day_of_datef(struct datef date)
    {
    int i;
    short months[12];
    short result;

    months[0] = 31;
    months[1] = 28;
    if ((date.year % 4) == 0) months[1]++;
    months[2] = 31;
    months[3] = 30;
    months[4] = 31;
    months[5] = 30;
    months[6] = 31;
    months[7] = 31;
    months[8] = 30;
    months[9] = 31;
    months[10] = 30;
    months[11] = 31;

    result = 0;
    for(i=0;i<date.month-1;i++)
	{
	result+=months[i];
	} /* for */

    result+=date.day;

    return result;
    } /* day_of_datef */
