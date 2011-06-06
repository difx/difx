/*  @(#)mjd2str.c  version 1.7  created 95/11/24 08:23:56
    %% function to convert Modified Julian Date to string
    LANGUAGE: C
    ENVIRONMENT: Any
*/

#include <string.h>
#include <stdio.h>
#include <math.h>
#include "other.h"

/* locals */
static int monlen[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

/*******************************************************************************
*/
char *mjd2str		/* convert MJD to date string */
    (
    long mjd,		/* input Modified Julian Date */
    char *pstring	/* returned date string ('\0' terminated) */
    )
/*
 * RETURNS original pointer to given string
 *
 * MJD2STR converts the given date to the form yyyymondd and stores it in the 
 * string.  The string will be nine characters in length plus a '\0' 
 * terminator.  If the date does not fall between 0001JAN01 AD (MJD = -678,575)
 * and 10000JAN00 AD (MJD = 2,973,483) the string "*********" (9 *'s plus 
 * terminator) will be returned.
 */
{
    static char *monlist[] = { "JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
			       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };
    int year;
    int month;
    int day;

    if (mjd2date (mjd, &year, &month, &day) == -1)
	{
	strcpy (pstring, "*********");
	return (pstring);
	}

    sprintf (pstring, "%4d%s%02d", year, monlist[month - 1], day);

    return pstring;
}

/*******************************************************************************
*/
int mjd2dayno	/* convert MJD to day number of year */
    (
    long mjd,		/* input Modified Julian Date */
    int *pDayNo		/* returned day number (1-366) */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * This function converts the given Modified Julian Date to a day number.  
 * If the date does not fall between 0001JAN01 AD (MJD = -678,575) and 
 * 10000JAN00 AD (MJD = 2,973,483) ERROR is returned.
 */
{
    int year, month, day;
    int status;
    int i;

    /* convert MJD to year, month, and day */
    if ((status = mjd2date (mjd, &year, &month, &day)) != 0)
	return status;

    /* add days previous to current month */
    for (i = 1; i < month; i++)
	day += monlen[i - 1];

    /* add a day if leap year and past February
       (algorithm does NOT work for 2100 but we won't be here) */
    if (year % 4 == 0 && month > 2)
	day++;

    /* set returned day number and return */
    *pDayNo = day;
    return 0;
}

/*******************************************************************************
*/
int mjd2date		/* convert MJD to date */
    (
    long mjd,		/* input Modified Julian Date */
    int *pYear,		/* pointer to returned year (1-10000) */
    int *pMonth,	/* pointer to returned month (1-12) */
    int *pDay		/* pointer to returned day (1-31) */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * This function converts the given date to a year, month, and day.  If the 
 * given date does not fall between 0001JAN01 AD (MJD = -678,575) and 
 * 10000JAN00 AD (MJD = 2,973,483) ERROR is returned.
 */
{
/* 2,400,000 (difference between Julian Date and Modified Julian Date) 
   minus # days from jan 1, 4713 BC (beginning of Julian calendar) */
#define AD 678576

    int icen4;
    int icen;
    int iyr4;
    int iyr;
    int imon;
    int iday;

    /* check input range and calc days since jan 1 1 AD (Gregorian Calendar) */
    if (mjd > 2973483)
	return -1;
    if ((mjd += AD - 1) < 0)
        return -1;

    /* calc number of fours of Gregorian centuries */
    icen4 = mjd / 146097;

    /* calc number of centuries since last 
	fours of Gregorian centuries (e.g. since 1600 or 2000) */
    mjd -= (icen4 * 146097);
    if ((icen = mjd / 36524) == 4)
        icen = 3; 

    /* calc number of quadrenia(four years) since jan 1, 1901 */
    mjd -= (icen * 36524);
    iyr4 = mjd / 1461;

    /* calc number of years since last quadrenia */
    mjd -= (iyr4 * 1461);
    if ((iyr = mjd / 365) == 4)
        iyr = 3;

    /* calc number of months, days since jan 1 of current year */
    iday = mjd - iyr * 365;
    for (imon = 0; iday >= 0; imon++)
	iday = iday - monlen[imon] - ((iyr == 3 && imon == 1) ? 1 : 0);
    imon--;		/* restore imon, iday to last loop value */
    iday = iday + monlen[imon] + ((iyr == 3 && imon == 1) ? 1 : 0);

    /* calc return values */
    *pYear = icen4 * 400 + icen * 100 + iyr4 * 4 + iyr + 1;
    *pMonth = imon + 1;
    *pDay = iday + 1;

    return 0;
}

void fix_dhms(int dorh, int *dorhp, int *minsp, double *secsp)
{
    *secsp = 0;
    *minsp += 1;
    if (*minsp < 59) return;
    *minsp = 0;
    *dorhp += (*dorhp >= 0) ? 1 : -1;
    if (*dorhp < dorh) return;
    *dorhp = 0;
}

int rad2dms	/* convert radians to degrees, minutes and seconds (vex format)*/
    (
    double r,		/* input radians */
    char *pstring	/* returned string ('\0' terminated) */
    )
{
    int degrees;
    int minutes;
    double seconds;
    /* convert to degrees*/
    r *= 180 / M_PI;
    degrees = r;
    r -= degrees;
    r *= 60;
    if (r < 0)
       r *= -1;
    minutes = r;
    r -= minutes;
    r *= 60;
    seconds = r;
    seconds += 0.5e-6;
    if (seconds >= 60) fix_dhms(360, &degrees, &minutes, &seconds);
    sprintf (pstring, "%02dd%02d'%09.6f\"", degrees, minutes, seconds);

    return 0;
}

int rad2hms	/* convert radians to hours, minutes and seconds (vex format)*/
    (
    double r,		/* input radians */
    char *pstring	/* returned string ('\0' terminated) */
    )
{
    int hours;
    int minutes;
    double seconds;
    /* convert to degrees*/
    r *= 12 / M_PI;
    hours = r;
    r -= hours;
    r *= 60;
    minutes = r;
    r -= minutes;
    r *= 60;
    seconds = r;
    seconds += 0.5e-7;
    if (seconds >= 60) fix_dhms(24, &hours, &minutes, &seconds);
    sprintf (pstring, "%02dh%02dm%010.7fs", hours, minutes, seconds);

    return 0;

}
