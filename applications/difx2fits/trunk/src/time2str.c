/*  @(#)time2str.c  version 1.7  created 96/12/06 07:06:13
    %% functions to convert time (or angle) to string
    LANGUAGE: C
    ENVIRONMENT: Any
*/

/* includes */
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include "other.h"

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif

/* forward declarations */
static int ltostr ();

/*******************************************************************************
*/
char *timeMjd2str
    (
    double inTime,	/* date/time (MJD w/ fractional day) to be converted */
    char *pOutStr	/* pointer to returned string */
    )
/*
 * RETURNS original pointer to given string
 *
 * This function converts the given MJD to a string with the form e.g. 
 *   "1991JAN01 12h00m00.00s".
 */
{
    return (time2str (inTime, "", pOutStr));
}

/*******************************************************************************
*/
char *time2str
    (
    double inTime,	/* date/time (MJD w/ fractional day) to be converted */
    char *pFormat,	/* input string specifing leading fill character 
			   and number of digits to right of decimal point 
			   in seconds field in output string, 
			   NULL or empty string- same as timeMjd2str() */
    char *pOutStr	/* pointer to returned string */
    )
/*
 * RETURNS original pointer to given string
 *
 * EXAMPLE FORMAT STRINGS AND THEIR RESULTS --
 *   ""             --  1991JAN01  6h00m00.00s
 *   ".0"           --  1991JAN01  6h00m00s
 *   ".4"           --  1991JAN01  6h00m00.0000s
 *   "0"            --  1991JAN01 06h00m00.00s
 *   "0.0"          --  1991JAN01 06h00m00s
 */
{
    char *poriginal = pOutStr;  /* hold output string address for return */
    long mjd;
    double rads;
    char formatStr[40];
    char tempStr[40];

    /* derive format string to be used in rad2str() call */
    strcpy (formatStr, "h");
    if (pFormat != 0 && *pFormat != '\0')
	strcat (formatStr, pFormat);

    /* separate date (MJD) and time (radian) parts */
    mjd = (int)inTime;
    rads = (inTime - (double)mjd) * 2.0*M_PI;

    /* build date string */
    mjd2str (mjd, pOutStr);
    strcat (pOutStr, " ");
    rad2str (rads, formatStr, tempStr);
    strcat (pOutStr, tempStr);

    return (poriginal);
}
/*******************************************************************************
*/
char *srvMjd2str
    (
    double inTime,	/* date/time (MJD w/ fractional day) to be converted */
    char *pOutStr	/* pointer to returned string */
    )
/*
 * RETURNS original pointer to given string
 *
 * Output format:   54235 07h00m02.5s
 */
{
    char *poriginal = pOutStr;  /* hold output string address for return */
    int mjd;
    double rads;
    char tempStr[40];

    /* separate date (MJD) and time (radian) parts */
    mjd = (int)inTime;
    rads = (inTime - (double)mjd) * 2.0*M_PI;

    /* build date string */
    sprintf(pOutStr, "%d ", mjd);
    rad2str (rads, "h", tempStr);
    strcat (pOutStr, tempStr);

    return (poriginal);
}

/*******************************************************************************
*/
char *rad2str		/* convert radians to string */
    (
    double angle,	/* input angle in radians */
    char *pFormat,	/* input string specifing format of output string */
    char *pOutStr	/* pointer to returned string */
    )
/*
 * RETURNS original pointer to given string
 * 
 * NOTE- The output is ROUNDED to the specified seconds-field precision.  Be 
 * careful, this sometimes produces unexpected results.
 */
{
    return (rad2strg (angle, pFormat, pOutStr, 1));
}

/*******************************************************************************
*/
char *rad2strg
    (
    double angle,	/* input angle in radians */
    char *pFormat,	/* input string specifing format of output string */
    char *pOutStr,	/* returned string ('\0' terminated) */
    int roundFlag	/* TRUE- output string is rounded, FALSE-truncated */
    )
/*
 * RETURNS original pointer to given string
 * 
 * Converts a floating point number presumed to contain an angle in radians to 
 * a string according to instructions in the format string.  The output string 
 * format can be either degrees (default form is iiidjj'kk.f") or time 
 * (default form is iihjjmkk.ffs) depending on the format string contents.  
 * The jj (minutes) and kk (integer seconds) fields are always two digits.  
 * The widths of the iii (degrees or hours) and ff (fractional seconds) fields 
 * are controlled by the format string.
 *
 * The format string is a null-terminated string of the general form "dm.n" or 
 * "hm.n" where m and n are digit strings.  The forms "d", "h" (use default 
 * values for m and n), "dm", "hm" (use given m and default n), "d.n",  and 
 * "h.n" (use default m and given n) are also permitted.
 *
 * The digit string m controls the width of the degrees/hours field in the 
 * output string.  If m does not specify sufficient digits to represent the 
 * degrees/hours, the field will be widened to the exact number of digits 
 * needed.  If the first digit of m is zero, zero fill will be used; otherwise 
 * blank fill will be used.
 *
 * The digit string n controls the number of decimal places in the fractional 
 * portion of the seconds field.  If n is zero, no seconds fraction and no 
 * decimal point will appear in the seconds field.
 *
 * If a ':' is included after the 'h', colons are used as a delimiter between 
 * the hours & minutes and between the minutes and seconds, a NULL is placed 
 * after the last digit of the seconds.  All other 'hour' formatting rules 
 * described above still apply.
 *
 * EXAMPLE FORMAT STRINGS AND THEIR RESULTS --
 *   "d", or indecipherable string --  111d22'33.3"
 *   "h"            --  11h22m33.33s
 *   "d4"           --     1d22'33.3"
 *   "h.1"          --  11h22m33.3s
 *   "h:.1"         --  11:22:33.3
 *   "d03.2"        --  001d22'33.33"
 */
{
    char *poriginal = pOutStr;  /* hold output string address for return */
    char units_char[4];         /* field separation chars (d'" or hms) */
    int m;                      /* degrees/hours field width */
    int n;                      /* fractional seconds field width */
    int lzero_flag = 0;    /* TRUE => use leading zeros fill */
    int sign_flag = 1;          /* 1=positive, -1=negative angle */
    long long_tmp;
    int i;

    /* decode measure type from format string */
    if (*pFormat == 'h')
        {                   /* hour format */
	angle *= 12.0/M_PI;         /* convert angle to hours */
	if (*(pFormat+1) == ':')     /* if colon delimiter specified */
	    {
	    strcpy (units_char, "::\0"); /* set field separator chars */
	    pFormat++;                   /* account for extra char */
	    }
	else                       /* use h m s format */
	    strcpy (units_char, "hms");  /* set field separator chars */
	m = 2;                     /* default hours field width */
	n = 2;                     /* default frac seconds field width */
        }
    else
        {                   /* degrees (or default) format */
	angle *= 180.0/M_PI;          /* convert angle to degrees */
	strcpy (units_char, "d'\"");  /* set field separator chars */
	m = 3;                     /* default degrees field width */
	n = 1;                     /* default frac seconds field width */
        }

    /* advance to next char if not at end */
    if (*pFormat != '\0')
        pFormat++;

    /* decode given degree/hour field width (if any) */
    if (isdigit (*pFormat))
	{
	if (*pFormat == '0')
	    {
	    lzero_flag = 1;        /* leading zeros fill */
	    pFormat++;
	    }
	if (isdigit (*pFormat))
	    {
	    m = 0;
	    while (isdigit (*pFormat))
		m = m * 10 + *pFormat++ - '0';
	    }
	}

    /* decode given fractional seconds field width (if any) */
    if (*pFormat == '.')
        {
	pFormat++;                     /* skip over . */
	if (isdigit (*pFormat))
	    {
	    n = 0;
	    while (isdigit (*pFormat))
		n = n * 10 + *pFormat++ - '0';
	    }
        }

    /* if value is negative set sign flag and convert to positive */
    if (angle < 0.0)
	{
	sign_flag = -1;
	angle = -angle;
	}

    /* add rounding fraction to angle appropriate for desired output */
    if (roundFlag)
	{
        double round = 1.3888889E-4;    /* 1/2 second = 1/7200 */
	for (i = 0; i < n;  i++)
	    round /= 10.0;
	angle += round;
	}

    /* calculate and place degree/hours in output string
     here units of angle are either hours or degrees set according to format */
    long_tmp = angle;          /* note use of truncation */
    if (lzero_flag)            /* neg field width => use leading zeros */
        m = -m;
    pOutStr += ltostr (long_tmp, pOutStr, m, sign_flag);
    *pOutStr++ = units_char[0];

    /* calculate and place minutes in output string */
    angle = (angle - long_tmp) * 60.0;
    long_tmp = angle;
    pOutStr += ltostr (long_tmp, pOutStr, -2, 0);
    *pOutStr++ = units_char[1];

    /* calculate and place integer seconds in output string */
    angle = (angle - long_tmp) * 60.0;
    long_tmp = angle;
    pOutStr += ltostr (long_tmp, pOutStr, -2, 0);

    /* calculate and place fractional seconds in output string */
    if (n != 0)
        {
	*pOutStr++ = '.';
	angle -= long_tmp;     /* angle = fractional part of secs  */
        for (i = 0; i < n; i++)
            angle *= 10.0;
        pOutStr += ltostr ((long)angle, pOutStr, -n, 0);
	}
    *pOutStr++ = units_char[2];
    *pOutStr++ = '\0';
 
    return (poriginal);
}

/*******************************************************************************
*/
static int ltostr		/* convert long to string */
    (
    long number,	/* number to be converted */
    char *pOutStr,	/* receiving string */
    int width,		/* minimum width of numeric field, 
			   positive width => use leading blank fill, 
			   negative => leading zero fill */
    int sign		/* only the sign of this int is important, 
			   negative => places '-' in output */
    )
/*
 * RETURNS number of characters that have been appended to string
 *
 * Output digits are always right-justified in the field.  BE CAREFUL - If 
 * 'width' is less than the number of digits needed to express the number, the 
 * field will be expanded to the exact number of characters required.
 *
 * A minus sign is prefixed if either 'number' or 'sign' is negative.  This 
 * permits a 'signed zero' to be generated.  The minus sign is the first 
 * character in the string if leading zeros are requested, else it is the 
 * character immediately preceeding the most significant digit.
 *
 * The output string DOES NOT have a terminating null.
 */
{
    char *preverse;
    char fillchar;
    char tempch;
    int nochar = 0;
 
    /* if input number is negative ... */
    if (number < 0)
	{
        sign = -1;
        number = -number;
	}
 
    /* set fillchar = proper fill character */
    if (width < 0)
	{
        width = -width;
        fillchar = '0';
	}
    else
        fillchar = ' ';

    /* if input number is negative with leading zeroes fill,
       insert minus sign in first character of string */
    if (sign < 0 && fillchar == '0')
	{
        *pOutStr++ = '-';
        nochar++;
        sign = 0;
	}
 
    /* record pointer where string reversal will begin */
    preverse = pOutStr;

    /* convert input number to ascii in reverse order */
    do 
	{
        *pOutStr++ = '0' + number % 10;
        nochar++;
	} while ((number /= 10) > 0);

    /* if input number is negative with leading blank fill,
       insert minus sign in front of most significant digit */
    if (sign < 0 && fillchar == ' ')
	{
        *pOutStr++ = '-';
        nochar++;
	}

    /* insert enough fill characters to fill minimum field width */
    while (nochar < width)
	{
        *pOutStr++ = fillchar;
        nochar++;
	}

    /* point to last inserted character */
    pOutStr--;

    /* digits were generated in reverse order, so re-reverse them */
    while (preverse < pOutStr)
	{
        tempch = *preverse;
        *preverse++ = *pOutStr;
        *pOutStr-- = tempch;
	}

    /* return number of characters inserted */
    return (nochar);
}

