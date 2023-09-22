/************************************************************************/
/*                                                                      */
/*      This routine receives 5 integer values representing a UT date,  */
/*      and returns a pointer to a string containing a unique code      */
/*      consisting of 6 lower-case alphabetic characters a-z.  This     */
/*      code appears in all data file names, being the encoded          */
/*      COREL processing date unique to each root, and thus ties files  */
/*      to their respective roots                                       */
/*                                                                      */
/*      On failure caused by bad inputs, a NULL pointer is returned     */
/*                                                                      */
/*      Created 881013 by CJL                                           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* [a-z]x6 before this unix clock and [0-9A-Z]x6 after it */
#define HOPS_ROOT_BREAK (1519659904)

int root_id_delta(time_t now)
{
    int delta = 4;
    if (now >= HOPS_ROOT_BREAK) delta = 1;
    return(delta);
}

char* root_id_later(time_t now)
{
    long m, u = now - HOPS_ROOT_BREAK;
    static char datestr[8];
    char *date = datestr;
    m = u / (36*36*36*36*36);
    u -= m * (36*36*36*36*36);
    *date++ = (m < 10) ? (char)('0' + m) : (char)('A' + m - 10);
    m = u / (36*36*36*36);
    u -= m * (36*36*36*36);
    *date++ = (m < 10) ? (char)('0' + m) : (char)('A' + m - 10);
    m = u / (36*36*36);
    u -= m * (36*36*36);
    *date++ = (m < 10) ? (char)('0' + m) : (char)('A' + m - 10);
    m = u / (36*36);
    u -= m * (36*36);
    *date++ = (m < 10) ? (char)('0' + m) : (char)('A' + m - 10);
    m = u / (36);
    u -= m * (36);
    *date++ = (m < 10) ? (char)('0' + m) : (char)('A' + m - 10);
    *date++ = (u < 10) ? (char)('0' + u) : (char)('A' + u - 10);
    *date = 0;
    return(datestr);
}

/* original implementation follows */
char* root_id(int year, int day, int hour, int min, int sec)
{
        int year_79, elapsed, nleaps;
        static char code[7];

                                /* Check inputs for validity */
        year_79 = (year % 100) - 79;                    /* Years since 1979 */
        if (year_79 < 0) year_79 += 100;                /* Take care of Y2K */
        nleaps = (year_79 + 2) / 4;                     /* Count leap days */
        if(day < 1 || day > 366) return(NULL);
        if(day == 366 && (year_79 + 3) % 4 != 0) return(NULL);  /* Leap year? */
        if(hour < 0 || hour > 23) return(NULL);
        if(min < 0 || min > 59) return(NULL);
        if(sec < 0) return(NULL);  // allow secs beyond 59 for sequential rcode gen

                                /* 4-sec periods elapsed since 00:00 Jan 1 1979 */
        elapsed = year_79*7884000 + (day+nleaps-1)*21600 + hour*900 + min*15 + (sec/4);

        code[6] = '\0';
        code[5] = 'a' + elapsed % 26;
        code[4] = 'a' + (elapsed/26) % 26;
        code[3] = 'a' + (elapsed/676) % 26;
        code[2] = 'a' + (elapsed/17576) % 26;
        code[1] = 'a' + (elapsed/456976) % 26;                  /* 0 --> 'aaaaaa' */
        code[0] = 'a' + elapsed/11881376;

        return(code);
}

char* root_id_break(time_t now, int year, int day, int hour, int min, int sec)
{
    if (now + sec >= HOPS_ROOT_BREAK) return(root_id_later(now + sec));
    else return(root_id(year, day, hour, min, sec));
}

// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
