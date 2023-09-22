// conv2date takes an mjd and converts it to an integer date in a date structure
// much of the code lifted from W. Brisken's mjd2date routine
//
// 2,400,000 (difference between Julian Date and Modified Julian Date) 
// minus # days from jan 1, 4713 BC (beginning of Julian calendar)
//
// first written                rjc  2020.2.26
//
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2mark4.h"

#define AD 678576

void conv2date (double dmjd,
                struct date *pdate) // returned date structure
    {
    int mjd, ihh, imm, iss;
    int icen4, icen, iyr4, iyr, iday;

    dmjd = dmjd + 1e-8;             // guard against truncation to lower second
    mjd = dmjd;
                                    // check input range and calc days 
                                    // since jan 1 1 AD (Gregorian Calendar)
    if (mjd > 2973483 || (mjd += AD - 1) < 0)
        {
        pdate->year = pdate->day = pdate->hour = pdate->minute = pdate->second = 1;
        fprintf (stderr, "bad mjd %lf, setting date to all 1's\n", dmjd);
        return;
        }

                                    // calc number of fours of Gregorian centuries
    icen4 = mjd / 146097;

                                    // calc number of centuries since last fours of
                                    // Gregorian centuries (e.g. since 1600 or 2000)
    mjd -= (icen4 * 146097);
    if ((icen = mjd / 36524) == 4)
        icen = 3; 

                                    // calc number of quadrenia(four years) since 
                                    // jan 1, 1901
    mjd -= (icen * 36524);
    iyr4 = mjd / 1461;

                                    // calc number of years since last quadrenia
    mjd -= (iyr4 * 1461);
    if ((iyr = mjd / 365) == 4)
        iyr = 3;

                                    // calc number of days since jan 1 of current year
    iday = mjd - iyr * 365;
                                    // now calculate hh, mm, and ss
    iss = (dmjd - (int)dmjd) * 86400.0;
    ihh = iss / 3600.0;

    iss -= ihh * 3600;
    imm = iss / 60.0;

    iss -= imm * 60;
                                    // now load up the return values
    pdate->year = icen4 * 400 + icen * 100 + iyr4 * 4 + iyr + 1;
    pdate->day = iday + 1;            // doy
    pdate->hour = ihh;
    pdate->minute = imm;
    pdate->second = iss;
    return;
}
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
