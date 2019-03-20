#ifndef __TIMEUTILS_H__
#define __TIMEUTILS_H__

/* convert MJD to date */
int mjd2date(long mjd, int *pYear, int *pMonth, int *pDay);

/* convert MJD to day number of year */
int mjd2dayno(long mjd, int *pDayNo);

#endif
