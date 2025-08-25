#ifndef __TIMEUTILS_H__
#define __TIMEUTILS_H__

//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: timeutils.h 9128 2019-09-03 18:49:27Z WalterBrisken $
// $HeadURL: $
// $LastChangedRevision: 9128 $
// $Author: WalterBrisken $
// $LastChangedDate: 2019-09-04 02:49:27 +0800 (三, 2019-09-04) $
//
//============================================================================


/* convert MJD to date */
int mjd2date(long mjd, int *pYear, int *pMonth, int *pDay);

/* convert MJD to day number of year */
int mjd2dayno(long mjd, int *pDayNo);

#endif
