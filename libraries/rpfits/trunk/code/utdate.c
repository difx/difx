/*============================================================================
*   Fortran wrapper for the unix library functions time(2) and gmtime(3) to
*   get the current UTC year, month and day.  Replaces the non-standard
*   Fortran intrinsics TIME and GMTIME for use by DATFIT.
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id: utdate.c,v 1.1 2006/10/25 01:50:07 cal103 Exp $
*===========================================================================*/

#include <time.h>

void utdate_(int *year, int *month, int *day)

{
  time_t now;
  struct tm *utc;

  time(&now);
  utc = gmtime(&now);

  *year  = 1900 + utc->tm_year;
  *month =    1 + utc->tm_mon;
  *day   = utc->tm_mday;

  return;
}
