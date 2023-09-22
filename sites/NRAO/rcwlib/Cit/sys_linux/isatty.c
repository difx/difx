/* If the logical unit is 5 (standard input   */
/* for Fortran), use the *real* (C) isatty    */
/* to determine if it is interactive or not.  */
/* Otherwise, assume not interactive          */

#include <unistd.h>

int isatty_(int *lunit)
{
  if (*lunit == 5) return isatty(0);
  else return 0;
}
