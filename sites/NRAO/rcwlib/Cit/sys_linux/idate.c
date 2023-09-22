/* this should replace the FORTRAN library function idate
 * $Log$
 * Revision 1.1  2005/06/20 15:16:24  reynolds
 * Initial revision
 *
 * Revision 1.3  1992/09/04  19:05:04  corbet
 * inserted missing include files
 *
 * Revision 1.2  1992/09/03  19:53:43  corbet
 * real routine from Andrew Wilcox replaces dummy
 *
 * Revision 1.1  1992/09/01  21:19:39  corbet
 * Initial revision
 *
 */
static char rcsid[] = "$Id: idate.c 2 2005-06-20 15:16:10Z reynolds $";
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
void
idate_(
 iarray )
  int iarray[];
{
  struct tm* tm;		/* date/time structure */
  time_t clock;			/* current time in seconds  */

  clock = time(0);
  tm = localtime(&clock);
  iarray[0] = tm->tm_mday;	/* day of month */
  iarray[1] = tm->tm_mon + 1;	/* month */
  iarray[2] = tm->tm_year;	/* year */
}
