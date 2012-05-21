/* GERROR -- returns system error message string [Linux]   */
/* ERRORSTRING  (output) : receives the error string       */
/* len (input) : gives the space allocated to ERRORSTRING) */
/* by kaj.wiik@hut.fi (6 Jul 1994)                         */

#include <sys/errno.h>
#include <string.h>

void gerror_ (char *errorstring, long len) {

char apuli[128];
int i,l;

strcpy(apuli, strerror(errno));
l = strlen(apuli);

if (l <= len)
  {
    for (i = 0; i < l; i++)
      errorstring[i] = apuli[i];

    for (i = l; i < len; i++)
      errorstring[i] = ' ';
  }

else
  {
    for (i = 0; i < l; i++)
      errorstring[i] = apuli[i];
  }

}
