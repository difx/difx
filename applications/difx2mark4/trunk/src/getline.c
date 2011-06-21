/*  
 *
 * Peter Lemkin
 * Image Processing Section
 * Laboratory of Mathematical Biology
 * National Cancer Institute, FCRDC
 * Frederick, MD 21702
 *
 *    Written 1997  by Peter F. Lemkin. lemkin@ncifcrf.gov
 *

 * $Date: 1999/06/08 15:40:10 $  $Revision: 1.46 $
 *
 */


/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 * getline() -  get line from input stream and return EOF status
 * Derived from the NCSA HTTP util.c code.
 * REturn data in preallocated buffer of max size [0:n-1].
 * Return value of feof(f).
*/

#include <stdio.h>

static int getline(char *s, int n, FILE *f) {
  int ch;
  int nMinus1= (n-1), i= 0;

  while (1) {
    ch = getc(f);
      
    if (ch == '\r') ch = getc(f);
    
    if ((ch == '\n') || (ch == EOF) || (i == nMinus1)) {
      *s= '\0';
      return(feof(f) ? 1 : 0);
    } else
      *s++ = ch;
    i++;
  }
}/*getline*/
