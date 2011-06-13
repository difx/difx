/* reverse.c - byte reversing procedures to be used on Fourfit data */
/*
 * Created by David E. Flynt, 8/8/94
 *
 */

#include <stdio.h>
#include "mk4_util.h"

/* short_reverse: reverse the order of bytes in a short integer */
void short_reverse(short *i)
{
  char *p1 = (char *)i, *p2, temp;
  p2 = p1 + 1;

  temp = *p1;
  *p1  = *p2;
  *p2  = temp;
}

/* unsig_reverse: reverse the order of bytes in a unsigned short integer */
void unsig_reverse(unsigned short *i)
{
  char *p1 = (char *)i, *p2, temp;
  p2 = p1 + 1;

  temp = *p1;
  *p1  = *p2;
  *p2  = temp;
}

/* int_reverse: reverse the order of bytes in an integer */
void int_reverse(int *j)
{
  char *p = (char *)j, b[4];		/* p points to the first byte of j */
  int i;

  for (i = 0; i < 4; i++)
    b[i] = *p++;

  p = (char *) j + 3;		/* p points to the last(4th) byte of j */

  for (i = 0; i < 4; i++)
    *p-- = b[i];
}

/* long_reverse: reverse the order of bytes in a 64-bit integer */
void long_reverse(long *j)
{
  char *p = (char *)j, b[8];		/* p points to the first byte of j */
  int i;

  for (i = 0; i < 8; i++)
    b[i] = *p++;

  p = (char *) j + 7;		/* p points to the last(8th) byte of j */

  for (i = 0; i < 8; i++)
    *p-- = b[i];
  
}

/* float_reverse: reverse the order of bytes in a float */
void float_reverse(float *j)
{
  char *p = (char *)j, b[4];		/* p points to the first byte of j */
  int i;

  for (i = 0; i < 4; i++)
    b[i] = *p++;

  p = (char *) j + 3;		/* p points to the last(4th) byte of j */

  for (i = 0; i < 4; i++)
    *p-- = b[i];
  
}

/* double_reverse: reverse the order of bytes in a double */
void double_reverse (double *j)
{
  char *p = (char *)j, b[8];		/* p points to the first byte of j */
  int i;

  for (i = 0; i < 8; i++)
    b[i] = *p++;

  p = (char *) j + 7;		/* p points to the last(8th) byte of j */

  for (i = 0; i < 8; i++)
    *p-- = b[i];
  
}
