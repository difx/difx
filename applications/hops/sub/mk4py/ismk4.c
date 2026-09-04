/************************************************************************/
/*                                                                      */
/* Given a filename and a file type, this routine determines whether    */
/* it is a Mk4 file or not.  If not, it doesn't bother to find out      */
/* anything else, leaving error handling to downstream routines         */
/*                                                                      */
/*      Inputs:     name        full pathname of file                   */
/*                  type        0 to 4                                  */
/*                                                                      */
/*      Output:     return value  TRUE or FALSE                         */
/*                                                                      */
/* Created September 10 1999 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "mk4_util.h"

#define TRUE 1
#define FALSE 0

int
ismk4(char *name, int type)
    {
    char *relname, *rootcode, buf[10];
    FILE *fp;

    relname = strrchr (name, '/') + 1;

    rootcode = strrchr (name, '.') + 1;
    if (strlen (rootcode) != 6) return (FALSE);
                                        /* Try to avoid file IO ... */
                                        /* After Jan 1 2000 => Mk4 */
                                        /* Before Sep 1 1999 => Mk3 */
/*     if (strcmp (rootcode, "nyoaya") > 0) return (TRUE); */
/*     if (strcmp (rootcode, "nsmaaa") < 0) return (FALSE); */
                                        /* Date method failed, must look closer */
    switch (type)
        {
                                        /* Mk3 root files start with binary 1000 */
        case 0:
            if ((fp = fopen (name, "r")) == NULL) return (FALSE);
            if (fread (buf, sizeof (char), 2, fp) != 2) 
                {
                fclose (fp);
                return (FALSE);
                }
            fclose (fp);
                                        /* Don't want to deal with byteflip, so */
                                        /* just check for ascii ( => vex ) */
                                        /* binary 1000 has a non-ascii byte */
            if (isascii (buf[0]) && isascii (buf[1])) return (TRUE);
            else return (FALSE);
                                        /* Mk4 type-1 files have no extent number */
        case 1:
            if (strstr (relname, "..") != NULL) return (TRUE);
            else return (FALSE);
                                        /* Mk4 type-2 files start with ascii "000" */
        case 2:
            if ((fp = fopen (name, "r")) == NULL) return (FALSE);
            if (fread (buf, sizeof (char), 3, fp) != 3) 
                {
                fclose (fp);
                return (FALSE);
                }
            fclose (fp);
            if (strncmp (buf, "000", 3) == 0) return (TRUE);
            else return (FALSE);
                                        /* Types 3 and 4 are Mk4 only */
        case 3:
        case 4:
            return (TRUE);

        default:
            return (FALSE);
        }
    }

