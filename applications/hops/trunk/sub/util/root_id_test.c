#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mk4_util.h"

char progname[]= "testutil";
int msglev = 2;

main (argc, argv)
int argc;
char *argv[];
    {
    char *tv = getenv("testverb"), *r2000, *r1999;
    int verb = (tv) ? atoi(tv) : 0, errs = 0;

    r2000 = root_id (2000, 1, 0, 0, 0);
    if (verb>0) printf ("Jan 1 2000 = '%s' == 'nyoaya'?\n", r2000);
    errs += strcmp(r2000, "nyoaya");

    r1999 = root_id (1999, 244, 0, 0, 0);
    if (verb>0) printf ("Sep 1 1999 = '%s' == 'nsucse'?\n", r1999);
    errs += strcmp(r1999, "nsucse");

    if (verb>0) printf ("Had %d errors\n", errs);
    return(errs);
    }
