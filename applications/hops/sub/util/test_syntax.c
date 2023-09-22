/*
 * Simple check that the syntax functions work
 * (c) Massachusetts Institute of Technology, 2021
 * The contents of the package Copyright statement apply here.
 */
#include <stdlib.h>

#include "mk4_util.h"

int main()
{
    char *tv = getenv("testverb");
    char memyself[] = "testing version and syntax";
    int verb = (tv) ? atoi(tv) : 0;
    set_progname("test_syntax");
    set_msglev(1);
    if (verb>0) printf("testing version\n");
    version(memyself);
    if (verb>0) printf("testing syntax\n");
    syntax(memyself);
    return(0);
}

/*
 * eof
 */
