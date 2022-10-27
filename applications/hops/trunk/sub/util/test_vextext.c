/*
 * Check that replacement of TEXT works
 */
#include <stdio.h>
#include <mk4_util.h>

int main()
{
    extern char textdir[];
    environment();
    printf("textdir is %s\n", textdir);
    return(0);
}

/*
 * eof
 */
