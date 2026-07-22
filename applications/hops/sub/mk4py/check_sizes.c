/*
 * Test program to verify structure sizes
 *
 */

#include <stdio.h>
#include "adata.h"

int main(int argc, char **argv)
{
    printf("sizeof(rootsum) is %lu\n", sizeof(rootsum));
    printf("sizeof(corelsum) is %lu\n", sizeof(corelsum));
    printf("sizeof(fringesum) is %lu\n", sizeof(fringesum));
    printf("sizeof(trianglesum) is %lu\n", sizeof(trianglesum));
    printf("sizeof(quadsum) is %lu\n", sizeof(quadsum));
    return(0);
}

/*
 * eof
 */
