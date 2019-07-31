/*
 * Test program to verify structure sizes
 *
 */

#include "adata.h"

int main(int argc, char **argv)
{
    printf("sizeof(rootsum) is %d\n", sizeof(rootsum));
    printf("sizeof(corelsum) is %d\n", sizeof(corelsum));
    printf("sizeof(fringesum) is %d\n", sizeof(fringesum));
    printf("sizeof(trianglesum) is %d\n", sizeof(trianglesum));
    printf("sizeof(quadsum) is %d\n", sizeof(quadsum));
    return(0);
}

/*
 * eof
 */
