#include <stdlib.h>
#include <string.h>


char *
sprint_char_arr( char* array, int n)
    {
    static char result[256];

/*    result = (char *) malloc(n+1); */
    if (n > 255) return (NULL);
    strncpy(result, array, n);
    result[n] = 0;
    return result;
    } /* sprint_char_arr */
