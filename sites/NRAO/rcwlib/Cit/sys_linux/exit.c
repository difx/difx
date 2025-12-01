/* Wrapper function for exit */
/* by kaj.wiik@hut.fi (25 May 1996) */

#include <stdlib.h>

void exit_ (int *errno) {

  exit(*errno);

}
