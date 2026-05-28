#define NONE     0
#define XWINDOW  1
#define HARDCOPY 2
#define PRINTLPR 3
#define GSDEVICE 4
#define DISKFILE 5
#define PSTOPDF  6

#include "type_203.h"
#include "type_205.h"

extern int parse_cmdline (
    int argc, char** argv, fstruct** files, int* display, char** file_name,
    int *poln);
extern int skip_poln(char *name,
    struct type_203 *t203, struct type_205 *t205, int poln);
#ifdef MK4_DATA
extern char display_221 (struct type_221 *, int);
#endif /* MK4_DATA */
