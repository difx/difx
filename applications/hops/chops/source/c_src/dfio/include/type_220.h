#ifndef T220_VERSION
#define T220_VERSION 0

struct type_220
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    short       width;                  /* Width of fringe plot in chars */
    short       height;                 /* Height of fringe plot in chars */
    char        **fplot;                /* Pointer to fringeplot array */
    };

#endif
