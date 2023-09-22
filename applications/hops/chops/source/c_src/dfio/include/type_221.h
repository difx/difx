#ifndef T221_VERSION
#define T221_VERSION 0

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_221_v0 type_221


struct type_221
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1;                /* Reserved space */
    short       padded;                 /* Flag for padding to 8-byte boundary */
    int         ps_length;              /* Size of postscript plot in chars */
    char        pplot[1];               /* Postscript (variable length) */
    };

#endif
