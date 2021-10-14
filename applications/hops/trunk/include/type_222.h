#ifndef T222_VERSION
#define T222_VERSION 0

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_222_v0 type_222


struct type_222
    {
    char            record_id[3];           /* Standard 3-digit id */
    char            version_no[2];          /* Standard 2-digit version # */
    char            unused1;                /* Reserved space */
    short           padded;                 /* Flag for padding to 8-byte boundary */
    unsigned int    setstring_hash;          /* Hash of setstring contents */
    unsigned int    control_hash;            /* Hash of control file contents */
    int    setstring_length;       /* Size of set string in chars */
    int    cf_length;              /* Size of control file in chars */
    
    /* The following char array is variable length padded to a */
    /* multple of 8 bytes, but its always at least 8 bytes long */
    char control_contents[8];  /* Set string and control file contents*/
    };

#endif
