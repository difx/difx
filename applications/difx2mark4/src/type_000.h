struct type_000 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    char                date[16];               /* Creation date " yyyyddd-hhmmss " */
    char                name[40];               /* exp/scan/name, null-terminated */
    };
