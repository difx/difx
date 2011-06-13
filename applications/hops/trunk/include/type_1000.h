struct binfo {
        char            refstat[8];             /*  */
        char            remstat[8];             /*  */
        char            baseline_id[2];         /*  */
        short           hardware_id;            /*  */
        short           bduration;              /*  */
        short           t_2000_no;              /*  */
};

struct type_1000 {
        short           record_id;              /*  */
        short           version_no;             /*  */
        short           last_extent;            /*  */
        short           no_baselines;           /*  */
        struct datec    utc_start;              /*  */
        short           duration;               /*  */
        char            program_name[8];        /*  */
        char            scan_name[8];           /*  */
        char            source[8];              /*  */
        char            name_freq[8];           /*  */
        short           procdate_yyddd;         /*  */
        unsigned short  procdate_hhmm;          /* has sec/4 in bits 12-15 */
        short           expt_no;                /*  */
        short           checksum;               /*  */
        struct binfo    barray[8];              /*  */
};
