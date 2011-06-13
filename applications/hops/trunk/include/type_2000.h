struct crossref {
        short           index_no;               /*  */
        short           extent_no;              /*  */
        short           corr_slot;              /*  */
        short           ref_track;              /*  */
        short           rem_track;              /*  */
        short           freq_index;             /*  */
        short           partner_track;          /*  */
        char            freq_group[2];          /*  */
        char            ref_tdrive;             /*  */
        char            rem_tdrive;             /*  */
        short           post_mortem;            /*  */
        short           delay_offset;           /*  */
        short           procdate_yyddd;         /*  */
        short           procdate_hhmm;          /*  */
        short           corr_type;              /*  */
        short           module_ser_no;          /*  */
        short           gate_on;                /*  */
        short           gate_off;               /*  */
        char            corel_code[2];          /*  */
};

struct type_2000 {
        short           record_id;              /*  */
        short           baseline_no;            /*  */
        char            baseline_id[2];         /*  */
        short           ref_head_pos;           /*  */
        struct datec    utc_apriori;            /*  */
        short           rem_head_pos;           /*  */
        short           no_frames;              /*  */
        char            ref_pass_no;            /*  */
        char            rem_pass_no;            /*  */
        short           ksel;                   /*  */
        short           corel_mode;             /*  */
        short           no_tracks;              /*  */
        short           high_index;             /*  */
        char            ref_tape_pass;          /*  */
        char            rem_tape_pass;          /*  */
        char            record_mode[2];         /*  */
        struct crossref xref[6];                /*  */
};
        
