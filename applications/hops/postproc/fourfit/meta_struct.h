/*
 * Capture various bits of meta-data that are computed
 * or generated during the plotting process/preparation
 * but not actually stored elsewhere.
 */

struct type_meta {
    /* defined in generate_text.c */
    char    corrvers[36];   /* correlator version */
    char    rt_sn_bl[80];   /* rootfile scanname baseline */
    char    polstr[13];     /* pol'n string worked out in ... */
    char    exper_name[10]; /* from (o)vex */
    int     exper_num;      /* from (o)vex */
    char    yr_doy[10];     /* year and doy */
    char    pass_start[13]; /* pass start time */
    char    pass_stop[13];  /* pass stop time */
    char    pass_frt[13];   /* pass fourfit ref time */
    char    corr_time[23];  /* correlation time */
    char    ff_time[23];    /* fourfit time */
    char    build_time[23]; /* build time */
    char    fourfitcmd[256];/* fourfit exec found by which */
    char    ra[23];         /* R.A. */
    char    dec[23];        /* Decl */

    /* defined in generate_graphs.c */
    int     start_plot;     /* starting freq for segmented data */
    int     nplots;         /* number of freqs for segmented data */
};

/*
 * eof
 */
