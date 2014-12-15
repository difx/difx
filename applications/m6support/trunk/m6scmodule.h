/*
 * $Id: m6scmodule.h.in 2560 2014-10-08 21:23:19Z gbc $
 *
 * Python interfaces to the scan checker
 */

#ifndef m6scmodule_h
#define m6scmodule_h

#define M6_VERSION 0.14

/*
 * Scan check links to main()
 */
extern char *m6sc_get_chk_opt(const char *arg);
extern int m6sc_set_chk_opt(const char *arg);
extern int m6sc_per_file(const char *file, int files, int verb);
extern void m6sc_verbosity(const int verbose);

/*
 * scan_check needs to return:
 *
 * scan_check ? <return code> : <cplane return code>:
 *    <group_ref> : <scan#> : <scan label> : <#streams>
 *    : <stream label1> : <status1> : <data format1> :
 *      <start time1> : <duration1> : <datasize1> :
 *      <stream rate1> : <missing bytes1>
 *    : <stream label2> :...: <missing bytes2>
 *    : <stream labeln> :...: <missing bytesn> ;
 *
 * scan_check will need to be called per-stream on a sg group
 *   status = OK | time? (framing) | data? (statistics)
 *   duration =  #seconds
 *   datasize = total, in GB
 *   stream rate = Gbps (inferred from sizes and seconds)
 *   missing bytes = estimated
 *
 * To avoid lots of interface issues, we'll start with this:
 *   m6sc_sg_group() declares the start of group checking
 *   call check_file() on all the files in the group
 *   m6sc_sg_status() returns status on the group
 * scanref is some identifying string: scanname|scan# : gref
 *
 * nf is planned number of files, mf is expected max frame rate (0 if unknown),
 * tol is fractional tolerance on statistics assessment (0 -> 0.2 default).
 */
extern void m6sc_sr_start(const char *scanref, int nf, int mf, double tol);
extern void m6sc_sr_accum(void);
extern char *m6sc_sr_status(const char *scanref);
#define m6sc_NULL_STATUS    "-:-:-:-:-:-:-:-"

/*
 * The above will retain relevant info in the following structure
 * which is initialized on m6sc_sr_start(), accumulated during
 * m6sc_per_file() and finally reported on with m6sc_sr_status();
 */
typedef struct sc_info {
    char   scanref[512];      /* copy of the current scanref */
    char   status[512];       /* final status message */
    int    nfiles;            /* number of files checked */
    int    nf_exp;            /* expected number of files */
    double pc[4];             /* statistics accumulators */
    double tol;               /* tolerance on stat assessment */
    int    bps;               /* bits / sample */
    int    stat_err;          /* statistics error */
    int    time_err;          /* timing error */
    int    ref_epoch;         /* reference epoch */
    int    alpha_s;           /* earliest first_secs */
    int    alpha_fr;          /* earliest first_frame */
    int    omega_s;           /* latest final_secs */
    int    omega_fr;          /* latest final_frame */
    int    max_fr;            /* max frame count seen */
    int    exp_fr;            /* expected max frame count */
    int    npkts;             /* accumulator of size packets */
    int    bpp;               /* bytes / packet */
} SCInfo;

/* This dumps the above in a string representation for debuggging */
extern char *m6sc_sr_scinfo(void);

#endif /* m6scmodule_h */

/*
 * eof
 */
