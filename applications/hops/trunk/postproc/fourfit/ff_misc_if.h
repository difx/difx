/*
 * Include various function prototypes to eliminate
 * many common compiler complaints  -- this is a
 * temporary convenience to avoid tracking down
 * and including all the files that might have been
 * included if the .h set had been done differently
 */
#ifndef FF_MISC_IF_H_
#define FF_MISC_IF_H_

// this file must be included last

extern int fcode(char c, char *);
extern void msg (char *, int, ...);
extern char *account (char *);

/* make_plotdata */
extern int parabola (double *, double, double, double *, double *, double *);
#ifdef PASS_STRUCT
extern void calc_rms (struct type_pass *pass);
#endif /* PASS_STRUCT */


/* norm_xf */
#ifdef PASS_STRUCT
extern void calc_normalization (int sb, int pol, struct data_corel *datum,
                         struct type_pass *pass, double *mean,
                         double *norm_const);
#endif /* PASS_STRUCT */

// lots of onesies
//      6   ‘account’ 
//      2   ‘ap_mean’ 
//      2   ‘clear_freq_corel’ 
//      2   ‘criteria_match’ 
//      2   ‘fcode’ 
//      2   ‘grid’ 
//      2   ‘iwin’; did you mean ‘dwin’? 
//     57   ‘msg’ 
//      2   ‘nullify_cblock’ 
//      2   ‘parabola’ 
//      2   ‘parse_control_file’ 
//      2   ‘skip_data’ 
//      2   ‘syntax’ 
//      2   ‘time_to_int’ 

#endif /* FF_MISC_IF_H_ */
/*
 * eof
 */
