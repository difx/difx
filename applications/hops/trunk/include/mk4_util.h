#ifndef MK4_UTIL_H
#define MK4_UTIL_H

/*----------------------------------------------------------------------------*/
#include <stdarg.h>
#include <sys/times.h>
#include "account.h"
#include "fileset.h"
#include "fstruct.h"
#include "general.h"
#include "mk4_typedefs.h"

/*----------------------------------------------------------------------------*/
extern char   *account (char *);
extern int    check_name (char *, fstruct *);
extern void   clear_date (date_struct *);
extern void   clear_fstruct (fstruct *);
extern int    confirm (char *);
extern void   datec_to_datef (struct datec *, struct datef *);
extern void   datef_to_datec (struct datef *, struct datec *);
extern short  day_of_datef (struct datef);
extern void   environment (void);
extern int    extract_filenames (char *, int, fstruct **, int *, int *, int *);
extern int    fileset (char *, struct fileset *);
extern int    get_abs_path (char [], char []);
extern int    get_filelist (int, char **, int, fstruct **);
extern int    hptoie4 (float *, float *);
extern int    hptoie8 (double *, double *);
extern int    hptoie (int *, int *, int);
extern void   int_to_time (int, int *, int *, int *, int *, int *);
extern int    ismk4 (char *, int);
extern int    imin (int, int);
extern int    imax (int, int);
extern int    iwin (int, int, int);
extern double dmin (double, double);
extern double dmax (double, double);
extern double dwin (double, double, double);
extern void   msg (char *, int, ...);
extern int    report_times (struct time_account *, int, struct tms *, int,
                            double);
extern int    root_belong (char *, char *);
extern char   *root_id (int, int, int, int, int);
extern int    sort_names (fstruct *, int);
extern void   short_reverse (short *);
extern void   unsig_reverse (unsigned short *);
extern void   int_reverse (int *);
extern void   long_reverse (long *);
extern void   float_reverse (float *);
extern void   double_reverse (double *);
extern void   syntax (void);
extern double time_to_double (struct date);
extern int    time_to_int (int, int, int, int, int);

#endif

