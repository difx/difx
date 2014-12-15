/*
 * $Id: vdif_time_util.h 2420 2014-08-28 16:24:37Z gbc $
 *
 * VDIF time utilities
 */

#ifndef vdif_time_util_h
#define vdif_time_util_h

//#include "bmr_common.h"
//
#ifndef TimeVal_defined
#define TimeVal_defined
typedef struct timeval TimeVal;
#endif /* TimeVal_defined */
#ifndef TimeSpec_defined
#define TimeSpec_defined
typedef struct timespec TimeSpec;
#endif /* TimeSpec_defined */

#ifndef PRI_VDIF_defined
#define PRI_VDIF_defined
#if __WORDSIZE == 32
#define PRI_TV "%ld.%06ld"
#define PRI_TS "%ld.%09ld"
#define PRI_TNS "%09ld"
#define PRI_TSS "%ld"
#elif __WORDSIZE == 64
#define PRI_TV "%ld.%06ld"
#define PRI_TS "%ld.%09ld"
#define PRI_TNS "%09ld"
#define PRI_TSS "%ld"
#else
/* #warning "__WORDSIZE not defined" */
#define PRI_TV "%d.%06d"    /* TimeVal */
#define PRI_TS "%d.%09d"    /* TimeSpec */
#define PRI_TNS "%09d"      /* TimeSpec ns */
#define PRI_TSS "%d"
#endif /* __WORDSIZE */
#endif /* PRI_VDIF_defined */


#define TIMESTYLE_SECS	0   /* UNIX system clock */
#define TIMESTYLE_WEEK	1   /* [+|-]SS[.ss] */
#define TIMESTYLE_DOT	2   /* [YY]YYMMDDHHMMSS[.ss] */
#define TIMESTYLE_ISO	3   /* YYYY-MM-DDTHH:MM:SS[.ss] */
#define TIMESTYLE_VEX	4   /* [YY]YYyDOYdHHhMMmSS[.ss] */
#define TIMESTYLE_VDIF	5   /* XX@SS.ss */
#define TIMESTYLE_MJD	6   /* ddddd.dddddddddd */
#define TIME_BUFFER_SIZE    40

/* 1970 calendar secs as a (Modified) Julian day. */
#define MJD_UNIX_EPOCH   (40587.0)/*days*/

/* Parses and input time and returns seconds on the UNIX clock */
extern int timevdifepoch;   /* 6-month periods from 2000 < 2032 */
extern int timetruncate;    /* whether to truncate or round */
extern int time_TZ_not_UTC;    /* means TZ may not be UTC */

/* front end parsers */
extern time_t timeis(char *t);	    /* just the seconds */
extern TimeSpec timens(char *t);    /* full nanoseconds */

/* various parsers invoked by timens() */
extern TimeSpec secsstr2TS(char *t);
extern TimeSpec weekstr2TS(char *t);
extern TimeSpec dot_str2TS(char *t);
extern TimeSpec iso_str2TS(char *t);
extern TimeSpec mjd_str2TS(char *t);
extern TimeSpec vex_str2TS(char *t);
extern TimeSpec vdifstr2TS(char *t);

/* Formats the time (several formats) by style */
extern char *timefmtTT(time_t tt, char *buf, int style);
extern char *timefmtTV(TimeVal tv, char *buf, int style);
extern char *timefmtTS(TimeSpec ts, char *buf, int style);
/* using these: */
extern char *timeTS2secsstr(TimeSpec ts, char *buf);
extern char *timeTS2weekstr(TimeSpec ts, char *buf);
extern char *timeTS2dot_str(TimeSpec ts, char *buf);
extern char *timeTS2iso_str(TimeSpec ts, char *buf);
extern char *timeTS2mjd_str(TimeSpec ts, char *buf);
extern char *timeTS2vex_str(TimeSpec ts, char *buf);
extern char *timeTS2vdifstr(TimeSpec ts, char *buf);
/* with style determined by */
extern int timestyle(char *t);
extern void time_ns_precision(int n);	/* how many (1..9) digits to print */

/* TimeSpec utility: *when -= *delta (*delta>0, *when>0), returns when */
extern TimeSpec *time_decrement(TimeSpec *when, TimeSpec *delta);
/* TimeSpec utility: *when += *delta (*delta>0, *when>0), returns when */
extern TimeSpec *time_increment(TimeSpec *when, TimeSpec *delta);

/*
 * Advances the command pointer string to something interesting
 * Returns 0 if nothing (of interest) is left.
 */
extern char *skip_leading_space(char *cmd);
extern void nuke_trailing_space(char *cmd);

/*
 * Report resource limits to stderr.
 */
extern void rlimit_env(void);

#endif /* vdif_time_util_h */

/*
 * eof
 */
