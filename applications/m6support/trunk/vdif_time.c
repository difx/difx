/*
 * $Id: bmr_time.c 1695 2012-02-22 20:21:45Z gbc $
 *
 * BMR Server time utilities
 *
 * We support a few flavors here (in order of separability):
 *
 *  Vdif XX@SS.ss
 *  Vex  [YY]YYyDOYdHHhMMmSS.ss
 *  Week [+|-]SS[.ss]
 *  UNIX SS[.ss]
 *  ISO  YYYY-MM-DDTHH:MM:SS[.ss]
 *  DOT  [YY]YYMMDDHHMMSS[.ss]
 *
 * Truncation at either end is (to be) supported if feasible.
 * Time is UTC (not local).
 * Vdif epochs are 6 bits (0..63) so validity limited to 2000..2031+
 */

#include <ctype.h>
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

#include "vdif_time_util.h"
#include "vdif_epochs.h"	/* for vdif_epochs[] */

#define UNIX_J2000  946684800	/* 2000-01-01T00:00:00 */
#define ONE_SEC_NS  1000000000
#define HALF_SEC_NS 500000000
#define HALF_SEC_US 500000

int timevdifepoch = 20;	    /* Jan 1 of 2010 */
int timetruncate = 1;	    /* 0 for round */
int time_TZ_not_UTC = 1;    /* unknown, really */

#define DEFAULT_secsfmt "%.9lf"
#define SECONDS_secsfmt "%.0lf"
#define DEFAULT_dot_fmt "%04d%02d%02d%02d%02d%02d.%09ld"
#define SECONDS_dot_fmt "%04d%02d%02d%02d%02d%02d"
#define DEFAULT_iso_fmt "%04d-%02d-%02dT%02d:%02d:%02d.%09ld"
#define SECONDS_iso_fmt "%04d-%02d-%02dT%02d:%02d:%02d"
#define DEFAULT_mjd_fmt "%16.10f"
#define SECONDS_mjd_fmt "%5.0f"
#define DEFAULT_vex_fmt "%04dy%03dd%02dh%02dm%02d.%09lds"
#define SECONDS_vex_fmt "%04dy%03dd%02dh%02dm%02ds"
#define DEFAULT_vdiffmt "%02d@%.9lf"
#define SECONDS_vdiffmt "%02d@%.0lf"
static char secsfmt[50] = DEFAULT_secsfmt;
static char dot_fmt[50] = DEFAULT_dot_fmt;
static char iso_fmt[50] = DEFAULT_iso_fmt;
static char mjd_fmt[50] = DEFAULT_mjd_fmt;
static char vex_fmt[50] = DEFAULT_vex_fmt;
static char vdiffmt[50] = DEFAULT_vdiffmt;
static int  tendivisor = 1;

/* Flag whether the user insisted on precision */
static int  preciset = 0;

/*
 * This forces the issue, *once*.
 */
static void set_the_tz(void)
{
    putenv("TZ=UTC");
    tzset();
    time_TZ_not_UTC = 0;
}

/*
 * Parses DOT [YY]YYMMDD[HHMMSS[.ss]]
 * Expects the century if you truncate too much.
 */
TimeSpec dot_str2TS(char *t)
{
    TimeSpec ts;
    struct tm tm;
    int num, cc, ix = 0;
    double ns = 0;
    if (time_TZ_not_UTC) set_the_tz();
    num = sscanf(t, "%2d%2d%2d%2d%2d%2d%2d%n",
	&cc, &tm.tm_year, &tm.tm_mon, &tm.tm_mday,
	&tm.tm_hour, &tm.tm_min, &tm.tm_sec, &ix);
    switch (num) {
    case 4: /* CCYYMMDD */
	tm.tm_hour = timetruncate ? 0 : 12;
	tm.tm_min = 0;
	tm.tm_sec = 0;
	break;
    case 5: /* CCYYMMDDHH */
	tm.tm_min = timetruncate ? 0 : 30;
	tm.tm_sec = 0;
	break;
    case 6: /* partly qualified */
	if (cc == 20 || cc == 19) {	/* [19|20]YYMMDDHHMM[??] */
	    ns = 0;
	    tm.tm_sec = timetruncate ? 0 : 30;
	} else {			/* [??]YYMMDDHHMMSS[.ss] */
	    if (t[ix] == '.') ns = ONE_SEC_NS * atof(t + ix);
	    else              ns = timetruncate ? 0 : HALF_SEC_NS;
	    tm.tm_sec = tm.tm_min;
	    tm.tm_min = tm.tm_hour;
	    tm.tm_hour = tm.tm_mday;
	    tm.tm_mday = tm.tm_mon;
	    tm.tm_mon = tm.tm_year;
	    tm.tm_year = cc;
	    cc = tm.tm_year > 50 ? 19 : 20;
	}
	break;
    default:
        ts.tv_sec = 0L;
        ts.tv_nsec = 0L;
        ns = 0;
	break;
    case 7: /* fully qualified */
	if (t[ix] == '.') ns = ONE_SEC_NS * atof(t + ix);
	else              ns = timetruncate ? 0 : HALF_SEC_NS;
	break;
    }
    tm.tm_year += (cc - 19) * 100;	/* years since 1900 */
    tm.tm_mon -= 1;			/* 1..12 -> 0..11 */
    ts.tv_sec = mktime(&tm);		/* tm_wday,tm_yday,tm_isdst */
    ts.tv_nsec = (long)ns;
    return(ts);
}

/*
 * Parses ISO  YYYY-MM-DDTHH:MM:SS[.ss]
 */
TimeSpec iso_str2TS(char *t)
{
    TimeSpec ts;
    struct tm tm;
    int num, cc, ix = 0;
    double ns = 0;
    if (time_TZ_not_UTC) set_the_tz();
    num = sscanf(t, "%4d-%2d-%2dT%2d:%2d:%2d%n",
        &tm.tm_year, &tm.tm_mon, &tm.tm_mday,
        &tm.tm_hour, &tm.tm_min, &tm.tm_sec, &ix);
    switch (num) {
    default:
        ts.tv_sec = 0L;
        ts.tv_nsec = 0L;
        ns = 0;
	break;
    case 3:
	tm.tm_hour = timetruncate ? 0 : 12;
    case 4:
	tm.tm_min = timetruncate ? 0 : 30;
    case 5:
        tm.tm_sec = timetruncate ? 0 : 30;
        ns = 0L;
        break;
    case 6:
        if (t[ix] == '.') ns = ONE_SEC_NS * atof(t + ix);
        else              ns = timetruncate ? 0 : HALF_SEC_NS;
        break;
    }
    tm.tm_year -= 1900;                 /* years since 1900 */
    tm.tm_mon -= 1;                     /* 1..12 -> 0..11 */
    ts.tv_sec = mktime(&tm);            /* tm_wday,tm_yday,tm_isdst */
    ts.tv_nsec = (long)ns;
    return(ts);
}

/*
 * promote unix clock to timespec
 */
TimeSpec secsstr2TS(char *t)
{
    TimeSpec ts;
    double ns, is;
    ns = ONE_SEC_NS * modf(atof(t), &is);
    ts.tv_sec = (long)is;
    ts.tv_nsec = (long)ns;
    return(ts);
}

/*
 * Brutally convert MJD to Unix
 */
TimeSpec mjd_str2TS(char *t)
{
    TimeSpec ts;
    double secs = strtod(t, 0);
    secs -= MJD_UNIX_EPOCH;
    secs *= 86400;
    ts.tv_sec = (long)secs;
    ts.tv_nsec = (long)rint( 1e9 * (secs - (double)ts.tv_sec) );
    return(ts);
}

/*
 * Week [+|-]SS[.ss]
 * time is relative to now (presumably within the week)
 * consequently, ts.tv_sec is rather nonzero.
 */
#ifdef USE_TIME_0_
TimeSpec weekstr2TS(char *t)
{
    TimeSpec ts;
    double ns, is;
    ns = ONE_SEC_NS * modf(atof(t), &is);
    ts.tv_sec = time(0) + (long)is;
    ts.tv_nsec = (long)ns;
    if (ns < 0) {
	ts.tv_nsec += ONE_SEC_NS;
	ts.tv_sec -= 1;
    }
    return(ts);
}
#else /* USE_TIME_0_ */
TimeSpec weekstr2TS(char *t)
{
    TimeSpec ts;
    TimeVal  tv;
    double ns, is;
    gettimeofday(&tv, 0);
    ns = ONE_SEC_NS * modf(atof(t), &is);
    ts.tv_sec = tv.tv_sec + (long)is;
    ts.tv_nsec = tv.tv_usec*1000 + (long)ns;
    while (ns < 0) {
	ts.tv_nsec += ONE_SEC_NS;
	ts.tv_sec -= 1;
    }
    while (ns > ONE_SEC_NS) {
	ts.tv_nsec -= ONE_SEC_NS;
	ts.tv_sec += 1;
    }
    return(ts);
}
#endif /* USE_TIME_0_ */

/*
 * Vex  [YY]YYyDOYdHHhMMmSS[.ss]
 * lower significance parts droppable.
 */
TimeSpec vex_str2TS(char *t)
{
    TimeSpec ts;
    int	num, yy, doy, hh, mm, leap;
    double ss, ns, is;
    num = sscanf(t,
	(t[2] == 'y') ? "%2dy%3dd%2dh%2dm%lf" : "%4dy%3dd%2dh%2dm%lf",
	&yy, &doy, &hh, &mm, &ss);
    if (t[2] == 'y') yy += (yy > 50) ? 1900 : 2000;
    switch (num) {
    case 5: /* all present */
	break;
    case 4: /* missing secs */
	ss = timetruncate ? 0 : 30;
	break;
    case 3: /* missing mins */
	mm = timetruncate ? 0 : 30;
	ss = 0;
	break;
    case 2: /* missing hours */
	hh = timetruncate ? 0 : 12;
	mm = ss = 0;
	break;
    case 1: /* missing doy, really? */
	doy = timetruncate ? 0 : 183;
	hh = mm = ss = 0;
	break;
    default:
	yy = 2000;
	hh = mm = ss = 0;
	break;
    }
    ns = ONE_SEC_NS * modf(ss, &is);
    ts.tv_nsec = (long)ns;
    ts.tv_sec = (long)is;
    ts.tv_sec += mm * 60;
    ts.tv_sec += hh * 3600;
    ts.tv_sec += doy * 86400;
    /* valid 2000..2099 */
    num = yy - 2001;
    leap = num/4;
    num ++;
    ts.tv_sec += (num * 365 + leap) * 86400;
    ts.tv_sec += UNIX_J2000;
    return(ts);
}

/*
 * Vdif XX@SS.ss
 */
TimeSpec vdifstr2TS(char *t)
{
    TimeSpec ts;
    int num, ep;
    double sc, ns, is;
    num = sscanf(t, "%d@%lf", &ep, &sc);
    switch (num) {
    default:
    case 0:
	ep = 0;
    case 1:
	sc = timetruncate ? 0 : 61 * 86400.0;
    case 2:
	if (ep < 0) ep = 0;
	if (ep >= NUM_VDIF_EPOCHS) ep = NUM_VDIF_EPOCHS-1;
	break;
    }
    ns = ONE_SEC_NS * modf(sc, &is);
    ts.tv_sec = vdif_epochs[ep] + (int)(is);
    ts.tv_nsec = ns;
    return(ts);
}

/*
 * Figure out what we were given--not completely unambiguous.
 */
int timestyle(char *t)
{
    time_t  secs;
    while (isspace(*t)) t++;
    if (t[2] == '@' || t[1] == '@') return(TIMESTYLE_VDIF);
    if (t[2] == 'y' || t[4] == 'y') return(TIMESTYLE_VEX);
    if (t[0] == '+' || t[0] == '-') return(TIMESTYLE_WEEK);
    if (t[4] == '-' || t[10] == 'T') return(TIMESTYLE_ISO);
    if (t[5] == '.')                 return(TIMESTYLE_MJD);
    secs = atol(t);
    if (secs < 7*86400L)            return(TIMESTYLE_WEEK);
    if (secs > 946684800L &&
	secs < 1956528000L)         return(TIMESTYLE_SECS);
    return(TIMESTYLE_DOT);
}

/*
 * Switch into the various cases, full precision
 */
TimeSpec timens(char *t)
{
    TimeSpec ts;
    switch(timestyle(t)) {
    case TIMESTYLE_WEEK: ts = weekstr2TS(t); break;
    case TIMESTYLE_DOT:  ts = dot_str2TS(t); break;
    case TIMESTYLE_ISO:  ts = iso_str2TS(t); break;
    case TIMESTYLE_MJD:  ts = mjd_str2TS(t); break;
    case TIMESTYLE_VEX:  ts = vex_str2TS(t); break;
    case TIMESTYLE_VDIF: ts = vdifstr2TS(t); break;
    case TIMESTYLE_SECS: ts = secsstr2TS(t); break;
    default:             ts.tv_sec = 0;
			 ts.tv_nsec = 0;     break;
    }
    return(ts);
}

/*
 * Switch into the various cases, seconds only.
 */
time_t timeis(char *t)
{
    TimeSpec ts = timens(t);
    return(ts.tv_sec);
}

/*
 * Brutally convert Unix TS into MJD
 */
char *timeTS2mjd_str(TimeSpec ts, char *buf)
{
    double days = ((double)ts.tv_sec + 1e-9*((double)ts.tv_nsec + 0.5));
    days /= 86400.0;
    days += MJD_UNIX_EPOCH;
    /* probably always want this */
    sprintf(buf, mjd_fmt, days);
    return(buf);
}

char *timeTS2secsstr(TimeSpec ts, char *buf)
{
    if (ts.tv_nsec || preciset) {
	sprintf(buf, secsfmt, ts.tv_sec + (ts.tv_nsec+0.5)/(double)ONE_SEC_NS);
    } else {
	sprintf(buf, "%d", (int)ts.tv_sec);
    }
    return(buf);
}
#ifdef USE_TIME_0_
char *timeTS2weekstr(TimeSpec ts, char *buf)
{
    ts.tv_sec -= time(0);   /* pretty useless, actually */
    return(timeTS2secsstr(ts, buf));
}
#else /* USE_TIME_0_ */
char *timeTS2weekstr(TimeSpec ts, char *buf)
{
    TimeVal tv;
    gettimeofday(&tv, 0);
    ts.tv_sec -= tv.tv_sec;
    ts.tv_nsec -= tv.tv_usec * 1000;
    while (ts.tv_sec && ts.tv_nsec < 0) {
	ts.tv_nsec += ONE_SEC_NS;
	ts.tv_sec -= 1;
    }
    while (ts.tv_nsec > ONE_SEC_NS) {
	ts.tv_nsec -= ONE_SEC_NS;
	ts.tv_sec += 1;
    }
    return(timeTS2secsstr(ts, buf));
}
#endif /* USE_TIME_0_ */
char *timeTS2dot_str(TimeSpec ts, char *buf)
{
    struct tm gmt;
    gmtime_r(&ts.tv_sec, &gmt);
    if (ts.tv_nsec || preciset)
        sprintf(buf, dot_fmt,
            gmt.tm_year+1900, gmt.tm_mon+1, gmt.tm_mday,
            gmt.tm_hour, gmt.tm_min, gmt.tm_sec,
            (long)rint((ts.tv_nsec+0.5)/tendivisor));
    else
        sprintf(buf, SECONDS_dot_fmt,
            gmt.tm_year+1900, gmt.tm_mon+1, gmt.tm_mday,
            gmt.tm_hour, gmt.tm_min, gmt.tm_sec);
    return(buf);
}
char *timeTS2iso_str(TimeSpec ts, char *buf)
{
    struct tm gmt;
    gmtime_r(&ts.tv_sec, &gmt);
    if (ts.tv_nsec || preciset)
        sprintf(buf, iso_fmt,
            gmt.tm_year+1900, gmt.tm_mon+1, gmt.tm_mday,
            gmt.tm_hour, gmt.tm_min, gmt.tm_sec,
            (long)rint((ts.tv_nsec+0.5)/tendivisor));
    else
        sprintf(buf, SECONDS_iso_fmt,
            gmt.tm_year+1900, gmt.tm_mon+1, gmt.tm_mday,
            gmt.tm_hour, gmt.tm_min, gmt.tm_sec);
    return(buf);
}
char *timeTS2vex_str(TimeSpec ts, char *buf)
{
    struct tm gmt;
    gmtime_r(&ts.tv_sec, &gmt);
    if (ts.tv_nsec || preciset)
        sprintf(buf, vex_fmt,
            gmt.tm_year+1900, gmt.tm_yday+1,
            gmt.tm_hour, gmt.tm_min, gmt.tm_sec,
	    (long)rint((ts.tv_nsec+0.5)/tendivisor));
    else
        sprintf(buf, SECONDS_vex_fmt,
            gmt.tm_year+1900, gmt.tm_yday+1,
            gmt.tm_hour, gmt.tm_min, gmt.tm_sec);
    return(buf);
}
char *timeTS2vdifstr(TimeSpec ts, char *buf)
{
    int  ep;
    for (ep = 0; ep < NUM_VDIF_EPOCHS-1; ep++)
	if (ts.tv_sec >= vdif_epochs[ep] &&
	    ts.tv_sec < vdif_epochs[ep+1]) break;
    ts.tv_sec -= vdif_epochs[ep];
    if (ts.tv_nsec || preciset)
        sprintf(buf, vdiffmt,
            ep, ts.tv_sec + (double)ts.tv_nsec/(double)ONE_SEC_NS);
    else
        sprintf(buf, SECONDS_vdiffmt, ep, rint((double)ts.tv_sec));
    return(buf);
}

/*
 * Since ns aren't always desirable
 */
void time_ns_precision(int n)
{
    int	xx, ni = n;
    if (n < 0) n = 0;
    if (n > 9) n = 9;
    if (n > 0) {
        preciset = 1;
	strcpy(secsfmt, DEFAULT_secsfmt);
	strcpy(dot_fmt, DEFAULT_dot_fmt);
	strcpy(iso_fmt, DEFAULT_iso_fmt);
	strcpy(mjd_fmt, DEFAULT_mjd_fmt);
	strcpy(vex_fmt, DEFAULT_vex_fmt);
	strcpy(vdiffmt, DEFAULT_vdiffmt);
        /* update 09 -> 0n; 10 -> 0n for MJD */
	secsfmt[2]  = '0' + n;
	dot_fmt[27] = '0' + n;
	iso_fmt[32] = '0' + n;
        if (ni != 10) {
            xx = 16 - (10 - n);
            mjd_fmt[1] = '0' + xx/10;
            mjd_fmt[2] = '0' + xx%10;
            mjd_fmt[4] = '0';
            mjd_fmt[5] = '0' + n;
        }
	vex_fmt[27] = '0' + n;
	vdiffmt[7]  = '0' + n;
	tendivisor  = 1;
    } else {
        preciset = 0;
	strcpy(secsfmt, SECONDS_secsfmt);
	strcpy(dot_fmt, SECONDS_dot_fmt);
	strcpy(iso_fmt, SECONDS_iso_fmt);
	strcpy(mjd_fmt, SECONDS_mjd_fmt);
	strcpy(vex_fmt, SECONDS_vex_fmt);
	strcpy(vdiffmt, SECONDS_vdiffmt);
	tendivisor  = 1;
    }
    for (xx = 0; xx < 9 - n; xx++) tendivisor *= 10;
}

/*
 * Have full ns precision
 */
char *timefmtTS(TimeSpec ts, char *buf, int style)
{
    static char	mine[80];
    if (!buf) buf = mine;
    switch (style) {
    case TIMESTYLE_SECS: (void)timeTS2secsstr(ts, buf);	break;
    case TIMESTYLE_WEEK: (void)timeTS2weekstr(ts, buf);	break;
    case TIMESTYLE_DOT:  (void)timeTS2dot_str(ts, buf);	break;
    case TIMESTYLE_ISO:  (void)timeTS2iso_str(ts, buf);	break;
    case TIMESTYLE_MJD:  (void)timeTS2mjd_str(ts, buf);	break;
    case TIMESTYLE_VEX:  (void)timeTS2vex_str(ts, buf);	break;
    case TIMESTYLE_VDIF: (void)timeTS2vdifstr(ts, buf);	break;
    default:             buf = "Compiler error";        break;
    }
    return(buf);
}

/*
 * Just have the microsecs
 */
char *timefmtTV(TimeVal tv, char *buf, int style)
{
    static char	mine[80];
    TimeSpec ts;
    if (!buf) buf = mine;
    ts.tv_sec = tv.tv_sec;
    ts.tv_nsec = tv.tv_usec * 1000;
    if (!timetruncate) ts.tv_nsec += 500;
    return(timefmtTS(ts, buf, style));
}

/*
 * Just have the seconds
 */
char *timefmtTT(time_t tt, char *buf, int style)
{
    static char	mine[80];
    TimeSpec ts;
    if (!buf) buf = mine;
    ts.tv_sec = tt;
    ts.tv_nsec = timetruncate ? 0 : HALF_SEC_NS;
    return(timefmtTS(ts, buf, style));
}

/*
 * eof
 */
