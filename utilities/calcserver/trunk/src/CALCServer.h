/*  @(#)CALCServer.h  version 1.00  created 99/07/26 10:00:00
    %% include for the CALC Server RPC interface
    LANGUAGE: C (include file)
    ENVIRONMENT: vxWorks, Solaris, Linux
*/

#ifndef _CALC_H_RPCGEN
#define	_CALC_H_RPCGEN

#include <rpc/rpc.h>

#define MAX_STR 32

/* Client makes a CALC request through getCALC_arg arguments */
/* CALC Server Version 5 */

struct getCALC_arg {
        long request_id;        /* RPC request id number, user's choice */
        long date;              /* CALC model date (MJD) */
        long ref_frame;         /* CALC reference frame: 0 = geocentric */
        long dummy;
        short int kflags[64];   /* CALC model component control flags */

        double time;            /* CALC model time UTC (fraction of day) */

        double a_x;             /* geocentric right-hand x coord (meters) */
	double a_y;             /* geocentric right-hand y coord (meters) */
	double a_z;             /* geocentric right-hand z coord (meters) */
        double axis_off_a;      /* non-intersecting axis offset (meters) */
	double b_x;             /* geocentric right-hand x coord (meters) */
	double b_y;             /* geocentric right-hand y coord (meters) */ 
	double b_z;             /* geocentric right-hand z coord (meters) */
	double axis_off_b;      /* non-intersecting axis offset (meters) */
        double ra;              /* J2000.0 coordinates (radians) */
	double dec;             /* J2000.0 coordinates (radians) */
        double dra;             /* J2000.0 arcsecs/year */
        double ddec;            /* J2000.0 arcsecs/year */
        double depoch;          /* reference date for which proper motion
                                 * corrections are zero, mjd.fract_day */
        double parallax;        /* source distance in arcsecs of annual
                                 * parallax, = 206265.0 / distance (au) */

                                /* Earth Orientation Parameters */
        double EOP_time[5];     /* EOP epoch date.time (MJD) */
        double tai_utc[5];      /* TAI - UTC (secs) */
        double ut1_utc[5];      /* UT1 - UTC (secs) */
        double xpole[5];        /* earth pole offset, x (arcsec) */
        double ypole[5];        /* earth pole offset, y (arcsecs) */
     
        double pressure_a;      /* surface pressure stna (millibars) 
                                 * enter 0.0 for none availiable */
        double pressure_b;      /* surface pressure stnb (millibars) 
                                 * enter 0.0 for none availiable */

        char  *station_a;       /* station A name */
        char  *axis_type_a;     /* station A mount type, 'altz', 'equa',
                                                         ,xyns', 'xyew' */
        char  *station_b;       /* station B name */
        char  *axis_type_b;     /* station B mount type, 'altz', 'equa',
                                                         'xyns', 'xyew' */
        char  *source;          /* source name */

};
typedef struct getCALC_arg getCALC_arg;

/* CALC server reply */

struct CALCRecord {
        long request_id;        /* RPC request id number, returned by Server */
	long date;              /* CALC model date (MJD) */
	double time;            /* CALC model time UTC (fraction of day) */
        double delay[2];        /* total group delay, rate (secs, sec/sec) */
        double dry_atmos[4];    /* dry atmosphere delay, rate (secs, sec/sec) */
        double wet_atmos[4];    /* wet atmosphere delay, rate (secs, sec/sec) */
        double az[4];           /* stations azimuth angle, rate (rad, rad/sec) */
        double el[4];           /* station elevation angle, rate (rad, rad/sec) */
        double UV[3];           /* u, v, w coordinates in J2000.0 frame (meters) */
        double riseset[2];      /* estimated rise, set times for stnb VLBA Correlator */
};
typedef struct CALCRecord CALCRecord;

struct getCALC_res {
	int error;
	union {
		struct CALCRecord record;
		char *errmsg;
	} getCALC_res_u;
};
typedef struct getCALC_res getCALC_res;

#define	CALCPROG ((unsigned long)(0x20000340))
#define	CALCVERS ((unsigned long)(1))
#define	GETCALC ((unsigned long)(1))
extern  getCALC_res * getcalc_1();
extern int calcprog_1_freeresult();

/* the xdr functions */
extern bool_t xdr_getCALC_arg();
extern bool_t xdr_CALCRecord();
extern bool_t xdr_getCALC_res();

#endif /* !_CALC_H_RPCGEN */
