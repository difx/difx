#include "MATHCNST.H"
#include "STDDEFS.H"
#include <rpc/rpc.h>
#include <stdio.h>    /* defines stderr */
#include <stdlib.h>
#include <strings.h>
#include <time.h>
#include <math.h>
#include <sys/syslog.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include "CALCServer.h"     /* RPCGEN creates this from CALCServer.x */
#define INCocean
#include "ocean.h"


/* externals */
int  calcinit_ (int *jobn, int *mjd, short int kflags[64], int *iret);
void calcmodl2_(int *mjd, double *time, int *srcid, int *stnaid, int *stnbid,
	double *delay, double *rate, double *u, double *v, double *w, double atmos[4],
	double datmos[4], double *accel, double *risetime, double *settime, 
	double xelev[2], double relev[2], double xaz[2], double raz[2],
	double partials[28], int *iret);

char *rad2str(double angle, char *pFormat, char *pOutStr);
char *mjd2str(long mjd, char *pstring);
char *cvrtuc(char *s);
char *cvrtlc(char *s);
int ingra(double radeg, char *rastr);
int ingdec(double decdeg, char *decstr);
int iword(char *instr, char *delim, int nword, char *outstr);

bool_t pmap_unset(unsigned long prognum, unsigned long versnum);

struct JPL_Horizons_Tbl {
       double  ref_time;   /* Reference time (Julian Date) */
       double  ra;         /* Source RA (J2000/ICRF) */
       double  dec;        /* Source DEC (J2000/ICRF) */
       double  delta;      /* Geocentric range in AU */
       double  deldot;     /* Geocentric delta range in km/sec */
} solar_target[5000], *p_ss_targ;

int  n_Horizons_rows;
int  ilog_Horizons = 1;
int  iparallax = 1;
char Horizons_srcname[24];
char Horizons_filename[256];

int  ifirst, ilogdate = 1;
FILE  *flog = 0;
char spawn_time[64];
char Version[48];
getCALC_arg  *p_request;

struct ocean_coeffs *p_ocean_row[1000];

/*  @(#)loadOcean.c  version 1.00  created 96/01/18 13:11:29
    %% function to return pointer to ocean load tbl row for current stn.
    LANGUAGE: C
    ENVIRONMENT: VLBA Correlator real-time system running under vxWorks
*/

/*******************************************************************************
*/
void            date2str (date, str)
    double          date;	       /* date to be converted */
    char           *str;	       /* where the output string goes */
{
    long            MJD;
    double          Time;
    char            tmpStr[32];

    MJD = date;
    Time = (date - MJD) * TWOPI;
    mjd2str (MJD, str);
    strcat (str, "_");
    rad2str (Time, "h02.2", tmpStr);
    strcat (str, tmpStr);
}

/*******************************************************************************
*/
STATUS loadOcean(p_stn_name, jrow)
    char *p_stn_name;
    int   jrow;
/*
 * RETURNS OK | ERROR
 *
 * Finds the current station in the ocean loading table. Assign pointer
 * p_ocean_row[jrow] to ocean loading table row, for the jrow stn number.
 */
{
    int nrows;
    int irow;
    int icount;
    char tst_name[16];
    char stn_name[16];

    strcpy (stn_name, p_stn_name);

    nrows = sizeof (oc_coeffs) / sizeof (struct ocean_coeffs);

    /* point to ocean table row with zero coefficients, default */
    p_ocean_row[jrow] = &oc_coeffs[0];

    /* now loop through the ocean table row by row, 
       look for match with input station name */
    icount = 0;
    for (irow = 0; irow < nrows; irow++)
    {
	strcpy (tst_name, oc_coeffs[irow].stn_id);

	if (strcmp (tst_name, stn_name) == 0)
	{
	    icount++;
	    /* printf ("Found ocean tbl row for %s, jrow = %d\n", stn_name, jrow);  */
            p_ocean_row[jrow] = &oc_coeffs[irow];
            return (OK);
	}
    }

    return ERROR;
}
/*  @(#)d_fetch.c  version 1.11  created 93/10/17 23:09:23
                fetched from SCCS 95/06/26 14:13:46
%% retrieve a double precision value from user requested correlator data table
LANGUAGE: C
ENVIRONMENT: Any
::
*/


/*******************************************************************************
*/
double         ast_dopplr (mjdtime, u, v, w)
    double          mjdtime;
    double          u, v, w;
{

  double value[2], time[2], outval, lo_off;
    double deltim, dist2_km, dist_km, vel_radial, vel_tang, vel_check;
    double srcpos[3][2], srcvel[3], src[3], srcstn[3], sdec,cdec,sra,cra;
    int    i;

    if (n_Horizons_rows == 0)
      return 0.0;

    outval = 0.0;
    for (i = 1; i < n_Horizons_rows; i++)
    {
        if (mjdtime >= solar_target[i-1].ref_time && mjdtime <= solar_target[i].ref_time)
	{
	  /*
	  printf ("ast_dopplr : row, mjdtime, rowtime %d %12.5f %12.5f\n",
                  i, mjdtime, solar_target[i].ref_time);
	  */
           time[0] = solar_target[i-1].ref_time;
           time[1] = solar_target[i].ref_time;
           deltim = solar_target[i].ref_time - solar_target[i-1].ref_time;

	   /* calculate the distance to the asteroid (km) */
           value[0] = solar_target[i-1].delta;           
           value[1] = solar_target[i].delta;
           dist_km  = value[0] + (mjdtime - time[0]) * (value[1] - value[0])
                    / (time[1] - time[0]);
           dist_km *= ASTR_UNIT * 1.0e-03;

           /* calculate the radial velocity (km/sec) at time = mjdtime */
           value[0] = solar_target[i-1].deldot;           
           value[1] = solar_target[i].deldot;
           vel_radial  = value[0] + (mjdtime - time[0]) * (value[1] - value[0])
                       / (time[1] - time[0]);

	   /* calculate the src position unit vector in J2000 x,y,z */
	   sra  = sin(solar_target[i-1].ra);
	   cra  = cos(solar_target[i-1].ra);
	   sdec = sin(solar_target[i-1].dec);
 	   cdec = cos(solar_target[i-1].dec);
           srcpos[0][0] = cdec * cra;
           srcpos[1][0] = cdec * sra;
           srcpos[2][0] = sdec;

	   sra  = sin(solar_target[i].ra);
	   cra  = cos(solar_target[i].ra);
	   sdec = sin(solar_target[i].dec);
 	   cdec = cos(solar_target[i].dec);
           srcpos[0][1] = cdec * cra;
           srcpos[1][1] = cdec * sra;
           srcpos[2][1] = sdec;

           /* calculate the asteroid velocity vector (J2000) */
           /* tangential velocity component */
           srcvel[0] = (srcpos[0][1]-srcpos[0][0]) * dist_km / (deltim * 86400.0);
           srcvel[1] = (srcpos[1][1]-srcpos[1][0]) * dist_km / (deltim * 86400.0);
           srcvel[2] = (srcpos[2][1]-srcpos[2][0]) * dist_km / (deltim * 86400.0);
           vel_tang = sqrt(srcvel[0]*srcvel[0] + srcvel[1]*srcvel[1]
                           + srcvel[2]*srcvel[2]);
           /* check that the tangential velocity has a near zero value
            * projected on to the radial vector */
           vel_check = srcvel[0]*(-1.0*srcpos[0][1])
                     + srcvel[1]*(-1.0*srcpos[1][1])
	             + srcvel[2]*(-1.0*srcpos[2][1]);
     

           /* add the radial velocity component */
           srcvel[0] += vel_radial * srcpos[0][1];
           srcvel[1] += vel_radial * srcpos[1][1];
           srcvel[2] += vel_radial * srcpos[2][1];
           
           /* calculate the vector from the observing station to the
            * asteroid */
           
           /* first, src position at time = mjdtime */
           src[0]  = srcpos[0][0] 
                   + (mjdtime - time[0]) * (srcpos[0][1] - srcpos[0][0])
                   / (time[1] - time[0]);
           src[1]  = srcpos[1][0] 
                   + (mjdtime - time[0]) * (srcpos[1][1] - srcpos[1][0])
                   / (time[1] - time[0]);
           src[2]  = srcpos[2][0] 
                   + (mjdtime - time[0]) * (srcpos[2][1] - srcpos[2][0])
                   / (time[1] - time[0]);

           /* src position distance vector */
           src[0] = srcpos[0][1] * dist_km;
           src[1] = srcpos[1][1] * dist_km;
           src[2] = srcpos[2][1] * dist_km;

           /* src position distance vector from observing station */
           srcstn[0] = src[0] + u * 1.0e-03;
           srcstn[1] = src[1] + v * 1.0e-03;
           srcstn[2] = src[2] + w * 1.0e-03;

           /* make unit vector */
           dist2_km = sqrt(srcstn[0]*srcstn[0] + srcstn[1]*srcstn[1]
			  + srcstn[2]*srcstn[2]);
           srcstn[0] = srcstn[0] / dist2_km;
           srcstn[1] = srcstn[1] / dist2_km;
           srcstn[2] = srcstn[2] / dist2_km;

           /* stn src unit vector dot src velocity vector */
           srcstn[0] = srcstn[0] * srcvel[0];
           srcstn[1] = srcstn[1] * srcvel[1];
           srcstn[2] = srcstn[2] * srcvel[2];

           /* radial velocity from station */
           outval =  srcstn[0] + srcstn[1] + srcstn[2];

           lo_off = 2.2e+09 * (outval - vel_radial) / (C_LIGHT * 1.0e-03);
           /*
           printf ("ast_dopplr : vrad,  vtang, vchk, vproj, delvel %12.5e %12.5e %12.5e %12.5e %12.5e %12.5e\n",
		   vel_radial, vel_tang, vel_check,  outval, vel_radial-outval,
                   lo_off);
	   */
           outval = outval - vel_radial;
           break;
	}
    }
    return outval;
}

/*******************************************************************************
*/
static void calcprog_1 (pRequest, pTransport)    /* server dispatch routine */
struct svc_req *pRequest;
SVCXPRT *pTransport;
/*
* This function is called each time a client requests service.  It is passed 
* an argument via the RPC specifing the requested CALC data, i.e. the station 
* and source coordinates and date.  It returns via the RPC the interferometer
* delay and delay rate.
* 
* This function was built by modifying Ron Heald's Tcal RPC server 
* function.
*/
{
    double ast_dopplr();
    getCALC_arg argument;    /* RPC caller passes this */
    getCALC_res result;      /* we return this to RPC caller */
    struct sockaddr_in  *sock_in;
    struct tm *timeptr;
    time_t  timep, *tp;

    int     imjd, station_index_a, station_index_b, source_index;
    int     jobnum, iret, i, itoday;
    short int kflags[64];
    double  delay, rate, atmos[4], datmos[4], radtime, u, v, w;
    double  accel, risetime, settime, xelev[2], relev[2], xaz[2], raz[2];
    double  mjd_time, partials[28], outval;
    char    *destdotaddr, mjd_str[24], filename[256], hostname[24];

    tp = &timep;
    time (tp);

    /* check if we have crossed a day boundry in local time. If so, open a
     * new logfile */
    if (ilogdate != 0)
    {
       timeptr = localtime (tp);
       itoday = timeptr->tm_mday + 100*(timeptr->tm_mon+1) + 10000*(timeptr->tm_year+1900);
       if (itoday != ilogdate)
       {
          gethostname(hostname, 23);
          snprintf (filename, 256, "%s%02d%02d%02d:%02d%02d%02d.log", hostname, 
             timeptr->tm_year-100,
             timeptr->tm_mon+1, timeptr->tm_mday, timeptr->tm_hour,
             timeptr->tm_min, timeptr->tm_sec);
          ilogdate = itoday;
	 if (flog != 0)
	 {
	   fclose(flog);
	 }
         if ((flog = fopen (filename, "w")) == NULL)
         {
            printf ("ERROR: cannot open log file %s\n", filename);
            exit(EXIT_FAILURE);
         }
         printf ("opened new CALCServer log file : %s\n", filename);
         fprintf (flog, "%s Started : %s", Version, spawn_time);
         fprintf (flog, "Logfile Opened      : %s", ctime(tp));
         fprintf (flog, "Horizons Table      : %s\n", Horizons_filename);
         fprintf (flog, "Horizons Table Rows : %d\n", n_Horizons_rows);
         fprintf (flog, "Horizons Source Name: %s\n", Horizons_srcname);
         fprintf (flog, "Horizons Log Flag   : %d\n", ilog_Horizons);
         fprintf (flog, "Horizons iparallax  : %d\n", iparallax);
         ifirst = 1;
       }
    }
    /* handle null procedure */
    if (pRequest->rq_proc == NULLPROC)
	{
	  if (!svc_sendreply (pTransport, (xdrproc_t)xdr_void, NULL))
	    svcerr_systemerr (pTransport);
	return;
	}

    /* handle unimplemented procedure number */
    if (pRequest->rq_proc != GETCALC)
	{
	svcerr_noproc (pTransport);
	return;
	}

    /* get RPC input argument */
    bzero (&argument, sizeof (argument));    /* see "The Art of DA" p193 */
    if (!svc_getargs (pTransport, xdr_getCALC_arg, (char *)&argument))
	{
	svcerr_decode (pTransport);
	return;
	}

#ifdef VERBOSE_DEBUG    
    printf ("request arg: date = %d\n", argument.date);
    printf ("request arg: time = %e\n", argument.time);
    printf ("request arg: src  = %s\n", argument.source);
    printf ("request arg: ra   = %e\n", argument.ra);
    printf ("request arg: dec  = %e\n", argument.dec);
    printf ("request arg: dra  = %e\n", argument.dra);
    printf ("request arg: ddec = %e\n", argument.ddec);
    printf ("request arg: deph = %e\n", argument.depoch);
    printf ("request arg: plx  = %e\n", argument.parallax);
    printf ("request arg: stn  = %s\n", argument.station_a);
    printf ("request arg: x    = %16.10e\n", argument.a_x);
    printf ("request arg: y    = %16.10e\n", argument.a_y);
    printf ("request arg: z    = %16.10e\n", argument.a_z);
    printf ("request arg: axis = %s\n", argument.axis_type_a);
    printf ("request arg: off  = %e\n", argument.axis_off_a);
    printf ("request arg: press= %e\n", argument.pressure_a);
    printf ("request arg: stn  = %s\n", argument.station_b);
    printf ("request arg: x    = %16.10e\n", argument.b_x);
    printf ("request arg: y    = %16.10e\n", argument.b_y);
    printf ("request arg: z    = %16.10e\n", argument.b_z);
    printf ("request arg: axis = %s\n", argument.axis_type_b);
    printf ("request arg: off  = %e\n", argument.axis_off_b);
    printf ("request arg: press= %e\n", argument.pressure_b);

    for (i = 0; i < 5; i++)
    {
        date2str (argument.EOP_time[i], mjd_str);
        printf ("request arg: %s %5.1f %8.5f %8.5f %8.5f\n",
                 mjd_str, argument.tai_utc[i], argument.ut1_utc[i],
                          argument.xpole[i], argument.ypole[i]);
    }
#endif

    /* sketch out interface to Calc here */

    p_request = &argument;

    jobnum = 1;
    source_index = station_index_a = 0;
    station_index_b = 1;
    imjd = argument.date;
    radtime = argument.time * TWOPI;

    /* Signal CALC through calcinit to write CALC state stuff into
     * the .log file. Once only on first call to the server */
    iret = ifirst;

    for (i = 0; i < 50; i++)
        p_ocean_row[i] = &oc_coeffs[0];
    if (strcmp(argument.station_a, "EC") != 0)
       loadOcean ((char *)argument.station_a, 1);
    loadOcean ((char *)argument.station_b, 2);

    for (i = 0; i < 64; i++)
        kflags[i] = p_request->kflags[i];

    calcinit_ (&jobnum, &imjd, kflags, &iret);
    if (ifirst == 1) fflush(flog);

    calcmodl2_ (&imjd, &radtime, &source_index, &station_index_a,
                   &station_index_b,
		   &delay, &rate, &u, &v, &w, atmos, datmos,
		   &accel,
		   &risetime, &settime, xelev, relev, 
                   xaz, raz, partials, &iret);

    delay *= 1.0e-6;

#ifdef VERBOSE_DEBUG
    printf ("CALCServer: delay = %20.14e\n", delay);
    printf ("CALCServer: rate  = %e\n", rate);
    printf ("CALCServer: dry, wet = %e %e\n", 
            atmos[0], atmos[1]);
#endif

    time (tp);

    sock_in = svc_getcaller (pTransport);
    destdotaddr = inet_ntoa(sock_in->sin_addr);

    if (strcmp(getenv("SERVERLOG"), "ON") == 0)
    {
       mjd_time = (double)argument.date + argument.time;
       date2str (mjd_time, mjd_str);
    fprintf (flog, "request from %s:  for args  %5d %4s %4s %10s %s (dly = %20.12e) at %s", 
             destdotaddr,
             (int)(argument.request_id), argument.station_a, argument.station_b,
             argument.source, mjd_str, delay, ctime (tp));
       fflush (flog);
    }
    /* outval goes nowhere for now */
    outval = 0.0;
    if ((n_Horizons_rows > 0) &&
        (strcmp(p_request->source, Horizons_srcname) == 0))
    {
       mjd_time = (double)argument.date + argument.time;
       /* outval =  ast_dopplr (mjd_time, u, v, w); */
    }

    result.getCALC_res_u.record.date  = argument.date;
    result.getCALC_res_u.record.time  = argument.time;
    result.getCALC_res_u.record.delay[0] = delay;
    result.getCALC_res_u.record.delay[1] = rate;
    result.getCALC_res_u.record.request_id = argument.request_id;
    /* Changed the order of atmos, datmos - JMB Jan 28, 2004 */
    result.getCALC_res_u.record.dry_atmos[0] = atmos[0];
    result.getCALC_res_u.record.dry_atmos[1] = atmos[0];
    result.getCALC_res_u.record.wet_atmos[0] = atmos[1];
    result.getCALC_res_u.record.wet_atmos[1] = atmos[1];
    result.getCALC_res_u.record.dry_atmos[2] = datmos[0];
    result.getCALC_res_u.record.dry_atmos[3] = datmos[0];
    result.getCALC_res_u.record.wet_atmos[2] = datmos[1];
    result.getCALC_res_u.record.wet_atmos[3] = datmos[1];

    result.getCALC_res_u.record.el[0] = xelev[0];
    result.getCALC_res_u.record.el[1] = xelev[1];
    result.getCALC_res_u.record.az[0] = xaz[0];
    result.getCALC_res_u.record.az[1] = xaz[1];
    result.getCALC_res_u.record.UV[0] = u;
    result.getCALC_res_u.record.UV[1] = v;
    result.getCALC_res_u.record.UV[2] = w;
    result.getCALC_res_u.record.riseset[0] = risetime;
    result.getCALC_res_u.record.riseset[1] = settime;

    result.error = 0;
    ifirst = 0;

    /* return result to client */

    if (!svc_sendreply (pTransport, xdr_getCALC_res, (char *)&result))
	svcerr_systemerr (pTransport);

    /* free any memory allocated by xdr routines when argument was decoded */
    if (!svc_freeargs (pTransport, xdr_getCALC_arg, (char *)&argument))
	{
	syslog (LOG_ERR, "unable to free arguments %m\n");
	exit(EXIT_FAILURE);
	}
}

/*++****************************************************************************
*/
int main (int argc, char *argv[])
/*
* This program is a remote procedure call(RPC) server for the GSFC
* CALC Program. The current client is the VLBA Correlator on-line
* computer. The CALC Server is called from within the modlTask, and
* replaces running the CALC program itself in the on-line computer.
-*/
{
    SVCXPRT *pTransport;    /* transport handle */
    time_t  timep, *tp;
    char    card[200], word[20], *cp, *b_ptr;
    const char *filename;
    int     i, nbytes, ilength, irow;
    double  rarad, decrad, rahr, ramn, rasc, decdeg, decmn, decsc, decsign;
    FILE    *fp;

    /* Save the Server startup time and version number for the log file */
    tp = &timep;
    time (tp);
    strcpy (spawn_time, ctime(tp));
    strcpy (Version, "CALCServer Version 5.5");

    /* open system log file to send messages */
    openlog ("CALCServer", LOG_PID, LOG_USER);

    /* if logfile name is specified in argv[1], use it instead of datetime
    * logfile name that is opened later in the program */
    if (argc > 1)
    {
       filename = argv[1];
       if (flog != 0)
       {
	  fclose(flog);
       }
       if ((flog = fopen (filename, "w")) == NULL)
       {
          printf ("ERROR: cannot open log file %s\n", filename);
          exit(EXIT_FAILURE);
       }
       printf ("opened new CALCServer log file %s\n", filename);
       ilogdate = 0;
    }
    /* print to the console the values of the environment variables */
    printf ("Env variable : JPLEPH    = %s\n", getenv("JPLEPH"));
    printf ("Env variable : CALC_USER = %s\n", getenv("CALC_USER"));
    printf ("Env variable : WET_ATM   = %s\n", getenv("WET_ATM"));
    printf ("Env variable : SERVERLOG = %s\n", getenv("SERVERLOG"));
    printf ("Env variable : Horizons_file = %s\n", getenv("HORIZONS_FILENAME"));
    printf ("Env variable : Horizons_src  = %s\n", getenv("HORIZONS_SRCNAME"));

    /* un-register service with portmapper in case one already exists */
    (void) pmap_unset (CALCPROG, CALCVERS);

    /* create TCP transport handle */
    if ((pTransport = svctcp_create (RPC_ANYSOCK, 0, 0)) == NULL)
	{
	syslog (LOG_ERR, "cannot create tcp service %m\n");
	exit(EXIT_FAILURE);
	}

    /* register service with portmapper */
    if (svc_register (pTransport, CALCPROG, CALCVERS, 
		      calcprog_1, IPPROTO_TCP) == 0)
	{
	syslog (LOG_ERR, "unable to register service %m\n");
	exit(EXIT_FAILURE);
	}

    /* load the planet or asteroid positions here */

    strcpy (Horizons_filename, getenv("HORIZONS_FILENAME"));
    strcpy (Horizons_srcname,  getenv("HORIZONS_SRCNAME"));

    printf ("Horizons_filename = %s\n", Horizons_filename);
    printf ("Horizons_srcname  = %s\n", Horizons_srcname);
    printf ("List 10 Horizons_filename rows : \n");

    p_ss_targ = &solar_target[0];
    n_Horizons_rows = 0;
    nbytes = 132;
    if((fp = fopen (Horizons_filename,"r")) != NULL)
    {
        b_ptr = &card[0];
        cp    = &word[0];
        for (i = 0, irow = 0; i < 5000; i++)
        {      
            if ((fgets (b_ptr, nbytes, fp)) == NULL)
               break;
            if ((ilength = strlen (b_ptr)) <= 0)
               break; 
            iword (b_ptr, " ", 1, cp);
            if (atof(cp) == 0.0)
               continue;
            irow++;
            solar_target[irow].ref_time = atof(cp) - 2400000.5;
            iword (b_ptr, " ", 2, cp);
            rahr = atof(cp);
            iword (b_ptr, " ", 3, cp);
            ramn = atof(cp);
            iword (b_ptr, " ", 4, cp);
            rasc = atof(cp);
            iword (b_ptr, " ", 5, cp);
            decdeg = atof(cp);
            iword (b_ptr, " ", 6, cp);
            decmn  = atof(cp);
            iword (b_ptr, " ", 7, cp);
            decsc  = atof(cp);

            decsign = 1.0;
            if (decdeg < 0.0) decsign = -1.0;
            decdeg = abs(decdeg);

            rarad = (rahr + ramn/60.0 + rasc/3600.0) * TWOPI / 24.0;
            decrad= (decdeg + decmn/60.0 + decsc/3600.0) * TWOPI / 360.0;
            decrad= decsign * decrad;

            solar_target[irow].ra = rarad;
            solar_target[irow].dec = decrad;

            iword (b_ptr, " ", 8, cp);
            solar_target[irow].delta = atof (cp);
            iword (b_ptr, " ", 9, cp);
            solar_target[irow].deldot = atof (cp);
            n_Horizons_rows++;
            if (n_Horizons_rows <= 10) { 
               printf ("%3d %f %f %f %f %f\n", irow, solar_target[irow].ref_time,
		  solar_target[irow].ra, solar_target[irow].dec, solar_target[irow].delta, solar_target[irow].deldot);
	    }
       }
       printf ("loaded nrows for solar system body ephemeris table : %d %s\n", 
                n_Horizons_rows, Horizons_filename);
       fclose(fp);
    }
    /*    */

    ifirst = 1;

    /* dispatch RPC client calls */
    svc_run ();    /* should never return */

    /* svc_run() returned ??? */
    svc_destroy (pTransport);
    syslog (LOG_ERR, "svc_run returned %m\n");

    return EXIT_SUCCESS;
}

/*++***********************************************************************/
 double d_mathcnst_ (chr1_ptr,chr1_len)
 char     *chr1_ptr;  /* requested variable name */
 int       chr1_len;  /* string length for chr2_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and MATHCNST.H
   Function d_mathcnst is called by the Fortran subroutine GET4 which
is in turn called by CALC. GET4 is the CALC interface to the Goddard
VLBI DataBase System. Function d_mathcnst retrieves the requested parameter
from defined values in the MATHCNST.H include file.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  02Mar95  J. Benson  Initial Submission
--$
-*/
{
     double d_val;
     char inword1[16];
/* copy the Fortran string into array */
     strncpy(inword1,chr1_ptr,chr1_len);
     inword1[chr1_len] = '\0';

     d_val = -99999.0;
     if (strcmp (inword1, "VLIGHT") == 0)
        d_val = C_LIGHT;
     if (strcmp (inword1, "GAUSS") == 0)
        d_val = GAUSS_GRAV;
     if (strcmp (inword1, "ACCELGRV") == 0)
        d_val = ACCEL_GRV;
     if (strcmp (inword1, "GMEARTH") == 0)
        d_val = GRAV_GEO;
     if (strcmp (inword1, "GMSUN") == 0)
        d_val = GRAV_HELIO;
     if (strcmp (inword1, "GMMOON") == 0)
        d_val = GRAV_MOON;
     if (strcmp (inword1, "TSECAU") == 0)
        d_val = TAU_A;
     if (strcmp (inword1, "EARTHRAD") == 0)
        d_val = E_EQ_RADIUS;
     if (strcmp (inword1, "EMSMMS") == 0)
        d_val = 1.0 / LMASS_RATIO;
     if (strcmp (inword1, "UGRVCN") == 0)
        d_val = GRAV_CONST;
     if (strcmp (inword1, "EFLAT") == 0)
        d_val = E_FLAT_FCTR;
     if (strcmp (inword1, "PRECONST") == 0)
        d_val = PRECESS;
     if (strcmp (inword1, "ETIDE_LAG") == 0)
        d_val = ETIDE_LAG;
     if (strcmp (inword1, "LOVE_L") == 0)
        d_val = LOVE_L;
     if (strcmp (inword1, "LOVE_H") == 0)
        d_val = LOVE_H;

     if (ifirst) fprintf (flog, "CONSTANT      %s = %20.10e\n", inword1, d_val);
     return d_val;
  }


/*++***********************************************************************/
 double d_ftoc_ (int1_ptr,chr1_ptr,int2_ptr,chr2_ptr,chr1_len,chr2_len)
 int      *int1_ptr;  /* job index number */
 char     *chr1_ptr;  /* data table name */
 int      *int2_ptr;  /* data table row number, first = 0 */
 char     *chr2_ptr;  /* requested variable name */
 int       chr1_len;  /* string length for chr1_ptr */
 int       chr2_len;  /* string length for chr2_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and data tables.
   Function d_ftoc is called by the Fortran subroutine GET4 which
is in turn called by CALC. GET4 is the CALC interface to the Goddard
VLBI DataBase System. Function d_ftoc retrieves the requested parameter
from a data table by a call to d_fetch.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  22May89  J. Benson  Initial Submission
--$
-*/
{
     extern double d_fetch();
     double d_val;
     char inword1[16],inword2[16],*err=0;
     int  i1_val,i2_val;

     i1_val = *int1_ptr;
     i2_val = *int2_ptr;

/* copy the Fortran strings into arrays */
     strncpy(inword1,chr1_ptr,chr1_len);
     inword1[chr1_len] = '\0';
     strncpy(inword2,chr2_ptr,chr2_len);
     inword2[chr2_len] = '\0';

/* retrieve the requested parameter's value thru d_fetch */
     d_val = d_fetch(i1_val,inword1,i2_val,inword2,err);
     return d_val;
  }
/*++***********************************************************************/
 double mjd_ftoc_ (int1_ptr,chr1_ptr,int2_ptr,chr2_ptr,chr1_len,chr2_len)
 int      *int1_ptr;  /* job index number */
 char     *chr1_ptr;  /* data table name */
 int      *int2_ptr;  /* data table row number, first = 0 */
 char     *chr2_ptr;  /* requested variable name */
 int       chr1_len;  /* string length for chr1_ptr */
 int       chr2_len;  /* string length for chr2_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and data tables.
   Function mjd_ftoc is called by the Fortran subroutine GET4 which
is in turn called by CALC. GET4 is the CALC interface to the Goddard
VLBI DataBase System. Function mjd_ftoc retrieves the requested mjd
from a data table by a call to mjd_fetch.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  22May89  J. Benson  Initial Submission
--$
-*/
{
     extern double mjd_fetch();
     double d_val;
     char inword1[16],inword2[16],*err=0;
     int  i1_val,i2_val;

     i1_val = *int1_ptr;
     i2_val = *int2_ptr;

/* copy the Fortran strings into arrays */
     strncpy(inword1,chr1_ptr,chr1_len);
     inword1[chr1_len] = '\0';
     strncpy(inword2,chr2_ptr,chr2_len);
     inword2[chr2_len] = '\0';

/* retrieve the requested parameter's value thru mjd_fetch */
     d_val = mjd_fetch(i1_val,inword1,i2_val,inword2,err);

     return d_val;
  }
/*++***********************************************************************/
double f_ftoc_ (int1_ptr,chr1_ptr,int2_ptr,chr2_ptr,chr1_len,chr2_len)
 int      *int1_ptr;  /* job index number */
 char     *chr1_ptr;  /* data table name */
 int      *int2_ptr;  /* data table row number, first = 0 */
 char     *chr2_ptr;  /* requested variable name */
 int       chr1_len;  /* string length for chr1_ptr */
 int       chr2_len;  /* string length for chr2_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and data tables (float).
   Function f_ftoc is called by the Fortran subroutine GET4 which
is in turn called by CALC. GET4 is the CALC interface to the Goddard
VLBI DataBase System. Function f_ftoc retrieves the requested parameter
from a data table by a call to f_fetch.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  22May89  J. Benson  Initial Submission
--$
-*/
{
     extern float f_fetch();
     float f_val;
     double d_val;
     char inword1[16],inword2[16],*err=0;
     int  i1_val,i2_val;

     i1_val = *int1_ptr;
     i2_val = *int2_ptr;

/* copy the Fortran strings into arrays */
     strncpy(inword1,chr1_ptr,chr1_len);
     inword1[chr1_len] = '\0';
     strncpy(inword2,chr2_ptr,chr2_len);
     inword2[chr2_len] = '\0';
     
/* retrieve the requested parameter's value thru d_fetch */
     f_val = f_fetch(i1_val,inword1,i2_val,inword2,err);

/* trickery to get float back into Fortran, for Sun compiler  */
/*     return *((int *)&f_val);  */

/* return the single-precision float as a double because of G77 */
/* the above trick only works for Sun Fortran */
      d_val = (double) f_val;
      return d_val;     
  }
/*++***********************************************************************/
int i_ftoc_ (int1_ptr,chr1_ptr,int2_ptr,chr2_ptr,chr1_len,chr2_len)
 int      *int1_ptr;  /* job index number */
 char     *chr1_ptr;  /* data table name */
 int      *int2_ptr;  /* data table row number, first = 0 */
 char     *chr2_ptr;  /* requested variable name */
 int       chr1_len;  /* string length for chr1_ptr */
 int       chr2_len;  /* string length for chr2_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and data tables (int).
   Function i_ftoc is called by the Fortran subroutine GETI which
is in turn called by CALC. GETI is the CALC interface to the Goddard
VLBI DataBase System. Function i_ftoc retrieves the requested parameter
from a data table by a call to i_fetch.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  22May89  J. Benson  Initial Submission
--$
-*/
{
     extern int i_fetch();
     int i_val;
     char inword1[16],inword2[16],*err=0;
     int  i1_val,i2_val;

     i1_val = *int1_ptr;
     i2_val = *int2_ptr;

/* copy the Fortran strings into arrays */
     strncpy(inword1,chr1_ptr,chr1_len);
     inword1[chr1_len] = '\0';
     strncpy(inword2,chr2_ptr,chr2_len);
     inword2[chr2_len] = '\0';

     
/* retrieve the requested parameter's value thru i_fetch */
     i_val = i_fetch(i1_val,inword1,i2_val,inword2,err);

     return i_val;
  }
/*++***********************************************************************/
void c_ftoc_(ret_ptr,ret_len,int1_ptr,chr1_ptr,int2_ptr,chr2_ptr,chr1_len,chr2_len)
 char     *ret_ptr;   /* returned char string ptr */
 int       ret_len;   /* returned string length */
 int      *int1_ptr;  /* job index number */
 char     *chr1_ptr;  /* data table name */
 int      *int2_ptr;  /* data table row number, first = 0 */
 char     *chr2_ptr;  /* requested variable name */
 int       chr1_len;  /* string length for chr1_ptr */
 int       chr2_len;  /* string length for chr2_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and data tables (char).
   Function c_ftoc is called by the Fortran subroutine GETA which
is in turn called by CALC. GETA is the CALC interface to the Goddard
VLBI DataBase System. Function c_ftoc retrieves the requested parameter
from a data table by a call to c_fetch.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  22May89  J. Benson  Initial Submission
--$
-*/
{
     extern char *c_fetch();
     char inword1[16],inword2[16],*err=0;
     int  i1_val,i2_val,i;
     char  *cp0,*cp1;

     i1_val = *int1_ptr;
     i2_val = *int2_ptr;

/* copy the Fortran strings into arrays */
     strncpy(inword1,chr1_ptr,chr1_len);
     inword1[chr1_len] = '\0';
     strncpy(inword2,chr2_ptr,chr2_len);
     inword2[chr2_len] = '\0';

     
     if (ret_len > 16) ret_len = 16;
     cp0 = ret_ptr;
     
/* retrieve the requested parameter's value thru c_fetch */
     cp1 = c_fetch(i1_val,inword1,i2_val,inword2,err);

     for (i = 0; i < ret_len; i++) *cp0++ = *cp1++;
     return ;
  }

/*++***********************************************************************/
int n_rows_ (int1_ptr,chr1_ptr,chr1_len)
 int      *int1_ptr;  /* job index number */
 char     *chr1_ptr;  /* data table name */
 int       chr1_len;  /* string length for chr1_ptr */
/*++***********************************************************************
%% Fortran - C interface between CALC and job table (return num_rows).
   Function n_rows is called by one of the Fortran GETx subroutines which
are in turn called by CALC. Function n_rows gets the number of rows in
the requested data table by a call to job_row.
---
LANGUAGE: C
ENVIRONMENT: vxWorks
++$ AUDIT TRAIL
1.0  22May89  J. Benson  Initial Submission
--$
-*/
{

     char inword[16];
     int  nrows;

/* copy the Fortran string into an array */
     strncpy(inword, chr1_ptr, chr1_len);
     inword[chr1_len] = '\0';

     cvrtuc(inword);
     nrows = 1;
     if (strcmp (inword, "UTC") == 0)
        return (5);

     /* interface.f adds +1 to number of station table rows */
     if (strcmp (inword, "STATION") == 0)
        return (2);

     if (strcmp (inword, "SOURCE") == 0)
        return (1);

     if (strcmp (inword, "METRO") == 0)
     {
        if (p_request->pressure_b < 1.0)
           return (0);
        else if (p_request->pressure_b > 1.0) 
           return (1);
     }
     return nrows;
  }
/*  @(#)d_fetch.c  version 1.11  created 93/10/17 23:09:23
                fetched from SCCS 95/06/26 14:13:46
%% retrieve a double precision value from user requested correlator data table
LANGUAGE: C
ENVIRONMENT: Any
::
*/

/* externals */

/*++****************************************************************************
*/
double d_fetch (job_num, tbl_name, row_num, keyname, err)
int job_num;	       /* job index number in queue table */
char *tbl_name;	       /* data table name requested */
int row_num;	       /* data table row requested */
char *keyname;	       /* data table keyword requested */
char *err;	       /* some sort of error message */
/*
* RETURN ???
*
* retrieve a double precision value from user requested correlator data table
-*/
{

    int    nrows;
    double value, mjdtime, parallax, dist_au, asteroid();
    char in_keyword[16], in_tblname[16], mjd_str[24];
    char ra_str[24], dec_str[24];


    strcpy (in_keyword, keyname);
    cvrtuc (in_keyword);
    strcpy (in_tblname, tbl_name);
    cvrtuc (in_tblname);

    nrows = 1;
    if ((strcmp(in_tblname, "UTC")) == 0)
    {
       nrows = 5;
       if ((strcmp(in_keyword, "TAIUTC")) == 0)
       {
          value = p_request->tai_utc[row_num];
          return (value);
       }
       if ((strcmp(in_keyword, "UT1UTC")) == 0)
          return (p_request->ut1_utc[row_num]);

       if ((strcmp(in_keyword, "XPOLE")) == 0)
          return (p_request->xpole[row_num]);

       if ((strcmp(in_keyword, "YPOLE")) == 0)
          return (p_request->ypole[row_num]);
    }

    if ((strcmp(in_tblname, "STATION")) == 0)
    {
       if (((strcmp(in_keyword, "X")) == 0)&&row_num == 0)
          return (p_request->a_x);
       if (((strcmp(in_keyword, "X")) == 0)&&row_num == 1)
          return (p_request->b_x);
       if (((strcmp(in_keyword, "Y")) == 0)&&row_num == 0)
          return (p_request->a_y);
       if (((strcmp(in_keyword, "Y")) == 0)&&row_num == 1)
          return (p_request->b_y);
       if (((strcmp(in_keyword, "Z")) == 0)&&row_num == 0)
          return (p_request->a_z);
       if (((strcmp(in_keyword, "Z")) == 0)&&row_num == 1)
          return (p_request->b_z);
    }

    if ((strcmp(in_tblname, "SOURCE")) == 0)
    {
       if ((n_Horizons_rows > 0) &&
           (strcmp(p_request->source, Horizons_srcname) == 0))
       {
          mjdtime = (double)p_request->date + p_request->time;
          if (ilog_Horizons == 1) {
             date2str (mjdtime, mjd_str);
	     fprintf (flog,"recognized  horizons source : %s %s %f\n", p_request->source, 
                  in_keyword, mjdtime);
          }
          if ((strcmp(in_keyword, "RA")) == 0)
	  {
	     if (ilog_Horizons == 1)
	     {            
             ingra (asteroid(mjdtime, 2)*360.0/TWOPI, &ra_str[0]);
	     fprintf (flog,"%s : %s RA  = %s\n", p_request->source, 
                     mjd_str, ra_str);
	     }
             return (asteroid(mjdtime, 2));
          }
          if ((strcmp(in_keyword, "DEC")) == 0)
	  {  
	     if (ilog_Horizons == 1)
	     {
             ingdec (asteroid(mjdtime, 3)*360.0/TWOPI, &dec_str[0]);
             fprintf (flog,"%s : %s DEC = %s\n", p_request->source, 
                     mjd_str, dec_str);
             }
             return (asteroid(mjdtime,3));
	  }
          if ((strcmp(in_keyword, "PARALLAX")) == 0)
	  {
	     if (iparallax == 1) 
	     {
                dist_au = asteroid(mjdtime, 4);
                parallax = 206265.0 / dist_au;
                if (ilog_Horizons == 1)
	        {
                fprintf (flog,"%s : %s DIST = %20.12e (au)\n", p_request->source,
                        mjd_str, dist_au); 
                fflush (flog);
                }
                return (parallax);
             } else {
	       return (0.0);
             }
          }
       }
       else
       {
          if ((strcmp(in_keyword, "RA")) == 0)
             return (p_request->ra);
          if ((strcmp(in_keyword, "DEC")) == 0)
             return (p_request->dec);
          if ((strcmp(in_keyword, "PARALLAX")) == 0)
             return (p_request->parallax);
      }
    }

    return (0.0);
}

/*  @(#)f_fetch.c  version 1.11  created 93/10/17 23:16:16
                fetched from SCCS 95/06/26 14:41:08
%% retrieve a float value from user requested correlator data table
LANGUAGE: C
ENVIRONMENT: Any
::
*/

/* externals */

/*++**************************************************************************/
float f_fetch (job_num, tbl_name, row_num, keyname, err)
int job_num;	       /* job index number in queue table */
char *tbl_name;	       /* data table name requested */
int row_num;	       /* data table row requested */
char *keyname;	       /* data table keyword requested */
char *err;	       /* some sort of error message */
/*
* RETURN ???
*
* retrieve a float value from user requested correlator data table
-*/
{
    float value;
    char in_keyword[16], in_tblname[16];


    strcpy (in_keyword, keyname);
    cvrtuc (in_keyword);
    strcpy (in_tblname, tbl_name);
    cvrtuc (in_tblname);


    if ((strcmp (in_tblname, "STATION")) == 0)
    {
       if ((strcmp (in_keyword, "AXISOFF_X")) == 0)
       {
          if (row_num == 0)
             value = (float)p_request->axis_off_a;
          else
             value = (float)p_request->axis_off_b;
          return (value);
       }
    } 

    if ((strcmp(in_tblname, "METRO")) == 0)
    {
       if ((strcmp(in_keyword, "PRESSURE")) == 0)
          return ((float)p_request->pressure_b);
       if ((strcmp(in_keyword, "TEMP C")) == 0)
          return (-999.0);
       if ((strcmp(in_keyword, "REL.HUM.")) == 0)
          return (-999.0);
    }

    return (0.0);
}
/*  @(#)i_fetch.c  version 1.11  created 93/10/17 23:23:29
                fetched from SCCS 95/06/26 14:41:27
%% retrieve an integer value from user requested correlator data table.
LANGUAGE: C
ENVIRONMENT: Any
::
*/

/* externals */


/*++****************************************************************************
*/
int i_fetch (job_num, p_tbl_name, row_num, p_keyname, p_err)
int job_num;	       /* job index number in queue table */
char *p_tbl_name;	       /* data table name requested */
int row_num;	       /* data table row requested */
char *p_keyname;	       /* data table keyword requested */
char *p_err;	       /* some sort of error message */
/*
* RETURN ???
*
* retrieve an integer value from user requested correlator data table.
-*/
{
    int value;
    char in_keyword[16], in_tblname[16];


    strcpy (in_keyword, p_keyname);
    cvrtuc (in_keyword);
    strcpy (in_tblname, p_tbl_name);
    cvrtuc (in_tblname);


    if ((strcmp (in_tblname, "STATION")) == 0)
    {
       if ((strcmp (in_keyword, "AXISOFF_X")) == 0)
       {
          value = (float)p_request->axis_off_b;
          return (value);
       }
    } 
    return (0);
}

/*  @(#)c_fetch.c  version 1.11  created 93/10/17 22:15:12
                fetched from SCCS 95/06/26 14:40:39
%% function to retrieve a char string from user requested correlator data table
LANGUAGE: C
ENVIRONMENT: Any
:: index words
*/

/* externals */

/*++****************************************************************************
*/
char *c_fetch (job_num, p_tbl_name, row_num, p_keyname, p_err)
int job_num;	       /* job index number in queue table */
char *p_tbl_name;      /* data table name requested */
int row_num;	       /* data table row requested */
char *p_keyname;       /* data table keyword requested */
char *p_err;	       /* some sort of error message */
/*
* RETURN ???
-*/
{
    char *value;
    char in_keyword[16], in_tblname[16];


    strcpy (in_keyword, p_keyname);
    cvrtuc (in_keyword);
    strcpy (in_tblname, p_tbl_name);
    cvrtuc (in_tblname);


    if ((strcmp (in_tblname, "STATION")) == 0)
    {
       if ((strcmp (in_keyword, "AXISTYPE")) == 0)
       {
          if (row_num == 0)
             value = (char *)(p_request->axis_type_a); 
          else
             value = (char *)(p_request->axis_type_b); 
          cvrtuc (value);
          return (value);
       }
       if ((strcmp (in_keyword, "NAME")) == 0)
       {
          value = (char *)(p_request->station_b); 
          cvrtuc (value);
          return (value);
       }
    } 
    if ((strcmp (in_tblname, "SOURCE")) == 0)
    {
       if ((strcmp (in_keyword, "NAME")) == 0)
       {
          value = (char *)(p_request->source); 
          return (value);
       }
    }
    return (NULL);
}

/*  @(#)mjd_fetch.c  version 1.11  created 93/10/17 23:09:23
                fetched from SCCS 95/06/26 14:13:46
%% retrieve a deicmal mjd date/time from user requested correlator data table
LANGUAGE: C
ENVIRONMENT: Any
::
*/

/* externals */

/*++****************************************************************************
*/
double mjd_fetch (job_num, tbl_name, row_num, keyname, err)
int job_num;	       /* job index number in queue table */
char *tbl_name;	       /* data table name requested */
int row_num;	       /* data table row requested */
char *keyname;	       /* data table keyword requested */
char *err;	       /* some sort of error message */
/*
* RETURN ???
*
* retrieve a decimal mjd date/time from user requested correlator data table
-*/
{
    double value;
    char in_keyword[16], in_tblname[16];


    strcpy (in_keyword, keyname);
    cvrtuc (in_keyword);
    strcpy (in_tblname, tbl_name);
    cvrtuc (in_tblname);

    if ((strcmp (in_tblname, "UTC")) == 0)
       return (p_request->EOP_time[row_num]);

    if ((strcmp (in_tblname, "MASTER")) == 0)
    {
       value = (double)p_request->date + p_request->time;
       return (value);
    }

    return 0.0;
}

/*++****************************************************************************
*/
int  oce_fetch_ (istation, itype, coeffs)
     int *istation;
     int *itype;            /* itype = 1, sitocamp[11] */
                            /* itype = 2, sitocphs[11] */ 
                            /* itype = 3, sithocam[2][11] */
                            /* itype = 4, sithocph[2][11] */
     double *coeffs;
/*
* RETURN ???
*
* retrieves the ocean loading coefficients for CALC
-*/
{
    int i, j, imode, istn;

    imode = *itype;
    istn  = *istation;
    /*
    printf ("in oce_fetch, itype = %d\n", imode);
    printf ("              stn_id = %d\n", istn);
    */
    if (imode == 1)
       for (i = 0; i < 11; i++, coeffs++)
       {
           *coeffs = p_ocean_row[istn]->sitocamp[i];
	   /*   printf ("sitocamp = %e\n", p_ocean_row[istn]->sitocamp[i]);*/
       }
    if (imode == 2)
       for (i = 0; i < 11; i++, coeffs++)
           *coeffs = p_ocean_row[istn]->sitocphs[i];

    if (imode == 3)
       for (j = 0; j < 2; j++)
	 for (i = 0; i < 11; i++, coeffs++) {
           *coeffs = p_ocean_row[istn]->sithocam[j][i];
	   /* printf ("sithocam = %e\n", p_ocean_row[istn]->sithocam[j][i]); */
         }

    if (imode == 4)
       for (j = 0; j < 2; j++)
       for (i = 0; i < 11; i++, coeffs++)
           *coeffs = p_ocean_row[istn]->sithocph[j][i];

    
    return (0);
}
/*  @(#)CALC5Server.c  version 1.2  created 00/08/27 14:45:22
                fetched from SCCS 00/10/11 15:01:22
%% fortran-callable subroutines
LANGUAGE: C
ENVIRONMENT: VLBA Correlator real-time system running under vxWorks
:: Fortran
*/
/*
#include "vxWorks.h" 
#include "memLib.h"
#include "stdlib.h"
*/
/*++****************************************************************************
*/
void c1write_ (chr1_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
int  chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a single character 
* string.  It should be called from FORTRAN with the call:
*       c1write (s)
* where s is a character string of any length (7 is used as an example):
*       character*7 s
-*/
{
    char *inword;	/* pointer to string to copy input string into 
			   so that terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';
    
    printf ("%s\n", inword); 
    free (inword);
}
/*++****************************************************************************
*/
void c1log_ (chr1_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
int  chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a single character 
* string.  It should be called from FORTRAN with the call:
*       c1log (s)
* where s is a character string of any length (7 is used as an example):
*       character*7 s
* FUnction c1log writes the character string into the ctskPerror log file.
-*/
{
    char *inword;	/* pointer to string to copy input string into 
			   so that terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';
    
    printf ("%s\n", inword); 
    free (inword);
}

/*++****************************************************************************
*/
void c2write_ (chr1_ptr, chr2_ptr, chr1_len, chr2_len)
char *chr1_ptr;		/* pointer to first character string to be written */
char *chr2_ptr;		/* pointer to second character string to be written */
int  chr1_len;		/* length of first character string */
int  chr2_len;		/* length of second character string */
/*
* This function is a FORTRAN callable subroutine to write two character 
* strings.  The strings are appended, with no intervening characters.
* It should be called from FORTRAN with the call:
*       c2write (s1, s2)
* where s1 and s2 are character strings of any length (7 and 9 are used
* as an example):
*       character*7 s1
*       character*9 s2
-*/
{
    char *inword;	/* pointer to string to copy first string into so that
			   terminating character can be added */
    char *in2word;	/* pointer to string to copy second string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    in2word = malloc (chr2_len + 1);
    strncpy (in2word, chr2_ptr, chr2_len);
    in2word [chr2_len] = '\0';

    printf ("%s%s\n", inword, in2word); 
    free (inword);
    free (in2word);
}
/*++****************************************************************************
*/
void cnwrite_ (chr1_ptr, chr2_ptr, p_i2char, chr1_len, chr2_len)
char *chr1_ptr;		/* pointer to first character string to be written */
char *chr2_ptr;		/* pointer to second character string to be written */
short *p_i2char;         /* number of integer*2 characters for CALC */
int chr1_len;		/* length of first character string */
int chr2_len;		/* length of second character string */
/*
* This function is a FORTRAN callable subroutine to write two character 
* strings.  The strings are appended, with no intervening characters.
* It should be called from FORTRAN with the call:
*       c2write (s1, s2)
* where s1 and s2 are character strings of any length (7 and 9 are used
* as an example):
*       character*7 s1
*       character*9 s2
-*/
{
    long  nchars;
    char *inword;	/* pointer to string to copy first string into so that
			   terminating character can be added */
    char *in2word;	/* pointer to string to copy second string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    nchars = *p_i2char;
    in2word = malloc (2*nchars + 1);
    strncpy (in2word, chr2_ptr, 2*nchars);
    in2word [2*nchars] = '\0';

    printf ("%s%s\n", inword, in2word); 
    free (inword);
    free (in2word);
}
/*++****************************************************************************
*/
void cfwrite_ (chr1_ptr, chr2_ptr, p_i2char, chr1_len, chr2_len)
char *chr1_ptr;		/* pointer to first character string to be written */
char *chr2_ptr;		/* pointer to second character string to be written */
short *p_i2char;         /* number of char*1 characters for CALC */
int chr1_len;		/* length of first character string */
int chr2_len;		/* length of second character string */
/*
* This function is a FORTRAN callable subroutine to write two character 
* strings.  The strings are appended, with no intervening characters.
* It should be called from FORTRAN with the call:
*       cfwrite (s1, s2)
* where s1 and s2 are character strings of any length (7 and 9 are used
* as an example):
*       character*7 s1
*       character*9 s2
-*/
{
    long  nchars;
    char *inword;	/* pointer to string to copy first string into so that
			   terminating character can be added */
    char *in2word;	/* pointer to string to copy second string into so that
			   terminating character can be added */

    extern FILE *flog;

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    nchars = *p_i2char;
    in2word = malloc (nchars + 1);
    strncpy (in2word, chr2_ptr, nchars);
    in2word [nchars] = '\0';

    fprintf (flog,"%s%s\n", inword, in2word); 
    free (inword);
    free (in2word);
}
/*++****************************************************************************
*/
void c2logerror_ (chr1_ptr, chr2_ptr, chr1_len, chr2_len)
char *chr1_ptr;		/* pointer to first character string to be written */
char *chr2_ptr;		/* pointer to second character string to be written */
int chr1_len;		/* length of first character string */
int chr2_len;		/* length of second character string */
/*
* This function is a FORTRAN callable subroutine to write two character 
* strings.  The strings are appended, with no intervening characters.
* It should be called from FORTRAN with the call:
*       c2write (s1, s2)
* where s1 and s2 are character strings of any length (7 and 9 are used
* as an example):
*       character*7 s1
*       character*9 s2
-*/
{
    char *inword;	/* pointer to string to copy first string into so that
			   terminating character can be added */
    char *in2word;	/* pointer to string to copy second string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    in2word = malloc (chr2_len + 1);
    strncpy (in2word, chr2_ptr, chr2_len);
    in2word [chr2_len] = '\0';
/*
    ctskPerrStrcpy (1, inword);
    ctskPerrStrcpy (2, in2word);
    ctskPerror (ctskERROR, 0x1122,"CALC ERROR");
*/
    free (inword);
    free (in2word);
}

/*++****************************************************************************
*/
void d1write_ (chr1_ptr,float1_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
double *float1_ptr;	/* pointer to double precision number to write */
int chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a character string
* followed by a REAL*8 number.  It is intended that the string be a label
* for the number.  A single space is added between the string and the number.
* It should be called from FORTRAN with the call:
*       d1write (s, a)
* where s is a character string of any length (7 is used as an example):
*       character*7 s
* and where a is a REAL*8 number:
*       real*8 a
*        -or-
*       double precision a
-*/ 
{
    char *inword;	/* pointer to string to copy input string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    printf ("%s %22.16le\n", inword, *float1_ptr); 
    free (inword);
}

/*++****************************************************************************
*/
void d2write_ (chr1_ptr,float1_ptr,float2_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
double *float1_ptr;	/* pointer to first double number to write */
double *float2_ptr;	/* pointer to second double number to write */
int chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a character string
* followed by two REAL*8 numbers.  It is intended that the string be a label
* for the numbers.  A single space is added between the string and the first
* number, and between the numbers.
* It should be called from FORTRAN with the call:
*       d2write (s, a, b)                           
* where s is a character string of any length (7 is used as an example):
*       character*7 s
* and where a and b are REAL*8 numbers:
*       real*8 a
*       real*8 b
*        -or-
*       double precision a
*       double precision b
-*/ 
{
    char *inword;	/* pointer to string to copy input string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';
 
    printf ("%s %18.10le %18.10le\n", inword, *float1_ptr, *float2_ptr); 
    free (inword);
}
/*++****************************************************************************
*/
void c2i2writ_ (chr1_ptr,chr2_ptr,int1_ptr,int2_ptr,chr1_len,chr2_len)
char *chr1_ptr;		/* pointer to character string to write */
char *chr2_ptr;		/* pointer to character string to write */
int *int1_ptr;	        /* pointer to first int number to write */
int *int2_ptr;	        /* pointer to second int number to write */
int chr1_len;		/* length of character string */
int chr2_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write 2 character strings
* followed by two INTEGER*4 numbers.  It is intended that the string be a label
* for the numbers.  A single space is added between the string and the first
* number, and between the numbers.
* It should be called from FORTRAN with the call:
*       c2i2writ (s1, s2, a, b)                           
* where s is a character string of any length (7 is used as an example):
*       character*7 s
* and where a and b are INTEGER*4 numbers
-*/ 
{
    char *inword1;	/* pointer to string to copy input string into so that
			   terminating character can be added */
    char *inword2;

    inword1 = malloc (chr1_len + 1);
    inword2 = malloc (chr2_len + 1);
    strncpy (inword1, chr1_ptr, chr1_len);
    strncpy (inword2, chr2_ptr, chr2_len);
    inword1 [chr1_len] = '\0';
    inword2 [chr2_len] = '\0';
 
    printf ("%s %s kerr[%d] = %d\n", inword1, inword2, *int1_ptr, *int2_ptr); 
    free (inword1);
    free (inword2);
}

/*++****************************************************************************
*/
void d10write_ (chr1_ptr,float1_ptr,float2_ptr,float3_ptr,float4_ptr,
		float5_ptr,float6_ptr,float7_ptr,float8_ptr,
		float9_ptr,float10_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
double *float1_ptr,*float2_ptr,*float3_ptr,*float4_ptr,
       *float5_ptr,*float6_ptr,*float7_ptr,*float8_ptr,
       *float9_ptr,*float10_ptr;  /* pointers to dp numbers to write */
int chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a character string
* followed by ten REAL*8 numbers.  It is intended that the string be a label
* for the numbers.  The label is written on a line by itself.  Then the
* numbers are written, separated by spaces, in two lines of four followed
* by a line of two.
* It should be called from FORTRAN with the call:
*       d10write (s, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
* where s is a character string of any length (7 is used as an example):
*       character*7 s
* and where a1 to a10 are REAL*8 numbers:
*       real*8 a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
*        -or-
*       double precision a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
-*/
{
    char *inword;	/* pointer to string to copy input string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    printf("%s\n\
%18.10le %18.10le %18.10le %18.10le\n\
%18.10le %18.10le %18.10le %18.10le\n\
%18.10le %18.10le\n",
	   inword,
	   *float1_ptr, *float2_ptr, *float3_ptr, *float4_ptr,
	   *float5_ptr, *float6_ptr, *float7_ptr, *float8_ptr,
	   *float9_ptr, *float10_ptr);
    free (inword);
}

/*++****************************************************************************
*/
void r1write_ (chr1_ptr, float1_ptr, chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
float *float1_ptr;	/* pointer to REAL number to write */
int chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a character string
* followed by a REAL*4 number.  It is intended that the string be a label
* for the number.  A single space is added between the string and the number.
* It should be called from FORTRAN with the call:
*       d1write (s, a)                           
* where s is a character string of any length (8 is used as an example):
*       character*8 s
* and where a is a REAL*4 number:
*       real*4 a                 
*        -or-
*       real a
-*/
{
    char *inword;	/* pointer to string to copy input string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';
    
    printf ("%s %e\n", inword, *float1_ptr);
    free (inword);
}

/*++****************************************************************************
*/
void i1write_ (chr1_ptr,int1_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
int *int1_ptr;		/* pointer to integer*4 number to write */
int chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a character string
* followed by a INTEGER*4 number.  It is intended that the string be a label
* for the number.  A single space is added between the string and the number.
* It should be called from FORTRAN with the call:
*       i1write (s, i)                           
* where s is a character string of any length (8 is used as an example):
*       character*8 s
* and where i is a INTEGER*4 number:
*       integer*4 i                 
*        -or-
*       integer i
-*/
{
    char *inword;	/* pointer to string to copy input string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';
 
    printf ("%s %d\n", inword, *int1_ptr);
    free (inword);
}

/*++****************************************************************************
*/
void i2write_ (chr1_ptr,int1_ptr,int2_ptr,chr1_len)
char *chr1_ptr;		/* pointer to character string to write */
int *int1_ptr;	        /* pointer to first int*4number to write */
int *int2_ptr;	        /* pointer to second int*4 number to write */
int chr1_len;		/* length of character string */
/*
* This function is a FORTRAN callable subroutine to write a character string
* followed by two INTEGER*4 numbers.  It is intended that the string be a label
* for the numbers.  A single space is added between the string and the first
* number, and between the numbers.
* It should be called from FORTRAN with the call:
*       i2write (s, a, b)                           
* where s is a character string of any length (7 is used as an example):
*       character*7 s
* and where a and b are INTEGER*4 numbers:
*       integer*4 a
*       integer*4 b
*        -or-
*       integer a
*       integer b
-*/ 
{
    char *inword;	/* pointer to string to copy input string into so that
			   terminating character can be added */

    inword = malloc (chr1_len + 1);
    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';
 
    printf ("%s %d %d\n", inword, *int1_ptr, *int2_ptr); 
    free (inword);
}

/*++****************************************************************************
*/
int fstr2cpy_ (chr1_ptr, chr2_ptr, chr1_len, chr2_len)
char *chr1_ptr;		/* pointer to string to be copied to */
char *chr2_ptr;		/* pointer to string to be copied from */
int chr1_len;		/* length of string to be copied to.  IGNORED. */
int chr2_len;		/* length of string to be copied from.  IGNORED. */
/*
* RETURN 0 always
*
* This function is a FORTRAN callable subroutine to copy exactly two 
* characters of one string to another string.  It does not check the length 
* of either string.  It always returns 0.
*
* The string is copied explicitly, character by character, instead of using
* the c function strncpy(), in case there are embedded nulls in the string,
* which would cause strncpy() to detect end of string, and stop.
*
* It should be called from FORTRAN with the call:
*       fstr2cpy (sto, sfrom)
* where sto and sfrom are character strings of length 2.  Two characters of the
* string sfrom are copied to the string sto.
*       character*2 sto, sfrom
*        -or-
*       integer*2 sto, sfrom
-*/ 
{
    int  i;	/* loop counter for character copy */

    for (i = 0; i < 2; i++, chr1_ptr++, chr2_ptr++)
        *chr1_ptr = *chr2_ptr;
    return (0);
}

/*++****************************************************************************
*/
int fstrcmp_ (chr1_ptr,chr2_ptr,chr1_len,chr2_len)
char *chr1_ptr;		/* pointer to first string of pair to be compared */
char *chr2_ptr;		/* pointer to second string of pair to be compared */
int chr1_len;		/* length of first string */
int chr2_len;		/* length of second string */
/*
* RETURN return of function strcmp()
*
* This function is a FORTRAN callable subroutine to compare two character
* strings.  It uses the c function strcmp(), which means that a null character
* ('\0') embedded in either string will stop the comparison at that point. 
* This function is used only in the Fortran subroutines 'get4' and 'geti'
* that are called by CALC.  The maximum character string length in the CALC
* functions is 14 characters.  This function is limited to comparing strings
* 20 characters long.
-*/
{
    char inword[20];	/* string to copy first string into so that
			   terminating character can be added */
    char in2word[20];	/* string to copy second string into so that
			   terminating character can be added */

    strncpy (inword, chr1_ptr, chr1_len);
    inword [chr1_len] = '\0';

    strncpy (in2word, chr2_ptr, chr2_len);
    in2word [chr2_len] = '\0';

    return (strcmp (inword, in2word));
}

/*++****************************************************************************
*/
int fstrcpy_ (chr1_ptr, chr2_ptr, chr1_len, chr2_len)
char  *chr1_ptr;	/* pointer to string to be copied to */
char  *chr2_ptr;	/* pointer to string to be copied from */
int chr1_len;   	/* length of string to be copied to */
int chr2_len;   	/* length of string to be copied from */
/*
* RETURN 0 always
*
* This function is a FORTRAN callable subroutine to copy one string to another
* string.  It always returns 0.
* 
* The string is copied explicitly, character by character, instead of using
* the c function strncpy(), in case there are embedded nulls in the string,
* which would cause strncpy() to detect end of string, and stop.  If the from
* string is longer than the to string, the end of the to string is padded
* with null characters.
* 
* It should be called from FORTRAN with the call:
*       fstrcpy (sto, sfrom)                    
* where sto and sfrom are character strings of any length (8 and 10 are used
* as examples).  The characters of the string sfrom are copied to the string
* sto.
*       character*8 sto
*       character*10 sfrom
-*/
{
    int num_copy;	/* number of chars to copy from chr2_ptr to chr1_ptr */
    int  i;		/* counter for number of chars copied to chr1_ptr */

    num_copy = ((chr1_len < chr2_len) ? chr1_len : chr2_len);
    for (i = 0; i < num_copy; i++, chr1_ptr++, chr2_ptr++)
        *chr1_ptr = *chr2_ptr;
    for ( ; i < chr1_len; i++, chr1_ptr++)
        *chr1_ptr = '\0';
    return (0);
}
/*******************************************************************************
*/
double         asteroid (mjdtime, icolumn)
    double          mjdtime;
    int             icolumn;
{

    double value[2], time[2], outval;
    int    i;

    if (n_Horizons_rows == 0)
      return (0.0);

    outval = 0.0;
    for (i = 1; i < n_Horizons_rows; i++)
    {
        if (mjdtime >= solar_target[i-1].ref_time && mjdtime <= solar_target[i].ref_time)
	{
           if (icolumn == 2)
	   {
              value[0] = solar_target[i-1].ra;
              value[1] = solar_target[i].ra;
           }
           else if (icolumn == 3)
	   {
              value[0] = solar_target[i-1].dec;
              value[1] = solar_target[i].dec;
           }
           else if (icolumn == 4)
	   {
              value[0] = solar_target[i-1].delta;
              value[1] = solar_target[i].delta;
           }
           else if (icolumn == 5)
	   {
              value[0] = solar_target[i-1].deldot;
              value[1] = solar_target[i].deldot;
           }
	   else
	   {
	      value[0] = 0.0;
	      value[1] = 0.0;
	      /* FIXME -- issue warning as this should not happen */
	   }
 
           time[0] = solar_target[i-1].ref_time;
           time[1] = solar_target[i].ref_time;

           outval = value[0] + (mjdtime - time[0]) * (value[1] - value[0])
                  / (time[1] - time[0]);
           break;
	}
    }
    return (outval);
}
