

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <fcntl.h>
#include <rpc/rpc.h>
#include "MATHCNST.H"
#include "CALCServer.h"
#include "difxio/parsedifx.h"

static struct timeval TIMEOUT = {10, 0};
/* 
 * Define numerous string constants needed for data mapping 
 */
#define TELESCOPE        "TELESCOPE"
#define SCAN             "SCAN"
#define EOP              "EOP"

#define START_MJD        "START MJD"
#define START_YEAR       "START YEAR"
#define START_MONTH      "START MONTH"
#define START_DAY        "START DAY"
#define START_HOUR       "START HOUR"
#define START_MINUTE     "START MINUTE"
#define START_SECOND     "START SECOND"
#define INCREMENT        "INCREMENT (SECS)"
#define NUM_TELESCOPES   "NUM TELESCOPES"
#define TELESCOPE_NAME   "TELESCOPE %d NAME"
#define TELESCOPE_MOUNT  "TELESCOPE %d MOUNT"
#define TELESCOPE_OFFSET "TELESCOPE %d OFFSET (m)"
#define TELESCOPE_X      "TELESCOPE %d X (m)"
#define TELESCOPE_Y      "TELESCOPE %d Y (m)"
#define TELESCOPE_Z      "TELESCOPE %d Z (m)"
#define NUM_SCANS        "NUM SCANS"
#define SCAN_POINTS      "SCAN %d POINTS"
#define SCAN_START_PT    "SCAN %d START PT"
#define SCAN_SRC_NAME    "SCAN %d SRC NAME"
#define SCAN_SRC_RA      "SCAN %d SRC RA"
#define SCAN_SRC_DEC     "SCAN %d SRC DEC"
#define NUM_EOP          "NUM EOP"
#define EOP_TIME         "EOP %d TIME (mjd)"
#define EOP_TAI_UTC      "EOP %d TAI_UTC (sec)"
#define EOP_UT1_UTC      "EOP %d UT1_UTC (sec)"
#define EOP_XPOLE        "EOP %d XPOLE (arcsec)"
#define EOP_YPOLE        "EOP %d YPOLE (arcsec)"
#define DELAY_FILENAME   "DELAY FILENAME"
#define UVW_FILENAME     "UVW FILENAME"
#define RELATIVE_INC     "RELATIVE INC %d:"

#define CALC_DATA 0x0
#define UVW_DATA  0x1
#define DELAY_DATA 0x2
#define RATE_DATA 0x4

#define MAX_BUF_SIZE 8192

#define VALUE_OFFSET 20  /* Value is always 21 characters from beginning of line */

#define MAX_TELESCOPES 10
#define MAX_SCANS 100
#define MAX_POINTS 1000
#define MAX_EOPS 5

#define T_NAME_SIZE 8 /*telescope name size+1 */
#define T_MNT_SIZE 8 /* telsccope mount size+1 */
#define SRC_NAME_SIZE 128

#define U_FNAME_SIZE 64
#define D_FNAME_SIZE 64
#define I_FNAME_SIZE 64
#define R_FNAME_SIZE 64

#define MOUNT_TYPE "altz"

#define CALC_SERVER "parallax"

typedef enum
{
  STRING = 1,
  INT,
  FLOAT
} TE_DATA_TYPES;

/* 
 * These also form array indicies for Data_Map structure. Do not add or reorder 
 * to these without making corresponding change to Data_Map entry
 */
typedef enum
{
  MJD=0,
  YEAR,
  MONTH,
  DAY,
  HOUR,
  MINUTE,
  SECOND,
  INCR,
  N_TELESCOPES,
  T_NAME, 
  T_MOUNT,
  T_OFFSET, 
  T_X,    
  T_Y,  
  T_Z,   
  N_EOP,  
  E_TIME,    
  E_TAI_UTC, 
  E_UT1_UTC,  
  E_XPOLE,   
  E_YPOLE,   
  N_SCANS,  
  S_POINTS,
  S_START_PT, 
  S_SRC_NAME, 
  S_SRC_RA,   
  S_SRC_DEC, 
  GET_CALC,
  NOOP,
  MAX_STATES
} TE_STATES;

  
typedef struct 
{
  char tag[64];
  int state;     /* for state specific actions */
  void *c_data;  /* pointer to calc data */
  int  d_type;   /* Data type */
  int  dest;     /* CALC data, UVW data, or DELAY data */ 
} TS_DATA_MAP;

typedef struct
{
  char   t_name[MAX_TELESCOPES][T_NAME_SIZE]; /* telescope name */
  char   t_mount[MAX_TELESCOPES][T_MNT_SIZE]; /* telescope mount type */
  char   source_name[SRC_NAME_SIZE];
  double t_offset[MAX_TELESCOPES];
  double t_x[MAX_TELESCOPES];
  double t_y[MAX_TELESCOPES]; 
  double t_z[MAX_TELESCOPES]; /* x,y,z coordinate*/
  double e_time[MAX_EOPS]; 
  double e_tai_utc[MAX_EOPS];
  double e_ut1_utc[MAX_EOPS];
  double e_xpole[MAX_EOPS];
  double e_ypole[MAX_EOPS];
  double declination;
  double r_ascension;
  double mjd;
  int scan_start_pt;  
  int increment; 
} TS_CALC_PARAMS;
static TS_CALC_PARAMS Calc_Params;


static int processStateData(TS_DATA_MAP *, DifxParameters *, void *, FILE *, FILE *, FILE *);
static int findStateEntry(TS_DATA_MAP *, int, int);
static getCALC_res *getCalcData(TS_CALC_PARAMS, double, int, CLIENT *);

const char program[] = "calcif";
const char version[] = "1.0";
const char verdate[] = "20070625";
const char author[]  = "Pete Whiteis";

int usage(const char *pgm)
{
	fprintf(stderr, "%s ver. %s   %s %s\n\n", program, version,
		author, verdate);
	fprintf(stderr, "usage : %s <input file> \n\n", pgm);

	return 0;
}

int main (int argc, char *argv[])
{

  FILE *u_fd, *d_fd, *r_fd; /* uvw, delay, rate file descriptors */
  int i;
  int call_cnt;
  int state;
  int entry;
  int num_telescopes; 
  int num_scans; 
  int num_eops; 
  int num_points;
  int which_telescope=0;
  int which_scan=0;
  int which_eop=0;
  char uvw_fname[U_FNAME_SIZE], 
    delay_fname[D_FNAME_SIZE], 
    rate_fname[R_FNAME_SIZE];
  char *i_fname;
  double time_incr;
  struct getCALC_res *p_result;

  DifxParameters *dp;
  CLIENT    *cl;
  
  static TS_DATA_MAP Data_Map[] =
    {
      {START_MJD,       MJD,      NULL,FLOAT, CALC_DATA},
      {START_YEAR,      YEAR,     NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {START_MONTH,     MONTH,    NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {START_DAY,       DAY,      NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {START_HOUR,      HOUR,     NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {START_MINUTE,    MINUTE,   NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {START_SECOND,    SECOND,   NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {INCREMENT,       INCR,     NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {NUM_TELESCOPES,  N_TELESCOPES, NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {TELESCOPE_NAME,  T_NAME,   NULL,STRING,UVW_DATA|RATE_DATA|DELAY_DATA},
      {TELESCOPE_MOUNT, T_MOUNT,  NULL,STRING,UVW_DATA|RATE_DATA},
      {TELESCOPE_OFFSET,T_OFFSET, NULL,FLOAT, RATE_DATA}, 
      {TELESCOPE_X,     T_X,      NULL,FLOAT, UVW_DATA|RATE_DATA},
      {TELESCOPE_Y,     T_Y,      NULL,FLOAT, UVW_DATA|RATE_DATA},
      {TELESCOPE_Z,     T_Z,      NULL,FLOAT, UVW_DATA|RATE_DATA},
      {NUM_SCANS,       N_SCANS,  NULL,INT,   UVW_DATA|RATE_DATA|DELAY_DATA},
      {SCAN_POINTS,     S_POINTS, NULL,INT,   UVW_DATA|RATE_DATA|DELAY_DATA},
      {SCAN_START_PT,   S_START_PT, NULL,INT, UVW_DATA|RATE_DATA|DELAY_DATA},
      {SCAN_SRC_NAME,   S_SRC_NAME, NULL,STRING, UVW_DATA|RATE_DATA|DELAY_DATA},
      {SCAN_SRC_RA,     S_SRC_RA,  NULL,FLOAT, UVW_DATA|RATE_DATA},
      {SCAN_SRC_DEC,    S_SRC_DEC, NULL,FLOAT, UVW_DATA|RATE_DATA},
      {NUM_EOP,         N_EOP,     NULL,INT, CALC_DATA},
      {EOP_TIME,        E_TIME,    NULL,FLOAT, CALC_DATA},
      {EOP_TAI_UTC,     E_TAI_UTC, NULL,FLOAT, CALC_DATA},
      {EOP_UT1_UTC,     E_UT1_UTC, NULL,FLOAT, CALC_DATA},
      {EOP_XPOLE,       E_XPOLE,   NULL,  FLOAT, CALC_DATA},
      {EOP_YPOLE,       E_YPOLE,   NULL,  FLOAT, CALC_DATA}
    };
  int num_dmap_entries = sizeof(Data_Map)/sizeof(TS_DATA_MAP);
  
  if(argc < 2)
    {
      return usage(argv[0]);
    }
  
  i_fname = argv[1];
  
  /* load the input parameter from file */
  dp = newDifxParametersfromfile(i_fname);
  
  /* Before we do anything else, we'll need the file names. If not found, 
     do not pass go, do not collect $200 */
  /* uvw file */
  i = DifxParametersfind(dp, 0, UVW_FILENAME);
  if (i >= 0)
    {
      strcpy (uvw_fname, DifxParametersvalue(dp, i));
      printf("%s = %s\n", dp->rows[i].key, uvw_fname);
      /* Make rate filename from uvw file */
      strcpy(rate_fname, uvw_fname);
      *strstr(rate_fname, ".uvw") = 0x00;
      strcat(rate_fname, ".rate");
      printf("Rate file = %s\n", rate_fname);
    }
  /*delay file */
  i = DifxParametersfind(dp, 0, DELAY_FILENAME);
  if (i >= 0)
    {
      strcpy (delay_fname, DifxParametersvalue(dp, i));
      printf("%s = %s\n", dp->rows[i].key, delay_fname);
    }
  
  u_fd = fopen(uvw_fname, "w");
  if (!u_fd)
    {
      perror("Calcif-E-cannot open uvw file: \n");
      exit(1);
    }
  d_fd = fopen(delay_fname, "w");
  if (!d_fd)
    {
      perror("Calcif-E-cannot open delay file: \n");
      exit(1);
    }
  r_fd = fopen(rate_fname, "w");
  if (!r_fd)
    {
      perror("Calcif-E-cannot open rate file: \n");
      exit(1);
    }

  fprintf(r_fd, "CALC SERVER:        %s\n", CALC_SERVER);
  fprintf(r_fd, "CALC PROGRAM:       %d\n", (int)(CALCPROG));
  fprintf(r_fd, "CALC VERSION:       %d\n", (int)(CALCVERS));
    
  /* Create an rcp client */
  if (!(cl = clnt_create (CALC_SERVER, CALCPROG, CALCVERS, "tcp")))
    {
      clnt_pcreateerror (CALC_SERVER);
      printf("ERROR: rpc clnt_create fails for host : %-s\n", CALC_SERVER);
      /*        exit (0); */
    }
  
  printf("init state machine \n");
  /* init state machine */
  state = MJD;

  /*
   * Use state machine to 1) search .calc file for data of interest 
   * 2) call CALC 3) output results to .uvw, .delay and .rate files
   */
  while (state != NOOP)
    {
      
      entry = findStateEntry(Data_Map, state, num_dmap_entries); /*get table idx*/
      switch (state)
	{
	case MJD:
	  printf("MJD\n");
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.mjd, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with MJD processing \n");
	      state = NOOP;
	      break;
	    }
	  
	  state = YEAR;
	  break;
	  
	case YEAR:
	  printf("Year\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Year processing \n");
	      state = NOOP;
	      break;
	    }
	  state = MONTH;
	  break;
	case MONTH:
	  printf("Month\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with month processing \n");
	      state = NOOP;
	      break;
	    }
	  state = DAY;
	  break;
	case DAY:
	  printf("Day\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with day processing \n");
	      state = NOOP;
	      break;
	    }
	  state = HOUR;
	  break;
	case HOUR:
	  printf("Hour\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with hour processing \n");
	      state = NOOP;
	      break;
	    }
	  state = MINUTE;
	  break;
	case MINUTE:
	  printf("Minute\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with minute processing \n");
	      state = NOOP;
	      break;
	    }
	  state = SECOND;
	  break;
	case SECOND:
	  printf("Second\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with second processing \n");
	      state = NOOP;
	      break;
	    }
	  state = INCR;
	  break;
	case INCR:
	  printf("Increment\n");
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.increment, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Increment processing \n");
	      state = NOOP;
	      break;
	    }
	  state = N_TELESCOPES;
	  break;
	case N_TELESCOPES:
	  printf("Num Telescopes\n");
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&num_telescopes, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with num_telescopes processing \n");
	      state = NOOP;
	      break;
	    }
	  which_telescope = 0;
	  state = T_NAME;
	  break;
	case T_NAME:
	  printf("Telescope name\n");
	  sprintf(Data_Map[entry].tag, TELESCOPE_NAME, which_telescope); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)Calc_Params.t_name[which_telescope], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Telescope Name \n");
	      state = NOOP;
	      break;
	    }
	  state = T_MOUNT;
	  break;
	case T_MOUNT:
	  printf("Telescope Mount\n");
	  sprintf(Data_Map[entry].tag, TELESCOPE_MOUNT, which_telescope); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)Calc_Params.t_mount[which_telescope], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Telescope Mount \n");
	      state = NOOP;
	      break;
	    }
	  state = T_OFFSET;
	  break;
	case T_OFFSET:
	  printf("Telescope Offset\n");
	  sprintf(Data_Map[entry].tag, TELESCOPE_OFFSET, which_telescope); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.t_offset[which_telescope], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Telescope Offset \n");
	      state = NOOP;
	      break;
	    }
	  state = T_X;
	  break;
	case T_X:
	  printf("Telescope X\n");
	  sprintf(Data_Map[entry].tag, TELESCOPE_X, which_telescope); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.t_x[which_telescope], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Telescope X \n");
	      state = NOOP;
	      break;
	    }
	  state = T_Y;
	  break;
	case T_Y:
	  printf("Telescope Y\n");
	  sprintf(Data_Map[entry].tag, TELESCOPE_Y, which_telescope); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.t_y[which_telescope], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Telescope Y \n");
	      state = NOOP;
	      break;
	    }
	  state = T_Z;
	  break;
	case T_Z:
	  printf("Telescope Z\n");
	  sprintf(Data_Map[entry].tag, TELESCOPE_Z, which_telescope); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.t_z[which_telescope], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Telescope Z \n");
	      state = NOOP;
	      break;
	    }
	  
	  if (++which_telescope < num_telescopes)
	    state = T_NAME;
	  else
	    state = N_EOP;
	  
	  break;
	case N_EOP:
	  printf("Num EOPs\n");
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&num_eops, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with num_EOPs processing \n");
	      state = NOOP;
	      break;
	    }
	  which_eop = 0;
	  state = E_TIME;
	  break;
	case E_TIME:
	  printf("Eop Time\n");
	  if(which_eop < MAX_EOPS)
	  {
	    sprintf(Data_Map[entry].tag, EOP_TIME, which_eop); 
	    if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.e_time[which_eop], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with EOP Time processing \n");
	      state = NOOP;
	      break;
	    }
	  }
	  state = E_TAI_UTC;
	  break;
	case E_TAI_UTC:
	  printf("Eop tai utc\n");
	  if(which_eop < MAX_EOPS)
	  {
	    sprintf(Data_Map[entry].tag, EOP_TAI_UTC, which_eop); 
	    if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.e_tai_utc[which_eop], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with EOP tai utc processing \n");
	      state = NOOP;
	      break;
	    }
	  }
	  state = E_UT1_UTC;
	  break;
	case E_UT1_UTC:
	  printf("Eop ut1 utc\n");
	  if(which_eop < MAX_EOPS)
	  {
	    sprintf(Data_Map[entry].tag, EOP_UT1_UTC, which_eop); 
	    if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.e_ut1_utc[which_eop], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with EOP ut1 utc processing \n");
	      state = NOOP;
	      break;
	    }
	  }
	  state = E_XPOLE;
	  break;
	case E_XPOLE:
	  printf("Eop xpole\n");
	  if(which_eop < MAX_EOPS)
	  {
	    sprintf(Data_Map[entry].tag, EOP_XPOLE, which_eop); 
	    if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.e_xpole[which_eop], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with EOP xpole processing \n");
	      state = NOOP;
	      break;
	    }
	  }
	  state = E_YPOLE;
	  break;
	case E_YPOLE:
	  printf("Eop ypole\n");
	  if(which_eop < MAX_EOPS)
	  {
	    sprintf(Data_Map[entry].tag, EOP_YPOLE, which_eop); 
	    if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.e_ypole[which_eop], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with EOP ypole processing \n");
	      state = NOOP;
	      break;
	    }
	  }
	  if (++which_eop < num_eops)
	    state = E_TIME;
	  else
	    state = N_SCANS;
	  break;
	case N_SCANS:
	  printf("Num Scans\n");
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&num_scans, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with num_scans processing \n");
	      state = NOOP;
	      break;
	    }
	  which_scan = 0;
	  state = S_POINTS;
	  break;
	case S_POINTS:
	  printf("Scan Points\n");
	  sprintf(Data_Map[entry].tag, SCAN_POINTS, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&num_points, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with scan points processing \n");
	      state = NOOP;
	      break;
	    }
	  state = S_START_PT;
	  break;
	case S_START_PT:
	  printf("Scan Start Point\n");
	  sprintf(Data_Map[entry].tag, SCAN_START_PT, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.scan_start_pt, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with scan start point \n");
	      state = NOOP;
	      break;
	    }
	  state = S_SRC_NAME;
	  break;
	case S_SRC_NAME:
	  printf("Source Name\n");
	  sprintf(Data_Map[entry].tag, SCAN_SRC_NAME, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.source_name, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with source name \n");
	      state = NOOP;
	      break;
	    }
	  state = S_SRC_RA;
	  break;
	case S_SRC_RA:
	  printf("Right Ascension \n");
	  sprintf(Data_Map[entry].tag, SCAN_SRC_RA, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.r_ascension, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with right ascension \n");
	      state = NOOP;
	      break;
	    }
	  state = S_SRC_DEC;
	  break;
	case S_SRC_DEC:
	  printf("Declination \n");
	  sprintf(Data_Map[entry].tag, SCAN_SRC_DEC, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&Calc_Params.declination, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with declination \n");
	      state = NOOP;
	      break;
	    }
	  state = GET_CALC;
	  break;
	case GET_CALC:
	  /* 
	   * Heres the meat of the calc specific work.   We'll iterate through 
	   * data parsed from the file, and use it to call CALC.   Data returned 
	   * from CALC will be used to fill in the remaining u, v, w coordinates 
	   * as well as delays in both files respectively
	   */
	  printf("Calling CALC for scan %d , call count: \n", which_scan);
	  call_cnt = 0;
	  /* Go through all points for this scan */
	  for (i=-1; i<=num_points+1; i++)
	    {
	      int u_len, d_len, r_len;
	      int u_pos, d_pos, r_pos;
	      int j;
	      char u_buf[1024];  /* UVW line buffer */
	      char d_buf[1024];  /* Delay line buffer */
	      char r_buf[1024];  /* Rate line buffer */
	      /* Write tag to buffer(s) */
	      d_pos = u_pos = r_pos = 0;
	      u_len = sprintf(&u_buf[u_pos], RELATIVE_INC, i);
	      u_pos += u_len;
	      for (; u_pos<VALUE_OFFSET; u_pos++) /* pad w/ spaces to col 21 */
		u_buf[u_pos] = ' ';
	      r_len = sprintf(&r_buf[r_pos], RELATIVE_INC, i);
	      r_pos += r_len;
	      for (; r_pos<VALUE_OFFSET; r_pos++) /* pad w/ spaces to col 21 */
		r_buf[r_pos] = ' ';
	      d_len = sprintf(&d_buf[d_pos], RELATIVE_INC, i);
	      d_pos += d_len;
	      for (; d_pos<VALUE_OFFSET; d_pos++) /* pad w/ spaces to col 21 */
		d_buf[d_pos] = ' ';
	      /* Get information for each telescope */
	      for (j=0; j<num_telescopes; j++)
		{
		  time_incr = ((Calc_Params.scan_start_pt+i)*Calc_Params.increment)/86400.0;
		  p_result = getCalcData(Calc_Params, time_incr, j, cl);
		  if (++call_cnt%8==0)
		    printf("\r%d",call_cnt); /*entertain user */
		  /* 
		   * Write per telescope uvw, rate, and delay information
		   * to respective files 
		   */
		  u_len = sprintf(&u_buf[u_pos], 
				  "%7.6lf\t%7.6lf\t%7.6lf\t",
				  p_result->getCALC_res_u.record.UV[0],
				  p_result->getCALC_res_u.record.UV[1],
				  p_result->getCALC_res_u.record.UV[2]);
		  u_pos += u_len;
		  r_len = sprintf(&r_buf[r_pos],
				  "%12.10lf\t%14.12lf\t%14.12lf\t",
				  -p_result->getCALC_res_u.record.delay[1]*1e6,
				  p_result->getCALC_res_u.record.dry_atmos[0]*1e6,
				  p_result->getCALC_res_u.record.wet_atmos[0]*1e6);
		  r_pos += r_len;
		  if(p_result->getCALC_res_u.record.delay[0] >= 0)
		  {
		  	p_result->getCALC_res_u.record.delay[0] = -0.0;
		  }
		  d_len = sprintf(&d_buf[d_pos],"%12.10lf \t", 
				  -p_result->getCALC_res_u.record.delay[0]*1e6);
		  d_pos += d_len;
		}
	      u_len = sprintf(&u_buf[u_pos],"\n");
	      u_pos += u_len;
	      r_len = sprintf(&r_buf[r_pos],"\n");
	      r_pos += r_len;
	      d_len = sprintf(&d_buf[d_pos],"\n");
	      d_pos += d_len;
	      /* Fwrite line to uvw, delay file */
	      fwrite(u_buf, u_pos, 1, u_fd);
	      fwrite(r_buf, r_pos, 1, r_fd);
	      fwrite(d_buf, d_pos, 1, d_fd);
	    }
	  
	  printf("\n");
	  
	  if (++which_scan < num_scans)
	    state = S_POINTS;
	  else
	    state = NOOP ;
	  
	  break;

	default:
	  state=NOOP;
	  break;
	}
    }
  
  fclose(u_fd);
  fclose(d_fd);
  
  clnt_destroy (cl);
  
  return(0);
}

/* Here we simply find the index of lookup table entry */ 
static int findStateEntry(TS_DATA_MAP *dm, int state, int max_entries)
{
  int i;
  int found;

  found = FALSE;

  for (i=0; i<max_entries; i++)
    {
      if (dm[i].state == state)
	{
	  found = TRUE;
	  break;
	}
    }
  return ((found)?i:-1);
}

/*
 * Ues difx library routine to search our file for information of interest. 
 * Then write this to uvw and/or delay file.  Also save our data value if 
 * appropriate. 
 */
static int processStateData(TS_DATA_MAP *dm ,
			    DifxParameters *dp, 
			    void *c_data, 
			    FILE *u_fd,
			    FILE *r_fd,
			    FILE *d_fd)
{
  int i;
  
  i = DifxParametersfind(dp, 0, dm->tag);
  if (i<0)
    {
      printf("CALCIF-W_Could not find entry for %s \n", dm->tag);
      return(1);
    }
  
  
  if (dm->dest&UVW_DATA)
    fprintf(u_fd, "%s",dp->rows[i].line);
  
  if (dm->dest&RATE_DATA)
    fprintf(r_fd, "%s",dp->rows[i].line);
  
  if (dm->dest&DELAY_DATA)
    fprintf(d_fd, "%s",dp->rows[i].line);
  
  /* Here we assign data to our calc structure, if appropriate */
  if (c_data)
    {
      if (dm->d_type == INT)
	{
	  printf("%s = %d\n", dp->rows[i].key, atoi(DifxParametersvalue(dp, i)));
	  *(int *)c_data = atoi(DifxParametersvalue(dp, i));
	}
      if (dm->d_type == FLOAT)
	{
	  printf("%s = %f\n", dp->rows[i].key, atof(DifxParametersvalue(dp, i)));
	  *(double *)c_data = atof(DifxParametersvalue(dp, i));
	}
      if (dm->d_type == STRING)
	{
	  strcpy(c_data, DifxParametersvalue(dp, i));
	}
    }
  return(0);
}

/*
 * Here we copy the information we've parsed from the .calc file
 * into the data structure used by CALC.  Finally, invoke RPC call
 * and (hopefully) get our delays back from the CALC server 
 */ 
static getCALC_res *getCalcData(TS_CALC_PARAMS c_params, 
				double time_incr, 
				int telescope, 
				CLIENT *cl)
{

  char   stnnamea[8]; 
  char   axistypea[6];
  char   stnnameb[8];
  char   axistypeb[6];
  int i;
  double mjd_date;
  double mjd;
  struct getCALC_arg request_args;
  struct getCALC_res *p_result;

  /* init calc request structure */
  memset(&request_args,0,sizeof(getCALC_arg));

  request_args.request_id = 150;
  for (i = 0; i < 64; i++)
    request_args.kflags[i] = -1;
  request_args.ref_frame = 0;
  mjd = c_params.mjd;
  mjd += time_incr;
  request_args.time = modf(mjd, &mjd_date);
  request_args.date = (int)mjd_date;
  /* Fill in EOP information */
  for (i=0; i<MAX_EOPS; i++)
    {
      request_args.EOP_time[i] = c_params.e_time[i]; 
      request_args.tai_utc[i] = c_params.e_tai_utc[i];
      request_args.ut1_utc[i] = c_params.e_ut1_utc[i];
      request_args.xpole[i]   = c_params.e_xpole[i];
      request_args.ypole[i]   = c_params.e_ypole[i];
    }
  
  /* source information */
  request_args.source = c_params.source_name;
  request_args.ra  =  c_params.r_ascension;
  request_args.dec =  c_params.declination;
  
  request_args.dra  = 0.0;
  request_args.ddec = 0.0;
  request_args.depoch = 0.0;
  request_args.parallax = 0.0;
  
  request_args.pressure_a = 0.0;
  request_args.pressure_b = 0.0;
  
  /* reference telescope information*/
  strcpy (stnnamea, "EC");
  request_args.station_a = stnnamea;
  request_args.a_x =  0.000;
  request_args.a_y =  0.000;
  request_args.a_z =  0.000;
  strcpy (axistypea, MOUNT_TYPE);
  request_args.axis_type_a = axistypea;
  request_args.axis_off_a = 0.00;
  /* Telescope information */
  strcpy(stnnameb, 
	  c_params.t_name[telescope]);
  request_args.station_b = stnnameb;   /* Telescope name */
  request_args.b_x =  c_params.t_x[telescope];     /* geocentric coordinates */
  request_args.b_y =  c_params.t_y[telescope];
  request_args.b_z =  c_params.t_z[telescope];
  strcpy (axistypeb, MOUNT_TYPE);          
  request_args.axis_type_b = axistypeb;/* mount type */
  request_args.axis_off_b = c_params.t_offset[telescope];   /* mount offset */
	  
  p_result = getcalc_1(&request_args, cl);
  
  return (p_result);
}

/* The RPC call which invokes CALC server */
getCALC_res *
getcalc_1(argp, clnt)
	struct getCALC_arg *argp;
	CLIENT *clnt;
{
	static getCALC_res clnt_res;
        enum clnt_stat clnt_stat;
	memset((char *)&clnt_res, 0, sizeof (clnt_res));
	clnt_stat = clnt_call(clnt, GETCALC,
		(xdrproc_t) xdr_getCALC_arg, (caddr_t) argp,
		(xdrproc_t) xdr_getCALC_res, (caddr_t) &clnt_res,
		TIMEOUT);

        if (clnt_stat != RPC_SUCCESS)
           printf ("clnt_call failed\n");

	return (&clnt_res);
}

