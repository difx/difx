#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <fcntl.h>
#include <rpc/rpc.h>
#include <glob.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "MATHCNST.H"
#include "CALCServer.h"
#include "difxio/parsedifx.h"
#include "config.h"

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
#define SCAN_REAL_NAME   "SCAN %d REAL NAME"
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
#define NUM_SPACECRAFT   "NUM SPACECRAFT"
#define SPACECRAFT_NAME  "SPACECRAFT %d NAME"
#define SPACECRAFT_ROWS  "SPACECRAFT %d ROWS"
#define SPACECRAFT_DATA  "SPACECRAFT %d ROW %d"

#define CALC_DATA 0x0
#define UVW_DATA  0x1
#define DELAY_DATA 0x2
#define RATE_DATA 0x4

#define MAX_BUF_SIZE 8192

#define VALUE_OFFSET 20  /* Value is always 21 characters from beginning of line */

#define MAX_TELESCOPES 10
#define MAX_SCANS 100
#define MAX_EOPS 5
#define MAX_SPACECRAFT 10
#define MAX_SPACECRAFT_ROWS 10000

#define T_NAME_SIZE 32 /* max telescope name size+1 */
#define T_MNT_SIZE 32  /* max telsccope mount size+1 */
#define SC_NAME_SIZE 32
#define SRC_NAME_SIZE 128

#define U_FNAME_SIZE 64
#define D_FNAME_SIZE 64
#define I_FNAME_SIZE 64
#define R_FNAME_SIZE 64

#define MOUNT_TYPE "altz"

char CALC_SERVER[128] = "localhost";

int verbose = 0;

typedef struct
{
	int mjd;
	long double fracDay;
	long double X, Y, Z, dX, dY, dZ;
} SpacecraftPos;

typedef enum
{
  STRING = 1,
  INT,
  FLOAT,
  SPACECRAFT_POS
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
  S_REAL_NAME,
  S_SRC_RA,   
  S_SRC_DEC, 
  GET_CALC,
  N_SC,
  SC_NAME,
  SC_ROWS,
  SC_DATA,
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
  char   real_name[SRC_NAME_SIZE];
  double t_offset[MAX_TELESCOPES];
  double t_x[MAX_TELESCOPES];
  double t_y[MAX_TELESCOPES]; 
  double t_z[MAX_TELESCOPES]; /* x,y,z coordinate*/
  double e_time[MAX_EOPS]; 
  double e_tai_utc[MAX_EOPS];
  double e_ut1_utc[MAX_EOPS];
  double e_xpole[MAX_EOPS];
  double e_ypole[MAX_EOPS];
  int nSpacecraft;
  SpacecraftPos sc_pos[MAX_SPACECRAFT][MAX_SPACECRAFT_ROWS];
  char   sc_name[MAX_SPACECRAFT][SC_NAME_SIZE];
  int    sc_rows[MAX_SPACECRAFT];
  
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
static int processFile(const char *i_fname, int doforce);

const char program[] = PACKAGE_NAME;
const char version[] = VERSION;
const char author[]  = "Pete Whiteis / Walter Brisken";

static int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n", 
		program, version, author);
	fprintf(stderr, "A program to calculate a model for DiFX using a calc "
		"server.\n\n");
	fprintf(stderr, "Usage : %s [options] { <calc file> | -a }\n\n", pgm);
	fprintf(stderr, "<calc file> should be a '.calc' file as generated by "
		"job2difx.\n\n");
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h                      Print this help and quit\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v                      Be more verbose in operation\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --force\n");
	fprintf(stderr, "  -f                      Force recalc\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --all\n");
	fprintf(stderr, "  -a                      Do all calc files found\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --yes\n");
	fprintf(stderr, "  -y                      Really run calcif? \n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  --server <servername>\n");
	fprintf(stderr, "  -s       <servername>   Use <servername> as calcserver\n\n");
	fprintf(stderr, "      By default 'localhost' will be the calcserver.  An "
		"environment\n");
	fprintf(stderr, "      variable CALC_SERVER can be used to override that.  The "
		"command line\n");
	fprintf(stderr, "      overrides all.\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "NOTE: You should be using calcif2, unless you have a "
		"particular reason\nto use calcif.  Please run with -y if you want "
		"to run calcif\n");
	fprintf(stderr, "\n");

	return 0;
}

int main (int argc, char *argv[])
{
  char *serv = 0;
  int i;
  int doall = 0;
  int doforce = 0;
  char *i_fname[2048];
  int nfiles = 0;
  int skipped;
  int nskipped = 0;
  glob_t globbuf;
  int yes = 0;
  
  serv = getenv("CALC_SERVER");
  if(serv)
  {
  	strcpy(CALC_SERVER, serv);
  }

  if(getenv("DIFX_GROUP_ID"))
  {
        umask(2);
  }

  for(i = 1; i < argc; i++)
  {
  	if(argv[i][0] == '-')
	{
		if(strcmp(argv[i], "--help") == 0 ||
		   strcmp(argv[i], "-h") == 0)
		{
			return usage(argv[0]);
		}
		if(strcmp(argv[i], "--verbose") == 0 ||
		   strcmp(argv[i], "-v") == 0)
		{
			verbose++;
		}
		if(strcmp(argv[i], "--yes") == 0 ||
		   strcmp(argv[i], "-y") == 0)
		{
			yes = 1;
		}
		if(strcmp(argv[i], "--all") == 0 ||
		   strcmp(argv[i], "-a") == 0)
		{
			doall = 1;
		}
		if(strcmp(argv[i], "--force") == 0 ||
		   strcmp(argv[i], "-f") == 0)
		{
			doforce = 1;
		}
		else if(i+1 < argc)
		{
			if(strcmp(argv[i], "--server") == 0 ||
			   strcmp(argv[i], "-s") == 0)
			{
				i++;
				strcpy(CALC_SERVER, argv[i]);
			}
		}
	}
	else 
	{
		i_fname[nfiles] = argv[i];
		nfiles++;
	}
  }

  if(!yes)
  {
  	usage(argv[0]);
  	printf("Will not run without -y or --yes\n\n");
	return 0;
  }

  if((nfiles == 0 && doall == 0) || (nfiles > 0 && doall > 0))
  {
  	return usage(argv[0]);
  }
  
  printf("Using calcserver : %s\n", CALC_SERVER);

  if(nfiles > 0)
  {
  	for(i = 0; i < nfiles; i++)
	{
  		skipped = processFile(i_fname[i], doforce);
		if(skipped)
		{
			printf("Skipped file %s .  Use --force if necessary.\n",
				i_fname[i]);
			nskipped++;
		}
	}
  }
  else
  {
  	glob("*.calc", 0, 0, &globbuf);
	nfiles = globbuf.gl_pathc;
	if(nfiles <= 0)
	{
		printf("no .calc files found.  Did you run job2difx yet?\n\n");
	}
	for(i = 0; i < nfiles; i++)
	{
		skipped = processFile(globbuf.gl_pathv[i], doforce);
		if(skipped)
		{
			printf("Skipped file %s .  Use --force if necessary.\n",
				globbuf.gl_pathv[i]);
			nskipped++;
		}
	}
	globfree(&globbuf);
  }

  if(nskipped > 0)
  {
  	printf("\nWarning: %d files were skipped due to file modification times.\n", nskipped);
  }

  return 0;
}

/* return 1 if f2 exists and is older than f1 */
int skipFile(const char *f1, const char *f2)
{
	struct stat s1, s2;
	int r1, r2;

	r2 = stat(f2, &s2);
	if(r2 != 0)
	{
		return 0;
	}
	r1 = stat(f1, &s1);
	if(r1 != 0)
	{
		return 0;
	}

	if(s2.st_mtime > s1.st_mtime)
	{
		return 1;
	}

	return 0;
}

static int processFile(const char *i_fname, int doforce)
{
  FILE *u_fd, *d_fd, *r_fd; /* uvw, delay, rate file descriptors */
  int i;
  int call_cnt;
  int state;
  int entry;
  int num_telescopes; 
  int num_scans; 
  int num_eops; 
  int num_spacecraft;
  int num_points;
  int which_telescope=0;
  int which_scan=0;
  int which_eop=0;
  int which_spacecraft=0;
  int which_sc_point=0;

  char uvw_tname[U_FNAME_SIZE], 
    delay_tname[D_FNAME_SIZE], 
    rate_tname[R_FNAME_SIZE];
  char uvw_fname[U_FNAME_SIZE], 
    delay_fname[D_FNAME_SIZE], 
    rate_fname[R_FNAME_SIZE];
  char cmd[1024];
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
      {SCAN_REAL_NAME,  S_REAL_NAME, NULL,STRING, CALC_DATA},
      {SCAN_SRC_RA,     S_SRC_RA,  NULL,FLOAT, UVW_DATA|RATE_DATA},
      {SCAN_SRC_DEC,    S_SRC_DEC, NULL,FLOAT, UVW_DATA|RATE_DATA},
      {NUM_EOP,         N_EOP,     NULL,INT, CALC_DATA},
      {EOP_TIME,        E_TIME,    NULL,FLOAT, CALC_DATA},
      {EOP_TAI_UTC,     E_TAI_UTC, NULL,FLOAT, CALC_DATA},
      {EOP_UT1_UTC,     E_UT1_UTC, NULL,FLOAT, CALC_DATA},
      {EOP_XPOLE,       E_XPOLE,   NULL,  FLOAT, CALC_DATA},
      {EOP_YPOLE,       E_YPOLE,   NULL,  FLOAT, CALC_DATA},
      {NUM_SPACECRAFT,  N_SC,      NULL, INT, CALC_DATA},
      {SPACECRAFT_NAME, SC_NAME,   NULL, STRING, CALC_DATA},
      {SPACECRAFT_ROWS, SC_ROWS,   NULL, INT, CALC_DATA},
      {SPACECRAFT_DATA, SC_DATA,   NULL, SPACECRAFT_POS, CALC_DATA}
    };
  int num_dmap_entries = sizeof(Data_Map)/sizeof(TS_DATA_MAP);
  /* load the input parameter from file */
  dp = newDifxParametersfromfile(i_fname);
  
  /* Before we do anything else, we'll need the file names. If not found, 
     do not pass go, do not collect $200 */
  /* uvw file */
  i = DifxParametersfind(dp, 0, UVW_FILENAME);
  if (i >= 0)
    {
      strcpy(uvw_fname, DifxParametersvalue(dp, i));
      sprintf(uvw_tname, "/tmp/calcif-%s", uvw_fname);
      printf("%s = %s\n", dp->rows[i].key, uvw_fname);
      /* Make rate filename from uvw file */
      strcpy(rate_fname, uvw_fname);
      *strstr(rate_fname, ".uvw") = 0x00;
      strcat(rate_fname, ".rate");
      strcpy(rate_tname, uvw_tname);
      *strstr(rate_tname, ".uvw") = 0x00;
      strcat(rate_tname, ".rate");
      printf("RATE FILENAME = %s\n", rate_fname);
    }
  /*delay file */
  i = DifxParametersfind(dp, 0, DELAY_FILENAME);
  if (i >= 0)
    {
      strcpy(delay_fname, DifxParametersvalue(dp, i));
      sprintf(delay_tname, "/tmp/calcif-%s", delay_fname);
      printf("%s = %s\n", dp->rows[i].key, delay_fname);
    }

  if(doforce == 0 &&
     skipFile(i_fname, uvw_fname) && 
     skipFile(i_fname, delay_fname) &&
     skipFile(i_fname, rate_fname))
  {
    deleteDifxParameters(dp);
    return 1;
  }
  
  /* erase any existing file to reduce possible confusion */
  /* new files will be constructed in /tmp and moved when finished */
  sprintf(cmd, "rm -f %s", uvw_fname);
  system(cmd);
  sprintf(cmd, "rm -f %s", delay_fname);
  system(cmd);
  sprintf(cmd, "rm -f %s", rate_fname);
  system(cmd);

  u_fd = fopen(uvw_tname, "w");
  if (!u_fd)
    {
      perror("Calcif-E-cannot open uvw file: \n");
      exit(1);
    }
  d_fd = fopen(delay_tname, "w");
  if (!d_fd)
    {
      perror("Calcif-E-cannot open delay file: \n");
      exit(1);
    }
  r_fd = fopen(rate_tname, "w");
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
  memset(&Calc_Params, 0, sizeof(Calc_Params));
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
	  if(verbose) printf("MJD\n");
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
	  if(verbose) printf("Year\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with Year processing \n");
	      state = NOOP;
	      break;
	    }
	  state = MONTH;
	  break;
	case MONTH:
	  if(verbose) printf("Month\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with month processing \n");
	      state = NOOP;
	      break;
	    }
	  state = DAY;
	  break;
	case DAY:
	  if(verbose) printf("Day\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with day processing \n");
	      state = NOOP;
	      break;
	    }
	  state = HOUR;
	  break;
	case HOUR:
	  if(verbose) printf("Hour\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with hour processing \n");
	      state = NOOP;
	      break;
	    }
	  state = MINUTE;
	  break;
	case MINUTE:
	  if(verbose) printf("Minute\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with minute processing \n");
	      state = NOOP;
	      break;
	    }
	  state = SECOND;
	  break;
	case SECOND:
	  if(verbose) printf("Second\n");
	  if(processStateData(&Data_Map[entry], dp, (void *)NULL, u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with second processing \n");
	      state = NOOP;
	      break;
	    }
	  state = INCR;
	  break;
	case INCR:
	  if(verbose) printf("Increment\n");
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
	  if(verbose) printf("Num Telescopes\n");
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
	  if(verbose) printf("Telescope name\n");
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
	  if(verbose) printf("Telescope Mount\n");
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
	  if(verbose) printf("Telescope Offset\n");
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
	  if(verbose) printf("Telescope X\n");
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
	  if(verbose) printf("Telescope Y\n");
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
	  if(verbose) printf("Telescope Z\n");
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
	  if(verbose) printf("Num EOPs\n");
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
	  if(verbose) printf("Eop Time\n");
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
	  if(verbose) printf("Eop tai utc\n");
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
	  if(verbose) printf("Eop ut1 utc\n");
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
	  if(verbose) printf("Eop xpole\n");
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
	  if(verbose) printf("Eop ypole\n");
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
	    state = N_SC;
	  break;
	case N_SC:
	  if(verbose) printf("Num Spacecraft\n");
	  if(processStateData(&Data_Map[entry], dp, 
			      (void *)&num_spacecraft, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-notice : no spacecraft data found\n");
	      num_spacecraft = 0;
	      Calc_Params.nSpacecraft = 0;
	      state = N_SCANS;
	      break;
	    }
	  which_spacecraft = 0;
	  if(num_spacecraft <= 0)
	  {
	    state = N_SCANS;
	    break;
	  }
	  if(num_spacecraft > MAX_SPACECRAFT)
	  {
	    printf("calcif-E-problem : too many spacecraft\n");
	    state = NOOP;
	    break;
	  }
	  Calc_Params.nSpacecraft = num_spacecraft;
	  state = SC_NAME;
	  break;
	case SC_NAME:
	  if(verbose) printf("Spacecraft Name\n");
	  sprintf(Data_Map[entry].tag, SPACECRAFT_NAME, which_spacecraft); 
	  if(processStateData(&Data_Map[entry], dp, 
			      Calc_Params.sc_name[which_spacecraft], 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with spacecraft_name processing\n");
	      state = NOOP;
	      break;
	    }
	  state = SC_ROWS;
	  break;
	case SC_ROWS:
	  if(verbose) printf("Spacecraft Rows\n");
	  sprintf(Data_Map[entry].tag, SPACECRAFT_ROWS, which_spacecraft); 
	  if(processStateData(&Data_Map[entry], dp,
	  		      &(Calc_Params.sc_rows[which_spacecraft]),
	  		      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with spacecraft_rows processing\n");
	      state = NOOP;
	      break;
	    }
	  which_sc_point = 0;
	  state = SC_DATA;
	  break;
	case SC_DATA:
	  sprintf(Data_Map[entry].tag, SPACECRAFT_DATA, which_spacecraft, which_sc_point); 
	  if(processStateData(&Data_Map[entry], dp,
	  		      &(Calc_Params.sc_pos[which_spacecraft][which_sc_point]),
	  		      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with spacecraft_data processing\n");
	      state = NOOP;
	      break;
	    }
	  which_sc_point++;
	  if(which_sc_point >= Calc_Params.sc_rows[which_spacecraft])
	  {
	    which_sc_point = 0;
	    which_spacecraft++;
	    if(which_spacecraft >= num_spacecraft)
	      state = N_SCANS;
	    else
	      state = SC_NAME;
	  }
	  break;
	case N_SCANS:
	  if(verbose) printf("Num Scans\n");
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
	  if(verbose) printf("Scan Points\n");
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
	  if(verbose) printf("Scan Start Point\n");
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
	  if(verbose) printf("Source Name\n");
	  sprintf(Data_Map[entry].tag, SCAN_SRC_NAME, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      Calc_Params.source_name, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-problem with source name \n");
	      state = NOOP;
	      break;
	    }
	  state = S_REAL_NAME;
	  break;
	case S_REAL_NAME:
	  if(verbose) printf("Real Name\n");
	  sprintf(Data_Map[entry].tag, SCAN_REAL_NAME, which_scan); 
	  if(processStateData(&Data_Map[entry], dp, 
			      Calc_Params.real_name, 
			      u_fd, r_fd ,d_fd))
	    {
	      printf("calcif-E-warning no real name, assuming same as src name\n");
	      strcpy(Calc_Params.real_name, Calc_Params.source_name);
	    }
	  state = S_SRC_RA;
	  break;
	case S_SRC_RA:
	  if(verbose) printf("Right Ascension \n");
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
	  if(verbose) printf("Declination \n");
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
		  if (++call_cnt%8==0 && verbose)
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
  fclose(r_fd);
  
  clnt_destroy (cl);

  sprintf(cmd, "mv %s %s", uvw_tname, uvw_fname);
  system(cmd);
  sprintf(cmd, "mv %s %s", delay_tname, delay_fname);
  system(cmd);
  sprintf(cmd, "mv %s %s", rate_tname, rate_fname);
  system(cmd);

  deleteDifxParameters(dp);
  
  return 0;
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
	  if(verbose)
	  {
	    printf("%s = %d\n", dp->rows[i].key, atoi(DifxParametersvalue(dp, i)));
	  }
	  *(int *)c_data = atoi(DifxParametersvalue(dp, i));
	}
      if (dm->d_type == FLOAT)
	{
	  if(verbose)
	  {
	    printf("%s = %f\n", dp->rows[i].key, atof(DifxParametersvalue(dp, i)));
	  }
	  *(double *)c_data = atof(DifxParametersvalue(dp, i));
	}
      if (dm->d_type == STRING)
	{
	  strcpy(c_data, DifxParametersvalue(dp, i));
	}
      if (dm->d_type == SPACECRAFT_POS)
        {
		const char *str;
		int n;
		double time;
		SpacecraftPos *sp;

		sp = (SpacecraftPos *)c_data;
		str = DifxParametersvalue(dp, i);
		n = sscanf(str, "%lf%Lf%Lf%Lf%Lf%Lf%Lf",
			&time,
			&(sp->X),  &(sp->Y),  &(sp->Z),
			&(sp->dX), &(sp->dY), &(sp->dZ));
		sp->mjd = (int)time;
		time -= sp->mjd;
		sp->fracDay = ((int)(time*86400.0 + 0.5))/86400.0L;

		/* convert rates from m/s to m/day for local calculations */
		sp->dX *= 86400.0L;
		sp->dY *= 86400.0L;
		sp->dZ *= 86400.0L;
	}
    }
  return(0);
}

static void evalPoly(long double poly[4], long double t, long double *V, long double *dV)
{
	*V = poly[0] + t*(poly[1] + t*(poly[2] + t*poly[3]));
	*dV = poly[1] + t*(2.0L*poly[2] + 3.0L*t*poly[3]);
}

int calcSpacecraftPosition(const TS_CALC_PARAMS *c_params,
	struct getCALC_arg *request_args, int spacecraftId)
{
	int nRow;
	const SpacecraftPos *pos;
	long double t0, t1, tMod, t, deltat;
	long double xPoly[4], yPoly[4], zPoly[4];
	int r, r0, r1;
	long double X, Y, Z, dX, dY, dZ;
	long double R2, D;
	double muRA, muDec;
	
	nRow = c_params->sc_rows[spacecraftId];
	pos = &(c_params->sc_pos[spacecraftId][0]);
	
	tMod = request_args->date + request_args->time;
	
	/* first find interpolation points */
	t0 = 0.0;
	t1 = pos[0].mjd + pos[0].fracDay;
	for(r = 1; r < nRow; r++)
	{
		t0 = t1;
		t1 = pos[r].mjd + pos[r].fracDay;
		if(t0 <= tMod && tMod <= t1)
		{
			break;
		}
	}
	if(r == nRow)
	{
		return -1;
	}

	/* calculate polynomial for X, Y, Z */
	r0 = r-1;
	r1 = r;
	deltat = t1 - t0;
	t = (tMod - t0)/deltat; /* time, fraction of interval, between 0 and 1 */

	xPoly[0] = pos[r0].X;
	xPoly[1] = pos[r0].dX*deltat;
	xPoly[2] = -3.0L*(pos[r0].X-pos[r1].X) - (2.0L*pos[r0].dX+pos[r1].dX)*deltat;
	xPoly[3] =  2.0L*(pos[r0].X-pos[r1].X) + (    pos[r0].dX+pos[r1].dX)*deltat;
	yPoly[0] = pos[r0].Y;
	yPoly[1] = pos[r0].dY*deltat;
	yPoly[2] = -3.0L*(pos[r0].Y-pos[r1].Y) - (2.0L*pos[r0].dY+pos[r1].dY)*deltat;
	yPoly[3] =  2.0L*(pos[r0].Y-pos[r1].Y) + (    pos[r0].dY+pos[r1].dY)*deltat;
	zPoly[0] = pos[r0].Z;
	zPoly[1] = pos[r0].dZ*deltat;
	zPoly[2] = -3.0L*(pos[r0].Z-pos[r1].Z) - (2.0L*pos[r0].dZ+pos[r1].dZ)*deltat;
	zPoly[3] =  2.0L*(pos[r0].Z-pos[r1].Z) + (    pos[r0].dZ+pos[r1].dZ)*deltat;

	evalPoly(xPoly, t, &X, &dX);
	evalPoly(yPoly, t, &Y, &dY);
	evalPoly(zPoly, t, &Z, &dZ);

	/* convert to m/day */
	dX /= deltat;
	dY /= deltat;
	dZ /= deltat;

	/* Hack here -- this makes much smoother output! */
	dX = pos[r0].dX + t*(pos[r1].dX - pos[r0].dX);
	dY = pos[r0].dY + t*(pos[r1].dY - pos[r0].dY);
	dZ = pos[r0].dZ + t*(pos[r1].dZ - pos[r0].dZ);

	D = sqrtl(X*X + Y*Y + Z*Z);
	R2 = X*X + Y*Y;

	/* proper motion in radians/day */
	muRA = (X*dY - Y*dX)/R2;
	muDec = (R2*dZ - X*Z*dX - Y*Z*dY)/(D*D*sqrtl(R2));
	
	/* convert to arcsec/yr */
	muRA *= (180.0*3600.0/M_PI)*365.24;
	muDec *= (180.0*3600.0/M_PI)*365.24;

	request_args->ra  =  atan2(Y, X);
	request_args->dec =  atan2(Z, sqrtl(R2));
	request_args->dra  = muRA;
	request_args->ddec = muDec;
	request_args->parallax = 3.08568025e16/D;
        request_args->depoch = tMod;

	return 0;
}

int getSpacecraftId(const TS_CALC_PARAMS *c_params, const char *src_name, const char *real_name)
{
	int s;

	if(c_params->nSpacecraft <= 0)
	{
		return -1;
	}

	for(s = 0; s < c_params->nSpacecraft; s++)
	{
		if(strcmp(src_name, c_params->sc_name[s]) == 0 ||
		   strcmp(real_name, c_params->sc_name[s]) == 0)
		{
			return s;
		}
	}
	
	return -1;
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
  int i, t;
  int spacecraftId;
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

  /* WARNING HACK HERE -- this is to force the "time" portion to be an integer number of seconds */
  t = (int)(request_args.time*86400.0 + 0.5);
  request_args.time = t/86400.0;
  
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
  spacecraftId = getSpacecraftId(&c_params, c_params.source_name, c_params.real_name);
  
  if(spacecraftId < 0)
  {
    request_args.ra  =  c_params.r_ascension;
    request_args.dec =  c_params.declination;
    request_args.dra  = 0.0;
    request_args.ddec = 0.0;
    request_args.parallax = 0.0;
    request_args.depoch = 0.0;
  }
  else
  {
    calcSpacecraftPosition(&c_params, &request_args, spacecraftId);
  }
  
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

