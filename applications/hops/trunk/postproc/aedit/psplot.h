#define TRUE 1
#define FALSE 0

#include "sizelimits.h"
					/* Page layout definitions */
#define MIN_BASE_PLOT  20
#define MAX_BASE_PLOT  60
#define MIN_SCAN_PLOT  35
#define MAX_SCAN_PLOT  70
					/* Colour definitions */
#define  UNPROC     19
#define  MINUS      20
#define  TEXT       21
#define  ACTIVE     22
#define  INACTIVE   23
#define  TAG_COLOUR 24
					/* Button coordinates */
#define  PREV_BUTTON       140., 280., 960., 998.
#define  NEXT_BUTTON       320., 460., 960., 998.
#define  SELECT_BUTTON     500., 640., 960., 998.
#define  FPLOT_BUTTON      680., 820., 960., 998.
#define  QUIT_BUTTON       860., 998., 960., 998.
#define  RETAIN_BUTTON     0., 75., 900., 940.
#define  BAND_XORIGIN      0.
#define  BAND_YORIGIN      850.
#define  BAND_SIZE         40.
					/* Button return values */
#define   PREV    -1
#define   NEXT    -2
#define   SELECT  -3
#define   FPLOT   -4
#define   QUIT    -5
#define   RETAIN  -6
#define   BLAB    -7
#define   SLAB    -8
#define   KEY     -9
#define   DATA_PT -10
#define   NODATA  -11
#define   NOTHING -12
#define   BAND    -100
					/* Plotting area coordinates */
#define  PLOT_XMIN  140. 
#define  PLOT_XMAX  1000. 
#define  PLOT_YMIN  100. 
#define  PLOT_YMAX  820. 
					/* Label areas */
#define  SCANLABEL   140., 1000., 822., 958.
#define  BASELABEL   110., 138., 100., 820.
					/* Info display area */
#define  INFO     65., 200., 10., 70.
#define  INFO_X   65.
#define  INFO_Y   10.

#define  GUARD_BAND 2.0
					/* Standardized quality codes */
#define  QUALITIES " 0123456789ABCDEFGH.-"
#define  KEY_XMIN  400.
#define  KEY_YMIN  50.
#define  KEYINC    20.


struct psplot_cell
    {
    int data_index[MAXBANDS];
    int colour_index[MAXBANDS];
    int flag[MAXBANDS];
    };

struct psplot_baseline
    {
    char id[3];
    int alloced;
    int tagged[MAXBANDS];
    struct psplot_cell  *scan;
    };

struct psplot_station
    {
    char stn;
    char fglist[MAXBANDS];
    int minus;                  /* Boolean, but could contain reason code */
    };

struct psplot_scantime
    {
    int scantime;
    int seconds;
    int tagged[MAXBANDS];
    char scan_name[32];
    struct psplot_station stations[MAXSTEXP];
    int nst;
    };

struct psplot_plotparam
    {
    int npages;
    int nbasepage;
    int scanpage;
    int basepage;
    int scans_per_page;
    int base_per_page;
    int band;
    float base_sep;
    float scan_sep;
    float tagsize;
    float xgb_fract;
    float ygb_fract;
    };

struct psplot_fgroup
    {
    char fgroup[3];
    char subgroups[MAXBANDS];
    };

struct ps_array
    {
    int nbaselines;
    int nscans;
    int displayed;
    int fplot_open;
    int ntagged;
    int qtagged[17];
    int retain;
    struct psplot_fgroup fglist[MAXGROUPS];
    char subgroups[MAXBANDS];
    char stnlist[MAXSTEXP];
    struct psplot_scantime *time;
    struct psplot_plotparam param;
    struct psplot_baseline baseline[MAXBASE];
    };

struct ps_count
    {
    char baseline[3];
    char band;
    int nqual[22];
    };
