					/* Various plotting mode definitions */
#define Y_SINGLE 	1
#define Y_MULTIPLE	2
#define SOURCE_NOSPLIT	0
#define SOURCE_SPLIT	01
					/* Plotted point quality indicators */
#define PQ_GOOD		0
#define PQ_SUSPECT	1
#define PQ_BAD		2

#define STATION_PLOT	01		/* Identified families of axes below */
#define BASELINE_PLOT	02
#define TRIANGLE_PLOT	04
#define QUAD_PLOT	010
#define ALL_PLOT	020
#define ANY_PLOT	037
					/* Used for station-based quantities */
					/* in baseline-based plots */
#define REFERENCE	0
#define REMOTE		1
					/* Axis IDs, with default plot mode */
#define NO_AXIS		-1

#define AX_TIMETAG	0		/* By any */

#define AX_PCAL_PHASE	100		/* By station (1st 4 extracted params only, */
#define AX_PCAL_DIFF	101		/* ref and remote in aux input variable) */
#define AX_PCAL_AMP	102
#define AX_ERRORATE	103
#define AX_ELEVATION	104
#define AX_AZIMUTH	105

#define AX_SNR		201		/* By baseline */
#define AX_AMPLITUDE	202
#define AX_PHASE	203
#define AX_SBDELAY	204
#define AX_MBDELAY	205
#define AX_DRATE	206
#define AX_NCOTIME	207
#define AX_SCOTIME	208

#define AX_CPHASE	300		/* By triangle */
#define AX_CRATE	301
#define AX_CSBDELAY	302
#define AX_CMBDELAY	303

#define AX_CAMP		400		/* By quad */

#define AX_U		500		/* By all */
#define AX_V		501
#define AX_UVDIST	502

#define AX_PARAMETER	600		/* parameter number in aux input variable */

/* was 2000, boosted to 20000 Nov 11, 2016 */
#define MAXPLT 20000			/* Max # of points in single plot, */
					/* applies separately to the 3 qualities */
					/* of plotted points */
#define NSYMBOL 11

struct plot_info 
    {
    int		npts;			/* Total number of points plotted */
    int		ngood;			/* # of good points in this plot */
    int		nsusp;			/* # of suspect points in this plot */
    int		nbad;			/* # of bad points in this plot */
    int		nbadscale;		/* # of points off scale, omitted */
    int		xebar;			/* Are X error bars plotted? */
    int		yebar;			/* Are Y error bars plotted? */
    int		onscreen;		/* Visible on interactive screen? */
    int 	plotby;			/* By station, baseline, whatever? */
    int		xaind;			/* Variable plotted on X axis */
    int		x_aux;			/* Auxiliary field for parms */
    char	xtype[30];		/* Variable plotted on X axis */
    int		yaind;			/* Variable plotted on Y axis */
    int		y_aux;			/* Auxiliary field for parms */
    char	ytype[30];		/* Variable plotted on Y axis */
    float	xmax;			/* X-axis value extrema */
    float	xmin;
    float	ymax;			/* Y-axis value extrema */
    float	ymin;
    int		toffset;		/* For time axes .. avoids f.p. roundoff */
    float	vport[4];		/* Viewport in normalized device coords */
    float	window[4];		/* World coordinate extrema of viewport */
    short	index[MAXPLT];		/* Indices of all points in data arrays */
    char	symbol[MAXPLT];		/* Point-by-point PGPLOT symbol used */
    char 	frq;			/* Frequency code plotted */
    int		expt;			/* Experiment number plotted */
    char	source[32];		/* Source name (if applicable) */
    char	station;		/* Station plotted */
    char	bas[3];			/* Baseline plotted */
    char	triangle[4];		/* Stations in closure triangle */
    char	ampcl[5];		/* Stations in closure amplitude quad */
    };

struct plot_ptqual 
    {
    float	x[MAXPLT];		/* X-axis values */
    float	y[MAXPLT];		/* Y-axis values */
    float	xerrh[MAXPLT];		/* upper extent of X error bar */
    float	xerrl[MAXPLT];		/* lower extent of X error bar */
    float	yerrh[MAXPLT];		/* upper extent of Y error bar */
    float	yerrl[MAXPLT];		/* lower extent of Y error bar */
    };

struct plot_points 
    {
    struct plot_ptqual	good;		/* Contains info for good points */
    struct plot_ptqual	suspect;	/* Contains info for suspect points */
    struct plot_ptqual	bad;		/* Contains info for bad points */
    };

