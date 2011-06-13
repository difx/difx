#ifndef usearray_done			/* Allow multiple includes */
#define usearray_done

#define MAX_PARMS 32
					/* Extractable parameters */
#define         REF_PCAL_AMP    1
#define         REF_PCAL_PHASE  2
#define         REF_PCAL_DIFF   3
#define         REF_PCAL_FREQ   4
#define         REF_PCAL_RATE   5
#define         REM_PCAL_AMP    6
#define         REM_PCAL_PHASE  7
#define         REM_PCAL_DIFF   8
#define         REM_PCAL_FREQ   9
#define         REM_PCAL_RATE   10
#define         TRKNO_REF_USB   11
#define         TRKNO_REF_LSB   12
#define         TRKNO_REM_USB   13
#define         TRKNO_REM_LSB   14
#define         ERRATE_REF_USB  15
#define         ERRATE_REF_LSB  16
#define         ERRATE_REM_USB  17
#define         ERRATE_REM_LSB  18
#define         NAP_USB         19
#define         NAP_LSB         20
#define         COREL_AMP       21
#define         COREL_PHASE     22
/* #define         OBS_DELAY_RATE  0  */
#define         RATE_ERROR      23
#define         MBDELAY_ERROR   24
#define         SBDELAY_ERROR   25
#define         TOTAL_PHASE     26
#define         TOT_PHASE_MID   27
#define         INCOH_AMP_SEG   28
#define         INCOH_AMP_FREQ  29
/***********  Redundant with A-file format version >= 2
  #define         REF_ELEVATION   0
  #define         REM_ELEVATION   0
  #define         FR_ARCSEC_NS    0
  #define         FR_ARCSEC_EW    0
***********/
#define         MHZ_ARCSEC_NS   30
#define         MHZ_ARCSEC_EW   31
#define         PCNT_DISCARD    32
#define         MIN_MAX_RATIO   33
/***********  Redundant with A-file format version >= 2
  #define         REF_FREQUENCY   0
***********/
#define         LO_FREQUENCY    34
#define         XPERROR         35
#define         YPERROR         36
#define         SUPPRESS        37
#define         PPUPDATE        38
#define         XSLIP           39
#define         YSLIP           40
#define         BADSYNC         41
#define		REF_DRIVE	42
#define		REM_DRIVE	43
#define		UTC_CENTRAL	44
#define		UTC_EPOCH	45
#define		CLOCK_DELAY	46
#define 	PROB_FALSE	47

#define		IN_ALINE	50	/* Threshold value */
					/* Numeric parameters already in A-line */
#define		LENGTH		51
#define		TIMETAG		52
#define		AMPLITUDE	53
#define		SNR		54
#define		PHASE		55
#define		RESID_SBD	56
#define		RESID_MBD	57
#define		AMBIGUITY	58
#define		RESID_RATE	59
#define		REF_ELEVATION	60
#define		REM_ELEVATION	61
#define		REF_AZIMUTH	62
#define		REM_AZIMUTH	63
#define		U		64
#define		V		65
#define		REF_FREQUENCY	66
#define		TOTAL_EC_PHASE	67
#define		TOTAL_RATE	68
#define		TOTAL_MBD	69
#define		TOTAL_SBD_MBD	70
#define		PROCDATE     	71
#define		QCODE    	72

struct udat
    {
    char	*parameter_name;	/* String id of parameter */
    int		parameter_index;	/* array element for parameters with */
					/* multiple entries (e.g. per freq) */
    int		parameter_id;		/* Numerical identifying tag */
    };

struct usearray
    {
    int		nparms;			/* Number of parameters currently used */
    int		npoints;		/* Number of points with extracted parms */
    struct udat type[MAX_PARMS];	/* Info on what parameter fields are */
    int		allocated[MAX_PARMS];	/* Flag to avoid malloc/free screwups */
    double	*parameter[MAX_PARMS];	/* Actual data, dynamically allocated */
    };

#endif
