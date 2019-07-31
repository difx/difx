#ifndef OVEX_H
#define OVEX_H

/************************************************************************/
/*                                                                      */
/* This header file defines the memory structure for a scan described   */
/* in a vex file, along with a couple of miscellaneous structs for      */
/* internal vex library use.  When filled in, this structure should     */
/* contain all the information relevant to the correlator that is       */
/* present in the vex file for the requested scan.  The structure is    */
/* filled (and parts are dynamically allocated) via the library call    */
/* scan_info(), which returns a pointer to the filled in structure.     */
/*                                                                      */
/* January 1998, CJL                                                    */
/*                                                                      */
/*                                                                      */
/* Special notes:                                                       */
/*      1. Not all arrays are dynamically allocated.  Overflows should  */
/*         be handled gracefully (I hope).  End of information in       */
/*         partially filled arrays is generally denoted by undefined    */
/*         values.                                                      */
/*      2. End of information in the channel arrays is probably best    */
/*         checked by looking for the first undefined sky frequency     */
/*      3. The roll_seq array is 3-dimensional.  The three dimensions   */
/*         are headstack number (1-relative), home track number (only   */
/*         elements 2-33 are meaningful, and rolled track number.       */
/*                                                                      */
/************************************************************************/
#include "mk4_typedefs.h"
#include "mk4_sizes.h"
                                        /* source_type */
#define         QUASAR      1
#define         STAR        2
#define         SATELLITE   3
#define         DUMMY       4
                                        /* position_ref_frame */
#define         B1950       1
#define         J2000       2
                                        /* tape_motion */
#define         CONTINUOUS  1
#define         START_STOP  2
#define         ADAPTIVE    3
                                        /* site_type */
#define         FIXED       1
#define         EARTH_ORBIT 2
                                        /* axis_type */
#define         EQUATORIAL  1
#define         X_YNS       2
#define         AZEL        3
#define         X_YEW       4
                                        /* rack_type or recorder type */
                                        /* Also track format */
#define         MARK3A      1
#define         MARK4       2
#define         VLBA        3
#define         VLBAG       4
#define         S2          5
#define         K4          6
#define         MARK5A      7
#define         MARK5B      8
                                        /* Modulation and roll */
#define         ON          1
#define         OFF         2
                                        /* "Magic" values */
#define         C_UNDEFINED '\0'
#define         I_UNDEFINED 16383
#define         F_UNDEFINED -1.0e30
                                        /* For internal library use */

struct source_struct
    {
    char                source_name[32];
    char                iau_name[32];
    short               source_type;            /* Defines above */
    short               calibrator;             /* True or false */
    struct sky_coord    position;               /* Standard Mk4 struct */
    struct date         position_epoch;         /* Standard Mk4 struct */
    short               position_ref_frame;     /* Defines above */
    float               ra_rate;                /* Radians/sec */
    float               dec_rate;               /* Radians/sec */
    };

struct chan_struct
    {
    char                chan_name[32];          /* External channel name */
    char                polarization;           /* R or L */
    double              sky_frequency;          /* Hz */
    char                net_sideband;           /* U or L */
    double              bandwidth;              /* Hz */
    char                band_id[32];            /* Linkword (internal use) */
    char                chan_id[32];            /* Linkword (internal use) */
    char                bbc_id[32];             /* Linkword (internal use) */
    char                pcal_id[32];            /* Linkword (internal use) */
    char                if_id[32];              /* Linkword (internal use) */
    short               bbc_no;                 /* Physical BBC# */
    char                if_name[8];             /* Physical IF name */
    double              if_total_lo;            /* Hz */
    char                if_sideband;            /* U or L */
    float               pcal_spacing;           /* Hz */
    float               pcal_base_freq;         /* Hz */
    short               pcal_detect[16];        /* Integer tone #s */
    short               sign_tracks[4];         /* Track #s */
    short               sign_headstack;         /* 1-4 */
    short               mag_tracks[4];          /* Track #s */
    short               mag_headstack;          /* 1-4 */
    };
    

struct station_struct
    {
    short               start_offset;           /* Seconds */
    short               stop_offset;            /* Seconds */
    float               start_tapepos;          /* Meters */
    short               tape_motion;            /* Defines above */
    short               early_start;            /* Seconds */
    short               late_finish;            /* Seconds */
    short               tape_gap;               /* Seconds */
    char                subpass;                /* Standard vex meaning */
    short               passno;                 /* Standard vex meaning */
    short               drive_no;
    short               site_type;              /* Defines above */
    char                site_name[9];
    char                site_id[3];             /* International 2 char code */
    char                mk4_site_id;            /* 1-char correlator alias */
    double              coordinates[3];         /* Meters */
    struct date         coordinate_epoch;       /* Standard Mk4 struct */
    double              site_velocity[3];       /* Meters/sec */
    float               zenith_atm;             /* Seconds */
    char                occucode[5];            /* Standard 4-char code */
    short               axis_type;              /* Defines above */
    float               axis_offset;            /* Meters */
    short               recorder_type;          /* Defines above */
    short               rack_type;              /* Defines above */
    float               record_density;         /* Bits/inch */
    float               tape_length;            /* Meters */
    short               recorder_id;            /* Unique integer */
    float               clock_early;            /* Seconds */
    struct date         clockrate_epoch;        /* Standard Mk4 struct */
    float               clockrate;              /* sec/sec */
    char                tape_id[9];             /* Standard tape label */
    double              samplerate;             /* Samples/sec */
    short               track_format;           /* Defines above */
    short               modulation;             /* Defines above */
    short               bits_sample;            /* 1 or 2 */
    short               multiplex_ratio;        /* 1, 2 or 4 */
    char                pass_direction;         /* F or R */
    float               head_position[4];       /* Meters */
    short               roll;                   /* Defines above */
    short               roll_increment;         /* Frames */
    float               roll_period;            /* Seconds */
    short               roll_seq[5][34][32];    /* Track numbers */
    struct chan_struct  channels[MAX_CHAN];
    };

struct scan_struct
    {
    char                        filename[256];  /* Name of input vex file */
    short                       exper_num;      /* Standard 4-digit */
    char                        exper_name[32];
    char                        correlator[32];
    char                        scan_name[32];
    struct date                 start_time;     /* Standard Mk4 struct */
    struct date                 ffit_reftime;   /* Standard Mk4 struct */
    float                       tai_utc;        /* EOP parameters (global) */
    float                       a1_tai;
    int                         neop;           /* Number of eop entries */
    struct date                 eop_reftime;    /* Time of 1st entry */
    int                         eop_interval;   /* Seconds */
    float                       ut1_utc[10];    /* Seconds */
    float                       x_wobble[10];   /* Radians */
    float                       y_wobble[10];   /* Radians */
    struct source_struct        src;
    short                       nst;            /* Number of st elements */
    struct station_struct       *st;            /* Allocated */
    };

#endif 
