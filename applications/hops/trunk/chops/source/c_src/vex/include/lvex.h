#ifndef LVEX_H
#define LVEX_H

/************************************************************************/
/*                                                                      */
/* Header file for LVEX.  Initial implementation stores only essentials */
/* for correlation.                                                     */
/*                                                                      */
/* Created March 1 1999 by CJL                                          */
/*                                                                      */
/************************************************************************/
#define MAXSTATIONS 32
#define MAXDISCS 16

struct discinfo
    {
    char    disc_serial_num[MAX_NAMESIZE];
    char    disc_model_num[MAX_NAMESIZE];
    int     disc_size;
    };

struct station_log
    {
    char    station;                        /* 1-char Mk4 ID */
    char    linked_stations[10];            /* Possibly multiple linked stns */
    char    vsn[MAX_NAMESIZE];              /* Volume serial no. */
    char    disc_set_ID[MAX_NAMESIZE];      /* For Mk5 */
    int     ndiscs;
    struct discinfo disc[MAXDISCS]; 
    float   headpos;                        /* Meters */
    struct date tapestart;                  /* Standard Mk4 structs */
    struct date tapestop;
    float   start_footage;                  /* Meters */
    float   start_speed;                    /* Meters/sec */
    float   end_footage;                    /* Meters */
    struct date discstart;                  /* For Mk5 */
    struct date discstop;
    long long start_byte;
    long long stop_byte;
    char    source[MAX_NAMESIZE];           /* Name of source */
    struct date time_on_source;             /* Time source acquired by telescope */
    int     autopeak_interval;
    char    pass[MAX_NAMESIZE];             /* JBall's VLBA pass number */
    };
 
struct lvex_struct
    {
    int     nstation;                       /* Number actually present */
    struct station_log stn[MAXSTATIONS];
    };

#endif
