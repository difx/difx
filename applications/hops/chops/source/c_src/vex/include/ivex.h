#ifndef IVEX_H
#define IVEX_H

/* ivex.h
Tue Feb  2 11:35:07 EST 1999
*/

/* typedef unsigned short U16; */
/* typedef unsigned long  U32; */

enum    { DRV_MK4, DRV_VLBA, DRV_MK5P, DRV_MK5A, DRV_MK5B };
enum    { EVEN, ODD, NONE };
enum    { HSC0, HSC1, HSC2, HSC3, MAX_HSC };
enum    { REVERSE, FORWARD };

#define MAXHEADS 2

struct bad_frame_struct
        {
        short   CRC_error;              /* off|on                       */
        short   PE_limit_exceeded;      /* off|on                       */
        short   lost_sync;              /* off|on                       */
        short   unexpected_sync;        /* off|on                       */
        };

#define TRM_ABSENT -1
#define TRM_SIGN0 0
#define TRM_SIGN1 1
#define TRM_SIGN2 2
#define TRM_SIGN3 3
#define TRM_MAG0  10
#define TRM_MAG1  11
#define TRM_MAG2  12
#define TRM_MAG3  13

struct TRM_config_struct
        {
        short   mux4_2bit[8];           /* Values given by defines above */
        short   mux4_1bit[4];
        short   mux2_2bit[4];
        short   mux2_1bit[2];
        short   mux1_2bit[2];
        short   mux1_1bit;
        double  parity_error_limit;     /* rx parity error threshold    */
        struct  bad_frame_struct        invalid_frame_control; /* boolean flags */
        };

struct vacuum_struct
        {
        short   inches_of_H2O;          /* eg. 5" and 15"               */
        unsigned short setting;
        };

struct headstack_DIM_connect_struct
        {
        short   headstack;              /* 1|2|3|4                      */
        short   parity;                 /* EVEN|ODD|NONE                */
        short   eqlzr;                  /* equalizer (Mbps)             */
        };

struct moni_read_struct
        {
        short   track[2];               /* REV|FWD: track value         */
        short   equalizer[3];           /* 0|1|2=std,alt1,alt2          */
        };

struct headstack_parms_struct
        {
        /* { ffspeed, fsspeed, rfspeed, rsspeed, pscale, nscale, 0.0, 0.0, fbias, rbias } */
        double  param[10];
        };

struct drive_init_struct
        {
        short   drive_type;                     /* Mark4|VLBA                   */
        char    computer_name[MAX_NAMESIZE];    /* For Mark 5 */
        int     mcb_addr_start;
        int     mcb_addr_id;
        float   capstan_relative_diameter;      /* eg. 1.014                    */
        struct  vacuum_struct           vacuum[2];      /* eg 5" & 15" hijklmno */
        float   tape_acceleration;              /* Mark4 default: 67.5 ips/sec  */
        struct  headstack_DIM_connect_struct    DIM_connect[MAX_HSC];   /* HSC0..3      */
        short   nheads;                         /* number of headstacks         */
        struct  moni_read_struct        monitor_module[MAXHEADS+1];
        struct  headstack_parms_struct  headstack[MAXHEADS+1];
        };
        
struct su_connect_struct
        {
        short   corr_segment[4];
        short   inputbd_chan_grp[4];
        };

struct tim_mon_struct
        {
        char    measurement_name[MAX_NAMESIZE];
        short   switch_pos;
        float   minimum;
        float   maximum;
        int     fail_action;
        };

struct CUCC_port_struct
        {
        short   crate_number;
        char    server_envname[MAX_NAMESIZE];
        short   port_A_number;
        short   port_B_number;
        };

struct SUCC_port_struct
        {
        char    server_envname[MAX_NAMESIZE];
        short   console_port;
        short   DPU_port;
        };

struct pbs_init_struct
        {
        short                           SU_ID;
        char                            cHost[MAX_NAMESIZE];
        float                           tape_servo_interval;
        struct  SUCC_port_struct        succ_port;
        struct  tim_mon_struct          timing_monitor;
        struct  TRM_config_struct       TRM_config;
        struct  drive_init_struct       drive;
        struct  su_connect_struct       su_connect;
        };

struct ivex_struct
        {
        float   system_tempo;
        unsigned long bocf_period;
        short   header_duration;
        struct CUCC_port_struct cucc_ports[2];
        char    counter_envname[MAX_NAMESIZE];
        short   counter_port;
        char    switch_envname[MAX_NAMESIZE];
        short   switch_port;
        int     cf_edit_mask;
        short   nmonitor;
        struct  tim_mon_struct          timing_monitor[10];
        short   nplayback_systems;
        struct  pbs_init_struct pbs_init[16];
        };

#endif
