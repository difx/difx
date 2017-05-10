#ifndef PASS_STRUCT
#define PASS_STRUCT

#include <complex.h>
#include "control.h"
#include "mk4_data.h"
#include "mk4_sizes.h"

#define USB_FLAG 1
#define LSB_FLAG 2
                                    // polarization values in pass array structure
#define POL_LL 0
#define POL_RR 1
#define POL_LR 2
#define POL_RL 3
                                    // polarization values in param array structure
#define POL_ALL 0
#define POLMASK_LL 1
#define POLMASK_RR 2
#define POLMASK_LR 4
#define POLMASK_RL 8
#define POL_IXY 31
                           /* maximum # of pcal tones per channel */
#define MAX_PCF 64

struct interp_sdata
        {
        complex phasecal_lcp[MAX_PCF];               /* USB only for now */
        float pcweight_lcp;                 /* (see pcal_interp routine) */
        complex phasecal_rcp[MAX_PCF];
        float pcweight_rcp;
        float bigpos[4];                    /* State count statistics as fractions: */
        float pos[4];                       /* lcp_usb, lcp_lsb, rcp_usb, rcp_lsb */
        float neg[4];
        float bigneg[4];
        complex mt_pcal[2];                 // pc phasor for multitone mode L:R or H:V
        double mt_delay[2];                 // multitone delay (us) [L:R or H:V or X:Y]
        };

struct data_corel 
        {
        int flag;
        struct type_120 *apdata_ll[2];          /* By sideband */
        struct type_120 *apdata_rr[2];
        struct type_120 *apdata_lr[2];
        struct type_120 *apdata_rl[2];
        int sband;
        complex *sbdelay;                       /* Allocated in fringe_search */
        struct interp_sdata ref_sdata;
        struct interp_sdata rem_sdata;
        float usbfrac;
        float lsbfrac;
        complex pc_phasor[4];  // extracted in rotate_pcal [LL:RR:LR:RL]
        };

struct freq_corel 
        {
        char freq_code;         /* Fourfit frequency identifier (a..zA..Z$%) */
        double frequency;       /* sky frequency (MHz) */
        char fgroup;
        int  ch_idx[2];         // [ref/rem] index into ovex channels array for this freq
        char ch_usb_lcp[2][8];  /* VEX channel ids by station */
        char ch_usb_rcp[2][8];
        char ch_lsb_lcp[2][8];
        char ch_lsb_rcp[2][8];
        short trk_lcp[2][16];    /* Contributing tracks by station */
        short trk_rcp[2][16];
        float mean_lcp_trk_err[2][16];    /* Mean track error rates by station */
        float mean_rcp_trk_err[2][16];
        float pc_freqs[2][MAX_PCF];
        short bbc_lcp[2];       /* Physical BBC numbers by station */
        short bbc_rcp[2];
        int index[8];           /* Corel index number by sideband/pol'n */
        int data_alloc;         /* Flag indicating whether space alloced for data */
        struct data_corel *data;
        };


struct type_pass 
        {
        struct freq_corel       pass_data[MAX_CHAN];
        int                     nfreq;
        int                     channels;
        int                     nlags;
        int                     num_ap;
        int                     npctones;     // max number of pcal tones in any freq
        int                     ap_off;
        int                     pol;          // 0|1|2|3 = LL|RR|LR|RL or last pol if param.pol!=0
        int                     linpol[2];    // 0|1 if circular|linear pol for ref & remote
        int                     npols;        // number of polarizations in coherent sum
        int                     pprods_present[4]; // flags for pol.product present, like pol
        int                     autocorr;
        int                     pci[2][MAX_CHAN];  /* index of desired pcal tone by stn */
        int                     pcinband[2][MAX_CHAN][MAX_PCF];
        double                  start;
        double                  stop;
        double                  reftime;
        struct c_block          control;
        };
        
static char fchars[64] =
	{'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
     'q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F',
     'G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V',
     'W','X','Y','Z','0','1','2','3','4','5','6','7','8','9','$','%'};
#endif
