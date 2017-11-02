#define TRUE 1
#define FALSE 0

#include "sizelimits.h"

struct com {
        char    cmnd[50];
        char    arg1[256];
        char    arg2[256];
        char    remarg[512];
        short   narg;
};

struct inputs {
        int     begin;                  /* Seconds since 0000 Jan 1 1980 */
        int     end;
        int     proc_begin;
        int     proc_end;
        char    stations[2*MAXSTEXP];  /*2 chars per station (1 char + space) */
        char    baselines[3*MAXBASE];  /*3 chars per baseline */

        /*4 chars per triangle, for 40 stations (MAXSTEXP) this is around 50kb 
        /*on the stack which is probably unecessarily large */
        /*however, this shouldn't be a problem for most modern computers */
        char    triangles[4*MAXCLOSE]; 
        char    quads[1024];           /*quads are not yet implemented*/
        char    frequencies[2*MAXFREQ];/*2 chars per frequency (1 char + space) */
        char    polarizations[128];
        int     experiment;
        char    qcodes[20];
        char    type[10];
        float   snr[2];                 /* Baseline snr min/max */
        float   bsnr[2];                /* Triangle (bispectrum) snr min/max */
        int     length;
        int     fraction;               /* 10ths of data passed by FRNGE */
        int     nfreq[2];               /* Range of # freqs processed by FRNGE */
        float   parameter[3];           /* User param id tag, min, max */
        char    sources[200];
        char    x_axis[20];
        char    x_units[20];
        int     xaind;
        int     x_aux;
        char    y_axis[20];
        char    y_units[20];
        int     yaind;
        int     y_aux;
        int     plotby;
        int     refrem;
        float   xscale[2];
        float   yscale[2];
        int     grid[2];
        int     mode;
        char    device[30];
        int     dev_auto;
};

struct ibaselist { int index; char baseline[3]; };
struct ptrilist { int present; char triangle[4]; };

#include "aedit_proto.h"
#include "mk4_util.h"
