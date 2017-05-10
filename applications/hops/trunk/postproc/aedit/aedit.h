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
        char    stations[15];
        char    baselines[50];
        char    triangles[80];
        char    quads[100];
        char    frequencies[10];
        char    polarizations[49];
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
