/*
 * Output a file of data formatted for handling by gnuplot
 * Per base-pol data is captured in exdata->edata[bno]
 * and we here finish up for a scan_boundary.  This file
 * contains templates for the gnuplot commands.
 */
#ifndef __exam_gnuplot_h__
#define __exam_gnuplot_h__

/* lower left and upper right */
#define KEYTBL          7
#define KEYBOT  "inside bottom left vertical reverse Left"
#define KEYTOP  "inside top right vertical Right"

/* holds label and object data */
typedef struct labobj {
    double x, y,  lft,bot,rgt,top,  dens;
    char *fc;
} labobj;

/* pdfcairo terminal setup */
#define GNUPLOT_PDFILE "\
set term pdfcairo size %lf,%lf font '%s,%d'\n\
set output '%s'\n\
"
#define GNUPLOT_PDFILE_BASE 100

/* variable stuff that changes per data file */
#define GNUPLOT_PERBNO "\
# begin with the variable stuff that changes per data file\n\
if (%d) { fringe='{/:Bold %s}'; }\n\
else { fringe='%s'; }\n\
df='%s'\n\
set xrange [%lf:%lf]\n\
ampn=%lf; ampx=%lf; snrn=%lf; snrx=%lf\n\
ampcoh=%lf # 95%% coherence seg. time\n\
snrcoh=%lf # max snr seg. time\n\
"
#define GNUPLOT_PERBNO_BASE 300

/* a swath of choices that make makes something sensible */
#define GNUPLOT_LINETYPES   9
#define GNUPLOT_SETTINGS "\
ampytics='%%5.2f' # must be same width so boxes line up\n\
snrytics='%%5.0f' # must be same width so boxes line up\n\
#\n\
set xtics border mirror in scale 1,0.5\n\
set ytics border mirror in scale 1,0.5\n\
set mxtics default\n\
set mytics 5\n\
set grid xtics ytics nomxtics nomytic\n\
\n\
# override linetypes for clarity below\n\
# 1:amp 2:fitaps 3:fitapo 4:fitaso\n\
# 5:snr 6:fitsnr 7:fit2p8 8:fit2p7\n\
set lt 1 lw %d %s ps 1 pt 22 lc rgb '%s'\n\
set lt 2 lw %d %s ps 1 pt  0 lc rgb '%s'\n\
set lt 3 lw %d %s ps 1 pt 22 lc rgb '%s'\n\
set lt 4 lw %d %s ps 1 pt  0 lc rgb '%s'\n\
set lt 5 lw %d %s ps 1 pt 22 lc rgb '%s'\n\
set lt 6 lw %d %s ps 1 pt  0 lc rgb '%s'\n\
set lt 7 lw %d %s ps 1 pt  0 lc rgb '%s'\n\
set lt 8 lw %d %s ps 1 pt  0 lc rgb '%s'\n\
#\n\
"
#define GNUPLOT_SETTINGS_BASE 900

/* these generate a two-panel plot similar to what PGPLOT does */
#define GNUPLOT_TWOPANEL_HEAD_BLK "\
#\n\
set multiplot title fringe\n\
set log x\n\
#\n\
set origin 0,0.00\n\
set size 1.00,0.53\n\
set key %s\n\
unset object; unset label\n\
if (%d&&snrcoh>0) {# labels\n\
set label  1 'SNR Max. %.1lf' at %lf,%lf left front tc rgb '%s'\n\
set object 1 rect from %lf,%lf to %lf,%lf fc rgb '%s' fs solid %lf\\\n\
  border rgb '%s'\n\
}\n\
set ylabel 'SNR'\n\
set yrange [snrn:snrx]\n\
set xlabel 'Segmentation Time (sec)'\n\
set xtics format '%%3.0f'\n\
set ytics format snrytics\n\
#\n\
"
#define GNUPLOT_TWOPANEL_HEAD_BLK_BASE 500
#define GNUPLOT_TWOPANEL_PLOT_SNR "\
plot df u 1:10 w line %s, \\\n\
     df u 1:11 w line %s, \\\n\
     df u 1:12 w line %s, \\\n\
     df u 1:5:6 w yerrorlines lt 5 tit 'SNR data'\n\
#\n\
"
#define GNUPLOT_TWOPANEL_PLOT_SNR_BASE 300 
#define GNUPLOT_TWOPANEL_MID_BLCK "\
set origin 0,0.48\n\
set size 1.00,0.49\n\
set key %s\n\
unset object; unset label\n\
if (%d&&ampcoh>0) {# labels and when plateau-slope best\n\
set label  2 'Amp. B.P. %.1lf' at %lf,%lf right front tc rgb '%s'\n\
set object 2 rect from %lf,%lf to %lf,%lf fc rgb '%s' fs solid %lf\\\n\
  border rgb '%s'\n\
}\n\
if (%d&&ampcoh>0) {# labels\n\
set label  3 'Amp. Coh. %.1lf'  at %lf,%lf left front tc rgb '%s'\n\
set object 3 rect from %lf,%lf to %lf,%lf fc rgb '%s' fs solid %lf\\\n\
  border rgb '%s'\n\
}\n\
set ylabel 'Amplitude'\n\
unset xlabel\n\
set yrange [ampn:ampx]\n\
set xtics format ''\n\
set ytics format ampytics\n\
set log x\n\
#\n\
"
#define GNUPLOT_TWOPANEL_MID_BLCK_BASE 700 
#define GNUPLOT_TWOPANEL_PLOT_AMP "\
plot df u 1:7 w line %s, \\\n\
     df u 1:8 w line %s, \\\n\
     df u 1:9 w line %s, \\\n\
     df u 1:3:4 w yerrorlines lt 1 tit 'Amp data'\n\
#\n\
"
#define GNUPLOT_TWOPANEL_PLOT_AMP_BASE 300 

/* so the human knows this is all there is */
#define GNUPLOT_CODA "\
unset multiplot\n\
set output\n\
#\n\
# eof\n\
#\n\
"
#define GNUPLOT_CODA_BASE 100

#endif /* __exam_gnuplot_h__ */
/*
 * eof vim:nospell
 */
