/************************************************************************/
/*                                                                      */
/* Inserts new scan labels on horizontal axis, if necessary.            */
/*                                                                      */
/*      Inputs:         psarray         Contains all needed information */
/*                                                                      */
/*      Output:         none                                            */
/*                                                                      */
/* Created 22 February 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "cpgplot.h"
#include "psplot.h"

void
ps_scanlabel (psarray)
struct ps_array *psarray;
    {
    int scanpage, nvertpage, sno, soffset, scan;
    int year, day, hour, minute, second;
    char label[11];
    float ssep, xpos, ypos, xmin, xmax, ymin, ymax;
    static int current_scanpage;
    extern int data_version;

    if (! psarray->displayed) current_scanpage = -1;

    scanpage = psarray->param.scanpage;

                                        /* Already have correct labels */
    if (scanpage == current_scanpage) return;
    current_scanpage = scanpage;
                                        /* Erase existing label */
    cpgsci (0);
    cpgsfs (1);
    cpgrect (SCANLABEL);
                                        /* Set up some parameters */
    soffset = scanpage * psarray->param.scans_per_page;
    ssep = psarray->param.scan_sep;

    cpgsci (TEXT);
    cpgslw (1);
    cpgsfs (2);
    cpgsch (0.4 + (ssep/85.0));
    for (sno=0; sno<psarray->param.scans_per_page; sno++)
        {
                                        /* Get psarray baseline index */
        scan = sno + soffset;
        if (scan >= psarray->nscans) break;
                                        /* Can now calculate correct position */
        ypos = PLOT_YMAX + 5.0;
        xpos = PLOT_XMIN + (float)(sno+1) * ssep - 4.0;
                                        /* Draw text */
        int_to_time (psarray->time[scan].scantime, 
                        &year, &day, &hour, &minute, &second);
        if (data_version > 1)
            sprintf (label, "%03d-%02d%02d%02d", day, hour, minute, 
                                psarray->time[scan].seconds);
        else
            sprintf (label, "%03d-%02d%02d", day, hour, minute);
        if (strlen (psarray->time[scan].scan_name) > 0)
            strcpy (label, psarray->time[scan].scan_name);
        cpgptxt (xpos, ypos, 90.0, 0.0, label);
                                        /* Enclose it in a rectangle */
        xmax = xpos + 4.0;
        xmin = xmax - ssep;
        ymin = PLOT_YMAX;
        ymax = PLOT_YMAX + 130.;
        cpgrect (xmin, xmax, ymin, ymax);
        }

    }
