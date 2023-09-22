/************************************************************************/
/*                                                                      */
/* This routine takes an x,y coordinate returned from a cursor event,   */
/* and returns a value describing where on the screen the event         */
/* occurred, in terms of screen objects.  These are either buttons,     */
/* data elements identified by data array indices, or nothing of        */
/* interest.  Data indices are zero or positive, buttons and other      */
/* locations are negative.                                              */
/*                                                                      */
/*      Inputs:         x, y            Coordinates in pgplot world     */
/*                                      coord system (0-1000 each axis) */
/*                      psarray         contains info to make sense of  */
/*                                      locations in data display       */
/*                                                                      */
/*      Output:         return value    -1 to -5 buttons, -6 no data    */
/*                                      for this point, -7 nowhere of   */
/*                                      interest, >=0 is a data array   */
/*                                      index                           */
/*                                                                      */
/* Created 19 February 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <math.h>
#include "psplot.h"
#include "aedit.h"

int locate_pscurs (float *x, float *y, struct ps_array *psarray, struct psplot_cell **cell)
    {
    int xint, yint, nvertpage, basepage, scanpage, scan, baseline, ret=0;
    int dataindex, nbands, band;
    double xcoord, ycoord, rel_x, rel_y, xrem, yrem;

    nbands = strlen (psarray->subgroups);
    band = psarray->param.band;
                                        /* Check first for location in a */
                                        /* button rectangle */
    if (ps_inside (*x, *y, PREV_BUTTON)) return (PREV);
    if (ps_inside (*x, *y, NEXT_BUTTON)) return (NEXT);
    if (ps_inside (*x, *y, SELECT_BUTTON)) return (SELECT);
    if (ps_inside (*x, *y, FPLOT_BUTTON)) return (FPLOT);
    if (ps_inside (*x, *y, QUIT_BUTTON)) return (QUIT);
    if (ps_inside (*x, *y, RETAIN_BUTTON)) return (RETAIN);
    if (ps_inside (*x, *y, BASELABEL)) ret = BLAB;
    else if (ps_inside (*x, *y, SCANLABEL)) ret = SLAB;
    else if (ps_inside (*x, *y, KEY_XMIN + KEYINC, 
                                KEY_XMIN + (19. * KEYINC),
                                KEY_YMIN, KEY_YMIN + KEYINC)) ret = KEY;
    else if (ps_inside (*x, *y, BAND_XORIGIN, BAND_XORIGIN + BAND_SIZE,
                BAND_YORIGIN - BAND_SIZE*nbands, BAND_YORIGIN)) ret = BAND;
                                        /* If not inside plot area, this */
                                        /* is a useless event, discard */
    else if (! ps_inside (*x, *y, PLOT_XMIN, PLOT_XMAX, PLOT_YMIN, PLOT_YMAX))
                return (NOTHING);
                                        /* Process key */
    if (ret == KEY)
        {
        rel_x = *x - KEY_XMIN;
        xrem = fmod (rel_x, KEYINC);
        if ((xrem < GUARD_BAND) || ((KEYINC - xrem) < GUARD_BAND))
            return (NOTHING);
        xcoord = rel_x / KEYINC;
        xint = (int)floor (xcoord);
        *x = (float)xint + 0.5;
        return (KEY);
        }
                                        /* Process band */
    if (ret == BAND)
        {
        rel_y = BAND_YORIGIN - *y;
        yrem = fmod (rel_y, BAND_SIZE);
        if ((yrem < GUARD_BAND) || ((BAND_SIZE - yrem) < GUARD_BAND))
            return (NOTHING);
        ycoord = rel_y / BAND_SIZE;
        yint = (int)floor (ycoord);
        *y = (float)yint + 0.5;
        return (BAND);
        }
                                        /* OK, we are in the data array */
                                        /* somewhere or other. Get */
                                        /* coordinates in psarray page units */
    rel_x = *x - PLOT_XMIN;
    rel_y = PLOT_YMAX - *y;
    xcoord = rel_x / psarray->param.scan_sep;
    ycoord = rel_y / psarray->param.base_sep;
                                        /* Is this in a guard band? */
    xrem = fmod (xcoord, 1.0);
    yrem = fmod (ycoord, 1.0);
    if ((xrem < psarray->param.xgb_fract) ||
        ((1.0 - xrem) < psarray->param.xgb_fract))
        if (ret != BLAB) return (NOTHING);
    if ((yrem < psarray->param.ygb_fract) ||
        ((1.0 - yrem) < psarray->param.ygb_fract))
        if (ret != SLAB) return (NOTHING);
                                        /* In a valid screen cell, or a */
                                        /* label area, get integer coordinates */
    xint = (int)floor (xcoord);
    yint = (int)floor (ycoord);
                                        /* Move x, y to cell center */
    *x = ((float)xint + 0.5) * psarray->param.scan_sep + PLOT_XMIN;
    *y = PLOT_YMAX - (((float)yint + 0.5) * psarray->param.base_sep);
                                        /* Can now get psarray indices */
    basepage = psarray->param.basepage;
    scanpage = psarray->param.scanpage;
    baseline = basepage * psarray->param.base_per_page + yint;
    scan = scanpage * psarray->param.scans_per_page + xint;
                                        /* Is this beyond scheduled data? */
    if ((baseline >= psarray->nbaselines) || (scan >= psarray->nscans))
        return (NOTHING);
                                        /* If this is a label, we have what */
                                        /* we need already  */
    if (ret == BLAB) 
        {
        *y = (float)baseline + 0.5;
        return (BLAB);
        }
    if (ret == SLAB) 
        {
        *x = (float)scan + 0.5;
        return (SLAB);
        }
                                        /* Does this cell have any data in it? */
    *cell = psarray->baseline[baseline].scan + scan;
    dataindex = (*cell)->data_index[band];
    if (dataindex < 0) return (NODATA);
    else return (DATA_PT);

    }
