/************************************************************************/
/*                                                                      */
/* Implements the main cursor-reading loop for the psplot function.     */
/* A variety of actions are taken, depending on the cursor position     */
/* and the mouse button or keyboard key pressed.                        */
/*                                                                      */
/*      Inputs:         psarray         Contains all needed information */
/*                                                                      */
/*      Output:         return value    0=success, 1=failure            */
/*                                                                      */
/* Created 22 February 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "cpgplot.h"
#include "aedata.h"
#include "psplot.h"

int
run_pscursor (psarray, data)
struct ps_array *psarray;
esum *data;
    {
    int poskey;
    float x, y;
    int scan, baseline, npages, nbasepage, current_page, colour;
    int nbands, current_band;
    char key;
    static int do_fplot;
    struct psplot_cell *cell;
    extern int psplot_open;

    do_fplot = FALSE;
    psarray->fplot_open = FALSE;
    psarray->ntagged = 0;
    psarray->retain = FALSE;
                                        /* Local vars for convenience */
    npages = psarray->param.npages;
    nbasepage = psarray->param.nbasepage;
    nbands = strlen (psarray->subgroups);
                                        /* Initialization */
    current_page = 0;
    current_band = 0;
    x = (PLOT_XMIN + PLOT_XMAX) / 2.0;
    y = (PLOT_YMIN + PLOT_YMAX) / 2.0;
                                        /* Infinite cursor read loop, only */
                                        /* way out is quit button (= 'x' or 'X') */
    while (TRUE)
        {
        if (cpgcurs (&x, &y, &key) != 1)
            {
            msg ("Error reading cursor in run_pscursor()", 2);
            return (1);
            }
        poskey = locate_pscurs (&x, &y, psarray, &cell);

        switch (poskey)
            {
            case NOTHING:
                break;

            case NODATA:
                break;

            case QUIT:
                if (! psarray->retain)
                    {
                    cpgend();
                    psplot_open = FALSE;
                    }
                return (0);

            case FPLOT:
                cpgsci (INACTIVE);
                cpgsfs (2);
                cpgslw (2);
                cpgrect (SELECT_BUTTON);
                cpgsci (ACTIVE);
                cpgrect (FPLOT_BUTTON);
                do_fplot = TRUE;
                break;

            case SELECT:
                cpgsci (INACTIVE);
                cpgsfs (2);
                cpgslw (2);
                cpgrect (FPLOT_BUTTON);
                cpgsci (ACTIVE);
                cpgrect (SELECT_BUTTON);
                do_fplot = FALSE;
                break;

            case NEXT:
                cpgsfs (2);
                cpgslw (2);
                                        /* current_page 0-relative */
                if (current_page == (npages - 1)) break;
                current_page++;
                psarray->param.basepage = current_page % nbasepage; 
                psarray->param.scanpage = current_page / nbasepage; 
                cpgsci (ACTIVE);
                cpgrect (PREV_BUTTON);
                if (current_page == (npages - 1))
                    {
                    cpgsci (INACTIVE);
                    cpgsfs (2);
                    cpgslw (2);
                    cpgrect (NEXT_BUTTON);
                    }
                if (display_psdata (psarray) != 0)
                    {
                    msg ("Problem displaying next page", 2);
                    return (1);
                    }
                break;

            case PREV:
                cpgsfs (2);
                cpgslw (2);
                if (current_page == 0) break;
                current_page--;
                psarray->param.basepage = current_page % nbasepage; 
                psarray->param.scanpage = current_page / nbasepage; 
                cpgsci (ACTIVE);
                cpgrect (NEXT_BUTTON);
                if (current_page == 0)
                    {
                    cpgsci (INACTIVE);
                    cpgsfs (2);
                    cpgslw (2);
                    cpgrect (PREV_BUTTON);
                    }
                if (display_psdata (psarray) != 0)
                    {
                    msg ("Problem displaying previous page", 2);
                    return (1);
                    }
                break;
                                        /* RETAIN determines whether the */
                                        /* screen is zapped on quit or not */
            case RETAIN:
                cpgsfs (2);
                cpgslw (2);
                if (psarray->retain)
                    {
                    cpgsci (INACTIVE);
                    psarray->retain = FALSE;
                    }
                else
                    {
                    cpgsci (ACTIVE);
                    psarray->retain = TRUE;
                    }
                cpgrect (RETAIN_BUTTON);
                break;
                                        /* Baseline label for selection */
            case BLAB:
                                        /* Get baseline index and restore */
                                        /* cursor position */
                baseline = (int) y;
                y = PLOT_YMAX - (y * psarray->param.base_sep);
                y += (PLOT_YMAX - PLOT_YMIN) * psarray->param.basepage;
                if (do_fplot && (key == 'A')) break;
                if ((key != 'a') && (key != 'A')) break;
                ps_selbase (baseline, psarray);
                break;
                                        /* Scan label for selection */
            case SLAB:
                                        /* Get scan index and restore */
                                        /* cursor position */
                scan = (int) x;
                x = PLOT_XMIN + (x * psarray->param.scan_sep);
                x -= (PLOT_XMAX - PLOT_XMIN) * psarray->param.scanpage;
                if (do_fplot && (key == 'A')) break;
                if ((key != 'a') && (key != 'A')) break;
                ps_selscan (scan, psarray);
                break;
                                        /* Quality code for selection */
            case KEY:
                                        /* Get colour index + restore cursor */
                colour = (int)x;
                x = KEY_XMIN + (x * KEYINC);
                if (do_fplot && (key == 'A')) break;
                if ((key != 'a') && (key != 'A')) break;
                ps_selqual (colour, psarray);
                break;
                                        /* Band for display passed back in y */
            case BAND:
                                        /* Identify requested band and display */
                if ((int)y != current_band)
                    {
                    psarray->param.band = (int)y;
                    if (display_psdata (psarray) != 0)
                        {
                        msg ("Problem displaying requested subgroup", 2);
                        return (1);
                        }
                                        /* Change button highlight + restore cursor */
                    cpgsfs (2);
                    cpgslw (2);
                    cpgsci (INACTIVE);
                    cpgrect (BAND_XORIGIN, BAND_XORIGIN + BAND_SIZE,
                        BAND_YORIGIN - current_band*BAND_SIZE,
                        BAND_YORIGIN - (current_band+1)*BAND_SIZE);
                    current_band = psarray->param.band;
                    cpgsci (ACTIVE);
                    cpgrect (BAND_XORIGIN, BAND_XORIGIN + BAND_SIZE,
                        BAND_YORIGIN - current_band*BAND_SIZE,
                        BAND_YORIGIN - (current_band+1)*BAND_SIZE);
                    }
                x = BAND_XORIGIN + BAND_SIZE/2;
                y = BAND_YORIGIN - current_band*BAND_SIZE - BAND_SIZE/2;
                break;
                

            case DATA_PT:
                ps_proc_datum (data, psarray, x, y, cell, key, do_fplot);
                break;

            default:
                break;
            }
        }
    }
