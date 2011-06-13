/************************************************************************/
/*                                                                      */
/* Takes a completed ps array, and figures out how to display the array */
/* on screen.  Sometimes, will need multiple pages, and here we decide  */
/* where and how to page-break.                                         */
/*                                                                      */
/*      Inputs:         psarray         full ps array, minus param info */
/*                                                                      */
/*      Output:         psarray         with param filled in            */
/*                                                                      */
/* Created  19 February 1993 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <math.h>
#include "psplot.h"

int
set_psparam (psarray)
struct ps_array *psarray;
    {
    int nvertpage, nhorizpage, base_page, scan_page;
    double fbase_page, fscan_page, vsep, hsep, sepmin;
                                        /* Pagination first, we */
                                        /* split by baseline first, */
                                        /* then scans */
    nvertpage = 1 + psarray->nbaselines / MAX_BASE_PLOT;
    nhorizpage = 1 + psarray->nscans / MAX_SCAN_PLOT;

                                        /* Adjust to make pages as full */
                                        /* as possible */
    fbase_page = (double)psarray->nbaselines / (double)nvertpage;
    base_page = (int) ceil (fbase_page);
    if (base_page < MIN_BASE_PLOT) base_page = MIN_BASE_PLOT;

    fscan_page = (double)psarray->nscans / (double)nhorizpage;
    scan_page = (int) ceil (fscan_page);
    if (scan_page < MIN_SCAN_PLOT) scan_page = MIN_SCAN_PLOT;

                                        /* These are separations of cells */
    vsep = (PLOT_YMAX - PLOT_YMIN) / (double)base_page;
    hsep = (PLOT_XMAX - PLOT_XMIN) / (double)scan_page;
    if (vsep < hsep) sepmin = vsep;
    else sepmin = hsep;
                                        /* Can fill structure now */
    psarray->param.scanpage = 0;
    psarray->param.basepage = 0;
    psarray->param.npages = nvertpage * nhorizpage;
    psarray->param.nbasepage = nvertpage;
    psarray->param.scans_per_page = scan_page;
    psarray->param.base_per_page = base_page;
    psarray->param.base_sep = vsep;
    psarray->param.scan_sep = hsep;
    psarray->param.tagsize = 0.4 + (sepmin / 40.0);
    psarray->param.xgb_fract = GUARD_BAND / vsep;
    psarray->param.ygb_fract = GUARD_BAND / hsep;

    return (0);
    }
