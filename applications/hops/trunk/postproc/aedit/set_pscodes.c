/************************************************************************/
/*                                                                      */
/* This loops though an otherwise complete ps array, dereferencing the  */
/* data array indices left there by get_ps_indices() and converting     */
/* the quality codes it finds to pgplot colour indices ready to plot.   */
/*                                                                      */
/*      Inputs:         data            Main data array                 */
/*                      psarray         Almost complete ps array        */
/*                                                                      */
/*      Output:         psarray         Completely complete ps array    */
/*                                                                      */
/* Created 18 February 1993 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include "psplot.h"
#include "aedata.h"

int
set_pscodes (data, psarray)
esum *data;
struct ps_array *psarray;
    {
    int i, j, k, q, dindex, ncodes, nbadqcodes, nbands;
    struct psplot_baseline *base;
    struct psplot_cell *cell;
    char qcode;
    fringearray *fdata;
    static char qcodes[] = QUALITIES;

    fdata = data->fdata;

    nbadqcodes = 0;
    nbands = strlen (psarray->subgroups);
                                        /* Loop over all baselines, scans */
    for (i=0; i < psarray->nbaselines; i++)
        {
        base = psarray->baseline + i;
        for (j=0; j < psarray->nscans; j++)
            {
                                        /* Convenience pointer */
            cell = base->scan + j;
            for (k=0; k<nbands; k++)
                {
                if ((dindex = cell->data_index[k]) < 0) continue;
                qcode = mk3_qf (&(fdata[dindex].data));
                                        /* Look for this qcode ... location */
                                        /* in static char array qcodes IS the */
                                        /* desired pgplot colour index */
                ncodes = strlen (qcodes);
                for (q=0; q<ncodes; q++) if (qcode == qcodes[q]) break;

                if (q == ncodes)
                    {
                    nbadqcodes++;
                    continue;
                    }
                else cell->colour_index[k] = q;
                }
            }
        }
                                        /* Lets be informative */
    if (nbadqcodes > 0)
        msg ("Warning: there were %d unrecognized qcodes", 2, nbadqcodes);

    return (0);
    }
    
