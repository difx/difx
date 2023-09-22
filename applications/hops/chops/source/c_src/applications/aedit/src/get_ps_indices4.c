/************************************************************************/
/*                                                                      */
/* This routine does the intricate job of figuring out which data array */
/* element belongs to which ps array element, and filling in the latter */
/*                                                                      */
/*      Inputs:         data            Main data array                 */
/*                      psarray         default filled ps array         */
/*                                                                      */
/*      Output:         psarray         Filled with pointers to data    */
/*                                                                      */
/* Adopted from Mk3 version of 18 February 1993 by CJL                  */
/* Rewritten and vastly simplified for Mk4, 25 Jan 2001 by CJL          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "psplot.h"
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int get_ps_indices4 (esum *data, struct ps_array *psarray)
    {
    int i, j, found, nbaselines, nscans;
    int base, scan, nbands, band;
    char id[3], stn1, stn2, fgroup;
    fringearray *fdata, *fdatum;
    fringesum *datum;
    extern int fscan;

    fdata = data->fdata;
    nscans = psarray->nscans;
    nbaselines = psarray->nbaselines;
                                        /* Loop over all the data */
    for (i=0; i<fscan; i++)
        {
        fdatum = fdata + i;
        datum = &(fdatum->data);
        if (fdatum->flag != 0) continue;
                                        /* search psarray for it by scan_id */
        for (scan=0; scan<psarray->nscans; scan++)
            if (strcmp (psarray->time[scan].scan_name, datum->scan_id) == 0) break;
        if (scan == psarray->nscans)
            {
            msg ("Error in get_ps_indices(), could not find a scan", 2);
            msg ("Scan = %s, baseline = %s", 2, datum->scan_id, datum->baseline);
            continue;
            }

                                        /* Now identify baseline */
        stn1 = datum->baseline[0];
        stn2 = datum->baseline[1];
        found = FALSE;
        for (j=0; j<nbaselines; j++)
            {
            strcpy (id, psarray->baseline[j].id);
            if ((stn1 != id[0]) && (stn1 != id[1])) continue;
            if ((stn2 != id[0]) && (stn2 != id[1])) continue;
                                        /* Got it */
            base = j;
            found = TRUE;
            break;
            }
        if (! found)
            {
            /* this is only an error if everything was correlated */
            msg ("baseline %s not found for scan %s", 0,
                 datum->baseline, datum->scan_id);
            continue;
            }
                                        /* Figure out band for this datum */
        fgroup = datum->freq_code;
        nbands = strlen (psarray->subgroups);
        for (j=0; j<nbands; j++)
            if (fgroup == psarray->subgroups[j]) break;
        if (j == nbands)
            {
            msg ("Error in get_ps_indices4(), fgroup '%c' not found in vex file",
                                                2, fgroup);
            continue;
            }
        band = j;
                                        /* Do it! */
        psarray->baseline[base].scan[scan].data_index[band] = i;
        }

    return (0);
    }
