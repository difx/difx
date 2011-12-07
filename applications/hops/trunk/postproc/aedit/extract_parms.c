/************************************************************************/
/*                                                                      */
/* Loops through all unflagged type 2 data in memory, and extracts      */
/* parameters specified by the user from the corresponding type 2 files */
/* on disk.  The array to receive these parameters should already have  */
/* been set up by get_param_list() and allocate_parms().                */
/*                                                                      */
/*      Inputs:         data            can't do anything without this  */
/*                      user_param      specifies what parameters are   */
/*                                      needed.                         */
/*                                                                      */
/*      Output:         user_param      parameter arrays filled in      */
/*                                                                      */
/* Created 11 August 1993 by CJL                                        */
/* deleted mk3 code                        2010.6.8  rjc                */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "aedata.h"
#include "summary.h"
#include "usearray.h"

int
extract_parms (data, user_param)
esum *data;
struct usearray *user_param;
    {
    int i, nbadname, nbadread, nbadfill, fringe_needed, id, mk4;
    char *fname, *fringename(), fullname[256];
    fringearray *fdata;
    static struct mk4_fringe fringe4;
    extern char datadir[];
    extern int fscan, fflag;
                                        /* First see if we need to read the */
                                        /* fringe files at all.  If timetag */
                                        /* or scan central UTC (utc_central) */
                                        /* requested, need summary to convert */
                                        /* to fractional days relative to day 1 */
    fringe_needed = FALSE;
    for (i=0; i<user_param->nparms; i++)
        {
        id = user_param->type[i].parameter_id;
        if ((id > 0) && (id < IN_ALINE)) fringe_needed = TRUE;
        if ((id == TIMETAG) || (id == UTC_CENTRAL)) summ_data (data, STANDARD);
        }
                                        /* Could take a while ... disk IO */
    if (((fscan - fflag) > 1000) && fringe_needed) 
        msg ("Extracting parameters for %d files, be patient", 2, fscan-fflag);

    user_param->npoints = nbadname = nbadread = nbadfill = 0;
    mk4 = -1;
    for (i=0; i<fscan; i++)
        {
        fdata = data->fdata + i;
                                        /* Do it only for unflagged data */
        if (fdata->flag != 0) continue;
                                        /* Identify type 2 file and read it */
                                        /* only if necessary */
        if (fringe_needed)
            {
            if ((fname = fringename (&(fdata->data))) == NULL)
                {
                nbadname++;
                continue;
                }
            sprintf (fullname, "%s/%s", datadir, fname);
                                        /* Is this Mk4 data? */
                                        /* Read in fringe file */
            if (read_mk4fringe (fullname, &fringe4) != 0)
                {
                nbadread++;
                continue;
                }
            }
                                        /* Set pointer in data and extract */
                                        /* parameters */
        fdata->param_ptr = user_param->npoints;
        if (fill4_parms (&fringe4, &(fdata->data), user_param) != 0)
            {
            nbadfill++;
            continue;
            }
                                        /* This one went well, so increment */
                                        /* count */
        user_param->npoints++;
        }
                                        /* Report errors */
    if (nbadname > 0)
        {
        msg ("Warning: %d scans could not be properly associated", 2, nbadname);
        msg ("with type 2 files on disk", 2);
        }
    if (nbadread > 0)
        msg ("Warning: %d type 2 files could not be read from disk", 2, nbadread);
    if (nbadfill > 0)
        {
        msg ("Warning: failed on %d files attempting to extract", 2, nbadfill);
        msg ("requested information from them", 2);
        }
    if (user_param->npoints == 0)
        {
        msg ("Error, unable to extract any parameters for this dataset!", 2);
        return (1);
        }

    msg ("Successfully extracted requested information for %d data records", 
                2, user_param->npoints);

    return (0);
    }
