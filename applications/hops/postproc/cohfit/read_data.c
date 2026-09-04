/************************************************************************/
/*                                                                      */
/* This routine is responsible for reading data into the internal       */
/* memory of cohfit.  The routine also takes care of                     */
/* automatically expanding the data array if it gets full, using a      */
/* standard UNIX memory allocation package                              */
/*                                                                      */
/*      Inputs:         data            Data array                      */
/*                      filename        Name of file to read            */
/*      Output:         return value    0 for success, -1 for failure   */
/*                                                                      */
/* Created October 5 1995 by CJL, borrowed from average and simplified  */
/* Hacked up April 26 2026 by GBC                                       */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "mk4_afio.h"
#include "mk4_util.h"
#include "cohfit.h"

int read_data (avgspace *avgp, char *filename, int *navg, int *nfails)
    {
    static int first = TRUE;
    FILE *fp;
    struct stat statbuf;
    int nkb, mixed_type, other_type, nbadtype, nfail, version, type, pret;
    char line[512];
                                        /* Open up input file */
    if (strcmp (filename, "stdin") == 0) fp = stdin;
    else if (stat (filename, &statbuf) != 0) 
        {
        if (errno == ENOENT) msg ("File '%s' does not exist", 3, filename);
        else msg ("Problem accessing file '%s'", 3, filename);
        return (-1);
        }

    else if ((fp = fopen (filename,"r")) == NULL) 
        {
        msg ("Problem opening '%s'", 3, filename);
        return (-1);
        }
                                        /* Read through entire file */
    msg ("Reading data from file '%s' ...", 1, filename);
    nbadtype = nfail = 0;
    mixed_type = FALSE;
    while (fgets (line, 511, fp) != NULL) 
        {
                                        /* only want type 2 lines */
        if (afile_comment(line)) continue;
                                        /* What type of line is this? */
                                        /* Handle case of old format */
        aline_id (line, &version, &type);
                                        /* Better be 0, 1, 2, 3 or 4 */
        if (type != 2)
            {
            msg ("Found line other than type-2, skipping", 2);
            continue;
            }
                                        /* call parser */
        other_type = FALSE;
        pret = parse_fsumm (line, &((avgp->data)[*navg].fdata));

                                        /* Count parsing failures */
        if (pret != 0)
            {
            msg ("line parse failed, pret = %d", -1, pret);
            msg ("line text was: %s", -2, line);
            nfail++;
            continue;
            }
        if ((avgp->data)[*navg].fdata.datatype[0] != 'J')
            {
            msg ("line is not incoherently scan-averaged data", 1);
            nfail++;
            continue;
            }
        if ((avgp->data)[*navg].fdata.amp == 0.0 &&
            (avgp->data)[*navg].fdata.snr == 0.0)
            {
            msg ("line failed in average() (amp == snr == 0.0)", 1);
            nfail++;
            continue;
            }
                                        /* Valid record, expand the */
                                        /* array if it is necessary */
        *navg += 1;
        if (*navg == avgp->space)
            {
            avgp->space += 100;
            avgp->data = (avg_data *)realloc(
                avgp->data, avgp->space*sizeof(avg_data));
            if (avgp->data == NULL)
                {
                perror("realloc");
                msg("Fatal error allocating memory for data - Abort!", 3);
                exit(1);
                }
            nkb = (avgp->space*sizeof(avg_data))/1024;
            msg ("Expanded array memory to %d Kb ...", 0, nkb);
            }

        }                               /* End of main read loop */

    nkb = (avgp->space*sizeof(avg_data))/1024;
    msg ("Total avg data space occupied is = %d Kb", 1, nkb);

    if ((*nfails = nfail) > 0)
        msg ("Warning, %d lines failed to parse in '%s' (ignored)", 2, 
                        nfail, filename);

    if (strcmp (filename, "stdin") != 0) fclose(fp);
    return(0);
    }
/*
 * eof vim:nospell
 */
