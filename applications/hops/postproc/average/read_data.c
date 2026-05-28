/************************************************************************/
/*                                                                      */
/* This routine is responsible for reading data into the internal       */
/* memory of average.  The routine also takes care of                   */
/* automatically expanding the data array if it gets full, using a      */
/* standard UNIX memory allocation package                              */
/*                                                                      */
/*      Inputs:         data            Data array                      */
/*                      fp              Open stream to read             */
/*                                                                      */
/*      Output:         return value    0 for success, -1 for failure   */
/*                                                                      */
/* Created September 9 1994 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "average.h"
#include "mk4_util.h"
#include "mk4_afio.h"

int
read_data (seg_data **data,
           FILE *fp,
           struct config configuration,
           int *nseg)
    {
    extern int space, datatype;
    static int first = TRUE;
    int nkb, mixed_type, other_type, nbadtype, nfail, version, type, pret, ret;
    char line[512];
                                        /* Read through entire file */
    nbadtype = nfail = ret = 0;
    mixed_type = FALSE;
    while (TRUE)
        {
                                        /* Binary data from fringex */
        if (configuration.binary_input)
            {
            datatype = 2;
            if (fread (&((*data)[*nseg].u.fdata), sizeof (fringesum), 1, fp) != 1)
                break;
                                        /* Scan delimiter */
            if ((*data)[*nseg].u.fdata.version < 0) 
                {
                if (configuration.multiscan) continue;
                ret = ENDOFSCAN;
                break;
                }
/*          else if (type != 2)
                {
                msg ("Error reading binary data, type-2 data only allowed", 3);
                return (-1);
                } */
            }
                                        /* Ascii A-file data, needs parsing */
        else
            {
                                        /* Read a line, break on EOF */
            if (fgets (line, 511, fp) == NULL) break;
                                        /* The "*endofscan" comment is */
                                        /* special */
            if (strncmp (line, "*endofscan", 10) == 0 && (! configuration.multiscan))
                {
                ret = ENDOFSCAN;
                break;
                }
                                        /* Ignore comment lines */
            if (afile_comment(line)) continue;
                                        /* What type of line is this? */
                                        /* Handle case of old format */
            aline_id (line, &version, &type);
                                        /* Better be 0, 1, 2, 3 or 4 */
            if ((type < 0) || (type > 4))
                {
                msg ("Found line with incomprehensible format, stopping.", 2);
                break;
                }
                                        /* call proper parser */
            other_type = FALSE;
            switch (type)
                {
                case 2:
                    if (datatype == 3) mixed_type = TRUE;
                    pret = parse_fsumm (line, &((*data)[*nseg].u.fdata));
                    break;

                case 3:
                    if (datatype == 2) mixed_type = TRUE;
                    pret = parse_tsumm (line, &((*data)[*nseg].u.tdata));
                    break;

                case 0:
                case 1:
                case 4:
                    other_type = TRUE;
                    nbadtype++;
                    break;
                default:
                    ;
                }
                                        /* Count parsing failures */
            if (pret != 0)
                {
                msg ("line parse failed, pret = %d type %d datatype %d", 2, pret, type, datatype);
                nfail++;
                continue;
                }
                                        /* Handle screwy data types */
            if (other_type) continue;
            if (mixed_type)
                {
                msg ("Your input data have mixed baseline and triangle records", 3);
                msg ("Average can only handle one at a time.", 3);
                return (-1);
                }
                                        /* First valid record sets data */
                                        /* type for entire run of average */
            if (first)
                {
                datatype = type;
                first = FALSE;
                }
            }
                                        /* Valid record, expand array if necessary */
        *nseg += 1;
        if (*nseg == space)
            {
            space += 100;
            *data = (seg_data *) realloc(*data, space*sizeof(seg_data));
            if (*data == NULL)
                {
                perror("realloc");
                msg("Fatal error allocating memory for data - Abort!", 3);
                exit(1);
                }
            nkb = (space*sizeof(seg_data))/1024;
            printf ("Expanded array memory to %d Kb ...\r", nkb); fflush (stdout);
            }

        }                               /* End of main read loop */

    nkb = (space*sizeof(seg_data))/1024;
    msg ("Total space occupied by data arrays = %d Kb", 0, nkb);
                                        /* Informational only */
    if (nbadtype > 0)
        msg ("Warning, %d type 0, 1 or 4 records (ignored)", 2, nbadtype);
    if (nfail > 0)
        msg ("Warning, %d lines failed to parse (ignored)", 2, nfail);

    return (ret);
    }
