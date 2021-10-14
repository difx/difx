/************************************************************************/
/*                                                                      */
/* This is the main routine which parses a vex file down to the def     */
/* level.  The information thus parsed is then utilized to fulfill      */
/* specific application-level requests, such as filling a structure     */
/* with all the information pertaining to a particular scan.            */
/*                                                                      */
/*      Inputs:         filename        Name of the vex file on disk    */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created 23 October 1998 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"

int
parse_vexfile (char *filename)
    {
    FILE *fp;

    if (vex_init() != 0)
        {
        msg ("Vex initialization failed", 2);
        return (-1);
        }
                                        /* Open file for reading */
    if ((fp = fopen (filename, "r")) == NULL)
        {
        msg ("Failed to open file '%s'", 2, filename);
        return (-1);
        }
                                        /* Read in the file */
    if (read_file (fp) < 0)
        {
        msg ("Failure in read_file()", 2);
        fclose (fp);
        return (-1);
        }
    fclose (fp);
                                        /* Find comments and quotes */
    if (locate_cq () != 0)
        {
        msg ("Failure in locatecq()", 2);
        return (-1);
        }
                                        /* Get all sanitized statements */
    if (find_statements () < 0)
        {
        msg ("Failure in find_statements()", 2);
        return (-1);
        }
                                        /* Find $BLOCK statements from list */
    if (locate_blocks () < 0)
        {
        msg ("Failure in locate_blocks()", 2);
        return (-1);
        }
                                        /* Check that statements are in */
                                        /* appropriate $blocks */
    if (check_stloc () != 0)
        {
        msg ("Failure in check_stloc()", 2);
        return (-1);
        }
                                        /* Find all the defs */
    if (fill_deflists () != 0)
        {
        msg ("Failure in fill_deflists()", 2);
        return (-1);
        }
                                        /* That's all we can do at this point */
                                        /* because further parsing will tend */
                                        /* to be application-specific */
    return (0);
    }
