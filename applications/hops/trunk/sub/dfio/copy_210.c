/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_210().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t210            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 8 March 2000 by CJL                                          */
/* version 2                        2010.1.5  rjc                       */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_210.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_210 (struct type_210 *t210,
          char **ptr)
    {
    int version;
    int i;
    struct type_210_v0 *t210_v0;
    struct type_210_v1 *t210_v1;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t210->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T210_VERSION) *ptr = (char *)t210;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_210_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_210()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_210()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t210_v0 = (struct type_210_v0 *) *ptr;
        strncpy (t210_v0->record_id, "210", 3);
        strncpy (t210_v0->version_no, "00", 2);
        for (i=0; i<16; i++)
            {
            cp_float (t210_v0->amp_phas[i].ampl, t210->amp_phas[i].ampl);
            cp_float (t210_v0->amp_phas[i].phase, t210->amp_phas[i].phase);
            }

        return (sizeof (struct type_210_v0));
        }
    else if (version == 1)
        {
        t210_v1 = (struct type_210_v1 *) *ptr;
        strncpy (t210_v1->record_id, "210", 3);
        strncpy (t210_v1->version_no, "01", 2);
        for (i=0; i<64; i++)
            {
            cp_float (t210_v1->amp_phas[i].ampl, t210->amp_phas[i].ampl);
            cp_float (t210_v1->amp_phas[i].phase, t210->amp_phas[i].phase);
            }

        return (sizeof (struct type_210_v1));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_210()", 2, version);
        return (-1);
        }
    }
