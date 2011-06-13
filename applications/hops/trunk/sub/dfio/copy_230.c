/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_230().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a simple pointer assignment operation, depending on version       */
/* control status                                                       */
/*                                                                      */
/*      Inputs:         t230            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 3 January 1997 by CJL                                        */
/* Modified 13 March 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_230.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_230 (struct type_230 *t230,
          char **ptr)
    {
    short nspec_pts;
    int i, size, version, xpow_len;
    struct type_230_v0 *t230_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t230->version_no, "%2d", &version);
                                        /* Used for size calculations */
    nspec_pts = t230->nspec_pts;
                                        /* Size of variable length array */
    xpow_len = 16 * nspec_pts;
                                        /* Disk format same as app struct, */
                                        /* simple pointer assignment. */
    if (version == T230_VERSION) *ptr = (char *)t230;
    else if (version == 0)
        {
        size = sizeof (struct type_230_v0) - sizeof (complex) + xpow_len;
        *ptr = (char *)malloc (size);
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_230()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_230()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t230_v0 = (struct type_230_v0 *) *ptr;
        strncpy (t230_v0->record_id, "230", 3);
        strncpy (t230_v0->version_no, "00", 2);
        cp_short (t230_v0->nspec_pts, t230->nspec_pts);
/*         strncpy (t230_v0->rootcode, t230->rootcode, 6); */
        cp_int (t230_v0->frq, t230->frq);
        cp_int (t230_v0->ap, t230->ap);
        cp_float (t230_v0->usbweight, t230->usbweight);
        cp_float (t230_v0->lsbweight, t230->lsbweight);
        for (i=0; i<nspec_pts; i++)
            {
            cp_double (t230_v0->xpower[i].re, t230->xpower[i].re);
            cp_double (t230_v0->xpower[i].im, t230->xpower[i].im);
            }
        size = sizeof (struct type_230_v0) - sizeof (complex) + xpow_len;
        return (size);
        }
    else
        {
        msg ("Unrecognized version number %d in copy_230()", 2, version);
        return (-1);
        }
    }
