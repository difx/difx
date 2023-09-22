/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.                                         */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                      size            True size of structure (bytes)  */
/*                                                                      */
/*      Output:         Return value    Address of filled app structure */
/*                                                                      */
/* Created 3 January 1997 by CJL                                        */
/* Revised 13 March 1998 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_230.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_230 *
addr_230 ( short version,
           void *address,
           int *size)
    {
    int malloced, i, size_230, xpow_len;
    short *dummy, nspec_pts;
    char *caddress;
    struct type_230 *t230;
    struct type_230_v0 *t230_v0;
    double rpart, ipart;
                                        /* Need number of lags */
                                        /* up front */
    caddress = (char *)address;
    dummy = (short *)address;
    cp_short (nspec_pts, dummy[3]);
                                        /* Size of variable length array */
    xpow_len = 16 * nspec_pts;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T230_VERSION) t230 = (struct type_230 *)address;
    else
        {
        size_230 = sizeof (struct type_230) - sizeof (hops_scomplex) + xpow_len;
        t230 = (struct type_230 *) malloc (size_230);
        if (t230 == NULL)
            {
            msg ("Memory allocation failure in addr_230()", 2);
            return (NULL);
            }
        clear_230 (t230);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually.  First overlay */
                                        /* a version-specific structure, */
                                        /* then copy structure elements */
    if (version == 0)
        {
                                        /* Must calculate true structure size */
                                        /* since sizeof() doesn't know we are */
                                        /* tricking compiler with variable numbers */
                                        /* of raw data blocks */
        *size = sizeof (struct type_230_v0) - sizeof ( hops_scomplex) + xpow_len;
        t230_v0 = (struct type_230_v0 *)address;
                                        /* Start copying structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t230->record_id, "230", 3);
        strncpy (t230->version_no, "00", 2);
        cp_short (t230->nspec_pts, t230_v0->nspec_pts);
/*         strncpy (t230->rootcode, t230_v0->rootcode, 6); */
        cp_int (t230->frq, t230_v0->frq);
        cp_int (t230->ap, t230_v0->ap);
        cp_float (t230->usbweight, t230_v0->usbweight);
        cp_float (t230->lsbweight, t230_v0->lsbweight);
        for (i=0; i<nspec_pts; i++)
            {                           // complex copy
            cp_double (rpart, t230_v0->xpower[i].real);
            cp_double (ipart, t230_v0->xpower[i].imag);
            t230->xpower[i].real = rpart;
            t230->xpower[i].imag = ipart;
            }
        return (t230);
        }
    else
        {
        msg ("Unrecognized type 230 record version number %d", 2, version);
        if (malloced) free (t230);
        return (NULL);
        }
    }
