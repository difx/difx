/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_221().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status   */
/*                                                                      */
/*      Inputs:         t221            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Rewritten 23 November 1999 by CJL                                    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_221.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define TRUE 1

int
copy_221 (struct type_221 *t221,
          char **ptr,
          int *alloced)
    {
    int n8, version, size_v0, pplot_len, clen, padded;
    struct type_221_v0 *t221_v0;
    char workspace[32784], *compressed;
                                        /* How big is it? */
    size_v0 = sizeof (struct type_221_v0) - 1 + t221->ps_length;
    pplot_len = t221->ps_length;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t221->version_no, "%2d", &version);
                                        /* This needs work for type 221 */
    if (version != T221_VERSION)
        {
        msg ("Version control not yet implemented for type 221 record", 2);
        return (-1);
        }

    if (version == 0)
        {
                                        /* Compress the postscript plot */
                                        /* Result guaranteed no longer than this */
        compressed = (char *)malloc (pplot_len + 4);
        if (compressed == NULL)
            {
            msg ("Memory allocation failure in copy_221()", 2);
            return (-1);
            }
        compress_compress (workspace, t221->pplot, pplot_len, compressed, &clen);
                                        /* Allocate space for output record */
        size_v0 = sizeof (struct type_221_v0) - 1 + clen;
                                        /* Make sure it's multiple of 8 bytes */
        if ((size_v0 % 8) != 0)
            {
            n8 = size_v0 / 8;
            size_v0 = 8 * (n8 + 1);
            }
        *ptr = (char *)malloc (size_v0);
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_221()", 2);
            return (-1);
            }
        *alloced = TRUE;
                                        /* Fill it in */
        t221_v0 = (struct type_221_v0 *) *ptr;
        strncpy (t221_v0->record_id, "221", 3);
        strncpy (t221_v0->version_no, "00", 2);
        padded = 1;
        cp_short (t221_v0->padded, padded);
        cp_int (t221_v0->ps_length, clen);
        memcpy (t221_v0->pplot, compressed, clen);
                                        /* Tidy up */
        free (compressed);

        return (size_v0);
        }
    else
        {
        msg ("Unrecognized version number %d in copy_221()", 2, version);
        return (-1);
        }
    }
