/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.  Either way, byte flipping is performed */
/* as necessary by the architecture-dependent macros cp_xxxx() defined  */
/* in bytflp.h                                                          */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                                                                      */
/*      Output:         size            number of bytes read from input */
/*                                      address                         */
/*                      Return value    Address of filled app structure */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 November 1999 by CJL                                   */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_221.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_221 *
addr_221 (short version,
          void *address,
          int *size)
    {
    int clen, dlen, reclen, n8;
    char *decompressed, workspace[32784];
    struct type_221 *t221;
    struct type_221_v0 *t221_v0;
                                        /* This needs work for 221 records ... */
    if (version != T221_VERSION)
        {
        msg ("Type 221 records not yet supported by version control", 2);
        return (NULL);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
        t221_v0 = (struct type_221_v0 *)address;
                                        /* Get compressed postscript plot length, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        cp_int (clen, t221_v0->ps_length);
                                        /* Allocate space for decompressed plot */
                                        /* Assume compression never exceeds factor 8 */
        decompressed = (char *)malloc (clen * 8);
        msg ("Allocated memory block %d", -1, decompressed);
                                        /* Decompress into new pplot location */
        compress_decompress (workspace, t221_v0->pplot, clen, decompressed, &dlen);
                                        /* Allocate output record, allow for null term. */
                                        /* Should condition this to be multiple of 8? */
        reclen = sizeof (struct type_221) - 1 + dlen + 1;
        t221 = (struct type_221 *)malloc (reclen);
        msg ("Allocated memory block %d", -1, t221);
                                        /* Fill in the record */
        strncpy (t221->record_id, "221", 3);
        strncpy (t221->version_no, "00", 2);
        cp_short (t221->padded, t221_v0->padded);
        t221->ps_length = dlen + 1;
        memcpy (t221->pplot, decompressed, dlen + 1);
                                        /* Null-terminate */
        t221->pplot[dlen] = '\0';
                                        /* Tidy up */
        free (decompressed);
        msg ("Freed memory block %d", -1, decompressed);
                                        /* Note input bytes so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_221_v0) - 1 + clen;
                                        /* Size is padded to multiple of 8 bytes */
                                        /* but some files from Apr/May '01 are not */
        msg ("Type 221 padding flag = %d", -1, t221->padded);
        if (t221->padded != 0)
            {
            if ((*size % 8) != 0)
                {
                n8 = *size / 8;
                *size = 8 * (n8 + 1);
                }
            }

        return (t221);
        }
    else 
        {
        msg ("Unrecognized type 221 record version number %d", 2, version);
        return (NULL);
        }
    }
