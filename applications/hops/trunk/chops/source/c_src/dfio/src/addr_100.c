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
/*      Output:         Return value    Address of filled app structure */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_100.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_100 *
addr_100 (short version,
          void *address,
          int *size)
    {
    int malloced;
    struct type_100 *t100;
    struct type_100_v0 *t100_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T100_VERSION) t100 = (struct type_100 *)address;
    else
        {
        t100 = (struct type_100 *)malloc (sizeof (struct type_100));
        if (t100 == NULL)
            {
            msg ("Memory allocation failure in addr_100()", 2);
            return (NULL);
            }
        clear_100 (t100);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually.  First overlay */
                                        /* a version-specific structure, */
                                        /* then copy structure elements */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_100_v0);
        t100_v0 = (struct type_100_v0 *)address;
                                        /* Start copying structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t100->record_id, "100", 3);
        strncpy (t100->version_no, "00", 2);
        cp_short (t100->procdate.year, t100_v0->procdate.year);
        cp_short (t100->procdate.day, t100_v0->procdate.day);
        cp_short (t100->procdate.hour, t100_v0->procdate.hour);
        cp_short (t100->procdate.minute, t100_v0->procdate.minute);
        cp_float (t100->procdate.second, t100_v0->procdate.second);
        if (t100 != t100_v0)
            {
            strncpy (t100->baseline, t100_v0->baseline, 2);
            strcpy (t100->rootname, t100_v0->rootname);
            strncpy (t100->qcode, t100_v0->qcode, 2);
            }
        cp_float (t100->pct_done, t100_v0->pct_done);
        cp_short (t100->start.year, t100_v0->start.year);
        cp_short (t100->start.day, t100_v0->start.day);
        cp_short (t100->start.hour, t100_v0->start.hour);
        cp_short (t100->start.minute, t100_v0->start.minute);
        cp_float (t100->start.second, t100_v0->start.second);
        cp_short (t100->stop.year, t100_v0->stop.year);
        cp_short (t100->stop.day, t100_v0->stop.day);
        cp_short (t100->stop.hour, t100_v0->stop.hour);
        cp_short (t100->stop.minute, t100_v0->stop.minute);
        cp_float (t100->stop.second, t100_v0->stop.second);
        cp_int (t100->ndrec, t100_v0->ndrec);
        cp_int (t100->nindex, t100_v0->nindex);
        cp_short (t100->nlags, t100_v0->nlags);
        cp_short (t100->nblocks, t100_v0->nblocks);

        return (t100);
        }
    else 
        {
        msg ("Unrecognized type 100 record version number %d", 2, version);
        if (malloced) free (t100);
        return (NULL);
        }
    }
