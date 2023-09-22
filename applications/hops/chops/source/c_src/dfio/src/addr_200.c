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
/* Redesigned 17 September 1997 by CJL                                  */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_200.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_200 *
addr_200 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_200 *t200;
    struct type_200_v0 *t200_v0;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T200_VERSION) t200 = (struct type_200 *)address;
    else
        {
        t200 = (struct type_200 *)malloc (sizeof (struct type_200));
        if (t200 == NULL)
            {
            msg ("Memory allocation failure in addr_200()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t200);
        clear_200 (t200);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_200_v0);
        t200_v0 = (struct type_200_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t200->record_id, "200", 3);
        strncpy (t200->version_no, "00", 2);
        for (i=0; i<10; i++) 
            cp_short (t200->software_rev[i], t200_v0->software_rev[i]);
        cp_int (t200->expt_no, t200_v0->expt_no);
        strncpy (t200->exper_name, t200_v0->exper_name, 32);
        strncpy (t200->scan_name, t200_v0->scan_name, 32);
        strncpy (t200->correlator, t200_v0->correlator, 8);
        cp_short (t200->scantime.year, t200_v0->scantime.year);
        cp_short (t200->scantime.day, t200_v0->scantime.day);
        cp_short (t200->scantime.hour, t200_v0->scantime.hour);
        cp_short (t200->scantime.minute, t200_v0->scantime.minute);
        cp_float (t200->scantime.second, t200_v0->scantime.second);
        cp_int (t200->start_offset, t200_v0->start_offset);
        cp_int (t200->stop_offset, t200_v0->stop_offset);
        cp_short (t200->corr_date.year, t200_v0->corr_date.year);
        cp_short (t200->corr_date.day, t200_v0->corr_date.day);
        cp_short (t200->corr_date.hour, t200_v0->corr_date.hour);
        cp_short (t200->corr_date.minute, t200_v0->corr_date.minute);
        cp_float (t200->corr_date.second, t200_v0->corr_date.second);
        cp_short (t200->fourfit_date.year, t200_v0->fourfit_date.year);
        cp_short (t200->fourfit_date.day, t200_v0->fourfit_date.day);
        cp_short (t200->fourfit_date.hour, t200_v0->fourfit_date.hour);
        cp_short (t200->fourfit_date.minute, t200_v0->fourfit_date.minute);
        cp_float (t200->fourfit_date.second, t200_v0->fourfit_date.second);
        cp_short (t200->frt.year, t200_v0->frt.year);
        cp_short (t200->frt.day, t200_v0->frt.day);
        cp_short (t200->frt.hour, t200_v0->frt.hour);
        cp_short (t200->frt.minute, t200_v0->frt.minute);
        cp_float (t200->frt.second, t200_v0->frt.second);

        return (t200);
        }
    else 
        {
        msg ("Unrecognized type 200 record version number %d", 2, version);
        if (malloced) 
            {
            free (t200);
            msg ("Freed memory block %d", -1, t200);
            }
        return (NULL);
        }
    }
