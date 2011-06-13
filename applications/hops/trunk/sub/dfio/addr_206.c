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
/* Expanded to 64 channels        rjc  2010.1.5                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_206.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_206 *
addr_206 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_206 *t206;
    struct type_206_v0 *t206_v0;
    struct type_206_v1 *t206_v1;
    struct type_206_v2 *t206_v2;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T206_VERSION) t206 = (struct type_206 *)address;
    else
        {
        t206 = (struct type_206 *)malloc (sizeof (struct type_206));
        if (t206 == NULL)
            {
            msg ("Memory allocation failure in addr_206()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t206);
        clear_206 (t206);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_206_v0);
        t206_v0 = (struct type_206_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t206->record_id, "206", 3);
        strncpy (t206->version_no, "00", 2);
        cp_short (t206->start.year, t206_v0->start.year);
        cp_short (t206->start.day, t206_v0->start.day);
        cp_short (t206->start.hour, t206_v0->start.hour);
        cp_short (t206->start.minute, t206_v0->start.minute);
        cp_float (t206->start.second, t206_v0->start.second);
        cp_short (t206->first_ap, t206_v0->first_ap);
        cp_short (t206->last_ap, t206_v0->last_ap);
        for (i=0; i<16; i++)
            {
            cp_short (t206->accepted[i].lsb, t206_v0->accepted[i].lsb);
            cp_short (t206->accepted[i].usb, t206_v0->accepted[i].usb);
            }
        cp_float (t206->intg_time, t206_v0->intg_time);
        cp_float (t206->accept_ratio, t206_v0->accept_ratio);
        cp_float (t206->discard, t206_v0->discard);
        for (i=0; i<16; i++)
            {
            cp_short (t206->reason1[i].lsb, t206_v0->reason1[i].lsb);
            cp_short (t206->reason1[i].usb, t206_v0->reason1[i].usb);
            cp_short (t206->reason2[i].lsb, t206_v0->reason2[i].lsb);
            cp_short (t206->reason2[i].usb, t206_v0->reason2[i].usb);
            cp_short (t206->reason3[i].lsb, t206_v0->reason3[i].lsb);
            cp_short (t206->reason3[i].usb, t206_v0->reason3[i].usb);
            cp_short (t206->reason4[i].lsb, t206_v0->reason4[i].lsb);
            cp_short (t206->reason4[i].usb, t206_v0->reason4[i].usb);
            cp_short (t206->reason5[i].lsb, t206_v0->reason5[i].lsb);
            cp_short (t206->reason5[i].usb, t206_v0->reason5[i].usb);
            cp_short (t206->reason6[i].lsb, t206_v0->reason6[i].lsb);
            cp_short (t206->reason6[i].usb, t206_v0->reason6[i].usb);
            cp_short (t206->reason7[i].lsb, t206_v0->reason7[i].lsb);
            cp_short (t206->reason7[i].usb, t206_v0->reason7[i].usb);
            cp_short (t206->reason8[i].lsb, t206_v0->reason8[i].lsb);
            cp_short (t206->reason8[i].usb, t206_v0->reason8[i].usb);
            }
        cp_short (t206->ratesize, t206_v0->ratesize);
        cp_short (t206->mbdsize, t206_v0->mbdsize);
        cp_short (t206->sbdsize, t206_v0->sbdsize);

        return (t206);
        }

    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_206_v1);
        t206_v1 = (struct type_206_v1 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t206->record_id, "206", 3);
        strncpy (t206->version_no, "01", 2);
        cp_short (t206->start.year, t206_v1->start.year);
        cp_short (t206->start.day, t206_v1->start.day);
        cp_short (t206->start.hour, t206_v1->start.hour);
        cp_short (t206->start.minute, t206_v1->start.minute);
        cp_float (t206->start.second, t206_v1->start.second);
        cp_short (t206->first_ap, t206_v1->first_ap);
        cp_short (t206->last_ap, t206_v1->last_ap);
        for (i=0; i<16; i++)
            {
            cp_short (t206->accepted[i].lsb, t206_v1->accepted[i].lsb);
            cp_short (t206->accepted[i].usb, t206_v1->accepted[i].usb);
            cp_double (t206->weights[i].lsb, t206_v1->weights[i].lsb);
            cp_double (t206->weights[i].usb, t206_v1->weights[i].usb);
            }
        cp_float (t206->intg_time, t206_v1->intg_time);
        cp_float (t206->accept_ratio, t206_v1->accept_ratio);
        cp_float (t206->discard, t206_v1->discard);
        for (i=0; i<16; i++)
            {
            cp_short (t206->reason1[i].lsb, t206_v1->reason1[i].lsb);
            cp_short (t206->reason1[i].usb, t206_v1->reason1[i].usb);
            cp_short (t206->reason2[i].lsb, t206_v1->reason2[i].lsb);
            cp_short (t206->reason2[i].usb, t206_v1->reason2[i].usb);
            cp_short (t206->reason3[i].lsb, t206_v1->reason3[i].lsb);
            cp_short (t206->reason3[i].usb, t206_v1->reason3[i].usb);
            cp_short (t206->reason4[i].lsb, t206_v1->reason4[i].lsb);
            cp_short (t206->reason4[i].usb, t206_v1->reason4[i].usb);
            cp_short (t206->reason5[i].lsb, t206_v1->reason5[i].lsb);
            cp_short (t206->reason5[i].usb, t206_v1->reason5[i].usb);
            cp_short (t206->reason6[i].lsb, t206_v1->reason6[i].lsb);
            cp_short (t206->reason6[i].usb, t206_v1->reason6[i].usb);
            cp_short (t206->reason7[i].lsb, t206_v1->reason7[i].lsb);
            cp_short (t206->reason7[i].usb, t206_v1->reason7[i].usb);
            cp_short (t206->reason8[i].lsb, t206_v1->reason8[i].lsb);
            cp_short (t206->reason8[i].usb, t206_v1->reason8[i].usb);
            }
        cp_short (t206->ratesize, t206_v1->ratesize);
        cp_short (t206->mbdsize, t206_v1->mbdsize);
        cp_short (t206->sbdsize, t206_v1->sbdsize);

        return (t206);
        }
    else if (version == 2)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_206_v2);
        t206_v2 = (struct type_206_v2 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t206->record_id, "206", 3);
        strncpy (t206->version_no, "02", 2);
        cp_short (t206->start.year, t206_v2->start.year);
        cp_short (t206->start.day, t206_v2->start.day);
        cp_short (t206->start.hour, t206_v2->start.hour);
        cp_short (t206->start.minute, t206_v2->start.minute);
        cp_float (t206->start.second, t206_v2->start.second);
        cp_short (t206->first_ap, t206_v2->first_ap);
        cp_short (t206->last_ap, t206_v2->last_ap);
        for (i=0; i<64; i++)
            {
            cp_short (t206->accepted[i].lsb, t206_v2->accepted[i].lsb);
            cp_short (t206->accepted[i].usb, t206_v2->accepted[i].usb);
            cp_double (t206->weights[i].lsb, t206_v2->weights[i].lsb);
            cp_double (t206->weights[i].usb, t206_v2->weights[i].usb);
            }
        cp_float (t206->intg_time, t206_v2->intg_time);
        cp_float (t206->accept_ratio, t206_v2->accept_ratio);
        cp_float (t206->discard, t206_v2->discard);
        for (i=0; i<16; i++)
            {
            cp_short (t206->reason1[i].lsb, t206_v2->reason1[i].lsb);
            cp_short (t206->reason1[i].usb, t206_v2->reason1[i].usb);
            cp_short (t206->reason2[i].lsb, t206_v2->reason2[i].lsb);
            cp_short (t206->reason2[i].usb, t206_v2->reason2[i].usb);
            cp_short (t206->reason3[i].lsb, t206_v2->reason3[i].lsb);
            cp_short (t206->reason3[i].usb, t206_v2->reason3[i].usb);
            cp_short (t206->reason4[i].lsb, t206_v2->reason4[i].lsb);
            cp_short (t206->reason4[i].usb, t206_v2->reason4[i].usb);
            cp_short (t206->reason5[i].lsb, t206_v2->reason5[i].lsb);
            cp_short (t206->reason5[i].usb, t206_v2->reason5[i].usb);
            cp_short (t206->reason6[i].lsb, t206_v2->reason6[i].lsb);
            cp_short (t206->reason6[i].usb, t206_v2->reason6[i].usb);
            cp_short (t206->reason7[i].lsb, t206_v2->reason7[i].lsb);
            cp_short (t206->reason7[i].usb, t206_v2->reason7[i].usb);
            cp_short (t206->reason8[i].lsb, t206_v2->reason8[i].lsb);
            cp_short (t206->reason8[i].usb, t206_v2->reason8[i].usb);
            }
        cp_short (t206->ratesize, t206_v2->ratesize);
        cp_short (t206->mbdsize, t206_v2->mbdsize);
        cp_short (t206->sbdsize, t206_v2->sbdsize);

        return (t206);
        }
    else 
        {
        msg ("Unrecognized type 206 record version number %d", 2, version);
        if (malloced) 
            {
            free (t206);
            msg ("Freed memory block %d", -1, t206);
            }
        return (NULL);
        }
    }
