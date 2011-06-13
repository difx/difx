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
/* Added version 1 16 March 2000 by CJL                                 */
/* Added version 2   2010.1.5    rjc                                    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_207.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_207 *
addr_207 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_207 *t207;
    struct type_207_v0 *t207_v0;
    struct type_207_v1 *t207_v1;
    struct type_207_v2 *t207_v2;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T207_VERSION) t207 = (struct type_207 *)address;
    else
        {
        t207 = (struct type_207 *)malloc (sizeof (struct type_207));
        if (t207 == NULL)
            {
            msg ("Memory allocation failure in addr_207()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t207);
        clear_207 (t207);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_207_v0);
        t207_v0 = (struct type_207_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t207->record_id, "207", 3);
        strncpy (t207->version_no, "00", 2);
        for (i=0; i<16; i++)
            {
            cp_float (t207->ref_pcamp[i].lsb, t207_v0->ref_pcamp[i].lsb);
            cp_float (t207->ref_pcamp[i].usb, t207_v0->ref_pcamp[i].usb);
            cp_float (t207->rem_pcamp[i].lsb, t207_v0->rem_pcamp[i].lsb);
            cp_float (t207->rem_pcamp[i].usb, t207_v0->rem_pcamp[i].usb);
            cp_float (t207->ref_pcphase[i].lsb, t207_v0->ref_pcphase[i].lsb);
            cp_float (t207->ref_pcphase[i].usb, t207_v0->ref_pcphase[i].usb);
            cp_float (t207->rem_pcphase[i].lsb, t207_v0->rem_pcphase[i].lsb);
            cp_float (t207->rem_pcphase[i].usb, t207_v0->rem_pcphase[i].usb);
            cp_float (t207->ref_pcfreq[i].lsb, t207_v0->ref_pcfreq[i].lsb);
            cp_float (t207->ref_pcfreq[i].usb, t207_v0->ref_pcfreq[i].usb);
            cp_float (t207->rem_pcfreq[i].lsb, t207_v0->rem_pcfreq[i].lsb);
            cp_float (t207->rem_pcfreq[i].usb, t207_v0->rem_pcfreq[i].usb);
            cp_float (t207->ref_errate[i], t207_v0->ref_errate[i]);
            cp_float (t207->rem_errate[i], t207_v0->rem_errate[i]);
            }
        cp_float (t207->ref_pcrate, t207_v0->ref_pcrate);
        cp_float (t207->rem_pcrate, t207_v0->rem_pcrate);

        return (t207);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_207_v1);
        t207_v1 = (struct type_207_v1 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t207->record_id, "207", 3);
        strncpy (t207->version_no, "01", 2);
        cp_int (t207->pcal_mode, t207_v1->pcal_mode);
        for (i=0; i<16; i++)
            {
            cp_float (t207->ref_pcamp[i].lsb, t207_v1->ref_pcamp[i].lsb);
            cp_float (t207->ref_pcamp[i].usb, t207_v1->ref_pcamp[i].usb);
            cp_float (t207->rem_pcamp[i].lsb, t207_v1->rem_pcamp[i].lsb);
            cp_float (t207->rem_pcamp[i].usb, t207_v1->rem_pcamp[i].usb);
            cp_float (t207->ref_pcphase[i].lsb, t207_v1->ref_pcphase[i].lsb);
            cp_float (t207->ref_pcphase[i].usb, t207_v1->ref_pcphase[i].usb);
            cp_float (t207->rem_pcphase[i].lsb, t207_v1->rem_pcphase[i].lsb);
            cp_float (t207->rem_pcphase[i].usb, t207_v1->rem_pcphase[i].usb);
            cp_float (t207->ref_pcoffset[i].lsb, t207_v1->ref_pcoffset[i].lsb);
            cp_float (t207->ref_pcoffset[i].usb, t207_v1->ref_pcoffset[i].usb);
            cp_float (t207->rem_pcoffset[i].lsb, t207_v1->rem_pcoffset[i].lsb);
            cp_float (t207->rem_pcoffset[i].usb, t207_v1->rem_pcoffset[i].usb);
            cp_float (t207->ref_pcfreq[i].lsb, t207_v1->ref_pcfreq[i].lsb);
            cp_float (t207->ref_pcfreq[i].usb, t207_v1->ref_pcfreq[i].usb);
            cp_float (t207->rem_pcfreq[i].lsb, t207_v1->rem_pcfreq[i].lsb);
            cp_float (t207->rem_pcfreq[i].usb, t207_v1->rem_pcfreq[i].usb);
            cp_float (t207->ref_errate[i], t207_v1->ref_errate[i]);
            cp_float (t207->rem_errate[i], t207_v1->rem_errate[i]);
            }
        cp_float (t207->ref_pcrate, t207_v1->ref_pcrate);
        cp_float (t207->rem_pcrate, t207_v1->rem_pcrate);

        return (t207);
        }
    else if (version == 2)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_207_v2);
        t207_v2 = (struct type_207_v2 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t207->record_id, "207", 3);
        strncpy (t207->version_no, "02", 2);
        cp_int (t207->pcal_mode, t207_v2->pcal_mode);
        for (i=0; i<64; i++)
            {
            cp_float (t207->ref_pcamp[i].lsb, t207_v2->ref_pcamp[i].lsb);
            cp_float (t207->ref_pcamp[i].usb, t207_v2->ref_pcamp[i].usb);
            cp_float (t207->rem_pcamp[i].lsb, t207_v2->rem_pcamp[i].lsb);
            cp_float (t207->rem_pcamp[i].usb, t207_v2->rem_pcamp[i].usb);
            cp_float (t207->ref_pcphase[i].lsb, t207_v2->ref_pcphase[i].lsb);
            cp_float (t207->ref_pcphase[i].usb, t207_v2->ref_pcphase[i].usb);
            cp_float (t207->rem_pcphase[i].lsb, t207_v2->rem_pcphase[i].lsb);
            cp_float (t207->rem_pcphase[i].usb, t207_v2->rem_pcphase[i].usb);
            cp_float (t207->ref_pcoffset[i].lsb, t207_v2->ref_pcoffset[i].lsb);
            cp_float (t207->ref_pcoffset[i].usb, t207_v2->ref_pcoffset[i].usb);
            cp_float (t207->rem_pcoffset[i].lsb, t207_v2->rem_pcoffset[i].lsb);
            cp_float (t207->rem_pcoffset[i].usb, t207_v2->rem_pcoffset[i].usb);
            cp_float (t207->ref_pcfreq[i].lsb, t207_v2->ref_pcfreq[i].lsb);
            cp_float (t207->ref_pcfreq[i].usb, t207_v2->ref_pcfreq[i].usb);
            cp_float (t207->rem_pcfreq[i].lsb, t207_v2->rem_pcfreq[i].lsb);
            cp_float (t207->rem_pcfreq[i].usb, t207_v2->rem_pcfreq[i].usb);
            cp_float (t207->ref_errate[i], t207_v2->ref_errate[i]);
            cp_float (t207->rem_errate[i], t207_v2->rem_errate[i]);
            }
        cp_float (t207->ref_pcrate, t207_v2->ref_pcrate);
        cp_float (t207->rem_pcrate, t207_v2->rem_pcrate);

        return (t207);
        }
    else 
        {
        msg ("Unrecognized type 207 record version number %d", 2, version);
        if (malloced) 
            {
            free (t207);
            msg ("Freed memory block %d", -1, t207);
            }
        return (NULL);
        }
    }
