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
/*  version 1                   2010.1.5  rjc                           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_205.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_205 *
addr_205 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_205 *t205;
    struct type_205_v0 *t205_v0;
    struct type_205_v1 *t205_v1;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T205_VERSION) t205 = (struct type_205 *)address;
    else
        {
        t205 = (struct type_205 *)malloc (sizeof (struct type_205));
        if (t205 == NULL)
            {
            msg ("Memory allocation failure in addr_205()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t205);
        clear_205 (t205);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_205_v0);
        t205_v0 = (struct type_205_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t205->record_id, "205", 3);
        strncpy (t205->version_no, "00", 2);
        cp_short (t205->utc_central.year, t205_v0->utc_central.year);
        cp_short (t205->utc_central.day, t205_v0->utc_central.day);
        cp_short (t205->utc_central.hour, t205_v0->utc_central.hour);
        cp_short (t205->utc_central.minute, t205_v0->utc_central.minute);
        cp_float (t205->utc_central.second, t205_v0->utc_central.second);
        cp_float (t205->offset, t205_v0->offset);
        strncpy (t205->ffmode, t205_v0->ffmode, 8);
        for (i=0; i<6; i++) cp_float (t205->search[i], t205_v0->search[i]);
        for (i=0; i<8; i++) cp_float (t205->filter[i], t205_v0->filter[i]);
        cp_short (t205->start.year, t205_v0->start.year);
        cp_short (t205->start.day, t205_v0->start.day);
        cp_short (t205->start.hour, t205_v0->start.hour);
        cp_short (t205->start.minute, t205_v0->start.minute);
        cp_float (t205->start.second, t205_v0->start.second);
        cp_short (t205->stop.year, t205_v0->stop.year);
        cp_short (t205->stop.day, t205_v0->stop.day);
        cp_short (t205->stop.hour, t205_v0->stop.hour);
        cp_short (t205->stop.minute, t205_v0->stop.minute);
        cp_float (t205->stop.second, t205_v0->stop.second);
        cp_double (t205->ref_freq, t205_v0->ref_freq);
        for (i=0; i<16; i++)
            {
            t205->ffit_chan[i].ffit_chan_id = t205_v0->ffit_chan[i].ffit_chan_id;
            cp_short (t205->ffit_chan[i].channels[0], t205_v0->ffit_chan[i].channels[0]);
            cp_short (t205->ffit_chan[i].channels[1], t205_v0->ffit_chan[i].channels[1]);
            cp_short (t205->ffit_chan[i].channels[2], t205_v0->ffit_chan[i].channels[2]);
            cp_short (t205->ffit_chan[i].channels[3], t205_v0->ffit_chan[i].channels[3]);
            }

        return (t205);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_205_v1);
        t205_v1 = (struct type_205_v1 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t205->record_id, "205", 3);
        strncpy (t205->version_no, "01", 2);
        cp_short (t205->utc_central.year, t205_v1->utc_central.year);
        cp_short (t205->utc_central.day, t205_v1->utc_central.day);
        cp_short (t205->utc_central.hour, t205_v1->utc_central.hour);
        cp_short (t205->utc_central.minute, t205_v1->utc_central.minute);
        cp_float (t205->utc_central.second, t205_v1->utc_central.second);
        cp_float (t205->offset, t205_v1->offset);
        strncpy (t205->ffmode, t205_v1->ffmode, 8);
        for (i=0; i<6; i++) cp_float (t205->search[i], t205_v1->search[i]);
        for (i=0; i<8; i++) cp_float (t205->filter[i], t205_v1->filter[i]);
        cp_short (t205->start.year, t205_v1->start.year);
        cp_short (t205->start.day, t205_v1->start.day);
        cp_short (t205->start.hour, t205_v1->start.hour);
        cp_short (t205->start.minute, t205_v1->start.minute);
        cp_float (t205->start.second, t205_v1->start.second);
        cp_short (t205->stop.year, t205_v1->stop.year);
        cp_short (t205->stop.day, t205_v1->stop.day);
        cp_short (t205->stop.hour, t205_v1->stop.hour);
        cp_short (t205->stop.minute, t205_v1->stop.minute);
        cp_float (t205->stop.second, t205_v1->stop.second);
        cp_double (t205->ref_freq, t205_v1->ref_freq);
        for (i=0; i<64; i++)
            {
            t205->ffit_chan[i].ffit_chan_id = t205_v1->ffit_chan[i].ffit_chan_id;
            cp_short (t205->ffit_chan[i].channels[0], t205_v1->ffit_chan[i].channels[0]);
            cp_short (t205->ffit_chan[i].channels[1], t205_v1->ffit_chan[i].channels[1]);
            cp_short (t205->ffit_chan[i].channels[2], t205_v1->ffit_chan[i].channels[2]);
            cp_short (t205->ffit_chan[i].channels[3], t205_v1->ffit_chan[i].channels[3]);
            }

        return (t205);
        }
    else 
        {
        msg ("Unrecognized type 205 record version number %d", 2, version);
        if (malloced) 
            {
            free (t205);
            msg ("Freed memory block %d", -1, t205);
            }
        return (NULL);
        }
    }
