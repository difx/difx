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
/* added support for version 1 records          2011.10.6   rjc         */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_203.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_203 *
addr_203 (short version,
          void *address,
          int *size)
    {
    int i, malloced;
    struct type_203 *t203;
    struct type_203_v0 *t203_v0;
    struct type_203_v1 *t203_v1;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T203_VERSION) t203 = (struct type_203 *)address;
    else
        {
        t203 = (struct type_203 *)malloc (sizeof (struct type_203));
        if (t203 == NULL)
            {
            msg ("Memory allocation failure in addr_203()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t203);
        clear_203 (t203);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_203_v0);
        t203_v0 = (struct type_203_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t203->record_id, "203", 3);
        strncpy (t203->version_no, "00", 2);
        for (i=0; i<32; i++)
            {
            cp_short (t203->channels[i].index, t203_v0->channels[i].index);
            cp_short (t203->channels[i].sample_rate, t203_v0->channels[i].sample_rate);
            t203->channels[i].refsb = t203_v0->channels[i].refsb;
            t203->channels[i].remsb = t203_v0->channels[i].remsb;
            t203->channels[i].refpol = t203_v0->channels[i].refpol;
            t203->channels[i].rempol = t203_v0->channels[i].rempol;
            cp_double (t203->channels[i].ref_freq, t203_v0->channels[i].ref_freq);
            cp_double (t203->channels[i].rem_freq, t203_v0->channels[i].rem_freq);
            strncpy (t203->channels[i].ref_chan_id, t203_v0->channels[i].ref_chan_id, 8);
            strncpy (t203->channels[i].rem_chan_id, t203_v0->channels[i].rem_chan_id, 8);
            }

        return (t203);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_203_v1);
        t203_v1 = (struct type_203_v1 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t203->record_id, "203", 3);
        strncpy (t203->version_no, "01", 2);
        for (i=0; i<8*MAX_CHAN_PP; i++)
            {
            cp_short (t203->channels[i].index, t203_v1->channels[i].index);
            cp_short (t203->channels[i].sample_rate, t203_v1->channels[i].sample_rate);
            t203->channels[i].refsb = t203_v1->channels[i].refsb;
            t203->channels[i].remsb = t203_v1->channels[i].remsb;
            t203->channels[i].refpol = t203_v1->channels[i].refpol;
            t203->channels[i].rempol = t203_v1->channels[i].rempol;
            cp_double (t203->channels[i].ref_freq, t203_v1->channels[i].ref_freq);
            cp_double (t203->channels[i].rem_freq, t203_v1->channels[i].rem_freq);
            strncpy (t203->channels[i].ref_chan_id, t203_v1->channels[i].ref_chan_id, 8);
            strncpy (t203->channels[i].rem_chan_id, t203_v1->channels[i].rem_chan_id, 8);
            }

        return (t203);
        }
    else 
        {
        msg ("Unrecognized type 203 record version number %d", 2, version);
        if (malloced) 
            {
            free (t203);
            msg ("Freed memory block %d", -1, t203);
            }
        return (NULL);
        }
    }
