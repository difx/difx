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
/* Created 19 April 2001 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_212.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_212 *
addr_212 (short version,
          void *address,
          int *size)
    {
    int i, malloced, good_address, rec_len0, rec_len1;
    char *dummy, *cdummy;
    short nap, idummy;
    struct type_212 *t212;
    struct type_212_v0 *t212_v0;
    struct type_212_v1 *t212_v1;
                                        /* Need number of blocks up front */
                                        /* Following code needed to handle */
                                        /* some April/May 2001 files with */
                                        /* alignment problems resulting from */
                                        /* unpadded type 221 records */
    if (((long int)address % 8) == 0) good_address = TRUE;
    else good_address = FALSE;
    dummy = (char *)(&idummy);
    cdummy = (char *)address;
    dummy[0] = cdummy[6];
    dummy[1] = cdummy[7];
/*     dummy = (short *)address; */
    cp_short (nap, idummy);
    rec_len0 = sizeof (struct type_212) - 12 + 8*nap;
                                        /* Ensure 8-byte boundary */
    rec_len1 = sizeof (struct type_212) - 12 + 12*nap;
    if ((nap % 2) == 1) rec_len1 += 12;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if ((version == T212_VERSION) && (good_address))
                                            t212 = (struct type_212 *)address;
    else
        {
        t212 = (struct type_212 *)malloc (rec_len1);
        if (t212 == NULL)
            {
            msg ("Memory allocation failure in addr_212()", 2);
            return (NULL);
            }
        clear_212 (t212);
        malloced = TRUE;
        }
                                        /* If bad address, copy information over */
                                        /* This, and code below, defeats version */
                                        /* control, but will only execute for */
                                        /* version 0 anyway */
    if (! good_address) memcpy ((char *)t212, address, rec_len1);
                                        /* Handle each version number */
                                        /* individually.  First overlay */
                                        /* a version-specific structure, */
                                        /* then copy structure elements */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */

        *size = rec_len0;
        if (good_address) 
            t212_v0 = (struct type_212_v0 *)address;
        else 
            t212_v0 = (void *) t212;
                                        /* Start copying structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t212->record_id, "212", 3);
        strncpy (t212->version_no, "00", 2);
        cp_short (t212->nap, t212_v0->nap);
        cp_short (t212->channel, t212_v0->channel);
        cp_short (t212->sbd_chan, t212_v0->sbd_chan);
        for (i=0; i<nap; i++)
            {
            cp_float (t212->data[i].amp, t212_v0->data[i].amp);
            cp_float (t212->data[i].phase, t212_v0->data[i].phase);
            t212->data[i].weight = 0.0;
            }

        return (t212);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */

        *size = rec_len1;
        t212_v1 = (struct type_212_v1 *)address;
                                        /* Start copying structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t212->record_id, "212", 3);
        strncpy (t212->version_no, "01", 2);
        cp_short (t212->nap, t212_v1->nap);
        cp_short (t212->channel, t212_v1->channel);
        cp_short (t212->sbd_chan, t212_v1->sbd_chan);
        for (i=0; i<nap; i++)
            {
            cp_float (t212->data[i].amp, t212_v1->data[i].amp);
            cp_float (t212->data[i].phase, t212_v1->data[i].phase);
            cp_float (t212->data[i].weight, t212_v1->data[i].weight);
            }

        return (t212);
        }
    else 
        {
        msg ("Unrecognized type 212 record version number %d", 2, version);
        if (malloced) free (t212);
        return (NULL);
        }
    }
