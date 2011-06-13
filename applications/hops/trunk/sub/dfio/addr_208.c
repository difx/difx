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
/* Added version 1 support, Feb 14 2000 by CJL                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_208.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_208 *
addr_208 (short version,
          void *address,
          int *size)
    {
    int malloced;
    struct type_208 *t208;
    struct type_208_v0 *t208_v0;
    struct type_208_v1 *t208_v1;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T208_VERSION) t208 = (struct type_208 *)address;
    else
        {
        t208 = (struct type_208 *)malloc (sizeof (struct type_208));
        if (t208 == NULL)
            {
            msg ("Memory allocation failure in addr_208()", 2);
            return (NULL);
            }
        msg ("Allocated memory block %d", -1, t208);
        clear_208 (t208);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_208_v0);
        t208_v0 = (struct type_208_v0 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t208->record_id, "208", 3);
        strncpy (t208->version_no, "00", 2);
        t208->quality = t208_v0->quality;
        t208->errcode = t208_v0->errcode;
        strncpy (t208->tape_qcode, t208_v0->tape_qcode, 6);
        cp_double (t208->adelay, t208_v0->adelay);
        cp_double (t208->arate, t208_v0->arate);
        cp_double (t208->aaccel, t208_v0->aaccel);
        cp_double (t208->tot_mbd, t208_v0->tot_mbd);
        cp_double (t208->tot_sbd, t208_v0->tot_sbd);
        cp_double (t208->tot_rate, t208_v0->tot_rate);
                                        /* next 3 lines added by rjc 2000.1.6 */
        cp_double (t208->tot_mbd_ref, t208_v0->tot_mbd_ref);
        cp_double (t208->tot_sbd_ref, t208_v0->tot_sbd_ref);
        cp_double (t208->tot_rate_ref, t208_v0->tot_rate_ref);

        cp_float (t208->resid_mbd, t208_v0->resid_mbd);
        cp_float (t208->resid_sbd, t208_v0->resid_sbd);
        cp_float (t208->resid_rate, t208_v0->resid_rate);
        cp_float (t208->mbd_error, t208_v0->mbd_error);
        cp_float (t208->sbd_error, t208_v0->sbd_error);
        cp_float (t208->rate_error, t208_v0->rate_error);
        cp_float (t208->ambiguity, t208_v0->ambiguity);
        cp_float (t208->amplitude, t208_v0->amplitude);
        cp_float (t208->inc_seg_ampl, t208_v0->inc_seg_ampl);
        cp_float (t208->inc_chan_ampl, t208_v0->inc_chan_ampl);
        cp_float (t208->snr, t208_v0->snr);
        cp_float (t208->prob_false, t208_v0->prob_false);
        cp_float (t208->totphase, t208_v0->totphase);
        cp_float (t208->resphase, t208_v0->resphase);

        return (t208);
        }
    else if (version == 1)
        {
                                        /* Overlay version-specific structure */
                                        /* noting size so we can maintain */
                                        /* pointer in file image */
        *size = sizeof (struct type_208_v1);
        t208_v1 = (struct type_208_v1 *)address;
                                        /* Copy structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t208->record_id, "208", 3);
        strncpy (t208->version_no, "01", 2);
        t208->quality = t208_v1->quality;
        t208->errcode = t208_v1->errcode;
        strncpy (t208->tape_qcode, t208_v1->tape_qcode, 6);
        cp_double (t208->adelay, t208_v1->adelay);
        cp_double (t208->arate, t208_v1->arate);
        cp_double (t208->aaccel, t208_v1->aaccel);
        cp_double (t208->tot_mbd, t208_v1->tot_mbd);
        cp_double (t208->tot_sbd, t208_v1->tot_sbd);
        cp_double (t208->tot_rate, t208_v1->tot_rate);
                                        /* next 3 lines added by rjc 2000.1.6 */
        cp_double (t208->tot_mbd_ref, t208_v1->tot_mbd_ref);
        cp_double (t208->tot_sbd_ref, t208_v1->tot_sbd_ref);
        cp_double (t208->tot_rate_ref, t208_v1->tot_rate_ref);

        cp_float (t208->resid_mbd, t208_v1->resid_mbd);
        cp_float (t208->resid_sbd, t208_v1->resid_sbd);
        cp_float (t208->resid_rate, t208_v1->resid_rate);
        cp_float (t208->mbd_error, t208_v1->mbd_error);
        cp_float (t208->sbd_error, t208_v1->sbd_error);
        cp_float (t208->rate_error, t208_v1->rate_error);
        cp_float (t208->ambiguity, t208_v1->ambiguity);
        cp_float (t208->amplitude, t208_v1->amplitude);
        cp_float (t208->inc_seg_ampl, t208_v1->inc_seg_ampl);
        cp_float (t208->inc_chan_ampl, t208_v1->inc_chan_ampl);
        cp_float (t208->snr, t208_v1->snr);
        cp_float (t208->prob_false, t208_v1->prob_false);
        cp_float (t208->totphase, t208_v1->totphase);
        cp_float (t208->totphase_ref, t208_v1->totphase_ref);
        cp_float (t208->resphase, t208_v1->resphase);

        return (t208);
        }
    else 
        {
        msg ("Unrecognized type 208 record version number %d", 2, version);
        if (malloced) 
            {
            free (t208);
            msg ("Freed memory block %d", -1, t208);
            }
        return (NULL);
        }
    }
