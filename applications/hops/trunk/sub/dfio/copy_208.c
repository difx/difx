/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_208().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a ptr assignment operation, depending on version control status.  */
/* To handle byte-flipping, the copy operation is now done in all cases */
/* which allows use of the bytflp.h macros                              */
/*                                                                      */
/*      Inputs:         t208            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 25 September 1995 by CJL                                     */
/* Redesigned 23 September 1997 by CJL                                  */
/* Added version 1 support, Feb 14 2000 by CJL                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "bytflp.h"
#include "type_208.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

int
copy_208 (struct type_208 *t208,
          char **ptr)
    {
    int version;
    struct type_208_v0 *t208_v0;
    struct type_208_v1 *t208_v1;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t208->version_no, "%2d", &version);
                                        /* Disk format same as app struct */
                                        /* Simple pointer assignment */
    if (version == T208_VERSION) *ptr = (char *)t208;
    else if (version == 0)
        {
        *ptr = (char *)malloc (sizeof (struct type_208_v0));
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_208()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_208()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t208_v0 = (struct type_208_v0 *) *ptr;
        strncpy (t208_v0->record_id, "208", 3);
        strncpy (t208_v0->version_no, "00", 2);
        t208_v0->quality = t208->quality;
        t208_v0->errcode = t208->errcode;
        strncpy (t208_v0->tape_qcode, t208->tape_qcode, 6);
        cp_double (t208_v0->adelay, t208->adelay);
        cp_double (t208_v0->arate, t208->arate);
        cp_double (t208_v0->aaccel, t208->aaccel);
        cp_double (t208_v0->tot_mbd, t208->tot_mbd);
        cp_double (t208_v0->tot_sbd, t208->tot_sbd);
        cp_double (t208_v0->tot_rate, t208->tot_rate);
                                        /* next 3 lines added by rjc 2000.1.6 */
        cp_double (t208_v0->tot_mbd_ref, t208->tot_mbd_ref);
        cp_double (t208_v0->tot_sbd_ref, t208->tot_sbd_ref);
        cp_double (t208_v0->tot_rate_ref, t208->tot_rate_ref);

        cp_float (t208_v0->resid_mbd, t208->resid_mbd);
        cp_float (t208_v0->resid_sbd, t208->resid_sbd);
        cp_float (t208_v0->resid_rate, t208->resid_rate);
        cp_float (t208_v0->mbd_error, t208->mbd_error);
        cp_float (t208_v0->sbd_error, t208->sbd_error);
        cp_float (t208_v0->rate_error, t208->rate_error);
        cp_float (t208_v0->ambiguity, t208->ambiguity);
        cp_float (t208_v0->amplitude, t208->amplitude);
        cp_float (t208_v0->inc_seg_ampl, t208->inc_seg_ampl);
        cp_float (t208_v0->inc_chan_ampl, t208->inc_chan_ampl);
        cp_float (t208_v0->snr, t208->snr);
        cp_float (t208_v0->prob_false, t208->prob_false);
        cp_float (t208_v0->totphase, t208->totphase);
        cp_float (t208_v0->resphase, t208->resphase);

        return (sizeof (struct type_208_v0));
        }
    else if (version == 1)
        {
        t208_v1 = (struct type_208_v1 *) *ptr;
        strncpy (t208_v1->record_id, "208", 3);
        strncpy (t208_v1->version_no, "01", 2);
        strncpy (t208_v1->unused1, t208->unused1, 3);
        t208_v1->quality = t208->quality;
        t208_v1->errcode = t208->errcode;
        strncpy (t208_v1->tape_qcode, t208->tape_qcode, 6);
        cp_double (t208_v1->adelay, t208->adelay);
        cp_double (t208_v1->arate, t208->arate);
        cp_double (t208_v1->aaccel, t208->aaccel);
        cp_double (t208_v1->tot_mbd, t208->tot_mbd);
        cp_double (t208_v1->tot_sbd, t208->tot_sbd);
        cp_double (t208_v1->tot_rate, t208->tot_rate);
                                        /* next 3 lines added by rjc 2000.1.6 */
        cp_double (t208_v1->tot_mbd_ref, t208->tot_mbd_ref);
        cp_double (t208_v1->tot_sbd_ref, t208->tot_sbd_ref);
        cp_double (t208_v1->tot_rate_ref, t208->tot_rate_ref);

        cp_float (t208_v1->resid_mbd, t208->resid_mbd);
        cp_float (t208_v1->resid_sbd, t208->resid_sbd);
        cp_float (t208_v1->resid_rate, t208->resid_rate);
        cp_float (t208_v1->mbd_error, t208->mbd_error);
        cp_float (t208_v1->sbd_error, t208->sbd_error);
        cp_float (t208_v1->rate_error, t208->rate_error);
        cp_float (t208_v1->ambiguity, t208->ambiguity);
        cp_float (t208_v1->amplitude, t208->amplitude);
        cp_float (t208_v1->inc_seg_ampl, t208->inc_seg_ampl);
        cp_float (t208_v1->inc_chan_ampl, t208->inc_chan_ampl);
        cp_float (t208_v1->snr, t208->snr);
        cp_float (t208_v1->prob_false, t208->prob_false);
        cp_float (t208_v1->totphase, t208->totphase);
        cp_float (t208_v1->totphase_ref, t208->totphase_ref);
        cp_float (t208_v1->resphase, t208->resphase);
        cp_float (t208_v1->tec_error, t208->tec_error);

        return (sizeof (struct type_208_v1));
        }
    else
        {
        msg ("Unrecognized version number %d in copy_208()", 2, version);
        return (-1);
        }
    }
