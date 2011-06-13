/************************************************************************/
/*                                                                      */
/* This routine constructs a fringesum structure from filled root and   */
/* fringe structures passed to it.                                      */
/*                                                                      */
/*      Inputs:         fringe          data_fringe structure (full)    */
/*                      root            data_root structure (full)      */
/*                      fname           filename (for extent # and      */
/*                                      other checks )                  */
/*                                                                      */
/*      Output:         fsumm           fringesum structure             */
/*                                                                      */
/* Created 5 October 1992 by CJL                                        */
/* Simplified version from fringex use, CJL July 14 1995                */
/* Adapted for use with the Mk4 file system     rjc  2007.9.28          */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "mk4_data.h"
#include "adata.h"
#include "vex.h"
#include "fringex.h"

int fill_aline (struct mk4_fringe *fringe,
                struct vex *root,
                char *fname,
                fringesum *fsumm)
    {
    int i, j, lastslash, p_extent, lastchan, sb;
    int pcal1, pcal2, pcal3, pcal4, filetype;
    char buf[7], c, baseline[3];
    extern int output_version, dofrnge, dofourfit;
                                        /* Initialize output record */
    clear_fsumm (fsumm);
    fsumm->version = CURRENT_VERSION;
                                        /* Get experiment number, scan name/id */
                                        /* the scan time, and the recording mode */
    fsumm->expt_no = fringe->t200->expt_no;
    strncpy (fsumm->scan_id, fringe->t200->scan_name, 32);

    fsumm->time_tag = time_to_int ((int)fringe->t200->scantime.year,
                                   (int)fringe->t200->scantime.day,
                                   (int)fringe->t200->scantime.hour,
                                   (int)fringe->t200->scantime.minute,
                                   (int)fringe->t200->scantime.second);
    fsumm->scan_offset = fringe->t200->start_offset;
                                        /* Duration overwritten in fringex.c */
                                        /* if segmented shorter than scan */
    fsumm->duration = (fringe->t206->last_ap - fringe->t206->first_ap + 1) 
                    * root->evex->ap_length;
                                        // following statement has unclear relevance
                                        // in the mk4 world    rjc  2009.9.28
    // fsumm->mode = root->base[basenum].t2000[0].record_mode[1];

    fsumm->archiv = fsumm->expt_no;     /* Unix convention */
    strcpy (fsumm->fname, "unix ");
                                        /* Decode as much as possible from filename */
    i = 0;                              /* then cross-check file contents for */
    lastslash = 0;                      /* consistency */

    while ((c = fname[i++]) != '\0')
        if (c == '/') 
            lastslash = i;

    if (sscanf (fname+lastslash, "%2s.%c.%hd.%6s", baseline, 
                &(fsumm->freq_code), &(fsumm->extent_no), fsumm->root_id) != 4)
        {
        msg ("Could not decode filename %s", 2, fname+lastslash);
        return (-1);
        }
    else 
        strncpy (fsumm->baseline, baseline, 2);

    if (strncmp (fsumm->baseline, fringe->t202->baseline, 2) != 0)
        {
        msg ("File %s actually contains baseline %2s", 2, fname, fringe->t202->baseline);
        return (-1);
        }

    if (fsumm->freq_code != fringe->t203->channels[0].ref_chan_id[0])
        {
        msg ("File %s actually contains freq code '%c'", 
             2, fname, fringe->t203->channels[0].ref_chan_id[0]);
        return (-1);
        }
                                        // there are no parent extents in mk4, as there
                                        // is only one type 1 per type 2
    for (i=0; i<4; i++)
        fsumm->parents[i] = 0;
                                        /* Mostly just tedious details from here on ... */
                                        /* making sure we get right struct elements */
    fsumm->corel_vers = 'a';            // there is no corel version in mk4
                        
    fsumm->procdate = time_to_int ((int)fringe->t200->fourfit_date.year,
                                   (int)fringe->t200->fourfit_date.day,
                                   (int)fringe->t200->fourfit_date.hour,
                                   (int)fringe->t200->fourfit_date.minute,
                                   (int)fringe->t200->fourfit_date.second);
    strncpy (fsumm->source, fringe->t201->source, 8);
    fsumm->source[8] = '\0';
    fsumm->quality = fringe->t208->quality;
    fsumm->errcode = fringe->t208->errcode;
    fsumm->polarization[0] = fringe->t203->channels[0].refpol;
    fsumm->polarization[1] = fringe->t203->channels[0].rempol;
    fsumm->polarization[2] = '\0';
                                    // Count frequencies from type 205
    for (i=0; i<NFX_SB_64; i++) 
        if (fringe->t205->ffit_chan[i].ffit_chan_id == ' ')
            break;
    fsumm->no_freq = i;

    fsumm->lags = fringe->t202->nlags;

    strncpy (fsumm->reftape, fringe->t202->ref_tape, 8);
    fsumm->reftape[8] = '\0';
    strncpy (fsumm->remtape, fringe->t202->rem_tape, 8);
    fsumm->remtape[8] = '\0';
    fsumm->sbdelay = fringe->t208->resid_sbd;
    fsumm->mbdelay = fringe->t208->resid_mbd;
    fsumm->delay_rate = fringe->t208->resid_rate * 1.0E6;
                                        /* Convert this to integer */
    strncpy (buf, fringe->t208->tape_qcode, 6);
    buf[6] = '\0';
    sscanf (buf, "%d", &(fsumm->esdesp));
    fsumm->total_rate = fringe->t208->tot_rate;
    fsumm->total_mbdelay = fringe->t208->tot_mbd;
    fsumm->total_sbresid = fringe->t208->tot_sbd- fsumm->total_mbdelay;
    fsumm->ambiguity = fringe->t208->ambiguity;

    fsumm->ref_elev = fringe->t202->ref_elev;
    fsumm->rem_elev = fringe->t202->rem_elev;
    fsumm->ref_az   = fringe->t202->ref_az;
    fsumm->rem_az   = fringe->t202->rem_az;

    return (0);
    }
