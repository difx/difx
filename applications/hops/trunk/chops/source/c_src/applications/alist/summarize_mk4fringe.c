/************************************************************************/
/*                                                                      */
/* This routine constructs a fringesum structure from a filled fringe   */
/* structure passed to it.                                              */
/*                                                                      */
/*      Inputs:         mk4fringe       mk4_fringe structure (full)     */
/*                                                                      */
/*      Output:         fsumm           fringesum structure             */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 10 September 1999 by CJL                                     */
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "mk4_data.h"
#include "mk4_afio.h"
#include "mk4_util.h"

int
summarize_mk4fringe (struct mk4_fringe *fr, fringesum *fsumm)
    {
    int i, j, basenum, lastslash, p_extent, lastchan, sb, nparents, frac;
    int pcal1, pcal2, pcal3, pcal4, filetype, polerr, chan;
    int btable_index, duration, fullepoch, rem, isec, nchans;
    char fname[40], buf[7], c, baseline[3], refpol, rempol;
    struct date tempdate;
    extern int output_version;
                                        /* Initialize output record */
    clear_fsumm (fsumm);
    fsumm->version = output_version;
    if (fsumm->version < 5) fsumm->version = 5;
                                        /* Need root for experiment number (!) */
                                        /* the scan time, and the recording mode */
    fsumm->expt_no = fr->t200->expt_no;
    isec = fr->t200->scantime.second + 0.5;
    fsumm->time_tag = time_to_int (fr->t200->scantime.year,
                                   fr->t200->scantime.day,
                                   fr->t200->scantime.hour,
                                   fr->t200->scantime.minute, 
                                   isec);
    fsumm->scan_offset = 0;
    strncpy (fsumm->scan_id, fr->t200->scan_name, 31);
                                        /* Nominal scan duration for this baseline */
    fsumm->duration = fr->t200->stop_offset - fr->t200->start_offset;

    fsumm->archiv = fsumm->expt_no;     /* Unix convention */
    strcpy (fsumm->fname, "unix ");
                                        /* Decode as much as possible from filename */
    strcpy (fname, fr->id->name);
    i = 0;                              /* then cross-check file contents for */
    lastslash = 0;                      /* consistency */
    while ((c = fname[i++]) != 0)
        if (c == '/') lastslash = i;
    if (sscanf (fname+lastslash, "%2s.%c.%hd.%6s", baseline, 
                &(fsumm->freq_code), &(fsumm->extent_no), fsumm->root_id) != 4)
        {
        msg ("Could not decode filename %s", 2, fname+lastslash);
        return (-1);
        }
    else strncpy (fsumm->baseline, baseline, 2);
    if (strncmp (fsumm->baseline, fr->t202->baseline, 2) != 0)
        {
        msg ("File %s actually contains baseline %2s", 2, fname, 
                                                fr->t202->baseline);
        return (-1);
        }
    nchans = (strcmp (fr->t205->version_no, "00") == 0) ? 16 : 64;
                                        /* Count frequencies from type 205 */
    for (i=0; i<nchans; i++) 
        if (fr->t205->ffit_chan[i].ffit_chan_id == ' ') break;
    fsumm->no_freq = i;
                                        /* fgroup from fourfit, in pass struct, */
                                        /* 0th element of pass_data array, should */
                                        /* be in 203 or 205 record??  Ignore for */
                                        /* now and rely on filename */
/*     if (fsumm->freq_code != fringe->t4300.freq_group[0]) */
/*         { */
/*         msg ("File %s actually contains freq code '%c'", 2, fname,  */
/*                                         fringe->t4300.freq_group[0]); */
/*         return (-1); */
/*         } */
                                        /* Figure out polarization from type 205/203 */
    refpol = rempol = ' ';
    polerr = FALSE;
    for (i=0; i<nchans; i++)
        {
        if (fr->t205->ffit_chan[i].ffit_chan_id == ' ') continue;
        for (j=0; j<4; j++)
            {
            chan = fr->t205->ffit_chan[i].channels[j];
            if (chan < 0) continue;
            if (refpol == ' ') refpol = fr->t203->channels[chan].refpol;
            else if (refpol != fr->t203->channels[chan].refpol) polerr = TRUE;
            if (rempol == ' ') rempol = fr->t203->channels[chan].rempol;
            else if (rempol != fr->t203->channels[chan].rempol) polerr = TRUE;
            }
        }
    if (polerr)
        {
        msg ("Error - mixed polarizations in type 2 file", 2);
        return (-1);
        }
    else
        {
        fsumm->polarization[0] = refpol;
        fsumm->polarization[1] = rempol;
        fsumm->polarization[2] = '\0';
        }
                                        /* Mostly just tedious details from here on ... */
                                        /* making sure we get right struct elements */
    fsumm->length = fr->t206->intg_time + 0.5;
    isec = fr->t200->fourfit_date.second + 0.5;
    fsumm->procdate = time_to_int (fr->t200->fourfit_date.year, 
                                            fr->t200->fourfit_date.day,
                                            fr->t200->fourfit_date.hour, 
                                            fr->t200->fourfit_date.minute, 
                                            isec);
    strncpy (fsumm->source, fr->t201->source, 31);
    fsumm->source[31] = '\0';
    fsumm->quality = fr->t208->quality;
    fsumm->errcode = fr->t208->errcode;
    fsumm->amp = fr->t208->amplitude * 10000.0;
    fsumm->snr = fr->t208->snr;
    fsumm->lags = fr->t202->nlags;
                                        /* All phases are 0-360 */
    fsumm->resid_phas = fmod ((double)(fr->t208->resphase + 360.0), 360.0);
    fsumm->sbdelay = fr->t208->resid_sbd;
    fsumm->mbdelay = fr->t208->resid_mbd;
    fsumm->delay_rate = fr->t208->resid_rate * 1.0E6;
                                        /* Convert this to integer */
    strncpy (buf, fr->t208->tape_qcode, 6);
    buf[6] = '\0';
    if (sscanf (buf, "%d", &(fsumm->esdesp)) != 1)
        msg ("Failed to decode tape qcode for %s", 2, fname);
                                        /* Compute scan length fraction parameter */
    rem = fsumm->esdesp % 10;
    fsumm->esdesp -= rem;
    frac = (10.0 * (double)fsumm->length / (double)fsumm->duration);
    if (frac == 10) frac = 9;
    if (frac < 0) frac = 0;
    fsumm->esdesp += frac;

    fsumm->epoch[0] = fr->t200->frt.minute;
    fsumm->epoch[1] = fr->t200->frt.second;
                                        /* Should do this calc'n in mk4fit */
/*     datef_to_date (&(fringe->t4000.utc_tag), &tempdate); */
/*     fullepoch = time_to_int (tempdate.year, tempdate.day_of_year, tempdate.hour,  */
/*                         tempdate.minute, tempdate.second); */
/*     fsumm->offset = fullepoch - fsumm->time_tag - fringe->t4500.epoch_offset; */
    fsumm->offset = 0;

    fsumm->ref_freq = fr->t205->ref_freq;
    fsumm->total_phas = fmod ((double)(fr->t208->totphase + 360.0), 360.0);
    fsumm->total_rate = fr->t208->tot_rate;
    fsumm->total_mbdelay = fr->t208->tot_mbd;
    fsumm->total_sbresid = fr->t208->tot_sbd - fsumm->total_mbdelay;
    fsumm->ambiguity = fr->t208->ambiguity;
                                        /* Phasecal info goes here*/

                                        /* Added for support of version 2 */
    strcpy (fsumm->datatype, "Sf");
    fsumm->ref_elev = fr->t202->ref_elev;
    fsumm->rem_elev = fr->t202->rem_elev;
    fsumm->ref_az = fr->t202->ref_az;
    fsumm->rem_az = fr->t202->rem_az;
                                        /* Convert to megalambda */
    fsumm->u = fr->t202->u * 0.2062648;
    fsumm->v = fr->t202->v * 0.2062648;

    sexigesimal2hrdeg(&fr->t201->coord, &fsumm->ra_hrs, &fsumm->dec_deg);

    fsumm->resid_delay = fsumm->mbdelay + fsumm->ambiguity *
        floor((fsumm->sbdelay - fsumm->mbdelay) / fsumm->ambiguity + 0.5);

    return (0);
    }
