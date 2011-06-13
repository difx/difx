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
/*                                                                      */
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "data.h"
#include "adata.h"

#define FRNGE 0
#define FOURFIT 1

int
summarize_fringe (fringe, root, fname, fsumm)
struct data_fringe *fringe;
struct data_root *root;
char *fname;
fringesum *fsumm;
    {
    int i, j, basenum, lastslash, p_extent, lastchan, sb, nparents;
    int pcal1, pcal2, pcal3, pcal4, filetype;
    int t1000_index, btable_index, duration, fullepoch;
    char buf[7], c, baseline[3];
    struct datec tempdate;
    extern int output_version, dofrnge, dofourfit;
                                        /* Is this a FRNGE or fourfit file? */
                                        /* If FRNGE, it's yymmdd, else it's */
                                        /* a version # with a period in it */
                                        /* Skip if necessary */
    filetype = FRNGE;
    for (i=0; i<6; i++)
        if (fringe->t4300.frnge_vers[i] == '.') filetype = FOURFIT;
    if (dofrnge && (filetype == FOURFIT)) return (1);
    if (dofourfit && (filetype == FRNGE)) return (1);
                                        /* Initialize output record */
    clear_fsumm (fsumm);
    fsumm->version = output_version;
                                        /* Need root for experiment number (!) */
                                        /* the scan time, and the recording mode */
    fsumm->expt_no = root->t1000[0].expt_no;
    fsumm->time_tag = time_to_int (root->t1000[0].utc_start.year,
                                   root->t1000[0].utc_start.day_of_year,
                                   root->t1000[0].utc_start.hour,
                                   root->t1000[0].utc_start.minute, 
                                   root->t1000[0].utc_start.second);
    fsumm->ssec = root->t1000[0].utc_start.second;
    fsumm->scan_offset = 0;
                                        /* Baseline-dependent info from root */
    basenum = fringe->t4000.baseline_no;
    t1000_index = (basenum - 1) / 8;
    btable_index = (basenum - 1) % 8;
    fsumm->duration = root->t1000[t1000_index].barray[btable_index].bduration;
    if (fsumm->duration == 0) fsumm->duration = root->t1000[0].duration;
    fsumm->mode = root->base[basenum].t2000[0].record_mode[1];

    fsumm->archiv = fsumm->expt_no;     /* Unix convention */
    strcpy (fsumm->fname, "unix ");
                                        /* Decode as much as possible from filename */
    i = 0;                              /* then cross-check file contents for */
    lastslash = 0;                      /* consistency */
    while ((c = fname[i++]) != NULL)
        if (c == '/') lastslash = i;
    if (sscanf (fname+lastslash, "%2s.%c.%hd.%6s", baseline, 
                &(fsumm->freq_code), &(fsumm->extent_no), fsumm->root_id) != 4)
        {
        msg ("Could not decode filename %s", 2, fname+lastslash);
        return (-1);
        }
    else strncpy (fsumm->baseline, baseline, 2);
    if (strncmp (fsumm->baseline, fringe->t4000.baseline_id, 2) != 0)
        {
        msg ("File %s actually contains baseline %2s", 2, fname, 
                                                fringe->t4000.baseline_id);
        return (-1);
        }
    if (fsumm->freq_code != fringe->t4300.freq_group[0])
        {
        msg ("File %s actually contains freq code '%c'", 2, fname, 
                                        fringe->t4300.freq_group[0]);
        return (-1);
        }
                                        /* Figuring out parent extent requires */
                                        /* checking all channels ... could come */
                                        /* from multiple type-1 files */
    fsumm->no_freq = fringe->t4000.channels;
    nparents = 0;
    for (i=0; i<fsumm->no_freq; i++)
        {
                                        /* Check both sidebands, rsb can be empty */
        for (sb = 0; sb < 2; sb++)
            {
            if (sb == 0) p_extent = fringe->t4100.corel_extent[i].rsb;
            else p_extent = fringe->t4100.corel_extent[i].psb;
            if (p_extent == 0) continue;
                                        /* Only add if new */
            for (j=0; j<nparents; j++)
                if (p_extent == fsumm->parents[j]) break;
            if ((j == nparents) && (nparents < 4))
                fsumm->parents[nparents++] = p_extent;
            }
        }
                                        /* Mostly just tedious details from here on ... */
                                        /* making sure we get right struct elements */
    fsumm->length = fringe->t4500.intg_time + 0.5;
    fsumm->corel_vers = 'a' - 1 + fringe->t4000.corel_vers;
                                        /* Yuk, two different date formats */
                                        /* Next 2 lines clean up undefined */
                                        /* seconds field left by FRNGE */
    if (fringe->t4000.utc_frnge.second >= 60) 
        fringe->t4000.utc_frnge.second = 0;
    datef_to_datec (&(fringe->t4000.utc_frnge), &tempdate);
    fsumm->procdate = time_to_int (tempdate.year, tempdate.day_of_year,
                                tempdate.hour, tempdate.minute, tempdate.second);
    strncpy (fsumm->source, fringe->t4300.src_name, 8);
    fsumm->source[8] = '\0';
    fsumm->quality = fringe->t4300.quality_code[1];
    strncpy (fsumm->reftape, fringe->t4300.reftape_id, 8);
    fsumm->reftape[8] = '\0';
    strncpy (fsumm->remtape, fringe->t4300.remtape_id, 8);
    fsumm->remtape[8] = '\0';
    fsumm->amp = fringe->t4500.corr_coeff * 10000.0;
    fsumm->snr = fringe->t4500.snr;
                                        /* Hardcode for Mk3 for now */
    fsumm->lags = 8;
                                        /* All phases are 0-360 */
    fsumm->resid_phas = fmod ((double)(fringe->t4500.resphase_ec + 360.0), 360.0);
    fsumm->sbdelay = fringe->t4500.sbd_resid;
    fsumm->mbdelay = fringe->t4500.resid_delay;
    fsumm->delay_rate = fringe->t4500.resid_rate * 1.0E6;
                                        /* Convert this to integer */
    strncpy (buf, fringe->t4300.tape_qcode, 6);
    buf[6] = '\0';
    if (sscanf (buf, "%d", &(fsumm->esdesp)) != 1)
        msg ("Failed to decode tape qcode for %s", 2, fname);

    fsumm->epoch[0] = fringe->t4000.utc_tag.minute;
    fsumm->epoch[1] = fringe->t4000.utc_tag.second;
    datef_to_datec (&(fringe->t4000.utc_tag), &tempdate);
    fullepoch = time_to_int (tempdate.year, tempdate.day_of_year, tempdate.hour, 
                        tempdate.minute, tempdate.second);
    fsumm->offset = fullepoch - fsumm->time_tag - fringe->t4500.epoch_offset;

    fsumm->ref_freq = fringe->t4400.ref_freq;
    fsumm->total_phas = fmod ((double)(fringe->t4500.totphase_ec + 360.0), 360.0);
    fsumm->total_rate = fringe->t4400.rate_obs;
    fsumm->total_mbdelay = fringe->t4400.delay_obs;
    fsumm->total_sbresid = fringe->t4400.nb_delay - fsumm->total_mbdelay;
    fsumm->ambiguity = fringe->t4400.gd_ambig;
                                        /* Extract relevant phasecal info */
                                        /* Need to convert to degrees */
    lastchan = fringe->t4000.channels - 1;
    pcal1 = fringe->t4100.pcal_data[0].refphase;
    pcal2 = fringe->t4100.pcal_data[lastchan].refphase;
    pcal3 = fringe->t4100.pcal_data[0].remphase;
    pcal4 = fringe->t4100.pcal_data[lastchan].remphase;
    pcal1 = (pcal1 + 36050) / 100;      /* This rounds to nearest integer, */
    pcal2 = (pcal2 + 36050) / 100;      /* and adds 360 to ensure +ve */
    pcal3 = (pcal3 + 36050) / 100;
    pcal4 = (pcal4 + 36050) / 100;
    fsumm->pcals[0] = pcal1 % 360;
    fsumm->pcals[1] = pcal2 % 360;
    fsumm->pcals[2] = pcal3 % 360;
    fsumm->pcals[3] = pcal4 % 360;
                                        /* Added for support of version 2 */
    strcpy (fsumm->datatype, "Sf");
    fsumm->ref_elev = fringe->t4500.elevation.ref;
    fsumm->rem_elev = fringe->t4500.elevation.rem;
    fsumm->u = fringe->t4500.fr_asec_ew * 0.2062648;
    fsumm->v = fringe->t4500.fr_asec_ns * 0.2062648;
                                        /* Awaits fourfit support */
                                        /* Kludged into 4400 record, 4/18/94 CJL */
    fsumm->ref_az = fringe->t4400.azimuth[0];
    fsumm->rem_az = fringe->t4400.azimuth[1];

    return (0);
    }
