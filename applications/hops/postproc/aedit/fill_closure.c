/************************************************************************/
/*                                                                      */
/* Performs the actual computation of closure triangle quantities, the  */
/* associated 1-sigma phase error based on SNR, and a series of flags   */
/* which indicate the reliability of the closure numbers.  The end      */
/* product is a filled-in trianglearray element                         */
/*                                                                      */
/*      Inputs:         tptr            Pointer to output structure     */
/*                      triangle        Closure triangle ID (3 chars)   */
/*                      indices         3-element int array containing  */
/*                                      main data array indices of the  */
/*                                      baselines making up this tri    */
/*                                                                      */
/*      Output:         tptr            Filled in structure             */
/*                                                                      */
/* Created 12 February 1992 by CJL                                      */
/* Greatly expanded for full closure support, 6 August 1994 by CJL      */
/*                                                                      */
/************************************************************************/
#include "aedata.h"
#include "close_flags.h"
#include "aedit.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "mk4_afio.h"
#include "mk4_dfio.h"

int fill_closure (trianglearray *tptr, char triangle[4], int indices[3], fringearray *fdata)
    {
    double sign, cphase, crate, csbd, cmbd, snr[3], epochdiff;
    double triple_amp, temp1, temp2, denom, s1sq, s2sq, s3sq;
    int i, j, length, nfreq, epoch, index, ambig[3], icmbd, cotime;
    int scan_offset, offset, totlen, esdesp, es, maxes[6], mines[6];
    int maxlen, minlen, maxfreq, minfreq, maxepoch, minepoch, quality;
    double rfreq, maxrfreq, minrfreq;
    char qcode;
    char *suspectqf = "D01234";
    char *badqf = "ABCEF";
    fringesum *datum;
    extern int force_closure;
                                        /* Initialize */
    cphase = crate = csbd = cmbd = 0.0;
    triple_amp = 1.0;
    scan_offset = offset = totlen = 0;
    maxlen = 0; minlen = 5000;
    maxfreq = 0; minfreq = MAXFREQ;
    maxrfreq = 0.0; minrfreq = 1.0e99;
    maxepoch = 0; minepoch = 3600;
    for (i=0; i<6; i++) 
        {
        maxes[i] = 0;
        mines[i] = 10;
        }
    tptr->reversed = tptr->flag = 0;
    clear_tsumm (&(tptr->data));
    strcpy (tptr->data.triangle, triangle);
    quality = 9;
    cotime = 1000;
                                        /* Loop over the 3 baselines */
    for (i=0; i<3; i++)
        {
                                        /* Fill in index and point to baseline */
        tptr->index[i] = indices[i] % 1000000;
        datum = &(fdata[tptr->index[i]].data);
                                        /* Correct for reversed baselines */
        sign = 1.0;
        if (indices[i] >= 1000000) 
            {
            tptr->reversed &= 1<<i;
            sign = -1.0;
            }
                                        /* Copy in baseline-related quantities */
        strcpy (tptr->data.root_id[i], datum->root_id);
        tptr->data.extent_no[i] = datum->extent_no;
        tptr->data.length[i] = datum->length;
        snr[i] = datum->snr;
                                        /* Record esdesp digit extrema */
        esdesp = datum->esdesp;
        for (j=0; j<6; j++)
            {
            es = esdesp % 10;
            if (es > maxes[j]) maxes[j] = es;
            if (es < mines[j]) mines[j] = es;
            esdesp /= 10;
            }
                                        /* Identify station, and insert az/el */
                                        /* if found in this baseline */
        if (triangle[i] == datum->baseline[0])
            {
            tptr->data.elevation[i] = datum->ref_elev;
            tptr->data.azimuth[i] = datum->ref_az;
            }
        else if (triangle[i] == datum->baseline[1])
            {
            tptr->data.elevation[i] = datum->rem_elev;
            tptr->data.azimuth[i] = datum->rem_az;
            }
                                        /* Accumulate closure phase, rate, delays */
                                        /* bispectral amplitude */
        cphase += (double) (datum->total_phas) * sign;
        crate += (double) (datum->delay_rate) * sign;
        csbd += (double) (datum->sbdelay) * sign;
        cmbd += (double) (datum->mbdelay) * sign;
        triple_amp *= (double) (datum->amp);
                                        /* Unequal mbd ambiguities invalidate cmbd */
        ambig[i] = (int)(datum->ambiguity * 100000.0);
                                        /* Make sure closure quality is no better */
                                        /* than that on any constituent baseline */
                                        /* (Unless it is a zero code due to non */
                                        /* detection for segmented data) */
        qcode = mk3_qf (datum);
        if ((qcode >= 'A') && (qcode <= 'Z')) tptr->data.scan_quality = qcode;
        else if (qcode > '0')
            {
            if (quality > (qcode - '0')) 
                quality = qcode - '0';
            }
        else if (datum->datatype[0] != 'A') quality = 0;
                                        /* Accumulate weighted offsets */
        offset += datum->length * datum->offset;
        scan_offset += datum->length * datum->scan_offset;
        totlen += datum->length;
                                        /* Find shortest coherence time */
        if (datum->srch_cotime < cotime) cotime = datum->srch_cotime;
                                        /* Record various parameters for */
                                        /* possible quality adjustements later */
        if ((length = datum->length) > maxlen) maxlen = length;
        if (length < minlen) minlen = length;
        if ((nfreq = datum->no_freq) > maxfreq) maxfreq = nfreq;
        if (nfreq < minfreq) minfreq = nfreq;
        epoch = 60*datum->epoch[0] + datum->epoch[1];
        if (epoch > maxepoch) maxepoch = epoch;
        if (epoch < minepoch) minepoch = epoch;
        rfreq = datum->ref_freq;
        if (rfreq > maxrfreq) maxrfreq = rfreq;
        if (rfreq < minrfreq) minrfreq = rfreq;
        }
                                        /* Assume nominal durations equal */
    tptr->data.duration = datum->duration;
                                        /* Check for screwed up triangle for mbd */
    if ((ambig[0] != ambig[1]) || (ambig[1] != ambig[2]))
        {
        tptr->data.ambiguity = 0.0;
        cmbd *= 1e3;                    // convert to ns
        }
    else if (ambig[0] != 0.0)
        {
                                        /* Remove multiple mbd ambiguities */
        icmbd = (int)(cmbd * 100000.0);
        icmbd %= ambig[0];
        if (icmbd > ambig[0]/2) icmbd -= ambig[0];
        if (icmbd < -ambig[0]/2) icmbd += ambig[0];
        cmbd = icmbd / 100.0;           /* To get nanoseconds */
        tptr->data.ambiguity = ambig[0];
        }
                                        /* Figure out new esdesp */
    esdesp = 0;
    for (i=5; i>=0; i--)
        esdesp = 10 * esdesp + maxes[i] - mines[i];
                                        /* Write various triangle quantities */
    tptr->data.bis_amp = triple_amp;
                                        /* 1080 addition ensures positivity */
    tptr->data.bis_phas = (float) fmod (cphase + 1080.0, 360.0);
    if (tptr->data.bis_phas > 180.0) tptr->data.bis_phas -= 360.0;      /* -180 to +180 */
    tptr->data.cdelay_rate = crate;
    tptr->data.csbdelay = csbd;
    tptr->data.cmbdelay = cmbd;
    tptr->data.esdesp = esdesp;
                                        /* Weighted mean offsets. rint() rounding */
                                        /* fails on Haystack machine  */
    if (totlen > 0)
        {
        if (offset > 0) tptr->data.offset = (float)offset / (float)totlen - 0.5;
        else tptr->data.offset = (float)offset / (float)totlen - 0.5;
        if (scan_offset > 0)
            tptr->data.scan_offset = (float)scan_offset / (float)totlen + 0.5;
        else tptr->data.scan_offset = (float)scan_offset / (float)totlen - 0.5;
        }
    else tptr->data.offset = tptr->data.scan_offset = 0;
                                        /* Calculate bispectrum snr using */
                                        /* rigorous formula from Rogers etal '94 */
                                        /* equation 38 */
    s1sq = snr[0] * snr[0];
    s2sq = snr[1] * snr[1];
    s3sq = snr[2] * snr[2];
    temp1 = (s1sq * s2sq) + (s1sq * s3sq) + (s2sq * s3sq);
    temp2 = s1sq + s2sq + s3sq;
    denom = sqrt (1.0 + temp1 / 4.0 + temp2 / 2.0);
    tptr->data.bis_snr = 0.5 * snr[0] * snr[1] * snr[2] / denom;
                                        /* Set tolerances, and adjust quality factor */
    if (((float)minlen / (float)maxlen) < 0.99) quality--;
    if (((float)minlen / (float)maxlen) < 0.8) quality--;
    if (((float)minfreq / (float)maxfreq) < 0.95) quality--;
    if ((strcmp (tptr->data.root_id[0], tptr->data.root_id[1]) != 0)
        || (strcmp (tptr->data.root_id[0], tptr->data.root_id[2]) != 0)) quality--;
                                        /* Unacceptable error, ignore this point */
    if (! force_closure)
        {
        if (maxepoch != minepoch) 
            {
            msg ("Mismatching epochs, %d,%d, triangle ignored", 2, minepoch, maxepoch);
            return (-1);
            }
        if (maxrfreq != minrfreq) 
            {
            msg ("Mismatching reference frequencies %f,%f, triangle ignored", 2,
                                minrfreq, maxrfreq);
            return (-1);
            }
        }
                                        /* Put in quality factor, previously */
                                        /* inserted letter code overrides */
    if (tptr->data.scan_quality == '?')
        {
        if (quality < 1) quality = 1;
        tptr->data.scan_quality = '0' + quality;
        }
    if (force_closure) tptr->data.scan_quality = 'X';
                                        /* Insert shortest coherence time in */
                                        /* the triangle */
    tptr->data.cotime = cotime;
                                        /* Copy over rest of scan information from */
                                        /* last baseline left in datum */
    tptr->data.version = datum->version;
    if (tptr->data.version < 2) tptr->data.version = 2;         /* By definition */
    tptr->data.expt_no = datum->expt_no;
    tptr->data.time_tag = datum->time_tag;
    strcpy (tptr->data.source, datum->source);
    tptr->data.freq_code = datum->freq_code;
    tptr->data.mode = datum->mode;
    tptr->data.ref_freq = datum->ref_freq;
    tptr->data.epoch[0] = datum->epoch[0];
    tptr->data.epoch[1] = datum->epoch[1];
                                        // added scan_id for v. 5  rjc 2007.11.2
    strcpy (tptr->data.scan_id, datum->scan_id);

    return (0);                         /* No error condition for now */
    }
