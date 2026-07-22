/************************************************************************/
/*                                                                      */
/* Computes averages for one source and one triangle                    */
/*                                                                      */
/*      Inputs:         data            The input data records          */
/*                      tbsum           info on this tribase            */
/*                      conf            program config struct           */
/*                      fpout           Open stream for output          */
/*                                                                      */
/*      Output:                         Output data records             */
/*                      return value    Number of output records        */
/*                                                                      */
/* Created 21 September 1994 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "average.h"
#include "mk4_util.h"
#include "mk4_afio.h"

int
triangle_avg (seg_data *data,
              tbsumm *tbsum,
              struct config conf,
              FILE *fpout)
    {
    int i, recno, startrec, nout, start, timesum[3], end, tdiff, scan_start;
    int timeoff, int_time, index, start_time, end_time, make_avg, new_intg;
    int earliest, latest;
    double sumimag, sumreal, mean_length, mean_timesum, azimuth[3], elevation[3];
    double timeoff_sum, phase_diff, signal_sum, noise_vec, noise_sq_sum;
    double ave_timeoff;
    char current_root[7];
    trianglesum *datum, *tempdatum;
    static trianglesum outdatum;
    static int first = TRUE;
                                        /* Init for first loop */
    int_time = conf.int_time;
    start_time = tbsum->start_time;
    end_time = start_time + int_time;
    if (int_time == 0) end_time += 1000000;
    recno = tbsum->start_index;
    strcpy (current_root, data[data[recno].order].u.tdata.root_id[0]);
    end = tbsum->end_index;
    new_intg = TRUE;
    nout = 0;
                                        /* loop over data segments */
    while (TRUE)
        {
                                        /* Get datum from sorted array */
        if (recno <= end) index = data[recno].order;
        datum = &(data[index].u.tdata);
                                        /* Is it time to compute an average? */
                                        /* End of integration time or a scan */
                                        /* change will do it.  Also, end of this */
                                        /* tribase forces computation */
        make_avg = FALSE;
        if (recno > end) make_avg = TRUE;
        else if ((datum->time_tag + tbsum->seglen) > end_time)
            {
                                        /* Discard segment that spans intgs */
            if (datum->time_tag < end_time) continue;
            make_avg = TRUE;
            }
        else if ((!conf.multiscan) 
                        && (strcmp (datum->root_id[0], current_root) != 0))
                make_avg = TRUE;
                                        /* Do actual averaging computation */
                                        /* and write the resulting record to */
                                        /* the output stream */
        if (make_avg)
            {
                                        /* Overwrite selected fields */
                                        /* First, we compute data-based SNR */
                                        /* from phases (eqn 74 in Rogers etal) */
                                        /* First compute vector avg phase */
            outdatum.bis_phas = ((float) atan2 (sumimag, sumreal)) * 57.2958;
            signal_sum = 0.0;
            noise_sq_sum = 0.0;
                                        /* Then loop over this averaging interval */
            for (i=startrec; i<recno; i++)
                {
                index = data[i].order;
                tempdatum = &(data[index].u.tdata);
                phase_diff = tempdatum->bis_phas - outdatum.bis_phas;
                if (phase_diff < -180.0) phase_diff += 360.0;
                if (phase_diff > 180.0) phase_diff -= 360.0;
                phase_diff /= 57.2958;
                signal_sum += tempdatum->bis_amp * cos (phase_diff);
                noise_vec = tempdatum->bis_amp * sin (phase_diff);
                noise_sq_sum += noise_vec * noise_vec;
                }
            if (recno-startrec == 1) noise_sq_sum = 0.0;
                                        /* SNR of zero means undefined */
            if (noise_sq_sum == 0.0) outdatum.bis_snr = 0.0;
            else outdatum.bis_snr = signal_sum / sqrt (noise_sq_sum);
                                        /* Overwrite other selected fields */
                                        /* Ignore delays and rates */
            outdatum.bis_amp = sqrt (sumimag*sumimag + sumreal*sumreal) / mean_timesum;
            outdatum.time_tag = start_time;
            outdatum.scan_offset = outdatum.time_tag - scan_start;
            outdatum.duration = latest - earliest;
            ave_timeoff = (float)timeoff_sum / (float)mean_timesum + 0.5;
            outdatum.offset = (int)ave_timeoff;
                                        /* Scan averages treated differently */
            if (int_time == 0)
                {
                outdatum.time_tag = scan_start;
                outdatum.scan_offset = 0;
                }
            for (i=0; i<3; i++) 
                {
                outdatum.length[i] = timesum[i];
                outdatum.azimuth[i] = azimuth[i] / mean_timesum;
                outdatum.elevation[i] = elevation[i] / mean_timesum;
                }
                                        /* Write out the result and set up */
                                        /* for next averaging interval */
            if (first)
                {
                outdatum.version = datum->version;
                if (conf.header) afile_header (outdatum.version, 3, fpout);
                first = FALSE;
                }
            write_tsumm (&outdatum, fpout);
            nout++;
                                        /* Flag to set up for next intg. */
            new_intg = TRUE;
                                        /* If we are at the end, exit */
            if (recno > end) break;
            }
                                        /* Initialize accumulators if we */
                                        /* are starting new averaging interval */
        if (new_intg)
            {
            for (i=0; i<3; i++) 
                {
                timesum[i] = 0;
                elevation[i] = 0;
                azimuth[i] = 0;
                }
            timeoff_sum = mean_timesum = 0.0;
            sumimag = sumreal = 0.0;
            startrec = recno;
            strcpy (current_root, datum->root_id[0]);
                                        /* Reset boundaries of integration */
                                        /* This intg. will contain current datum */
            tdiff = datum->time_tag - start_time;
            scan_start = datum->time_tag - datum->scan_offset;
            if (int_time != 0)
                {
                start_time += (tdiff / int_time) * int_time;
                end_time = start_time + int_time;
                }
            else end_time = start_time + 1000000;
                                        /* Keep track of time extrema to simulate */
                                        /* nominal duration field */
            earliest = datum->time_tag;
                                        /* Initialize new output record */
            memcpy (&outdatum, datum, sizeof (trianglesum));
            new_intg = FALSE;
            }
                                        /* Accumulate quantities for this */
                                        /* averaging interval */
                                        /* Mean seg length is used as weight */
        mean_length = (float)(datum->length[0] + datum->length[1] 
                                + datum->length[2]) / 3.0;
                                        /* Sum of weights */
        mean_timesum += mean_length;
                                        /* Sum reals and imaginaries */
                                        /* separately for vector average */
        sumimag += mean_length * datum->bis_amp * sin (datum->bis_phas/57.2958);
        sumreal += mean_length * datum->bis_amp * cos (datum->bis_phas/57.2958);
                                        /* Weighted sum of time tags */
        timeoff = datum->time_tag - start_time + datum->offset;
        if (int_time == 0) timeoff = datum->scan_offset + datum->offset;
        timeoff_sum += mean_length * timeoff;

        latest = datum->time_tag + datum->duration;
                                        /* Keep track of segment lengths for */
                                        /* quality code use later */
                                        /* Also accumulate azimuths/elevations */
        for (i=0; i<3; i++) 
                {
                timesum[i] += datum->length[i];
                azimuth[i] += mean_length * datum->azimuth[i];
                elevation[i] += mean_length * datum->elevation[i];
                }

        recno++;
        }

    return (nout);
    }
