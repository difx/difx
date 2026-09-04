/************************************************************************/
/*                                                                      */
/* Computes averages for one source and one baseline                    */
/*                                                                      */
/*      Inputs:         data            The input data records          */
/*                      tbsum           info on this source/tribase     */
/*                      conf            program config from cmdline     */
/*                      fpout           open output file ptr            */
/*                                                                      */
/*      Output:                         Averaged output records         */
/*                      return value    number of output records        */
/*                                                                      */
/* Created 21 September 1994 by CJL                                     */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "average.h"
#include "mk4_afio.h"

int
baseline_avg (seg_data *data,
              tbsumm *tbsum,
              struct config conf,
              FILE *fpout)
    {
    int nout, recno, nadded, nskips, timesum, timeoff, timeoff_sum, end, omode;
    int scan_start, start_time, end_time, index, make_avg, int_time, new_intg, tdiff;
    int earliest, latest, negamps;
    double usum, vsum, ampsqsum, deltampsq, sumimag, sumreal, ave_timeoff;
    double sigma, inc_sigma, coh_sigma, true_snr;
    double inc_sigma_sum, inc_sigma_sum1, inc_sigma_sum2;
    double coh_sigma_sum, weight, weightsum;
    double refazsum, remazsum, refelsum, remelsum;
    char current_root[7], afcom, omodesrc[2];
    fringesum *datum;
    static fringesum outdatum;
    static int first = TRUE;
                                        /* Init for first loop */
    int_time = conf.int_time;
    start_time = tbsum->start_time;
    end_time = start_time + int_time;
    if (int_time == 0) end_time += 10000;
    recno = tbsum->start_index;
    strcpy (current_root, data[data[recno].order].u.fdata.root_id);
    end = tbsum->end_index;
    new_intg = TRUE;
    negamps = nout = 0;
    msg("BL avg of records %d .. %d", 0, recno, end);
                                        /* loop over data segments */
    while (TRUE)
        {
                                        /* Get datum from sorted array */
        if (recno <= end) index = data[recno].order;
        datum = &(data[index].u.fdata);
                                        /* Check for omode (assumes it doesn't change) */
        if (datum->datatype[0] == 'O') omode = TRUE;
        else omode = FALSE;
        snprintf(omodesrc, 2, "%c", datum->datatype[0]);
                                        /* Is it time to compute an average? */
                                        /* End of integration time or a scan */
                                        /* change will do it.  Also, end of this */
                                        /* tribase forces computation */
        make_avg = FALSE;
        if (recno > end) make_avg = TRUE;
        else if ((datum->time_tag + tbsum->seglen) > end_time) 
                make_avg = TRUE;
        else if ((!conf.multiscan) && (strcmp (datum->root_id, current_root) != 0)) 
                make_avg = TRUE;
                                        /* Do actual averaging computation */
                                        /* and write the resulting record to */
                                        /* the output stream */
        if (make_avg)
            {
                                        /* Overwrite selected fields */
                                        /* Phase is same for coherent and */
                                        /* incoherent */
            if ((sumimag == 0.0) && (sumreal == 0.0))
                outdatum.resid_phas = 0.0;
            else
                outdatum.resid_phas = ((float) atan2 (sumimag, sumreal)) * 57.2958;
                                        /* Incoherent average amp and SNR */
            if (ampsqsum < 0.0)
                {
                ampsqsum = 0.0;
                negamps++;
                }
            outdatum.amp = sqrt (ampsqsum / weightsum);
                                        /* Eqn 17 in appendix of Shep's thesis */
            inc_sigma_sum = outdatum.amp * outdatum.amp * inc_sigma_sum1 + inc_sigma_sum2;
                                        /* In -o mode, factor sqrt(2) enters here */
                                        /* inc_sigma_sum & weightsum both doubled */
                                        /* so inc_sigma down by sqrt(2) */
            inc_sigma = sqrt (inc_sigma_sum) / weightsum;
            if (omode) inc_sigma *= sqrt(2.0);
            outdatum.snr = outdatum.amp * outdatum.amp / (2.0 * inc_sigma);

            /*
             * Originally, the SNR of A^2 was computed, but
             * what is expected here is the SNR of A, which is
             * fixed by the following formula:
             * For testing (and backward compatibility) snrfact<0
             * produces the old results.
             */
            if (conf.snrfact > 0)
                {
                if (outdatum.snr > 0.0)
                    outdatum.snr *= (1.0 + sqrt(1.0 + 1.0 / outdatum.snr));
                else
                    outdatum.snr = 0.0;
                }

            outdatum.datatype[0] = 'I';
                                        /* Coherent average amp and SNR */

            /*
             * conf.coherent appears to always be FALSE and thus
             * the following code block will NEVER be executed.
             */
            if (conf.coherent) 
                {
                outdatum.amp = sqrt(sumimag*sumimag + sumreal*sumreal)/timesum;
                coh_sigma = 1.0 / sqrt (coh_sigma_sum);
                outdatum.snr = sqrt((double)nadded) * outdatum.amp / coh_sigma;
                outdatum.datatype[0] = 'C';
                }
                                        /* Average times, u and v etc */
            outdatum.time_tag = start_time;
            outdatum.scan_offset = outdatum.time_tag - scan_start;
            outdatum.duration = latest - earliest;
                                        /* Scan averages treated differently */
            if (int_time == 0)
                {
                outdatum.time_tag = scan_start;
                outdatum.scan_offset = 0;
                }
                                        /* Next few lines for benefit of cofit */
            if (conf.cofit_output)
                {
                outdatum.duration = tbsum->seglen;
                outdatum.srch_cotime = nadded;
                outdatum.datatype[0] = 'J';
                }
            outdatum.length = timesum;
            ave_timeoff = (float)timeoff_sum / (float)timesum + 0.5;
            outdatum.offset = (int)ave_timeoff;
            outdatum.u = usum / (float)timesum;
            outdatum.v = vsum / (float)timesum;
            outdatum.ref_az = refazsum / (float)timesum;
            outdatum.rem_az = remazsum / (float)timesum;
            outdatum.ref_elev = refelsum / (float)timesum;
            outdatum.rem_elev = remelsum / (float)timesum;
                                        /* Write out the result and set up */
                                        /* for next averaging interval */
            if (first)
                {
                outdatum.version = datum->version;
                if (conf.header)
                    {
                    afile_header (outdatum.version, 2, fpout);
                    afcom = get_afile_com_char();
                    fprintf(fpout, "%c written with SNR-corrected version"
                        " using -s %g, omode was %s (test '%s' == 'O')\n",
                        afcom, conf.snrfact,
                        omode == TRUE ? "TRUE" : "FALSE", omodesrc);
                    }
                first = FALSE;
                }
            write_fsumm (&outdatum, fpout);
            //printf("%.4f   %.4f   %.4f   %.4f\n",outdatum.amp,outdatum.snr,outdatum.mbdelay,outdatum.delay_rate);
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
            nskips = nadded = timesum = timeoff_sum = 0;
            usum = vsum = ampsqsum = sumimag = sumreal = 0.0;
            inc_sigma_sum1 = inc_sigma_sum2 = coh_sigma_sum = weightsum = 0.0;
            refazsum = remazsum = refelsum = remelsum = 0.0;
            strcpy (current_root, datum->root_id);
                                        /* Reset boundaries of integration */
                                        /* This intg. will contain current datum */
            tdiff = datum->time_tag - start_time;
            scan_start = datum->time_tag - datum->scan_offset;
            if (int_time != 0) 
                {
                start_time += (tdiff / int_time) * int_time;
                end_time = start_time + int_time;
                }
            else end_time = start_time + 10000;
                                        /* Keep track of time extrema to simulate */
                                        /* nominal duration field */
            earliest = datum->time_tag;
                                        /* Initialize new output record */
            memcpy (&outdatum, datum, sizeof (fringesum));
            new_intg = FALSE;
            }
                                        /* Accumulate weighted sums */
        weight = datum->length * datum->length;
        weightsum += weight;
                                        /* Incoherent amplitude sum with noise */
                                        /* bias correction */
        if (datum->amp > 0.0)
            {
            true_snr = datum->snr * fabs(conf.snrfact);
            sigma = datum->amp / true_snr;
            deltampsq = weight * (datum->amp*datum->amp - (2.0*sigma*sigma));
            ampsqsum += deltampsq;
            msg("Amp^2 sum inc by %lg with weight %lg to %lg", -1, deltampsq, weight, ampsqsum);
                                        /* This needed for SNR calculation */
            inc_sigma_sum1 += weight * weight * sigma * sigma; 
            inc_sigma_sum2 += weight * weight * pow (sigma, 4.0);
            coh_sigma_sum += 1.0 / (sigma * sigma);
            }
        else
            {
            nskips++;
            }
                                        /* Sum reals and imaginaries */
                                        /* separately to arrive at averaged */
                                        /* phase.  Here, the weights are just */
                                        /* the times */
        sumimag += datum->length * datum->amp * sin (datum->resid_phas/57.2958);
        sumreal += datum->length * datum->amp * cos (datum->resid_phas/57.2958);
                                        /* Will need mean u, v and time */
                                        /* Also azimuths and elevations */
                                        /* These should be weighted just by the */
                                        /* time */
        timeoff = datum->time_tag - start_time + datum->offset;
        timeoff_sum += datum->length * timeoff;
        usum += (float)datum->length * datum->u;
        vsum += (float)datum->length * datum->v;
        refazsum += (float)datum->length * datum->ref_az;
        remazsum += (float)datum->length * datum->rem_az;
        refelsum += (float)datum->length * datum->ref_elev;
        remelsum += (float)datum->length * datum->rem_elev;
        timesum += datum->length;

        latest = datum->time_tag + datum->duration;

        nadded++;
        recno++;
        }
    msg("Added %d records, skipped %d records of %d in all", 0, nadded, nskips, recno);
    if (conf.header && negamps)
        {
        fprintf(fpout, "Amp^2 summation went negative %d times\n", negamps);
        }

    return (nout);
    }
