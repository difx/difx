// createType3s creates a type 3 fileset based upon the difx data structures
// there is one type 3 output file for each station in the difx scan
//
//  first created                          rjc  2010.2.23
//  added type 309 pcal record creation    rjc  2010.8.9

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "difx2mark4.h"
#include "other.h"


#define NUMFILS 50                  // maximum number of station files
#define NPC_TONES 64                // max number of pcal tones (and channels)

int createType3s (DifxInput *D,     // difx input structure, already filled
                  int startJob,
                  int endJob,
                  int scanId,
                  char *node,       // directory for output fileset
                  char *rcode,      // 6 letter root suffix
                  struct stations *stns, // structure containing names of stations
                  struct CommandLineOptions *opts) // ptr to input options

    {
    int i,
        j,
        b,
        l,
        n,
        jf,
        npol,
        nchan,
        ntones,
        np,
        nc,
        nt,
        nstates,
        nrc,
        nchars,
        mchars,
        norm_corr,
        isb,
        record_chan,
        once = FALSE,
        refDay,
        start,
        nclock;

        size_t linemax = 10000;


    double t,
           mjd,
           tint,
           cable_delay,
           freq,
           f_rel,
           srate,
           cquad,
           squad,
           xtones[NPC_TONES],
           deltat,
           clock[6];

    char outname[256],
         pcal_filnam[256],
         ant[16],
         buff[5],
         *line;

    FILE *fin;
    FILE *fout;

    struct type_000 t000;
    struct type_300 t300;
    struct type_301 t301;
    struct type_302 t302;
    struct type_309 t309;

                                    // initialize memory
    memset (&t000, 0, sizeof (t000));
    memset (&t300, 0, sizeof (t300));
    memset (&t301, 0, sizeof (t301));
    memset (&t302, 0, sizeof (t302));
    memset (&t309, 0, sizeof (t309));

                                    // fill in record boiler-plate and unchanging fields
    memcpy (t000.record_id, "000", 3);
    memcpy (t000.version_no, "01", 3);
    
    memcpy (t300.record_id, "300", 3);
    memcpy (t300.version_no, "00", 3);
    
    memcpy (t301.record_id, "301", 3);
    memcpy (t301.version_no, "00", 3);
    
    memcpy (t302.record_id, "302", 3);
    memcpy (t302.version_no, "00", 3);
    
    memcpy (t309.record_id, "309", 3);
    memcpy (t309.version_no, "01", 3);
                                    // pre-calculate sample rate (samples/s)
    srate = 2e6 * D->freq->bw * D->freq->overSamp;
                                    // loop over all antennas in scan
    line = calloc(linemax, sizeof(char));
    if(line == 0)
        {
        fprintf(stderr, "Error, malloc of line failed\n");
        return (-1);
        }
                                    // doy of start of observation
    mjd2dayno((int)(D->mjdStart), &refDay);

    for (n = 0; n < D->nAntenna; n++)
        {                           // n incremented at bottom 
                                    // and at every continue
        if (stns[n].invis == 0)
            continue;
        strcpy (outname, node);     // form output file name
        strcat (outname, "/");

        outname[strlen(outname)+1] = 0;
        outname[strlen(outname)] = (stns+n)->mk4_id;
        strcat (outname, "..");
        strcat (outname, rcode);
                                    // now open the file just named
        fout = fopen (outname, "w");
        if (fout == NULL)
            {
            perror ("difx2mark4");
            fprintf (stderr, "fatal error opening output type3 file %s\n", outname);
            free(line);
            return (-1);
            }
        printf ("    created type 3 output file %s\n", outname);
                                    // all files need a type 000 record
        strcpy (t000.date, "2001001-123456");
        strcpy (t000.name, outname);
        fwrite (&t000, sizeof (t000), 1, fout);

                                    // finish forming type 300 and write it
        t300.id = (stns+n)->mk4_id;
        memcpy (t300.intl_id, (stns+n)->intl_name, 2);
        memcpy (t300.name, (stns+n)->difx_name, 2);
        t300.name[2] = 0;           // null terminate to form string
                                    // check that model was read in OK
        if (D->scan[scanId].im == 0)
            {
            fprintf (stderr, "ERROR: problem accessing model array\n");
            free(line);
            return (-1);
            }
        t = (***(D->scan[scanId].im)).mjd + (***(D->scan[scanId].im)).sec / 86400.0;
        conv2date (t, &t300.model_start);

        t300.model_interval = (float)(***(D->scan[scanId].im)).validDuration;
        t300.nsplines = (short int) D->scan[scanId].nPoly;
        write_t300 (&t300, fout);

                                    // construct type 301 and 302's and write them
                                    // loop over channels
        for (i=0; i<D->nFreq; i++)
            {
            sprintf (t301.chan_id, "%c%02d?", getband (D->freq[i].freq), i);
            t301.chan_id[3] = (D->freq+i)->sideband;
            strcpy (t302.chan_id, t301.chan_id); 
                                    // loop over polynomial intervals
            for (j=0; j<D->scan[scanId].nPoly; j++)
                {
                t301.interval = (short int) j;
                t302.interval = t301.interval;
                                    // units of difx are usec, ff uses sec
                                    // shift clock polynomial to start of model interval
                deltat = 8.64e4 * ((**(D->scan[scanId].im+n))->mjd - (D->antenna+n)->clockrefmjd) 
                                                   + (**(D->scan[scanId].im+n))->sec;
                nclock = getDifxAntennaShiftedClock (D->antenna+n, deltat, 6, clock);
                                    // difx delay doesn't have clodk added in, so
                                    // we must do it here; also apply sign reversal
                                    // for opposite delay convention
                for (l=0; l<6; l++)
                    {
                    t301.delay_spline[l] 
                      = -1.e-6 * (**(D->scan[scanId].im+n)+j)->delay[l];

                    if (l < nclock) // add in those clock coefficients that are valid
                        t301.delay_spline[l] -= 1e-6 * clock[l];

                    t302.phase_spline[l] = t301.delay_spline[l] * (D->freq+i)->freq;
                    }

                write_t301 (&t301, fout);
                write_t302 (&t302, fout);
                }
            }

                                    // construct type 309 pcal records and write them
                                    // check to see if there is a input pcal file for this antenna
        for (j = startJob; j < endJob; j++)
            {
            strncpy (pcal_filnam, D->job[j].outputFile, 242);
            strcat (pcal_filnam, "/PCAL_"); 
            strcat (pcal_filnam, t300.name); 
            
            fin = fopen (pcal_filnam, "r");
            if (fin == NULL)
                printf ("      No input phase cal file %s for antenna %s\n",
                        pcal_filnam, t300.name);
            else
                {
                                        // input data is present - loop over records
                                        // read next input record
                while (TRUE)
                    {
                    i = getline(&line, &linemax, fin);
                    if (i < 0)          //EOF
                        break;
                    sscanf (line, "%s%lf%lf%lf%d%d%d%d%d%n", ant, &t, &tint, &cable_delay, 
                                     &npol, &nchan, &ntones, &nstates, &nrc, &nchars);
                    mjd = t - refDay + (int)(D->mjdStart);

                    if(mjd < D->scan[scanId].mjdStart)
                                        // skip to next line
                        {
                        if (opts->verbose > 1)
                            printf("      pcal early %13.6f<%13.6f\n", mjd, D->scan[scanId].mjdStart);
                        continue;
                        }
                    if(mjd > D->scan[scanId].mjdEnd)
                        {
                        if (opts->verbose > 1)
                            printf("      pcal late %13.6f>%13.6f\n", t, D->scan[scanId].mjdStart);
                        break;
                        }

                                        // calculate and insert rot start time of record
                    t309.rot = 3.2e7 * 8.64e4 * (t - 1.0);
                                        // pcal integration period same as main AP
                    t309.acc_period =  D->config->tInt;
                                        // debug print
                    if (opts->verbose > 2)
                        printf ("      pcal record ant %s t %lf tint %lf cable_delay %lf"
                              "\n      rot %lf acc_period %lf\n",
                                 ant, t, tint, cable_delay, t309.rot, t309.acc_period);
                                        // initialize list of next available tone location
                    for (i=0; i<NPC_TONES; i++)
                        xtones[i] = 0;
                    t309.ntones = 0;
                                        // loop over tones within record
                    for (np=0; np<npol; np++)
                        for (nc=0; nc<nchan; nc++)
                            for (nt=0; nt<ntones; nt++)
                                {
                                        // identify channel and tone
                                sscanf (line + nchars, "%d%lf%lf%lf%n", 
                                        &record_chan, &freq, &cquad, &squad, &mchars);
                                        // swap sign of imaginary part
                                squad *= -1;
                                nchars += mchars;
                                for (b=0; b<D->nFreq*npol; b++)
                                    {
                                    jf = b / npol;
                                        // skip over non-matching polarizations
                                    if (np != b % npol)
                                        continue;  
                                    isb = ((D->freq+jf)->sideband == 'U') ? 1 : -1;
                                    f_rel = isb * (freq - (D->freq+jf)->freq);
                                        // is it within the jfth frequency band?
                                    if (f_rel > 0.0 && f_rel < (D->freq+jf)->bw)
                                        // yes, insert phasor info into correct slot
                                        {
                                        sprintf (buff, "%c%02dU", getband (D->freq[jf].freq), b);
                                        buff[3] = (D->freq+jf)->sideband;
                                        strcpy (t309.chan[b].chan_name, buff);

                                        // find out which tone slot this goes in
                                        for (i=0; i<NPC_TONES; i++)
                                            {
                                            if (f_rel == xtones[i])
                                                break;
                                            else if (xtones[i] == 0.0)
                                                {
                                                xtones[i] = f_rel;
                                                t309.ntones++;
                                                break;
                                                }
                                            }
                                        // did we run out of slots before finding tone?
                                        if (i == NPC_TONES)
                                            {
                                            if (!once)
                                                {
                                                fprintf (stderr, "more than %d baseband pcal tones"
                                                                 " - ignoring the rest\n", NPC_TONES);
                                                once = TRUE;
                                                }
                                            continue;
                                            }
                                        // renormalize correlations to those created in the DOM
                                        norm_corr = - isb * floor (cquad * srate * t309.acc_period * 128.0 + 0.5);
                                        memcpy (&t309.chan[b].acc[i][0], &norm_corr, 4);

                                        norm_corr = floor (squad * srate * t309.acc_period * 128.0 + 0.5);
                                        memcpy (&t309.chan[b].acc[i][1], &norm_corr, 4);

                                        // tone freqs (in Hz) are spread through channel recs
                                        t309.chan[i].freq = 1e6 * isb * f_rel;
                                        break;
                                        }
                                    }
                                }
                                        // write output record
                    write_t309 (&t309, fout);
                    }
                fclose(fin);
                }
            }
        fclose(fout);
        }
    free(line);
    return 0;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
