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

#define NUMFILS 50                  // maximum number of station files
#define LINEMAX 20000               // max size of a pcal file line

int createType3s (DifxInput *D,     // difx input structure, already filled
                  char *baseFile,   // string containing common part of difx file names
                  char *node,       // directory for output fileset
                  char *rcode,      // 6 letter root suffix
                  struct stations *stns, // structure containing names of stations
                  struct CommandLineOptions *opts) // ptr to input options

    {
    int i,
        j,
        k,
        l,
        n,
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
        tonindex[16];

    double t,
           tint,
           cable_delay,
           freq,
           f_rel,
           srate,
           cquad,
           squad;

    char outname[256],
         pcal_filnam[256],
         ant[16],
         buff[5],
         line[LINEMAX];

    FILE *fin;
    FILE *fout[NUMFILS];

    DifxPolyModel *pdpm;

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
    memcpy (t309.version_no, "00", 3);
                                    // pre-calculate sample rate (samples/s)
    srate = 2e6 * D->freq->bw * D->freq->overSamp;
                                    // loop over all antennas in scan
    for (n=0; n<D->nAntenna; n++)
        {
        strcpy (outname, node);     // form output file name
        strcat (outname, "/");

        outname[strlen(outname)+1] = 0;
        k = (stns+n)->dind;
        outname[strlen(outname)] = (stns+k)->mk4_id;
        strcat (outname, "..");
        strcat (outname, rcode);
                                    // now open the file just named
        fout[n] = fopen (outname, "w");
        if (fout[n] == NULL)
            {
            perror ("difx2mark4");
            fprintf (stderr, "fatal error opening output type3 file %s\n", outname);
            return (-1);
            }
        fprintf (stderr, "created type 3 output file %s\n", outname);
                                    // all files need a type 000 record
        strcpy (t000.date, "2001001-123456");
        strcpy (t000.name, outname);
        fwrite (&t000, sizeof (t000), 1, fout[n]);

                                    // finish forming type 300 and write it
        t300.id = (stns+k)->mk4_id;
        memcpy (t300.intl_id, (stns+k)->intl_name, 2);
        memcpy (t300.name, (stns+k)->difx_name, 2);
        t300.name[2] = 0;           // null terminate to form string

        t = (***(D->scan->im)).mjd + (***(D->scan->im)).sec / 86400.0;
        conv2date (t, &t300.model_start);

        t300.model_interval = (float)(***(D->scan->im)).validDuration;
        t300.nsplines = (short int) D->scan->nPoly;
        write_t300 (&t300, fout[n]);

                                    // construct type 301 and 302's and write them
                                    // loop over channels
        for (i=0; i<D->nFreq; i++)
            {
            sprintf (t301.chan_id, "C%02d?", i);
            t301.chan_id[3] = (D->freq+i)->sideband;
            strcpy (t302.chan_id, t301.chan_id); 
                                    // loop over polynomial intervals
            for (j=0; j<D->scan->nPoly; j++)
                {
                t301.interval = (short int) j;
                t302.interval = t301.interval;
                                    // units of difx are usec, ff uses sec
                                    // difx delay doesn't have clodk added in, so
                                    // we must do it here; also apply sign reversal
                                    // for opposite delay convention
                for (l=0; l<6; l++)
                    {
                    t301.delay_spline[l] 
                      = -1.e-6 * ((**(D->scan->im+n))->delay[l] + (D->antenna+n)->clockcoeff[l]);

                    t302.phase_spline[l] = t301.delay_spline[l] * (D->freq+j)->freq;
                    }
                write_t301 (&t301, fout[n]);
                write_t302 (&t302, fout[n]);
                }
            }

                                    // construct type 309 pcal records and write them
                                    // check to see if there is a input pcal file for this antenna
        strncpy (pcal_filnam, baseFile, 242);
        strcat (pcal_filnam, ".difx/PCAL_"); 
        strcat (pcal_filnam, t300.name); 
        
        fin = fopen (pcal_filnam, "r");
        if (fin == NULL)
            printf ("No input phase cal for antenna %s\n", t300.name);
        else
            {
                                    // input data is present - loop over records
                                    // read next input record
            while (fgets (line, LINEMAX, fin) != NULL)
                {
                sscanf (line, "%s%lf%lf%lf%d%d%d%d%d%n", &ant, &t, &tint, &cable_delay, 
                                 &npol, &nchan, &ntones, &nstates, &nrc, &nchars);

                                    // calculate and insert rot start time of record
                t309.rot = 3.2e7 * 8.64e4 * (t - 1.0);
                                    // pcal integration period same as main AP
                t309.acc_period =  D->config->tInt;
                                    // debug print
                if (opts->verbose > 1)
                    fprintf (stderr, "pcal record ant %s t %lf tint %lf cable_delay %lf"
                             "\nrot %lf acc_period %lf\n",
                             ant, t, tint, cable_delay, t309.rot, t309.acc_period);
                                    // initialize list of next available tone location
                for (i=0; i<16; i++)
                    tonindex[i] = 0;
                                    // loop over tones within record
                for (np=0; np<npol; np++)
                    for (nc=0; nc<nchan; nc++)
                        for (nt=0; nt<ntones; nt++)
                            {
                                    // identify channel and tone
                            sscanf (line + nchars, "%d%lf%lf%lf%n", 
                                    &record_chan, &freq, &cquad, &squad, &mchars);
                            nchars += mchars;
                            for (j=0; j<D->nFreq; j++)
                                {
                                isb = ((D->freq+j)->sideband == 'U') ? 1 : -1;
                                f_rel = isb * (freq - (D->freq+j)->freq);
                                    // is it within the jth frequency band?
                                if (f_rel > 0.0 && f_rel < (D->freq+j)->bw)
                                    // yes, insert phasor info into correct slot
                                    {
                                    sprintf (buff, "C%02dU", j);
                                    buff[3] = (D->freq+j)->sideband;
                                    strcpy (t309.chan[j].chan_name, buff);
                                    // renormalize correlations to those created in the DOM
                                    norm_corr = - floor (cquad * srate * 128.0 + 0.5);
                                    memcpy (&t309.chan[j].acc[tonindex[j]][0], &norm_corr, 4);
                                    norm_corr = floor (squad * srate * 128.0 + 0.5);
                                    memcpy (&t309.chan[j].acc[tonindex[j]][1], &norm_corr, 4);

                                    // 16 tone freqs are spread through channel records
                                    t309.chan[tonindex[j]].freq = f_rel;
                                    tonindex[j]++;
                                    break;
                                    }
                                }
                            }
                                    // find maximum number of tones in any one channel
                t309.ntones = tonindex[0];
                for (i=1; i<16; i++)
                    if (tonindex[i] > t309.ntones)
                        t309.ntones = tonindex[i];
                                    // write output record
                write_t309 (&t309, fout[n]);
                }
            }

        }
    return 0;
    }
