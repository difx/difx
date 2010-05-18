// createType3s creates a type 3 fileset based upon the difx data structures
// there is one type 3 output file for each station in the difx scan
//
//  first created                          rjc  2010.2.23

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2mark4.h"

#define NUMFILS 50                  // maximum number of station files

int createType3s (DifxInput *D,     // difx input structure, already filled
                  char *node,       // directory for output fileset
                  char *rcode,      // 6 letter root suffix
                  struct stations *stns, // structure containing names of stations
                  struct CommandLineOptions *opts) // ptr to input options

    {
    int i,
        j,
        k,
        l,
        n;

    double t;

    char outname[256];

    FILE *fout[NUMFILS];

    DifxPolyModel *pdpm;

    struct type_000 t000;
    struct type_300 t300;
    struct type_301 t301;
    struct type_302 t302;

                                    // initialize memory
    memset (&t000, 0, sizeof (t000));
    memset (&t300, 0, sizeof (t300));
    memset (&t301, 0, sizeof (t301));
    memset (&t302, 0, sizeof (t302));

                                    // fill in record boiler-plate and unchanging fields
    memcpy (t000.record_id, "000", 3);
    memcpy (t000.version_no, "01", 3);
    
    memcpy (t300.record_id, "300", 3);
    memcpy (t300.version_no, "00", 3);
    
    memcpy (t301.record_id, "301", 3);
    memcpy (t301.version_no, "00", 3);
    
    memcpy (t302.record_id, "302", 3);
    memcpy (t302.version_no, "00", 3);
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
        t300.model_start.year    = short_reverse (t300.model_start.year);
        t300.model_start.day     = short_reverse (t300.model_start.day);
        t300.model_start.hour    = short_reverse (t300.model_start.hour);
        t300.model_start.minute  = short_reverse (t300.model_start.minute);
        t300.model_start.second  = float_reverse (t300.model_start.second);

        t300.model_interval = float_reverse ((float)(***(D->scan->im)).validDuration);
        t300.nsplines = short_reverse ((short int) D->scan->nPoly);
        fwrite (&t300, sizeof (t300), 1, fout[n]);

                                    // construct type 301's and write them
                                    // loop over channels
        for (i=0; i<D->nFreq; i++)
            {
            sprintf (t301.chan_id, "C%02d?", i);
            t301.chan_id[3] = D->freq->sideband;
                                    // loop over polynomial intervals
            for (j=0; j<D->scan->nPoly; j++)
                {
                t301.interval = short_reverse ((short int) j);
                                    // units of difx are usec, ff uses sec
                                    // difx delay doesn't have clodk added in, so
                                    // we must do it here; also apply sign reversal
                                    // for opposite delay convention
                for (l=0; l<6; l++)
                    t301.delay_spline[l] 
                      = double_reverse (-1.e-6 
                       * ((**(D->scan->im+n))->delay[l] + (D->antenna+n)->clockcoeff[l]));
                fwrite (&t301, sizeof (t301), 1, fout[n]);
                }
            }
        }
    return 0;
    }
