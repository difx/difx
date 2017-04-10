// new_type1 adds a type 1 fileset based upon the difx data structures
// for one baseline
//
//  first created from createType1s                  rjc  2012.5.8
//  broke out put_t101 into a routine, fixed ac's    rjc  2013.9.12
//  added Van Vleck scale factor calculation         rjc  2015.10.2
//  added cross auto correlations (e.g. XY, YX)      rjc  2017.4.7

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <difxio/difx_input.h>
#include <errno.h>
#include <math.h>
#include "difx2mark4.h"

#define XS_CONVENTION

#define SCALE 10000.0               // amplitude factor to normalize for fourfit

int new_type1 (DifxInput *D,                    // ptr to a filled-out difx input structure
               struct fblock_tag *pfb,          // ptr to filled-in fblock table
               int nb,                          // (next open) index to base_index array
               int a1, 
               int a2, 
               int blind, 
               int *base_index,
               double *scale_factor,            // Van Vleck correction per baseline
               struct stations *stns,
               char *blines,                    // array of character pairs
               struct CommandLineOptions *opts, // ptr to input options
               FILE *fout[NUMFILS],
               int nvis,
               char *rootname,
               char *node,                      // directory for output fileset
               char *rcode,                     // 6 letter root suffix
               char *corrdate,                  // modification date of input file
               int baseline,                    // numerical baseline index in difx-style
               int scanId)
    {
    int n,
        m,
        k,
        is,
        ref,
        rem;
    char xpol_string[6];
                                    // factors are sqrt (Van Vleck correction) for 1b, 2b case
                                    // quantization correction factor is pi/2 for
                                    // 1x1 bit, or ~1.13 for 2x2 bit (see TMS, p.300)
                                    // 1x2 bit uses harmonic mean of 1x1 and 2x2
                                    // Note that these values apply to the weak signal
                                    // (i.e. cross-correlation) case only.
    double factor[2] = {1.25331, 1.06448};

    struct type_000 t000;
    struct type_100 t100;
    struct type_101 t101;

    char outname[DIFXIO_FILENAME_LENGTH];

                                    // function prototypes
    void put_t101 (struct type_101 *, FILE *, int, char *, char *);

                                    // clear record areas
    memset (&t000, 0, sizeof (t000));
    memset (&t100, 0, sizeof (t100));
    memset (&t101, 0, sizeof (t101));
                                    // fill in record boiler-plate and unchanging fields
                                    // type_100
    memcpy (t000.record_id, "000", 3);
    memcpy (t000.version_no, "01", 2);
    memcpy (t000.unused1,   "000", 3);

    memcpy (t100.record_id, "100", 3);
    memcpy (t100.version_no, "00", 2);
    memcpy (t100.unused1,   "000", 3);
    t100.nlags = nvis;
    strncpy (t100.rootname, rootname, 34);
    conv2date (D->scan[scanId].mjdStart, &t100.start);
    conv2date (D->scan[scanId].mjdEnd,   &t100.stop);
    if (opts->verbose > 0)
        printf ("      mjdStart %g start %hd %hd %hd %hd %f\n", D->scan[scanId].mjdStart, 
                 t100.start.year, t100.start.day, 
                 t100.start.hour, t100.start.minute, t100.start.second);
                                    // dummy procdate - *could* set to file creation time
    conv2date (54321.0,   &t100.procdate);

    t100.nblocks = 1;               // blocks are mk4 corr. specific

                                    // type_101
    memcpy (t101.record_id, "101", 3);
    memcpy (t101.version_no, "00", 2);
    t101.nblocks = 1;               // blocks are mk4 corr. specific
                                    // append new baseline to list
    base_index[nb] = baseline;
    (stns + a1)->invis = TRUE;
    (stns + a2)->invis = TRUE;

                                    // create name & open new output file
                                    // assume that site ID order is same as station order
                                    // probably not valid, though - THIS NEEDS WORK!!  
    strcpy (outname, node);
    strcat (outname, "/");
    blines[2*nb]   = (stns+a1)->mk4_id;
    blines[2*nb+1] = (stns+a2)->mk4_id;
    if (opts->verbose > 0)
        printf ("      rec->baseline %d blines <%c%c>\n", 
                baseline, blines[2*nb], blines[2*nb+1]);
    strncat (outname, blines+2*nb, 2);
    strcat (outname, "..");
    strcat (outname, rcode);

    fout[nb] = fopen (outname, "w");
    if (fout[nb] == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening output type1 file %s\n", outname);
        return (-1);
        }
    printf ("      created type 1 output file %s\n", outname);

                                    // construct and write type 000 record
    strncpy (t000.date, corrdate, 16);
    if (opts->verbose > 0)
        printf ("        t000.date will be set to %s\n",corrdate);
    strncpy (t000.name, outname, 40);
    fwrite (&t000, sizeof (t000), 1, fout[nb]);

                                    // construct and write type 100 record
    memcpy (t100.baseline, blines+2*nb, 2);
    t100.nindex = D->baseline[blind].nFreq * D->baseline[blind].nPolProd[0];
    write_t100 (&t100, fout[nb]);

    if (a1 != a2)                   // cross-correlation
        {
                                    // loop through whole fblock table
        n = -1;
        while (pfb[++n].stn[0].ant >= 0) // check for end-of-table marker
            {
                                    // make sure baseline matches
                                    // and determine reference and remote antennas
            ref = -1;
            rem = -1;
                {
                for (k=0; k<2; k++)
                    {
                    if (a1 == pfb[n].stn[k].ant)
                        ref = k;
                    if (a2 == pfb[n].stn[k].ant)
                        rem = k;
                    }
                if (ref >= 0 && rem >= 0)
                    {
                    put_t101 (&t101, fout[nb], pfb[n].stn[0].find,
                              pfb[n].stn[0].chan_id, pfb[n].stn[1].chan_id);
                    scale_factor[nb] = SCALE * factor[pfb[n].stn[0].bs - 1]
                                             * factor[pfb[n].stn[1].bs - 1];
                    }
                }
            }
        }

    else                            // auto-correlation
        {
                                    // loop through whole fblock table
        n = -1;
        while (pfb[++n].stn[0].ant >= 0) // check for end-of-table marker
            {
            for (is=0; is<2; is++)  // loop over reference and remote station
                {
                                    // first time antenna match?
                if (a1 == pfb[n].stn[is].ant && pfb[n].stn[is].first_time)
                    {
                    put_t101 (&t101, fout[nb], pfb[n].stn[is].find,
                              pfb[n].stn[is].chan_id, pfb[n].stn[is].chan_id);
                    scale_factor[nb] = SCALE;
                                    // form cross-pol chan_id string 
                    strcpy (xpol_string, pfb[n].stn[is].chan_id);
                    switch (xpol_string[4])
                        {
                        case 'X': 
                             xpol_string[4] = 'Y';
                             break;
                        case 'Y': 
                             xpol_string[4] = 'X';
                             break;
                        case 'L': 
                             xpol_string[4] = 'R';
                             break;
                        case 'R': 
                             xpol_string[4] = 'L';
                             break;
                        default:
                             fprintf (stderr, "uh oh! unknown polarization <%s> in fblock table\n",
                                     xpol_string[4]);
                        }
                                    // if opposite chan_id present for ant, create a
                                    // type 101 for that as well
                                    // examine fblock table for xpol entry
                    m = -1;
                    while (pfb[++m].stn[0].ant >= 0)
                        if ((strcmp (pfb[m].stn[0].chan_id, xpol_string) == 0
                          && pfb[m].stn[0].ant == a1
                          && pfb[m].stn[0].first_time)
                         || (strcmp (pfb[m].stn[1].chan_id, xpol_string) == 0
                          && pfb[m].stn[1].ant == a1
                          && pfb[m].stn[1].first_time))
                            {
                            put_t101 (&t101, fout[nb], pfb[n].stn[is].find,
                                      pfb[n].stn[is].chan_id, xpol_string);
                            }
                    }
                }
            }
        }
    return (0);
    }

// function to finalize type 101 record and put it out

void put_t101 (struct type_101 *t101,
               FILE *fout,
               int find,
               char *ref_chan,
               char *rem_chan)
    {
    char a, b;
    a = ref_chan[4];
    b = rem_chan[4];
                                    // mk4 index is based on difx freq index & pol pair
    t101->index = 10 * find;
                                    // encode L,X,H pol the same, ditto for R,Y,V
    if      (strchr ("LXH", a) != NULL && strchr ("LXH", b) != NULL)
        t101->index += 1;
    else if (strchr ("RYV", a) != NULL && strchr ("RYV", b) != NULL)
        t101->index += 2;
    else if (strchr ("LXH", a) != NULL && strchr ("RYV", b) != NULL)
        t101->index += 3;
    else if (strchr ("RYV", a) != NULL && strchr ("LXH", b) != NULL)
        t101->index += 4;

                                    // insert channel ids into the type 101 record
    strcpy (t101->ref_chan_id, ref_chan);
    strcpy (t101->rem_chan_id, rem_chan);
                                    // and write this type 101
    write_t101 (t101, fout);
    }
