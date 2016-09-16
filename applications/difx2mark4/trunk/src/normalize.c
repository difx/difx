// normalize works through a visibility file image in memory,
// normalizing each cross and auto correlation by the harmonic
// mean of the power at each end of the baseline, as averaged
// across all channels
//
// created                                      rjc 2011.5.24
// modified to allow simultaneous R/L/X/Y pols  rjc 2015.11.2
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "difx2mark4.h"

void normalize (struct CommandLineOptions *opts,  // array of command line options
                vis_record *vrec,                 // pointer to start of vis. buffer
                int nvrtot,                       // total # of vis. records in buffer
                int nvis,                         // number of visibility points in record
                int vrsize)                       // size of each vis record (bytes)
    {
    int i,
        n,
        nbeg,
        nend,
        aref,
        arem,
        ant,
        fr,
        pol,
        polref,
        polrem,
        n_aczero = 0;               // number of 0 autocorrelations

    double t,                       // time of current records
           factor,
           sum,
           pant[256][64][4];        // sqrt of power per antenna avg over channels
                                    // indexed by [ant][freq][pol]
                                    // pol index mapping:
    char polchar[4] = {'L', 'R', 'X', 'Y'};

    vis_record *vr;                 // convenience pointer
    enum indices {REF, REM};
                                    // initialize for looping 
    nbeg = 0;
                                    // loop over all records in buffer
    while (nbeg < nvrtot)
        {
                                    // initialize pant array
        for (ant=0; ant<256; ant++)
            for (fr=0; fr<64; fr++)
                for (pol=0; pol<4; pol++)
                    pant[ant][fr][pol] = 0.0;
                                    // save time for this step
        vr = (vis_record *)((char *) vrec + (size_t) nbeg * vrsize);
        t = vr->iat;
                                    // find ending index for this time
        for (n=nbeg; n<nvrtot; n++)
            {
            vr = (vis_record *)((char *) vrec + (size_t) n * vrsize);
            if (vr->iat != t)
                break;
            }
        nend = n - 1;
                                    // read and calc all autocorr powers for current time
                                    // by building a table of antenna powers from autos
        for (n=nbeg; n<=nend; n++)
            {
            vr = (vis_record *)((char *) vrec + (size_t) n * vrsize);
                                    // decode antennas
            aref = vr->baseline / 256 - 1; 
            arem = vr->baseline % 256 - 1;
                                    // only want powers from autocorrelations with same pol
            if (aref == arem && vr->pols[REF] == vr->pols[REM])
                {
                fr = vr->freq_index;
                                    // convert polarization to an index
                for (pol=0; pol<4; pol++)
                    if (vr->pols[REF] == polchar[pol])
                        break;

                if (pol == 4)
                    {
                    printf ("unknown polarization type %c, skipping normalization\n",
                            vr->pols[REF]);
                    continue;
                    }
                                    // find average power across the band and save its sqrt
                sum = 0.0;
                for (i=0; i<nvis; i++)
                    sum += vr->comp[i].real;
                pant[aref][fr][pol] = sqrt (sum / nvis);
                }
            }
                                    // go through all records for current time again
                                    // dividing by harmonic means
        for (n=nbeg; n<=nend; n++)
            {
            vr = (vis_record *)((char *) vrec + (size_t) n * vrsize);
                                    // decode antennas
            aref = vr->baseline / 256 - 1; 
            arem = vr->baseline % 256 - 1;

            fr = vr->freq_index;
                                    // identify polarization for reference antenna
            for (polref=0; polref<4; polref++)
                if (vr->pols[REF] == polchar[polref])
                    break;

            if (polref == 4)
                {
                printf ("unknown ref stn polarization type %c, skipping normalization\n",
                        vr->pols[REF]);
                continue;
                }
                                    // identify polarization for remote antenna
            for (polrem=0; polrem<4; polrem++)
                if (vr->pols[REM] == polchar[polrem])
                    break;

            if (polrem == 4)
                {
                printf ("unknown rem stn polarization type %c, skipping normalization\n",
                        vr->pols[REM]);
                continue;
                }

                                    // ensure that there is no 0-divide
            if (pant[aref][fr][polref] == 0.0 || pant[arem][fr][polrem] == 0.0)
                {
                factor = 1.0;
                n_aczero++;
                }
            else
                factor = 1.0 / (pant[aref][fr][polref] * pant[arem][fr][polrem]);
            // printf("n %d factor %f pant[%d][%d][%d] %f pant[%d][%d][%d] %f\n",
            //        n,factor,aref,fr,polref,pant[aref][fr][polref],
            //        arem,fr,polrem,pant[arem][fr][polrem]);
            for (i=0; i<nvis; i++)
                {
                vr->comp[i].real *= factor;
                vr->comp[i].imag *= factor;
                }
            }
        if (nend == nvrtot)         // exit if all records have been processed
            break;
        else
            nbeg = nend + 1;        // go on to next time
        }
                                    // warn if there were problems
    if (n_aczero)
        {
        printf ("Warning! There were %d records with autocorrelation amplitude of 0.0\n",
                n_aczero);
        printf ("This will affect the amplitude normalization.\n");
        }
    }
