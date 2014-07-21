// normalize works through a visibility file image in memory,
// normalizing each cross and auto correlation by the harmonic
// mean of the power at each end of the baseline, as averaged
// across all channels
//
// created                                     rjc 2011.5.24
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
           pant[256][64][2];        // sqrt of power per antenna avg over channels
                                    // indexed by [ant][freq][pol]
    char polchar[2];

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
                for (pol=0; pol<2; pol++)
                    pant[ant][fr][pol] = 0.0;
        polchar[0] = 0;             // initialize polarization mapping
        polchar[1] = 0; 
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
            if (aref == arem        // only want powers from autocorrelations, same pol
             && vr->pols[REF] == vr->pols[REM])
                {
                fr = vr->freq_index;
                for (pol=0; pol<2; pol++)
                    if (vr->pols[REF] == polchar[pol])
                        break;
                     
                                    // if pol. wasn't there, try to add it
                if (pol == 2)
                    {
                    if (polchar[0] == 0)
                        {
                        polchar[0] = vr->pols[REF];
                        pol = 0;
                        }
                    else if (polchar[1] == 0)
                        {
                        polchar[1] = vr->pols[REF];
                        pol = 1;
                        }
                    else            // more than 2 polarizations present at this time!
                        {
                        printf ("more than 2 pols at time %lf, skipping norm\n", t);
                        continue;
                        }
                    }
                                    // find rms power across the band and save its root
                sum = 0.0;
                for (i=0; i<nvis; i++)
                    sum += vr->comp[i].real * vr->comp[i].real;
                pant[aref][fr][pol] = sqrt (sqrt (sum / nvis));
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
            for (polref=0; polref<2; polref++)
                {
                if (vr->pols[REF] == polchar[polref])
                    break;
                }
            if (polref == 2)
                {
                printf ("ref stn fr %d pol %c for bl #%d at time %f has no "
                        "autocorr(%c%c) - won't normalize\n",
                         fr, vr->pols[REF], vr->baseline, vr->iat, polchar[0], polchar[1]);
                continue;
                }
                                    // identify polarization for remote antenna
            for (polrem=0; polrem<2; polrem++)
                {
                if (vr->pols[REM] == polchar[polrem])
                    break;
                }
            if (polrem == 2)
                {
                printf ("rem stn fr %d pol %c for bl #%d at time %f has no "
                        "autocorr(%c%c) - won't normalize\n",
                         fr, vr->pols[REM], vr->baseline, vr->iat, polchar[0], polchar[1]);
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
