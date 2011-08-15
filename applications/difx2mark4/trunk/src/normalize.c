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
        a1,
        a2,
        ant,
        fr,
        pol,
        pol1,
        pol2;

    double t,                       // time of current records
           factor,
           sum,
           pant[256][64][2];        // sqrt of power per antenna avg over channels
                                    // indexed by [ant][freq][pol]
    char polchar[2];

    vis_record *vr;                 // convenience pointer
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
        for (n=nbeg; n<nvrtot-1; n++)
            {
            vr = (vis_record *)((char *) vrec + (size_t) n * vrsize);
            if (vr->iat != t)
                break;
            }
        nend = n;
                                    // read and calc all autocorr powers for current time
                                    // by building a table of antenna powers from autos
        for (n=nbeg; n<=nend; n++)
            {
            vr = (vis_record *)((char *) vrec + (size_t) n * vrsize);
                                    // decode antennas
            a1 = vr->baseline / 256 - 1; 
            a2 = vr->baseline % 256 - 1;
            if (a1 == a2            // only want powers from autocorrelations, same pol
             && vr->pols[0] == vr->pols[1])
                {
                fr = vr->freq_index;
                for (pol=0; pol<2; pol++)
                    {
                    if (vr->pols[0] == polchar[pol])
                        break;
                    }
                                    // if pol. wasn't there, try to add it
                if (pol == 2)
                    {
                    if (polchar[0] == 0)
                        {
                        polchar[0] = vr->pols[0];
                        pol = 0;
                        }
                    else if (polchar[1] == 0)
                        {
                        polchar[1] = vr->pols[0];
                        pol = 1;
                        }
                    else            // more than 2 polarizations present at this time!
                        {
                        printf ("more than 2 pols at time %lf, skipping norm\n", t);
                        continue;
                        }
                    }
                sum = 0.0;
                for (i=0; i<nvis; i++)
                    sum += vr->comp[i].real;
                pant[a1][fr][pol] = sqrt (sum / nvis);
                }
            }
                                    // go through all records for current time again
                                    // dividing by harmonic means
        for (n=nbeg; n<=nend; n++)
            {
            vr = (vis_record *)((char *) vrec + (size_t) n * vrsize);
                                    // decode antennas
            a1 = vr->baseline / 256 - 1; 
            a2 = vr->baseline % 256 - 1;

            fr = vr->freq_index;
                                    // identify polarization for reference antenna
            for (pol1=0; pol1<2; pol1++)
                {
                if (vr->pols[0] == polchar[pol1])
                    break;
                }
            if (pol1 == 2)
                {
                printf ("ref stn of baseline %d not in pol table, skipping record\n",
                         vr->baseline);
                continue;
                }
                                    // identify polarization for remote antenna
            for (pol2=0; pol2<2; pol2++)
                {
                if (vr->pols[1] == polchar[pol2])
                    break;
                }
            if (pol2 == 2)
                {
                printf ("rem stn of baseline %d not in pol table, skipping record\n",
                         vr->baseline);
                continue;
                }
                                    // ensure that there is no 0-divide
            if (pant[a1][fr][pol1] == 0.0 || pant[a2][fr][pol2] == 0.0)
                factor = 1.0;
            else
                factor = 1.0 / (pant[a1][fr][pol1] * pant[a2][fr][pol2]);
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


    }
