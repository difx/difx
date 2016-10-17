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
                int vrsize,                       // size of each vis record (bytes)
                struct fblock_tag *pfb)           // ptr to filled-in fblock table
    {
    int i,
        n,
        nbeg,
        nend,
        aref,
        arem,
        ant,
        fr,
        fr_remap,
        pol,
        polref,
        polrem,
        n_aczero = 0,               // number of 0 autocorrelations
        pmap[MAX_DFRQ],          // lowest freq index of matching channels, by [freq][pol]
        nf;

    double t,                       // time of current records
           factor,
           sum,
           pant[256][MAX_DFRQ][4];  // sqrt of power per antenna avg over channels
                                    // indexed by [ant][freq][pol]
                                    // pol index mapping:
    char polchar[4] = {'L', 'R', 'X', 'Y'};

    vis_record *vr;                 // convenience pointer
    enum indices {REF, REM};
    
                                    // create pmap array
    for (fr=0; fr<MAX_DFRQ; fr++)   // first initialize pmap array to an identity map
        pmap[fr] = fr;

                                    // now make a pass through the fblock, finding the lowest
                                    // freq index for pairs of antennas, and overwriting those
                                    // in the pmap
    nf = -1;
    while (pfb[++nf].stn[0].ant >= 0) // check for end-of-table marker
        {
        if (pfb[nf].stn[0].find != pfb[nf].stn[1].find)
            {
                                    // found matching channels with different freq id's
            if (pfb[nf].stn[0].find < pfb[nf].stn[1].find)
                                    // ref index lower, use it for remote antenna
                pmap[pfb[nf].stn[1].find] = pfb[nf].stn[0].find;
                 
            else
                                    // rem index lower, use it for reference antenna
                pmap[pfb[nf].stn[0].find] = pfb[nf].stn[1].find;
                 
            }
        }

                                    // initialize for looping 
    nbeg = 0;
                                    // loop over all records in buffer
    while (nbeg < nvrtot)
        {
                                    // initialize pant array
        for (ant=0; ant<256; ant++)
            for (fr=0; fr<MAX_DFRQ; fr++)
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
                fr = pmap[vr->freq_index];
                                    // find average power across the band and save its sqrt
                sum = 0.0;
                for (i=0; i<nvis; i++)
                    sum += vr->comp[i].real;
                pant[aref][fr][pol] = sqrt (sum / nvis);
                // printf("n %d aref %d fr %d pol %d pant %f\n",
                //         n,aref,fr,pol,pant[aref][fr][pol]);
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
            fr_remap = pmap[vr->freq_index];

                                    // ensure that there is no 0-divide
            if (pant[aref][fr_remap][polref] == 0.0 || pant[arem][fr_remap][polrem] == 0.0)
                {
                factor = 1.0;
                n_aczero++;
                printf("missing autocorr for record %d ants ref rem %d %d fr_remap %d pols %d%d\n", 
                        n,aref,arem,fr_remap,polref,polrem);
                }
            else
                factor = 1.0 / (pant[aref][fr_remap][polref] * pant[arem][fr_remap][polrem]);

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
