// normalize works through a visibility file image in memory,
// normalizing each cross and auto correlation by the harmonic
// mean of the power at each end of the baseline, as averaged
// across all channels
//
// created                                      rjc 2011.5.24
// modified to allow simultaneous R/L/X/Y pols  rjc 2015.11.2
// handle baseline-dependent nvis & vrsize      rjc  2018.10.18

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "difx2mark4.h"

void normalize (struct CommandLineOptions *opts,  // array of command line options
                vis_record *vrec,                 // pointer to start of vis. buffer
                int nvrtot,                       // total # of vis. records in buffer
                int *nvis,                        // number of visibility points in record
                int *vrsize,                      // size of each vis record (bytes)
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
        pmap[MAX_DFRQ],             // lowest freq index of matching chans, by [freq][pol]
        nf;

    double t,                       // time of current records
           factor,
           sum,
           pant[256][MAX_DFRQ][4];  // sqrt of power per antenna avg over channels
                                    // indexed by [ant][freq][pol]
                                    // pol index mapping:
    char polchar[4] = {'L', 'R', 'X', 'Y'},
         *pch;

    vis_record *vr, *vrloop;        // convenience pointers
    enum indices {REF, REM};
    
                                    // create pmap array
    for (fr=0; fr<MAX_DFRQ; fr++)   // first initialize pmap array to an identity map
        pmap[fr] = fr;

                                    // now make a pass through the fblock, finding the lowest
                                    // freq index for pairs of antennas, and overwriting those
                                    // in the pmap
    nf = -1;
    while (pfb[++nf].stn[REF].ant >= 0) // check for end-of-table marker
        {
        if (nf >= MAX_DFRQ)
            {
                printf ("too many frequencies, exceeding MAX_DFRQ; redimension\n");
                return;
            }
        if (pfb[nf].stn[REF].find != pfb[nf].stn[REM].find)
            {
                                    // found matching channels with different freq id's
            if (pfb[nf].stn[REF].find < pfb[nf].stn[REM].find)
                                    // ref index lower, use it for remote antenna
                pmap[pfb[nf].stn[REM].find] = pfb[nf].stn[REF].find;
                 
            else
                                    // rem index lower, use it for reference antenna
                pmap[pfb[nf].stn[REF].find] = pfb[nf].stn[REM].find;
                 
            }
        }

                                    // initialize for looping 
    nbeg = 0;
    vr = vrec;
                                    // loop over all records in buffer
    while (nbeg < nvrtot)
        {
                                    // initialize pant array
        for (ant=0; ant<256; ant++)
            for (fr=0; fr<MAX_DFRQ; fr++)
                for (pol=0; pol<4; pol++)
                    pant[ant][fr][pol] = 0.0;
                                    // save time for this step
        t = vr->iat;
                                    // find ending index for this time
        pch = (char *) vr;          // leave vr pointing to start of range
        for (n=nbeg; n<nvrtot; n++)
            {
            pch += vrsize[n];
            vrloop = (vis_record *) pch;
            if (vrloop->iat != t)
                break;
            }
        nend = n;
                                    // read and calc all autocorr powers for current time
                                    // by building a table of antenna powers from autos
        vrloop = vr;
        pch = (char *) vr;
        for (n=nbeg; n<=nend; n++)
            {
                                    // decode antennas
            aref = vrloop->baseline / 256 - 1; 
            arem = vrloop->baseline % 256 - 1;
                                    // only want powers from autocorrelations with same pol
            if (aref == arem && vrloop->pols[REF] == vrloop->pols[REM])
                {
                                    // convert polarization to an index
                for (pol=0; pol<4; pol++)
                    if (vrloop->pols[REF] == polchar[pol])
                        break;

                if (pol == 4)
                    {
                    printf ("unknown polarization type %c, skipping normalization\n",
                            vrloop->pols[REF]);
                    continue;
                    }
                fr = pmap[vrloop->freq_index];
                                    // find average power across the band and save its sqrt
                sum = 0.0;
                for (i=0; i<nvis[n]; i++)
                    sum += vrloop->comp[i].real;
                pant[aref][fr][pol] = sqrt (sum / nvis[n]);
                // printf("n %d aref %d fr %d pol %d pant %f\n",
                //         n,aref,fr,pol,pant[aref][fr][pol]);
                }
            pch += vrsize[n];
            vrloop = (vis_record *) pch;
            }
                                    // one last pass through all records for current time 
                                    // dividing by harmonic means
        for (n=nbeg; n<=nend; n++)
            {
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
                if (opts->verbose > 2)
                    printf("missing autocorr for record %d ants ref rem %d %d "
                            "fr_remap %d pol %c%c\n", 
                            n,aref,arem,fr_remap, vr->pols[REF],vr->pols[REM]);
                }
            else
                factor = 1.0 / (pant[aref][fr_remap][polref] * pant[arem][fr_remap][polrem]);

            for (i=0; i<nvis[n]; i++)
                {
                vr->comp[i].real *= factor;
                vr->comp[i].imag *= factor;
                }
            pch = (char *) vr + vrsize[n];
            vr = (vis_record *) pch;
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
