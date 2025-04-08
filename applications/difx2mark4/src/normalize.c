// normalize works through a visibility file image in memory,
// normalizing each cross and auto correlation by the harmonic
// mean of the power at each end of the baseline, as averaged
// across all channels
//
// created                                      rjc 2011.5.24
// modified to allow simultaneous R/L/X/Y pols  rjc 2015.11.2
// handle baseline-dependent nvis & vrsize      rjc 2018.10.18
// support redundant freq ids in ac lookup      jw  2024.07
// move nvis & vrsize into vrmeta               jw  2024.07

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "difx2mark4.h"


void normalize (const DifxInput * D,              // ptr to a filled-out difx input structure
                struct CommandLineOptions *opts,  // array of command line options
                vis_record_meta *vrmeta,          // array of vis. record meta data (num channels, size in byte, pfb index)
                vis_record *vrec,                 // pointer to start of vis. buffer
                int nvrtot,                       // total # of vis. records in buffer
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
           pant_ref,
           pant_rem,
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

                                    // now make a pass through the DiFX freq table, finding
                                    // all identical frequencies (whose IDs can be encountered
                                    // in SWIN visibilities, vrec[]::freq_index), and update
                                    // pmap[] to point all duplicate freqs to the lowest freq_index.
    for (i=0; i < D->nFreq-1 && i < MAX_DFRQ-1; i++)
        {
        if (pmap[i] != i)
            continue;
        for (n=i+1; n < D->nFreq && n < MAX_DFRQ; n++)
            {
                                    // at first keep distinction between sidebands, alas, this does
                                    // not work out well for mixed-sideband baselines
            if (isSameDifxFreq(&D->freq[i], &D->freq[n], /*ignore pcal:*/1))
                pmap[n] = i;
            else if(D->freq[i].sideband != D->freq[n].sideband)
                                    // try harder: "ignore" sideband, sufficient to have identical freq coverage
                {
                DifxFreq flipped;
                copyDifxFreq(&flipped, &D->freq[n]);
                flipped.freq = (flipped.sideband == 'U') ? (flipped.freq + flipped.bw) : (flipped.freq - flipped.bw);
                flipped.sideband = (flipped.sideband == 'U') ? 'L' : 'U';
                if (isSameDifxFreq(&D->freq[i], &flipped, /*ignore pcal:*/1))
                    pmap[n] = i;
               }
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
            pch += vrmeta[n].vrsize;
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
                for (i=0; i<vrmeta[n].nvis; i++)
                    sum += vrloop->comp[i].real;
                if (sum < 0.0)
                    {
                    sum = 0.0; // avoid pant[]=sqrt(<0)=NaN
                    //printf ("        bad auto data for ref/rem %d in record %d, DiFX frequency id %d\n", aref, n, vrloop->freq_index);
                    }
                pant[aref][fr][pol] = sqrt (sum / vrmeta[n].nvis);
                // printf("normalize() AUTO ant=%d vis#=%d %c %cSB visfq=%d pmap[%d]=%d sum=%.3f pant[%d][%d][%d]=%.3f\n",
                //         aref, n, polchar[pol], D->freq[vrloop->freq_index].sideband, vrloop->freq_index, vrloop->freq_index, fr, sum, aref, fr, pol, pant[aref][fr][pol]);
                // printf("n %d aref %d fr %d pol %d pant %f\n",
                //         n,aref,fr,pol,pant[aref][fr][pol]);
                }
            pch += vrmeta[n].vrsize;
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
            pant_ref = pant[aref][fr_remap][polref];
            pant_rem = pant[arem][fr_remap][polrem];

            // printf("normalize() VIZ %d-%d vis#=%d %c%c %cSB  pmap[visfq=%d]=%d   autopwr ref=%.3f rem=%.3f\n",
            //        aref, arem, n, polchar[polref], polchar[polrem], D->freq[vr->freq_index].sideband, vr->freq_index, fr_remap, pant_ref, pant_rem);

                                    // ensure that there is no 0-divide
            if (pant_ref == 0.0 || pant_rem == 0.0)
                {
                factor = 1.0;
                n_aczero++;
                if (opts->verbose > 2)
                    printf("missing autocorr for record %d ants ref rem %d %d "
                            "fr_remap %d pol %c%c\n", 
                            n,aref,arem,fr_remap, vr->pols[REF],vr->pols[REM]);
                }
            else
                factor = 1.0 / (pant_ref * pant_rem);

            for (i=0; i<vrmeta[n].nvis; i++)
                {
                vr->comp[i].real *= factor;
                vr->comp[i].imag *= factor;
                }
            pch = (char *) vr + vrmeta[n].vrsize;
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
