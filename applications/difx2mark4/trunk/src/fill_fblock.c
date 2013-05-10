// deconstruct difx table links that define selection and recording of channels,
// and how they were correlated. It then puts the information into an easy-to-use
// frequency structure

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "difx2mark4.h"

int fill_fblock (DifxInput *D,                    // difx input structure pointer
                 struct CommandLineOptions *opts, // ptr to input options
                 struct fblock_tag *pfb)          // pointer to table to be filled in
    {
    int i,
        j,
        k,
        n,
        nprod = 0,                  // index for a frequency & polarization pair
        irbAfid,
        irbBfid,
        ibandA,
        ibandB,
        irfAfid,
        irfBfid,
        ants[64],
        swapped,
        present,
        first,
        nant,
        nfreq,
        zoom;

    char pol,
         buff[5];

    double temp,
           freqs[128];

    DifxBaseline *pbl;
    DifxDatastream *pdsA,
                   *pdsB;
    DifxFreq *pfr;


                                    // first fill in the frequency block structure
    for (n=0; n<D->nBaseline; n++)
        {
        pbl = D->baseline + n;
        pdsA = D->datastream + pbl->dsA;
        pdsB = D->datastream + pbl->dsB;
        for (i=0; i<pbl->nFreq; i++)
            {
            for (j=0; j<*pbl->nPolProd; j++)
                {
                ibandA = pbl->bandA[i][j];
                ibandB = pbl->bandB[i][j];
                                    // bandA  (reference station)
                if (ibandA < pdsA->nRecBand)
                    {               // not zoom mode
                    zoom = FALSE;
                    irbAfid = pdsA->recBandFreqId[ibandA];
                    pol = pdsA->recBandPolName[ibandA];
                    irfAfid = pdsA->recFreqId[irbAfid];
                    pfr = D->freq + irfAfid;
                    }
                else                // zoom mode
                    {
                    zoom = TRUE;
                    irbAfid = pdsA->zoomBandFreqId[ibandA-pdsA->nRecBand];
                    pol = pdsA->zoomBandPolName[ibandA-pdsA->nRecBand];
                    irfAfid = pdsA->zoomFreqId[irbAfid];
                    pfr = D->freq + irfAfid;
                    }
                                    // stuff ref station fblock structure
                pfb[nprod].stn[0].pol      = pol;
                pfb[nprod].stn[0].ant      = pdsA->antennaId;
                pfb[nprod].stn[0].find     = irfAfid;
                pfb[nprod].stn[0].freq     = pfr->freq;
                pfb[nprod].stn[0].sideband = pfr->sideband;
                pfb[nprod].stn[0].bw       = pfr->bw;
                pfb[nprod].stn[0].bs       = pdsA->quantBits;
                pfb[nprod].stn[0].zoom     = zoom;

                                    // bandB  (remote station)
                if (ibandB < pdsB->nRecBand)
                    {               // not zoom mode
                    zoom = FALSE;
                    irbBfid = pdsB->recBandFreqId[ibandB];
                    pol = pdsB->recBandPolName[ibandB];
                    irfBfid = pdsB->recFreqId[irbBfid];
                    pfr = D->freq + irfBfid;
                    }
                else                // zoom mode
                    {
                    zoom = TRUE;
                    irbBfid = pdsB->zoomBandFreqId[ibandB-pdsB->nRecBand];
                    pol = pdsB->zoomBandPolName[ibandB-pdsB->nRecBand];
                    irfBfid = pdsB->zoomFreqId[irbBfid];
                    pfr = D->freq + irfBfid;
                    }
                                    // stuff rem station fblock structure
                pfb[nprod].stn[1].pol      = pol;
                pfb[nprod].stn[1].ant      = pdsB->antennaId;
                pfb[nprod].stn[1].find     = irfBfid;
                pfb[nprod].stn[1].freq     = pfr->freq;
                pfb[nprod].stn[1].sideband = pfr->sideband;
                pfb[nprod].stn[1].bw       = pfr->bw;
                pfb[nprod].stn[1].bs       = pdsB->quantBits;
                pfb[nprod].stn[1].zoom     = zoom;

                                    // if either antenna zoom, make both proper USB
                if (pfb[nprod].stn[0].zoom || pfb[nprod].stn[1].zoom)
                    for (k=0; k<2; k++)
                        if (pfb[nprod].stn[k].sideband == 'L')
                            {
                            pfb[nprod].stn[k].freq -= pfb[nprod].stn[k].bw;
                            pfb[nprod].stn[k].sideband = 'U';
                            }

                                    // bump and check product index
                if (++nprod > MAX_FPPAIRS)
                    {
                    printf ("too many freq-polpair combos; redimension\n");
                    return -1;
                    }
                }
            }
        }
                                    // now form & fill in the channel id's
                                    // make a list of all antennas present in fblock
    nant = 0;
    for (n=0; n<nprod; n++)
        for (k=0; k<2; k++)         // k = 0|1 for ref|rem antenna
            {
            present = FALSE;        // is antenna new?
            for (i=0; i<nant; i++)
                if (pfb[n].stn[k].ant == ants[i])
                    present = TRUE;
            if (!present)
                ants[nant++] = pfb[n].stn[k].ant;
            if (nant > 64)          // sanity check
                {
                printf ("too many antennas; redimension\n");
                return -1;
                }
            }

                                    // loop over those antennas
    for (i=0; i<nant; i++)
        {                           // for each antenna make a list of all frequencies
        nfreq = 0;
        for (n=0; n<nprod; n++)
            for (k=0; k<2; k++)     // k = 0|1 for ref|rem antenna
                {
                if (pfb[n].stn[k].ant == ants[i])
                    {               // antenna matches; look for unique freq
                    present = FALSE;// is frequency new?
                    for (j=0; j<nfreq; j++)
                        if (pfb[n].stn[k].freq == freqs[j])
                            present = TRUE;
                    if (!present)
                        freqs[nfreq++] = pfb[n].stn[k].freq;
                                    // sanity check
                    if (nfreq > 128)
                        {
                        printf ("too many frequencies; redimension\n");
                        return -1;
                        }
                    }
                }
                                    // bubble sort the frequency list
                                    // put LSB's just after corresponding USB's
        do
            {
            swapped = FALSE;
            for (j=0; j<nfreq-1; j++)
                if (fabs (freqs[j]) > fabs (freqs[j+1])
                 || fabs (freqs[j]) == fabs (freqs[j+1])
                 && freqs[j] < 0 && freqs[j+1] > 0)
                    {
                    temp = freqs[j];
                    freqs[j] = freqs[j+1];
                    freqs[j+1] = temp;
                    swapped = TRUE;
                    }
            }
        while (swapped);
                                    // remove (collapse out) redundant frequencies
        for (j=0; j<nfreq-1; j++)
            if (freqs[j] == freqs[j+1])
                {
                for (k=j;k<nfreq-1;k++)
                    freqs[k] = freqs[k+1];
                nfreq--;
                }
                                    // generate channel id's for each freq
        for (j=0; j<nfreq; j++)
            {
            sprintf (buff, "%c%02d", getband (fabs (freqs[j])), j);
            buff[3] = freqs[j] > 0 ? 'U' : 'L';
            buff[4] = 0;
            buff[5] = 0;
            first = TRUE;
                                    // insert channel id's back into fblock
                                    // everywhere that ant & freq match
            for (n=0; n<nprod; n++)
                for (k=0; k<2; k++)     // k = 0|1 for ref|rem antenna
                    if (pfb[n].stn[k].ant == i && pfb[n].stn[k].freq == freqs[j])
                        {
                        buff[4] = pfb[n].stn[k].pol;
                        strcpy (pfb[n].stn[k].chan_id, buff);
                        if (first)
                            {
                            pfb[n].stn[k].first_time = TRUE;
                            first = FALSE;
                            }
                        else
                            pfb[n].stn[k].first_time = FALSE;
                        }
            }
        }
    if (opts->verbose > 1)
        for (n=0; n<nprod; n++)     // debug - print out fblock table
            printf ("    fblock[%d] %c %c %d %d %d %d %.3f %.3f %s "
                    " %c %c %d %d %d %d %.3f %.3f %s\n",
                  n, pfb[n].stn[0].sideband, pfb[n].stn[0].pol, pfb[n].stn[0].first_time,
                  pfb[n].stn[0].ant, pfb[n].stn[0].find, pfb[n].stn[0].zoom, pfb[n].stn[0].freq,
                  pfb[n].stn[0].bw, pfb[n].stn[0].chan_id,
                  pfb[n].stn[1].sideband, pfb[n].stn[1].pol, pfb[n].stn[1].first_time,
                  pfb[n].stn[1].ant, pfb[n].stn[1].find, pfb[n].stn[1].zoom, pfb[n].stn[1].freq,
                  pfb[n].stn[1].bw, pfb[n].stn[1].chan_id);

    pfb[nprod].stn[0].ant = -1;     // mark end of table
    }
