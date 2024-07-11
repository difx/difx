// deconstruct difx table links that define selection and recording of channels,
// and how they were correlated. It then puts the information into an easy-to-use
// frequency structure

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "difx2mark4.h"

static int fblock_already_exists(const struct fblock_tag *, int, const struct fblock_tag *);
static int condense_internal_freqlist(fblock_tag_lite *, int *);

int fill_fblock (DifxInput *D,                    // difx input structure pointer
                 struct CommandLineOptions *opts, // ptr to input options
                 struct fblock_tag *pfb)          // pointer to table to be filled in
    {
    int i,
        j,
        k,
        n,
        m,
        nprod = 0,                  // index for a frequency & polarization pair
        irbAfid,
        irbBfid,
        ibandA,
        ibandB,
        irfAfid,
        irfBfid,
        irfABfid,
        ants[MAX_STN],
        swapped,
        present,
        first[2][2],                   // indexed by 0|1 for [USB/LSB][RCP/LCP]
        sbind,
        polind,
        nant,
        nantfreq,
        nallfreq,
        zoomA,
        zoomB,
        nbw,
        nfg;

    char polA, polB,
         buff[6];

    double filteredbw = 0.0;
    fblock_tag_lite antfreqs[MAX_DFRQ];
    fblock_tag_lite *allfreqs;

    DifxBaseline *pbl;
    DifxDatastream *pdsA,
                   *pdsB;
    DifxFreq *pfrA,
             *pfrB,
             *pfrAB;

                                    // bandwidth-based filtering option?
    if (strlen (opts->bandwidth) != 0)
        filteredbw = atof (opts->bandwidth);
    else
        filteredbw = 0.0;

                                    // first fill in the frequency block structure
    for (n=0; n<D->nBaseline; n++)
        {
        pbl = D->baseline + n;
        pdsA = D->datastream + pbl->dsA;
        pdsB = D->datastream + pbl->dsB;
        for (i=0; i<pbl->nFreq; i++)
            {
            irfABfid = pbl->destFq[i];
            pfrAB = D->freq + irfABfid;
            for (j=0; j<*pbl->nPolProd; j++)
                {
                ibandA = pbl->bandA[i][j];
                ibandB = pbl->bandB[i][j];

                                    // bandA  (reference station)
                if (ibandA < pdsA->nRecBand)
                    {               // not zoom mode
                    zoomA = FALSE;
                    irbAfid = pdsA->recBandFreqId[ibandA];
                    polA = pdsA->recBandPolName[ibandA];
                    irfAfid = pdsA->recFreqId[irbAfid];
                    pfrA = D->freq + irfAfid;
                    }
                else                // zoom mode
                    {
                    zoomA = TRUE;
                    irbAfid = pdsA->zoomBandFreqId[ibandA-pdsA->nRecBand];
                    polA = pdsA->zoomBandPolName[ibandA-pdsA->nRecBand];
                    irfAfid = pdsA->zoomFreqId[irbAfid];
                    pfrA = D->freq + irfAfid;
                    }

                                    // bandB  (remote station)
                if (ibandB < pdsB->nRecBand)
                    {               // not zoom mode
                    zoomB = FALSE;
                    irbBfid = pdsB->recBandFreqId[ibandB];
                    polB = pdsB->recBandPolName[ibandB];
                    irfBfid = pdsB->recFreqId[irbBfid];
                    pfrB = D->freq + irfBfid;
                    }
                else                // zoom mode
                    {
                    zoomB = TRUE;
                    irbBfid = pdsB->zoomBandFreqId[ibandB-pdsB->nRecBand];
                    polB = pdsB->zoomBandPolName[ibandB-pdsB->nRecBand];
                    irfBfid = pdsB->zoomFreqId[irbBfid];
                    pfrB = D->freq + irfBfid;
                    }

                                    // info of output band
                pfb[nprod].stn[0].pol      = polA;
                pfb[nprod].stn[0].ant      = pdsA->antennaId;
                pfb[nprod].stn[0].find     = irfAfid;
                pfb[nprod].stn[0].fdest    = irfABfid;
                pfb[nprod].stn[0].freq     = pfrAB->freq;
                pfb[nprod].stn[0].sideband = pfrAB->sideband;
                pfb[nprod].stn[0].bw       = pfrAB->bw;
                pfb[nprod].stn[0].bs       = pdsA->quantBits;
                pfb[nprod].stn[0].zoom     = zoomA;
                pfb[nprod].stn[0].pcal_int = pdsA->phaseCalIntervalMHz / pdsA->phaseCalIntervalDivisor;
                pfb[nprod].stn[0].n_spec_chan = pfrAB->nChan / pfrAB->specAvg;
                pfb[nprod].stn[1].pol      = polB;
                pfb[nprod].stn[1].ant      = pdsB->antennaId;
                pfb[nprod].stn[1].find     = irfBfid;
                pfb[nprod].stn[1].fdest    = irfABfid;
                pfb[nprod].stn[1].freq     = pfrAB->freq;
                pfb[nprod].stn[1].sideband = pfrAB->sideband;
                pfb[nprod].stn[1].bw       = pfrAB->bw;
                pfb[nprod].stn[1].bs       = pdsB->quantBits;
                pfb[nprod].stn[1].zoom     = zoomB;
                pfb[nprod].stn[1].pcal_int = pdsB->phaseCalIntervalMHz / pdsB->phaseCalIntervalDivisor;
                pfb[nprod].stn[1].n_spec_chan = pfrAB->nChan / pfrAB->specAvg;

                                // store info of the destination product i.e. output band, if new,
                                // and if not itself already covered by the source product stored higher above
                if (!fblock_already_exists(pfb, nprod, &pfb[nprod]))
                    ++nprod;

                                    // check product index
                if (nprod > MAX_FPPAIRS-1)
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
            if (nant > MAX_STN)     // sanity check
                {
                printf ("too many antennas; redimension\n");
                return -1;
                }
            }

                                    // global output frequency table from DiFX but simplified with PCal-related duplicated entries removed,
                                    // used in mapping the multiple DiFX indices per sky freq to single freqs for the mk4 t101 t120 indices
    nallfreq = 0;
    allfreqs = calloc(MAX_DFRQ*nant, sizeof(fblock_tag_lite));
    for (n=0; n<nprod; n++)
        for (k=0; k<2; k++)     // k = 0|1 for ref|rem antenna
            {
            const double f = D->freq[pfb[n].stn[k].fdest].freq;
            const char sb = D->freq[pfb[n].stn[k].fdest].sideband;
            present = FALSE;// is frequency new?
            for (j=0; j<nallfreq && !present; j++)
                if (allfreqs[j].freq == f && allfreqs[j].sideband == sb)
                    present = TRUE;
            if (!present)
                {
                allfreqs[nallfreq].sideband = sb;
                allfreqs[nallfreq].freq = f;
                nallfreq++;
                }
            if (nallfreq > MAX_DFRQ*nant)
                {
                printf ("too many frequencies, exceeding MAX_DFRQ x %d antennas; redimension MAX_DFRQ\n", nant);
                return -1;
                }
            }
    nallfreq = condense_internal_freqlist(allfreqs, &nallfreq);

                                    // loop over antennas for antenna- and baseline-specific frequencies
    for (i=0; i<nant; i++)
        {                           // for each antenna make a list of all of its frequencies
        nantfreq = 0;
        memset(antfreqs, 0, sizeof(antfreqs));
        for (n=0; n<nprod; n++)
            for (k=0; k<2; k++)     // k = 0|1 for ref|rem antenna
                {
                if (pfb[n].stn[k].ant == i)
                    {               // antenna matches; look for unique freq
                    const double f = D->freq[pfb[n].stn[k].fdest].freq;
                    const char sb = D->freq[pfb[n].stn[k].fdest].sideband;
                    present = FALSE;// is frequency new?
                    for (j=0; j<nantfreq; j++)
                        if (antfreqs[j].freq == f) // DiFX-2.5/2.6 -style: ignore sb for antenna freq list (but not for global freq list)
                            present = TRUE;
                    if (!present)
                        {
                        antfreqs[nantfreq].sideband = sb;
                        antfreqs[nantfreq].freq = f;
                        nantfreq++;
                        }
                                    // sanity check
                    if (nantfreq > MAX_DFRQ)
                        {
                        printf ("too many frequencies, exceeding MAX_DFRQ; redimension\n");
                        return -1;
                        }
                    }
                }
        nantfreq = condense_internal_freqlist(antfreqs, &nantfreq);

                                    // generate channel id's for each freq
        for (j=0; j<nantfreq; j++)
            {
            sprintf (buff, "%c%02d", getband (antfreqs[j].freq), j % 100);
            if (j >= 100)
                fprintf (stderr, "Warning: frequency record %d exceeds 2-digit limit of generated VEX channel IDs! Result might be unusable.\n", j);
            buff[3] = 'n';
            buff[4] = 0;
            buff[5] = 0;
            first[0][0] = TRUE;     // first time for [sb][pol]
            first[0][1] = TRUE;
            first[1][0] = TRUE;
            first[1][1] = TRUE;
                                    // insert channel id's back into fblock
                                    // everywhere that ant & freq match
            for (n=0; n<nprod; n++)
                for (k=0; k<2; k++)     // k = 0|1 for ref|rem antenna
                    {
                    const int fdest = pfb[n].stn[k].fdest;
                    if (pfb[n].stn[k].ant == i
                        && D->freq[fdest].freq == antfreqs[j].freq  // DiFX-2.5/2.6 -style: ignore sb for antenna freq list (but not for global 'allfreqs' freq list)
                        && (filteredbw <= 0.0 || D->freq[fdest].bw == filteredbw))
                        {
                        buff[3] = D->freq[fdest].sideband;
                        buff[4] = pfb[n].stn[k].pol;
                        strcpy (pfb[n].stn[k].chan_id, buff);
                        for (m=0; m<nallfreq; m++)
                            if (D->freq[fdest].freq == allfreqs[m].freq
                                && D->freq[fdest].sideband == allfreqs[m].sideband)
                                {
                                pfb[n].stn[k].fmk4 = m;
                                break;
                                }
                        sbind  = (buff[3] == 'U') ? 0 : 1;
                        polind = (pfb[n].stn[k].pol == 'R' 
                               || pfb[n].stn[k].pol == 'X') ? 0 : 1;
                                    // see if first mention for ant, freq, sb, & pol
                        if (first[sbind][polind])
                            {
                            pfb[n].stn[k].first_time = TRUE;
                            first[sbind][polind] = FALSE;
                            }
                        else
                            pfb[n].stn[k].first_time = FALSE;
                        }
                    }
            }

        }

        
                                    // if freq groups specified, remove any non-matching lines
    if (strlen (opts->fgroups) != 0)
        {
        nfg = 0;
        for (n=0; n<nprod; n++)
            if (strchr (opts->fgroups, pfb[n].stn[0].chan_id[0]) == NULL)
                {
                for (i=n; i<nprod-1; i++) // slide remaining entries down one location
                    pfb[i] = pfb[i+1];
                pfb[i].stn[0].ant = 0;    // mark new end
                nprod--;                  // one less pfb entry
                n--;                      //need to reexamine this slot now
                nfg++;
                }
        fprintf (stderr, "%d correlation product channels deleted due to fgroup not %s\n",
                 nfg, opts->fgroups);
        }
                                    // if desired bandwidth specified, 
                                    // remove any non-matching lines
    if (filteredbw > 0)
        {
        nbw = 0;
        for (n=0; n<nprod; n++)
            if (filteredbw != pfb[n].stn[0].bw)
                {
                for (i=n; i<nprod-1; i++) // slide remaining entries down one location
                    pfb[i] = pfb[i+1];
                pfb[i].stn[0].ant = 0;    // mark new end
                nprod--;                  // one less pfb entry
                n--;                      //need to reexamine this slot now
                nbw++;
                }
        fprintf (stderr, "%d correlation product channels deleted due to bw not %s (parsed as ~%.6f)\n",
                 nbw, opts->bandwidth, filteredbw);
        }

    if (opts->verbose > 1)
        {
        fprintf (stderr, " mk4 fqId   low edge\n");
        for (m=0; m<nallfreq; m++)
            fprintf (stderr, "      %3d   %.3f\n", m, allfreqs[m]);
        }

    if (opts->verbose > 1)
        {
        fprintf (stderr, "                ch_id s p 1st a fqi fqo fm4  z pc bs freq        bw        #vis\n");
        for (n=0; n<nprod; n++)     // debug - print out fblock table
            fprintf (stderr,
                    "   fblock[%04d] %s %c %c  %c %2d %3d %3d %3d %2d %3d %1d %-11.3f %-8.3f %4d\n"
                    "                %s %c %c  %c %2d %3d %3d %3d %2d %3d %1d %-11.3f %-8.3f %4d\n",
                  n, pfb[n].stn[0].chan_id, pfb[n].stn[0].sideband, pfb[n].stn[0].pol,
                  pfb[n].stn[0].first_time ? 'y' : 'n', pfb[n].stn[0].ant,
                  pfb[n].stn[0].find, pfb[n].stn[0].fdest, pfb[n].stn[0].fmk4,
                  pfb[n].stn[0].zoom, (int)(pfb[n].stn[0].pcal_int+0.5), pfb[n].stn[0].bs,
                  pfb[n].stn[0].freq, pfb[n].stn[0].bw, pfb[n].stn[0].n_spec_chan,

                  pfb[n].stn[1].chan_id, pfb[n].stn[1].sideband, pfb[n].stn[1].pol,
                  pfb[n].stn[1].first_time ? 'y' : 'n', pfb[n].stn[1].ant,
                  pfb[n].stn[1].find, pfb[n].stn[1].fdest, pfb[n].stn[1].fmk4,
                  pfb[n].stn[1].zoom, (int)(pfb[n].stn[1].pcal_int+0.5), pfb[n].stn[1].bs,
                  pfb[n].stn[1].freq, pfb[n].stn[1].bw, pfb[n].stn[1].n_spec_chan);
        }

    pfb[nprod].stn[0].ant = -1;     // mark end of table
    return 0;                       // signify that all is well
    }


static int fblock_already_exists(const struct fblock_tag *entries, int nentries, const struct fblock_tag *candidate)
    {
    int n, k;

    for (n=0; n<nentries; n++)
        {
        int match[2] = {0,0};
        for (k=0; k<2; k++)     // k = 0|1 for ref|rem antenna
            {
            if (entries[n].stn[k].pol      == candidate->stn[k].pol  &&
                entries[n].stn[k].ant      == candidate->stn[k].ant  &&
                entries[n].stn[k].find     == candidate->stn[k].find &&
                entries[n].stn[k].fdest    == candidate->stn[k].fdest    &&
                entries[n].stn[k].fmk4     == candidate->stn[k].fmk4     &&
                entries[n].stn[k].freq     == candidate->stn[k].freq     &&
                entries[n].stn[k].sideband == candidate->stn[k].sideband &&
                entries[n].stn[k].bw       == candidate->stn[k].bw       &&
                entries[n].stn[k].bs       == candidate->stn[k].bs       &&
                entries[n].stn[k].zoom     == candidate->stn[k].zoom     &&
                entries[n].stn[k].pcal_int == candidate->stn[k].pcal_int)
                    {
                    match[k] = 1;
                    }
            }
        if (match[0] && match[1])
            {
            return 1;
            }
        }

    return 0;
    }

static int condense_internal_freqlist(fblock_tag_lite *freqs, int *nfreq)
    {
    int j,
        k,
        swapped;

                                    // bubble sort the frequency list
    do
        {
        swapped = FALSE;
        for (j=0; j<*nfreq-1; j++)
            if (freqs[j].freq > freqs[j+1].freq)
                {
                fblock_tag_lite temp = freqs[j];
                freqs[j] = freqs[j+1];
                freqs[j+1] = temp;
                swapped = TRUE;
                }
        }
    while (swapped);

                                    // remove (collapse out) redundant frequencies
    for (j=0; j<*nfreq-1; j++)
        if (freqs[j].freq == freqs[j+1].freq && freqs[j].sideband == freqs[j+1].sideband)
            {
            for (k=j;k<*nfreq-1;k++)
                freqs[k] = freqs[k+1];
            *nfreq--;
            }

    return *nfreq;
    }

