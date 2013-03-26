// new_type1 adds a type 1 fileset based upon the difx data structures
// for one baseline
//
//  first created from createType1s                  rjc  2012.5.8

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

int new_type1 (DifxInput *D,                    // ptr to a filled-out difx input structure
               int nb,                          // (next open) index to base_index array
               int a1, 
               int a2, 
               int blind, 
               int *base_index,
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
    int i,
        j,
        pol,
        findex,
        gindex,
        indA,
        indB,
        autocorr_is_A,
        done[4];

    struct type_000 t000;
    struct type_100 t100;
    struct type_101 t101;

    char outname[DIFXIO_FILENAME_LENGTH],
         c,
         lchan_id[5],
         rchan_id[5],
         refpol,
         rempol;

    DifxDatastream *pdsA,
                   *pdsB;

    enum polars {LL, RR, LR, RL};

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

                                    // determine index into frequency table which
                                    // depends on the recorded subbands that are correlated

                                    // point to reference/A and remote/B datastreams
    pdsA = &D->datastream[D->baseline[blind].dsA];
    pdsB = &D->datastream[D->baseline[blind].dsB];
                                    // for auto-correlations need to tweak datastreams
                                    // since baseline table only contains cross-corrs
                                    // do so by overwriting index from baseline table,
                                    // and also doubling the used datastream
    if (a1 == a2)
        {
        if (pdsA->antennaId == a1)
            {                       // ds A and its index will be used for both ref & rem
            autocorr_is_A = TRUE;
            pdsB = pdsA;
            }
        else if (pdsB->antennaId == a1)
            {                       // ds B and its index will be used for both ref & rem
            autocorr_is_A = FALSE;
            pdsA = pdsB;
            }
        else
            printf ("Internal consistency error for autocorrelation! \n");
        }

                                    // construct and write type 101 records for each chan
    i = 0;
    for (i=0; i<D->baseline[blind].nFreq; i++)
        {
        for (pol=0; pol<4; pol++)
            done[pol] = FALSE;      // keep from having dups on 1x2 case

                                    // loop over 1, 2, or 4 pol'n. products
        for (pol=0; pol<D->baseline[blind].nPolProd[i]; pol++)
            {
                                    // find actual freq index of this recorded band
                                    // or possibly of a zoomed-in band
            indA = D->baseline[blind].bandA[i][pol];
            indB = D->baseline[blind].bandB[i][pol];
                                    // override A or B for autocorrelations
            if (a1 == a2)
                {
                if (autocorr_is_A)
                    indB = indA;
                else 
                    indA = indB;
                }
                                    // reference antenna
            if (indA < 0 || indA >= pdsA->nRecBand + pdsA->nZoomBand)
                {
                printf ("Error! bandA[%d][%d] = %d out of range\n",
                        i, pol, indA);
                printf ("nRecBand %d nZoomBand %d\n",
                        pdsA->nRecBand, pdsA->nZoomBand);
                return (-1);
                }
            if (indA < pdsA->nRecBand)
                {
                j = pdsA->recBandFreqId[indA];
                findex = pdsA->recFreqId[j];
                refpol = pdsA->recBandPolName[indA];
                }
            else                    // zoom mode
                {
                j = indA - pdsA->nRecBand;
                findex = pdsA->zoomFreqId[pdsA->zoomBandFreqId[j]];
                refpol = pdsA->zoomBandPolName[j];
                }
                                    // remote antenna
            if (indB < 0 || indB >= pdsB->nRecBand + pdsB->nZoomBand)
                {
                printf ("Error! bandB[%d][%d] = %d out of range\n",
                        i, pol, indB);
                printf ("nRecBand %d nZoomBand %d\n",
                        pdsB->nRecBand, pdsB->nZoomBand);
                return (-1);
                }
            if (indB < pdsB->nRecBand)
                {
                j = pdsB->recBandFreqId[indB];
                gindex = pdsB->recFreqId[j];
                rempol = pdsB->recBandPolName[indB];
                }
            else                    // zoom mode
                {
                j = indB - pdsB->nRecBand;
                gindex = pdsB->zoomFreqId[pdsB->zoomBandFreqId[j]];
                rempol = pdsB->zoomBandPolName[j];
                }

                                    // sanity check that both stations refer to same freq
            if (findex != gindex)
              printf ("Warning, mismatching frequency indices! (%d vs %d)\n",
                                        findex, gindex);
                                    // generate index that is 10 * freq_index + pol + 1
            t101.index = 10 * findex;
            c = getband (D->freq[findex].freq);

                                    // prepare ID strings for both pols, if there
            if (D->baseline[blind].nPolProd[i] > 1)
                {
                sprintf (lchan_id, "%c%02d?", c, 2 * i);
                lchan_id[3] = (D->freq+findex)->sideband;
                sprintf (rchan_id, "%c%02d?", c, 2 * i + 1);
                rchan_id[3] = (D->freq+findex)->sideband;
                }
            else                    // both the same (only one used)
                {
                sprintf (lchan_id, "%c%02d?", c, i);
                lchan_id[3] = (D->freq+findex)->sideband;
                strcpy (rchan_id, lchan_id);
                }
                                    // insert ID strings for reference & remote
            if (refpol == 'L' && rempol == 'L')
                {
                if (done[LL])       // eliminate 1x2 dups
                    continue;
                strcpy (t101.ref_chan_id, lchan_id);
                strcpy (t101.rem_chan_id, lchan_id);
                t101.index += 1;
                done[LL] = TRUE;
                }
            else if (refpol == 'R' && rempol == 'R')
                {
                if (done[RR])
                    continue;
                strcpy (t101.ref_chan_id, rchan_id);
                strcpy (t101.rem_chan_id, rchan_id);
                t101.index += 2;
                done[RR] = TRUE;
                }
            else if (refpol == 'L' && rempol == 'R')
                {
                if (done[LR])
                    continue;
                strcpy (t101.ref_chan_id, lchan_id);
                strcpy (t101.rem_chan_id, rchan_id);
                t101.index += 3;
                done[LR] = TRUE;
                }
            else if (refpol == 'R' && rempol == 'L')
                {
                if (done[RL])
                    continue;
                strcpy (t101.ref_chan_id, rchan_id);
                strcpy (t101.rem_chan_id, lchan_id);
                t101.index += 4;
                done[RL] = TRUE;
                }
            write_t101 (&t101, fout[nb]);
            }
        }
    return (0);
    }
