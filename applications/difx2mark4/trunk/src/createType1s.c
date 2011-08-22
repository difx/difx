// createType1s creates a type 1 fileset based upon the difx data structures
// there is one type 1 output file for each baseline in the difx scan
//
//  first created                              rjc  2010.2.23
//  modify vis file read logic; normalize vis  rjc  2011.5.18

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

#define NUMFILS 500                 // max number of type 1 output files
#define SCALE 10000.0               // amplitude factor to normalize for fourfit
#define NOVIS -9999                 // indicates no visibility records in RAM


int createType1s (DifxInput *D,     // ptr to a filled-out difx input structure
          int *jobId,               // ptr to curr job. May change here, if scan bridges jobs
          int scanId,               // id of this single scan
          char *node,               // directory for output fileset
          char *rcode,              // 6 letter root suffix
          struct stations *stns,    // struct contains station name information
          struct CommandLineOptions *opts, // ptr to input options
          char *rootname)           // full root file name
    {
    const int header_size = (8*sizeof(int)) + (5*sizeof(double)) + (2*sizeof(char)); 

    int i,
        findex,
        n,
        nvis,
        vrsize,                     // size of vis. records in bytes
        pol,
        a1, a2,                     // antenna indices for current baseline, 0-relative
        blind = 0,                  // baseline index into the baseline array structure
        nread = 0,                  // number of vis records read for current scan
        nflagged = 0,               // number of visibility records flagged
        noscan = 0,                 // number vis recs with no corresponding scan
        base_index[NUMFILS],        // base_index[i] contains baseline for file fout[i]
        n120[NUMFILS],              // # of records in each t120 file
        n120_flipped,
        n120_tot,
        gv_stat,
        oldScan,
        cn_lab;                     // channel # used as a label in the channel name

    char inname[DIFXIO_FILENAME_LENGTH],    // file name of input data file
         dirname[DIFXIO_FILENAME_LENGTH],
         outname[DIFXIO_FILENAME_LENGTH],
         blines[NUMFILS][3],        // null-terminated baselines list
         poltab [4][3] = {"LL", "RR", "LR", "RL"},
         lchan_id[5],
         rchan_id[5],
         c;

    DifxDatastream *pdsA,
                   *pdsB;
                                    // variables that need persistence due to exit
                                    // up into caller over scan boundaries
    static char corrdate[16];
    static int nvr = NOVIS,         // index of current visibility record
               nvrtot,              // total number of visibility records in buffer
               currentScan;
    static vis_record *rec,
                      *vrec;
    
    double q_factor,                // quantization correction factor
           scale_factor,            // scaling factor includes Van Vleck and SCALE
           sb_factor[64],           // +1 | -1 for USB | LSB by channel
           rscaled,
           iscaled;

    FILE *fout[NUMFILS];
    DIR *pdir;
    struct dirent *dent;

    struct type_000 t000;
    struct type_100 t100;
    struct type_101 t101;
    union u_tag
        {
        struct type_120 t120;
        float dummy[2*MAX_VIS+10];  // reserve enough space for MAX_VIS visibilities
        } u;

                                    // function prototypes
    int recordIsFlagged (double, int, int, const DifxJob *);
    int getBaselineIndex (DifxInput *, int, int);
    int openReadVisFile (FILE *, vis_record *, int);

                                    // initialize memory as necessary
                                    // quantization correction factor is pi/2 for
                                    // 1 bit, or ~1.13 for 2 bit (see TMS, p.300)
                                    // Note that these values apply to the weak signal
                                    // (i.e. cross-correlation) case only.
    q_factor = (D->quantBits == 1) ? 1.57080 : 1.13312;
    if (opts->verbose > 0)
        printf ("      visibility scale factor %9.5lf\n", q_factor * SCALE);
                                    // compensate for LSB fringe rotator direction
    for (i=0; i<D->nFreq; i++)
        sb_factor[i] = ((D->freq+i)->sideband == 'U') ? 1.0 : -1.0;

    for (i=0; i<NUMFILS; i++)
        {
        base_index[i] = -1;
        n120[i] = 0;
        }
                                    // number of (spectral) visibility points per array
    nvis = D->nOutChan;
                                    // make sure we don't overrun our arrays
    if (nvis > MAX_VIS)
        {
        fprintf (stderr, 
                "fatal error: # visibilities (%d) exceeds array dimension (%d)\n",
                nvis, MAX_VIS);
        return (-1);
        }
                                    // calculate (variable) size of visibility record
    vrsize = sizeof (vis_record) - sizeof (vrec->comp) + 2 * nvis * sizeof (float);

                                    // clear record areas
    memset (&t000, 0, sizeof (t000));
    memset (&t100, 0, sizeof (t100));
    memset (&t101, 0, sizeof (t101));
    memset (&u, 0, sizeof (u));
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

                                    // type_120
    memcpy (u.t120.record_id, "120", 3);
    memcpy (u.t120.version_no, "00", 2);
    u.t120.type = SPECTRAL;
    u.t120.nlags = nvis;
    memcpy (u.t120.rootcode, rcode, 6);

                                    // only open new file if one isn't already open
                                    // loop over all jobs with data for this scan
    currentScan = -1;
    while (TRUE)
        {
        if (nvr == NOVIS)           // do we need to read a(nother) Swinburne file?
            {
                                    // form directory name for input file, based on jobId
            strcpy (dirname, D->job[*jobId].outputFile);
                                    // open input (SWIN) .difx file
                                    // first open directory it resides in
            pdir = opendir (dirname);
            if (pdir == NULL)
                {
                perror ("difx2mark4");
                fprintf (stderr, "fatal error opening input data directory %s\n", dirname);
                return (-1);
                }
                                    // for now, assume there is only one datafile present
                                    // FIXME add support for
                                    // - multiple files
                                    // - multiple pulsar bins
                                    // - multiple phase centres
            do
                dent = readdir (pdir);
            while                       // skip over all files not starting with DIFX_
                (dent != NULL && strncmp (dent->d_name, "DIFX_", 5) != 0);

            if (dent == NULL)
                {
                if (errno)
                    perror ("difx2mark4");
                fprintf (stderr, "problem finding data in %s\n", dirname);
                return (-1);
                }
            free(pdir);

            strcpy (inname, dirname);
            strcat (inname, "/");
            strcat (inname, dent->d_name);
                                    // open and read a complete Swinburne file
            gv_stat = get_vis (inname, opts, nvis, vrsize, &vrec, &nvrtot, corrdate);
            if (gv_stat < -1)       // -1 is normal (EOF); anything less is an error
                {
                perror ("difx2mark4");
                fprintf (stderr, "error (%d) accessing input data file %s\n", 
                         gv_stat, inname);
                return (-1);
                }
            if (opts->verbose > 0)
                printf ("      get_vis read %d visibility records from \n       %s\n",
                         nvrtot, inname); fflush(stdout);
            nvr = -1;               // set index just prior to first record in buffer
                                    // unless raw mode requested, normalize visibilities
            if (opts->raw == 0)
                normalize (opts, vrec, nvrtot, nvis, vrsize);
            }

                                    // loop over records within both scan and job
        while (nvr != NOVIS)
            {
            nvr++;                  // move on to next input record
            nread++;
            if (nvr == nvrtot)
                {                   // we've used up all the records in the ram buffer
                nvr = NOVIS;
                free (vrec);        // free up all memory in ram buffer
                *(jobId) += 1;      // stop looping and increment job
                nread--;
                break;
                }
                                    // form pointer to current vis. record
            rec = (vis_record *) ((char *) vrec + nvr * vrsize);  
                                    // check for new scan
            oldScan = currentScan;
            currentScan = DifxInputGetScanIdByJobId (D, rec->mjd+rec->iat/8.64e4, *jobId);
        
            if (currentScan == -1)
                {
                if (opts->verbose > 0)
                    printf ("      Header time (%d:%6.1f) and jobId %d not in a scan\n",
                            rec->mjd, rec->iat, *jobId);
                                    // restore currentScan if bogus time encountered
                currentScan = oldScan;
                noscan++;
                continue;           // skip this record
                }
            else if (currentScan != scanId)
                {                   // current record is the start of a new scan
                printf ("      Header time (%d %6.1f) indicates new scan %d (current scanId %d)\n",
                        rec->mjd, rec->iat, currentScan, scanId);
                nvr--;              // reposition to allow re-read of record
                nread--;
                break;              // exit out of job loop
                }
                                    // compute antenna indices for this record
            if (D->job->antennaIdRemap)
                {
                a1 = *((D->job+*jobId)->antennaIdRemap + rec->baseline / 256 - 1); 
                a2 = *((D->job+*jobId)->antennaIdRemap + rec->baseline % 256 - 1); 
                }
            else                    // no remapping, just use indices from file
                {
                a1 = rec->baseline / 256 - 1; 
                a2 = rec->baseline % 256 - 1; 
                }
                                    // no Van Vleck for strong signal (autocorr) case
            scale_factor = (a1 == a2) ? SCALE : q_factor * SCALE;

                                    // check that both antennas are in the root file
            if((stns + a1)->inscan != TRUE || (stns + a2)->inscan != TRUE)
                {
                printf (
                "WARNING Visibility found for baseline %x %c%c-%c%c which isn't in scan!\n",
                 rec->baseline, (stns + a1)->intl_name[0], (stns + a1)->intl_name[1],
                                (stns + a2)->intl_name[0], (stns + a2)->intl_name[1]);
                continue;           // skip this record
                }

                                    // check if either antenna is flagged a priori
                                    // for this baseline. If so, disregard and move on

            if (recordIsFlagged(rec->mjd + rec->iat/8.64e4, a1, a2, &D->job[*jobId]))
                {
                if (opts->verbose > 1)
                    printf ("          flagged: an antenna (%d or %d) off source at %lf\n",
                            a1, a2, rec->mjd + rec->iat/8.64e4);
                nflagged++;
                continue;           // skip this record
                }


                                    // find the output file for this baseline
            for (n=0; n<NUMFILS; n++)
                {
                if (base_index[n] == rec->baseline)
                    break;          // found baseline, exit loop

                else if (base_index[n] < 0)
                    {               // at list end, must add a new baseline
                                    // determine which baseline array to use
                    if ((blind = getBaselineIndex (D, a1, a2)) < 0)
                        {
                        fprintf (stderr, 
                                "WARNING Couldn't properly identify baseline %d in .input file.\n",
                                rec->baseline);
                        blind = 0;      // use first one in list and muster on
                        }
                                    // append new baseline to list
                    base_index[n] = rec->baseline;
                    (stns + a1)->invis = TRUE;
                    (stns + a2)->invis = TRUE;

                                    // create name & open new output file
                                    // assume that site ID order is same as station order
                                    // probably not valid, though - THIS NEEDS WORK!!  
                    strcpy (outname, node);
                    strcat (outname, "/");
                    blines[n][0] = (stns+a1)->mk4_id;
                    blines[n][1] = (stns+a2)->mk4_id;
                    blines[n][2] = 0;
                    if (opts->verbose > 0)
                        printf ("      rec->baseline %d blines <%s>\n", rec->baseline, blines[n]);
                    strcat (outname, &blines[n][0]);
                    strcat (outname, "..");
                    strcat (outname, rcode);

                    fout[n] = fopen (outname, "w");
                    if (fout[n] == NULL)
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
                    fwrite (&t000, sizeof (t000), 1, fout[n]);

                                    // construct and write type 100 record
                    memcpy (t100.baseline, &blines[n][0], 2);
                    t100.nindex = D->baseline[blind].nFreq * D->baseline[blind].nPolProd[0];
                    write_t100 (&t100, fout[n]);

                                    // determine index into frequency table which
                                    // depends on the recorded subbands that are correlated

                                    // point to reference/A and remote/B datastreams
                    pdsA = &D->datastream[D->baseline[blind].dsA];
                    pdsB = &D->datastream[D->baseline[blind].dsB];

                                    // construct and write type 101 records for each chan
                    for (i=0; i<D->baseline[blind].nFreq; i++)
                        {
                                    // loop over 1, 2, or 4 pol'n. products
                        for (pol=0; pol<D->baseline[blind].nPolProd[i]; pol++)
                            {
                                    // find actual freq index of this recorded band
                            findex = pdsA->recBandFreqId[D->baseline[blind].bandA[i][pol]];
                                    // sanity check that both stations refer to same freq
                            if (findex != pdsB->recBandFreqId[D->baseline[blind].bandB[i][pol]])
                                printf ("Warning, mismatching frequency indices!\n");
                                    // generate index that is 10 * freq_index + pol + 1
                            t101.index = 10 * findex + pol + 1;
                            c = getband (D->freq[findex].freq);
                                    // prepare ID strings for both pols, if there
                            if (D->baseline[blind].nPolProd[i] > 1)
                                {
                                cn_lab = 2 * findex;
                                sprintf (lchan_id, "%c%02d?", c, cn_lab);
                                lchan_id[3] = (D->freq+findex)->sideband;
                                cn_lab++; 
                                sprintf (rchan_id, "%c%02d?", c, cn_lab);
                                rchan_id[3] = (D->freq+findex)->sideband;
                                }
                            else    // both the same (only one used)
                                {
                                sprintf (lchan_id, "%c%02d?", c, findex);
                                lchan_id[3] = (D->freq+findex)->sideband;
                                strcpy (rchan_id, lchan_id);
                                }

                            switch (pol)
                                {
                                case 0: // LL
                                    strcpy (t101.ref_chan_id, lchan_id);
                                    strcpy (t101.rem_chan_id, lchan_id);
                                    break;
                                case 1: // RR
                                    strcpy (t101.ref_chan_id, rchan_id);
                                    strcpy (t101.rem_chan_id, rchan_id);
                                    break;
                                case 2: // LR
                                    strcpy (t101.ref_chan_id, lchan_id);
                                    strcpy (t101.rem_chan_id, rchan_id);
                                    break;
                                case 3: // RL
                                    strcpy (t101.ref_chan_id, rchan_id);
                                    strcpy (t101.rem_chan_id, lchan_id);
                                    break;
                                }
                            write_t101 (&t101, fout[n]);
                            }
                        }
                    break;
                    }                   // end of block to append new baseline
                }                       // either found baseline file or created new one
                                    // unless record was invalid:
            if (n == NUMFILS)
                {                       // fell out of loop -- shouldn't happen!
                printf ("Error! More files (>%d) than program is compiled to handle.\n", n);
                return (-1);
                }

            if (base_index[n] < 0)
                continue;           // to next record 

                                    // copy visibilities into type 120 record
            for (i=0; i<nvis; i++)
                {                   
                rscaled = rec->comp[i].real;
                iscaled = rec->comp[i].imag;
                if (fabs(rscaled) > MAGLIM || fabs (iscaled) > MAGLIM)
                    {               // impossibly large values overwritten with 0
                    printf ("Warning! Corrupt visibility %le %le for baseline %s in input file\n",
                            rscaled, iscaled, blines[n]);
                    rscaled = 0.0;
                    iscaled = 0.0;
                    }
                else
                    {
                    rscaled = rec->comp[i].real * scale_factor;
                    iscaled = rec->comp[i].imag * scale_factor;
                    }

                if (sb_factor[rec->freq_index] > 0)
                    {
                    u.t120.ld.spec[i].re = rscaled;
                    u.t120.ld.spec[i].im = iscaled;
                    }
                else                // reverse order of points in LSB spectrum
                    {               // and conjugate for rotator direction difference
                    u.t120.ld.spec[nvis-i-1].re =  rscaled;
                    u.t120.ld.spec[nvis-i-1].im = -iscaled;
                    }
                }
            strncpy (u.t120.baseline, blines[n], 2);
                                    // FIXME (perhaps) -assumes all freqs have same PolProds as 0
                                    // insert index# for this channel
            u.t120.index = 10 * rec->freq_index + 1;
                                    // tack on offset that represents polarization
                                    // iff there is more than one polarization present
            if (D->baseline[blind].nPolProd[0] > 1)
                for (i=0; i<4; i++)     
                    if (strncmp (poltab[i], rec->pols, 2) == 0)
                        u.t120.index += i;

                                    // calculate accumulation period index from start of scan
            u.t120.ap = (8.64e4 * (rec->mjd - D->scan[scanId].mjdStart) + rec->iat)
                                 / D->config->tInt;
                                    // write a type 120 record to the appropriate file
            write_t120 (&u.t120, fout[n]);
            n120[n]++;
            }                       // bottom of record loop (over nvr)

        if (*jobId == D->nJob       // if no more jobs for this scan
         || currentScan != scanId)  // or we've bumped into the next scan
            break;                  // break out for cleanup
        }                           // bottom of job loop 

                                    // do clean up of output fileset before exiting
                                    // patch up each type 1 with correct # of records
    n120_tot = 0;
    for (i=0; i<NUMFILS; i++)
        {
        if (base_index[i] < 0)      // bail out at end of list
            break;
        if (opts->verbose > 0)
            printf ("      n120[%s] %d\n", blines[i], n120[i]);
                                    // position to ndrec in t100 record in file
        fseek (fout[i], 
              (long)(sizeof(t000)+((char *)&t100.ndrec-(char *)&t100.record_id)), 
              SEEK_SET);
                                    // update with actual number of records written
        n120_flipped = int_reverse (n120[i]);
        fwrite (&n120_flipped, sizeof (int), 1, fout[i]);
                                    // close each output file
        fclose (fout[i]);
        n120_tot += n120[i];
        }
    if (opts->verbose > 0)
        printf ("      total number of type 120 records %d\n", n120_tot);
                                    // generate warnings for antennas with no data
    for(i=0; i<D->nAntenna; i++)
        {
        if (stns[i].inscan && (!stns[i].invis))
            fprintf(stderr, "Warning Station %c%c in scan in vex file but no visibilities found!\n",
                    stns[i].difx_name[0], stns[i].difx_name[1]);
        }
                                    // print summary information
    printf ("      DiFX visibility records read in scan %8d\n", nread);
    printf ("      DiFX visibility records discarded    %8d\n", nflagged + noscan);
    return (currentScan);
    }

                                    // determine which baseline array matches the input antennas
int getBaselineIndex (DifxInput *D, int a1, int a2)
    {
    int i;

    for (i=0; i<D->nBaseline; i++)
        if ((D->datastream[D->baseline[i].dsA].antennaId == a1
         && D->datastream[D->baseline[i].dsB].antennaId == a2)
        || (a1 == a2 &&              // for autocorrelations, only one end needs to match
           (D->datastream[D->baseline[i].dsA].antennaId == a1
         || D->datastream[D->baseline[i].dsB].antennaId == a2)))
            return i;
         
    return -1;
    }


//FIXME add other sanity checks from difx2fits fitsUV.c

int recordIsFlagged (double t, int a1, int a2, const DifxJob *job)
    {
    int i;

    if(job->nFlag <= 0)
        return 0;

    for(i = 0; i < job->nFlag; i++)
        {
        if(job->flag[i].mjd1 <= t &&
           job->flag[i].mjd2 >= t)
            {
            if(job->flag[i].antennaId == a1 ||
               job->flag[i].antennaId == a2)
                {
                //fprintf (stderr, "flagged visibility baseline %d-%d mjd %f\n", a1+1, a2+1, t);
                return 1;
                }
            }
        }
    return 0;
    }

// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
