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

#define NOVIS -9999                 // indicates no visibility records in RAM
#define MAX_CH 256                  // max # of channels


int createType1s (DifxInput *D,     // ptr to a filled-out difx input structure
          struct fblock_tag *pfb,   // ptr to filled-in fblock table
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
        j,
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
        configId,
        rc;

    char inname[DIFXIO_FILENAME_LENGTH], // file name of input data file
         dirname[DIFXIO_FILENAME_LENGTH],
         blines[2*NUMFILS],         // baselines list, as in ABXYJK
         poltabc [4][3] = {"LL", "RR", "LR", "RL"},
         poltabl [4][3] = {"XX", "YY", "XY", "YX"},
         poltabcl[4][3] = {"LX", "RY", "LY", "RX"},
         poltablc[4][3] = {"XL", "YR", "XR", "YL"},
         c;
                                    // variables that need persistence due to exit
                                    // up into caller over scan boundaries
    static char corrdate[16];
    static int nvr = NOVIS,         // index of current visibility record
               nvrtot,              // total number of visibility records in buffer
               currentScan;
    static vis_record *rec,
                      *vrec;
    
    double scale_factor[NUMFILS],   // scaling factor includes Van Vleck and fourfit SCALE
           sb_factor[MAX_CH],       // +1 | -1 for USB | LSB by channel
           rscaled,
           iscaled,
           epsilon = 1e-9;          // 1 nano-day offset to ensure floating pt compares OK

    FILE *fout[NUMFILS];
    DIR *pdir;
    struct dirent *dent;

    struct type_000 t000;           // just used for pointer arithmetic
    struct type_100 t100;

    union u_tag
        {
        struct type_120 t120;
        float dummy[2*MAX_VIS+10];  // reserve enough space for MAX_VIS visibilities
        } u;

                                    // function prototypes
    int recordIsFlagged (double, int, int, const DifxJob *);
    int getBaselineIndex (DifxInput *, int, int);
    int openReadVisFile (FILE *, vis_record *, int);
    int new_type1 (DifxInput *, struct fblock_tag *, int, int, int, int, int *, double *,
                   struct stations *, char *, struct CommandLineOptions *, FILE **, 
                   int, char *, char *, char *, char *, int, int);
    void write_t120 (struct type_120 *, FILE *);

                                    // initialize memory as necessary
                                    // compensate for LSB fringe rotator direction
    if (D->nFreq > MAX_CH)
        {
        fprintf (stderr, 
                "fatal error: # frequencies (%d) exceeds array dimension (%d)\n",
                D->nFreq, MAX_CH);
        return (-1);
        }

    for (i=0; i<D->nFreq; i++)
        sb_factor[i] = ((D->freq+i)->sideband == 'U') ? 1.0 : -1.0;

    for (i=0; i<NUMFILS; i++)
        {
        base_index[i] = -1;
        scale_factor[i] = 1.0;
        n120[i] = 0;
        }
                                    // number of (spectral) visibility points per array
    nvis = D->nOutChan;
    if (opts->verbose > 1)
        printf ("      # of spectral points: %d\n", nvis);
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

    memset (&u, 0, sizeof (u));

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
                closedir (pdir);
                return (-1);
                }
            strcpy (inname, dirname);
            strcat (inname, "/");
            strcat (inname, dent->d_name);
            closedir (pdir);
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
                normalize (opts, vrec, nvrtot, nvis, vrsize, pfb);
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
            rec = (vis_record *) ((char *) vrec + nvr * (long int) vrsize);  
                                    // check for new scan
            oldScan = currentScan;
            currentScan = DifxInputGetScanIdByJobId (D, rec->mjd+rec->iat/8.64e4-epsilon, *jobId);
        
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
                                    // get configId for this scan
            configId = D->scan[scanId].configId;

                                    // compute antenna indices for this record
            if ((D->job+*jobId)->antennaIdRemap)
                {
                a1 = *((D->job+*jobId)->antennaIdRemap + rec->baseline / 256 - 1); 
                a2 = *((D->job+*jobId)->antennaIdRemap + rec->baseline % 256 - 1); 
                }
            else                    // no remapping, just use indices from file
                {
                a1 = rec->baseline / 256 - 1; 
                a2 = rec->baseline % 256 - 1; 
                }

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
                    {               // at list end, add new type 1 file for this baseline
                                    // determine which baseline array to use
                    if ((blind = getBaselineIndex (D, a1, a2)) < 0)
                        {
                        fprintf (stderr, 
                                "WARNING Couldn't properly identify baseline %d in .input file.\n",
                                rec->baseline);
                        blind = 0;      // use first one in list and muster on
                        }
                    rc = new_type1 (D, pfb, n, a1, a2, blind, base_index, scale_factor, stns,
                                    blines, opts, fout, nvis, rootname, node, rcode, corrdate, 
                                    rec->baseline, scanId);
                    if (rc < 0)
                        return (rc);
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
                if (fabs(rscaled) > MAGLIM || fabs (iscaled) > MAGLIM
                        || isinf (rscaled) || isinf (iscaled) 
                        || isnan (rscaled) || isnan (iscaled))
                    {               // impossibly large values overwritten with 0
                    printf ("Warning! Corrupt visibility %le %le for baseline %c%c in input file\n",
                            rscaled, iscaled, blines[2*n], blines[2*n+1]);
                    rscaled = 0.0;
                    iscaled = 0.0;
                    }
                else
                    {
                    rscaled = rec->comp[i].real * scale_factor[n];
                    iscaled = rec->comp[i].imag * scale_factor[n];
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
            strncpy (u.t120.baseline, blines+2*n, 2);
                                    // FIXME (perhaps) -assumes all freqs have same PolProds as 0
                                    // insert index# for this channel
            u.t120.index = 10 * rec->freq_index + 1;
                                    // tack on offset that represents polarization
            for (i=0; i<4; i++)     
                if (strncmp (poltabc[i],  rec->pols, 2) == 0
                 || strncmp (poltabl[i],  rec->pols, 2) == 0
                 || strncmp (poltabcl[i], rec->pols, 2) == 0
                 || strncmp (poltablc[i], rec->pols, 2) == 0)
                    u.t120.index += i;
                                    // copy over weight into former flag field
                                    // use -0.0 to denote zero, for backward compatibility
            if (rec->weight == 0)
                u.t120.fw.weight = -0.0;
            else
                u.t120.fw.weight = rec->weight;
                                    // calculate accumulation period index from start of scan
            u.t120.ap = (8.64e4 * (rec->mjd - D->scan[scanId].mjdStart) + rec->iat)
                                 / D->config[configId].tInt;
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
            printf ("      n120[%c%c] %d\n", blines[2*i], blines[2*i+1], n120[i]);
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
#warning "FIXME: getBaselineIndex return value incorrect/incomplete when one antenna has multiple datastreams"
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

double scale_baseline (struct fblock_tag *pfb, int a1, int a2)
    {
    }
