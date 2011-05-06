// createType1s creates a type 1 fileset based upon the difx data structures
// there is one type 1 output file for each baseline in the difx scan
//
//  first created                          rjc  2010.2.23

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <difxio/difx_input.h>
#include <errno.h>
#include "difx2mark4.h"

#define XS_CONVENTION

#define NUMFILS 80                  // max number of type 1 output files
#define SCALE 10000.0               // amplitude factor to normalize for fourfit


int createType1s (DifxInput *D,     // ptr to a filled-out difx input structure
          int *jobId,
          int scanId,
          char *node,               // directory for output fileset
          char *rcode,              // 6 letter root suffix
          struct stations *stns,    // struct contains station name information
          struct CommandLineOptions *opts, // ptr to input options
          char *rootname,           // full root file name
          FILE **vis_file,
          char *corrdate)           // file pointer to open file 
    {
    const int header_size = (8*sizeof(int)) + (5*sizeof(double)) + (2*sizeof(char)); 

    int i,
        ch,
        err,
        k,
        n,
        nvis,
        pol,
        a1, a2,                     // antenna indices for current baseline, 0-relative
        blind,                      // baseline index into the baseline array structure
        nread,                      // number of visibility records read
        nflagged,                   // number of visibility records flagged
        base_index[NUMFILS],        // base_index[i] contains baseline for file fout[i]
        n120[NUMFILS],              // # of records in each t120 file
        n120_flipped,
        currentScan,
        noscan,
        vis_file_status;

    char inname[DIFXIO_FILENAME_LENGTH],    // file name of input data file
         dirname[DIFXIO_FILENAME_LENGTH],
         outname[DIFXIO_FILENAME_LENGTH],
         blines[NUMFILS][3],        // null-terminated baselines list
         poltab [4][3] = {"LL", "RR", "LR", "RL"},
         lchan_id[5],
         rchan_id[5],
         buff[20];;
    
    double q_factor,                // quantization correction factor
           sb_factor[64];           // +1 | -1 for USB | LSB by channel

    FILE *fout[NUMFILS];
    DIR *pdir;
    struct dirent *dent;
    struct tm *mod_time;
    struct stat attrib;
    vis_record rec;

    struct type_000 t000;
    struct type_100 t100;
    struct type_101 t101;
    union u_tag
        {
        struct type_120 t120;
        float dummy[2*MAX_VIS+10];  // reserve enough space for MAX_VIS visibilities
        } u;

                                    // function prototypes
    int recordIsFlagged (vis_record *, const DifxJob *);
    int getBaselineIndex (DifxInput *, int, int);

                                    // initialize memory as necessary
                                    // quantization correction factor is pi/2 for
                                    // 1 bit, or 1.13 for 2 bit (see TMS, p.272)
                                    // Also, in 2 bit mode, difx mean square is 3.219
                                    // (for 0 correlation)
    // q_factor = (D->quantBits == 1) ? 1.57 : 1.13 / 3.219;
    q_factor = 1.0;
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
    nread = 0;
    nflagged = 0;
    nvis = D->nOutChan;
                                    // make sure we don't overrun our arrays
    if (nvis > MAX_VIS)
        {
        fprintf (stderr, 
                "fatal error: # visibilities (%d) exceeds array dimension (%d)\n",
                nvis, MAX_VIS);
        return (-1);
        }
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
                                    // loop over all records in input file
    currentScan = -1;
    while (TRUE)
        {
        noscan=0;
        if(*vis_file == 0)
            {
                                    // form directory name for input file
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
            while                       // ignore ".", "..", and pcal file names
                ((dent != NULL) && (strcmp (dent->d_name, ".") == 0 
                                || strcmp (dent->d_name, "..") == 0
                                || strncmp (dent->d_name, "PCAL", 4) == 0));
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

            *vis_file = fopen (inname, "r");
            if (*vis_file == NULL)
                {
                perror ("difx2mark4");
                fprintf (stderr, "fatal error opening input data file %s\n", inname);
                return (-1);
                }

            printf ("      opened input file %s\n", inname);
            //printf ("      file pointer %x\n", *vis_file);
            err = stat (inname, &attrib);
            if (err)
                {
                fprintf (stderr, "Warning: error stating file %s\n", inname);
                fprintf (stderr, "         t000.date will be set to 2000001-000000\n");
                sprintf (corrdate, "2000001-000000");
                }
            else
                {
                mod_time = gmtime (&(attrib.st_mtime));
                snprintf (corrdate, 16, "%4d%03d-%02d%02d%02d", 
                         mod_time->tm_year+1900,
                         mod_time->tm_yday, mod_time->tm_hour,
                         mod_time->tm_min,  mod_time->tm_sec);
                }
            }
                                    // read a header from the input file
        vis_file_status = get_vis_header (*vis_file, &rec);
        if (vis_file_status < 0)
            {
            if (vis_file_status == -1)
                {
                                    //EOF in .difx file
                if (opts->verbose > 0)
                    printf ("        EOF in input file\n");
                fclose (*vis_file);
                *vis_file = 0;
                //printf ("      file pointer %x\n", *vis_file);
                *(jobId) += 1;      // stop looping and increment job
                if(*jobId == D->nJob)   
                    break;          // final job
                continue;
                }
            else if (vis_file_status == -2)
                {
                fprintf (stderr, "unreadable input file\n");
                                    //unreadable .difx file
                fclose (*vis_file);
                *vis_file = 0;
                *(jobId) += 1;      // stop looping and increment job
                if(*jobId == D->nJob)   
                    break;          // final job
                continue;
                }
            }
                                    // check for new scan
        currentScan = DifxInputGetScanIdByJobId (D, rec.mjd+rec.iat/86400., *jobId);
        
        if (currentScan == -1)
            {
            printf ("      Header time (%d %6.1f) and jobId %d don't correspond to a scan\n",
                    rec.mjd, rec.iat, *jobId);
            noscan++;
            }
        else if (currentScan != scanId)
            {
            printf ("      Header time (%d %6.1f) indicates new scan %d (current scanId %d)\n",
                    rec.mjd, rec.iat, currentScan, scanId);
                                    // current header indicates start of new scan
                                    //
                                    // rewind back to the start of the 
                                    // current header
            fseek(*vis_file, -header_size, SEEK_CUR);
            break;
            }
        if (opts->verbose > 2)
            {
            printf ("        valid header read\n");
            printf ("          bl=%d time=%d %13.6f config=%d source=%d freq=%d, pol=%s pb=%d\n",
                    rec.baseline, rec.mjd, rec.iat, rec.config_index, rec.source_index, 
                    rec.freq_index, rec.pols, rec.pulsar_bin);
            }

                                    // read data associated with last header
        if (fread (rec.comp, sizeof (float), 2 * nvis, *vis_file) <= 0)
            {                       // encountered a read error or EOF
            fprintf (stderr, "Error reading data from file\n");
            fclose (*vis_file);
            *vis_file = 0;           // stop looping and increment job
                                    
            *jobId += 1;               
            if(*jobId == D->nJob)
                break;
            continue;
            }
        if (opts->verbose > 2)
            printf ("        read %d complex visibilities\n", nvis);
        nread++;
                                    // check if either antenna is flagged a priori
                                    // for this baseline. If so, disregard and move on
        if (noscan || recordIsFlagged(&rec, &D->job[*jobId]))
            {
            if (opts->verbose > 1)
                printf ("          flagged: antenna off source\n");
            nflagged++;
            continue;
            }


                                    // find the output file for this baseline
        for (n=0; n<NUMFILS; n++)
            {
            if (base_index[n] == rec.baseline)
                break;              // found baseline, exit loop
            else if (base_index[n] < 0)
                {                   
                                    // compute antenna indices
                if (D->job->antennaIdRemap)
                    {
                    a1 = *((D->job+*jobId)->antennaIdRemap + rec.baseline / 256 - 1); 
                    a2 = *((D->job+*jobId)->antennaIdRemap + rec.baseline % 256 - 1); 
                    }
                else                // no remapping, just use indices from file
                    {
                    a1 = rec.baseline / 256 - 1; 
                    a2 = rec.baseline % 256 - 1; 
                    }
                                    // first check that both antennas are in the
                                    // rootfile
                if((stns + a1)->inscan != TRUE || (stns + a2)->inscan != TRUE)
                    {
                    printf (
                    "WARNING Visibility found for baseline %x %c%c-%c%c which isn't in scan!\n",
                     rec.baseline, (stns + a1)->intl_name[0], (stns + a1)->intl_name[1],
                                   (stns + a2)->intl_name[0], (stns + a2)->intl_name[1]);
                    break;
                    }
                                    // determine which baseline array to use
                if ((blind = getBaselineIndex (D, a1, a2)) < 0)
                    {
                    fprintf (stderr, 
                            "WARNING Couldn't properly identify baseline %d in .input file.\n",
                            rec.baseline);
                    blind = 0;      // use first one in list and muster on
                    }

                                    // append new baseline to list
                base_index[n] = rec.baseline;
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
                    printf ("      rec.baseline %d blines <%s>\n", rec.baseline, blines[n]);
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
                write_t100 (&t100, fout[n]);

                                    // construct and write type 101 records
                for (i=0; i<D->baseline[blind].nFreq; i++)
                    {
                                    // generate index that is 10 * freq_index + pol
                    t101.index = 10 * i;
                                    // prepare ID strings for both pols, if there
                    ch = (D->baseline[blind].nPolProd[i] > 1) ? 2 * i     : i;
                    sprintf (lchan_id, "%c%02d?", getband (D->freq[i].freq), ch);
                    lchan_id[3] = (D->freq+i)->sideband;

                    ch = (D->baseline[blind].nPolProd[i] > 1) ? 2 * i + 1 : i;
                    sprintf (rchan_id, "%c%02d?", getband (D->freq[i].freq), ch);
                    rchan_id[3] = (D->freq+i)->sideband;
                                    // loop over 1, 2, or 4 pol'n. products
                    for (pol=0; pol<D->baseline[blind].nPolProd[i]; pol++)
                        {
                        t101.index++;
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
        if (base_index[n] < 0)
            continue;               // to next record 

                                    // copy visibilities into type 120 record
        for (i=0; i<nvis; i++)
            {                       // conjugate LSB for rotator direction difference
            if (sb_factor[rec.freq_index] > 0)
                {
                u.t120.ld.spec[i].re = rec.comp[i].real * SCALE * q_factor;
                u.t120.ld.spec[i].im = rec.comp[i].imag * SCALE * q_factor;
                }
            else                    // reverse order of points in LSB spectrum
                {
                u.t120.ld.spec[nvis-i-1].re =  rec.comp[i].real * SCALE * q_factor;
                u.t120.ld.spec[nvis-i-1].im = -rec.comp[i].imag * SCALE * q_factor;
                }
            }
        strncpy (u.t120.baseline, blines[n], 2);
                                    // FIXME (perhaps) -assumes all freqs have same PolProds as 0
                                    // insert index# for this channel
        u.t120.index = 10 * (rec.freq_index % D->baseline[blind].nFreq) + 1;
                                    // tack on offset that represents polarization
                                    // iff there is more than one polarization present
        if (D->baseline[blind].nPolProd[0] > 1)
            for (i=0; i<4; i++)     
                if (strncmp (poltab[i], rec.pols, 2) == 0)
                    u.t120.index += i;

        t100.nindex = D->baseline[blind].nFreq * D->baseline[blind].nPolProd[0];
        u.t120.ap = n120[n] / t100.nindex;
                                    // write a type 120 record to the appropriate file
        write_t120 (&u.t120, fout[n]);
        n120[n]++;
        } //We only break out of this loop at end of scan.
                                    // patch up each type 1 with correct # of records
    for (i=0; i<NUMFILS; i++)
        {
        if (base_index[i] < 0)  // bail out at end of list
            break;
        if (opts->verbose > 0)
            printf ("      n120[%d] %d\n", i, n120[i]);
                                // position to ndrec in t100 record in file
        fseek (fout[i], 
              (long)(sizeof(t000)+((char *)&t100.ndrec-(char *)&t100.record_id)), 
              SEEK_SET);
                                // update with actual number of records written
        n120_flipped = int_reverse (n120[i]);
        fwrite (&n120_flipped, sizeof (int), 1, fout[i]);
                                // close each output file
        fclose (fout[i]);
        }
    for(i=0; i<D->nAntenna; i++)
        {
        if (stns[i].inscan && (!stns[i].invis))
            fprintf(stderr, "Warning Station %c%c in scan in vex file but no visibilities found!\n",
                    stns[i].difx_name[0], stns[i].difx_name[1]);
        }
                                // print summary information
    printf ("      DiFX visibility records read       %8d\n", nread);
    printf ("      DiFX visibility records discarded  %8d\n", nflagged);
    return (currentScan);
    }

                                    // determine which baseline array matches the input antennas
int getBaselineIndex (DifxInput *D, int a1, int a2)
    {
    int i;

    for (i=0; i<D->nBaseline; i++)
        if ((D->datastream[D->baseline[i].dsA].antennaId == a1
         && D->datastream[D->baseline[i].dsB].antennaId == a2)
        || a1 == a2 &&              // for autocorrelations, only one end needs to match
           (D->datastream[D->baseline[i].dsA].antennaId == a1
         || D->datastream[D->baseline[i].dsB].antennaId == a2))
            return i;
         
    return -1;
    }


//FIXME add other sanity checks from difx2fits fitsUV.c

int recordIsFlagged (vis_record *vr, const DifxJob *job)
    {
    double mjd;
    int a1, a2;
    int i;


    if(job->nFlag <= 0)
        return 0;

    mjd = vr->mjd + vr->iat/86400.;
    a1  = (vr->baseline/256) - 1;
    a2  = (vr->baseline%256) - 1;

    for(i = 0; i < job->nFlag; i++)
        {
        if(job->flag[i].mjd1 <= mjd &&
           job->flag[i].mjd2 >= mjd)
            {
            if(job->flag[i].antennaId == a1 ||
               job->flag[i].antennaId == a2)
                {
                                //fprintf (stderr, "flagged visibility baseline %d-%d mjd %f\n", a1+1, a2+1, mjd);
                return 1;
                }
            }
        }
    return 0;
    }

// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
