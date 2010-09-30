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
#include "difx2mark4.h"

#define XS_CONVENTION

#define NUMFILS 80                  // max number of type 1 output files
#define SCALE 10000.0               // amplitude factor to normalize for fourfit

//FIXME add other sanity checks from difx2fits fitsUV.c
int RecordIsFlagged(vis_record *vr, const DifxJob *job)
{
	double mjd;
	int a1, a2;
	int i;


	if(job->nFlag <= 0)
	{
		return 0;
	}

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

int createType1s (DifxInput *D,     // ptr to a filled-out difx input structure
                  char *baseFile,   // string containing common part of difx file names
                  char *node,       // directory for output fileset
                  char *rcode,      // 6 letter root suffix
                  struct stations *stns, // struct contains station name information
                  struct CommandLineOptions *opts, // ptr to input options
                  char *rootname)   // full root file name
    {
    int i,
        ch,
        k,
        n,
        nvis,
        pol,
        nread,                      // number of visibility records read
        nflagged,                   // number of visibility records flagged
        base_index[NUMFILS],        // base_index[i] contains baseline for file fout[i]
        n120[NUMFILS],              // # of records in each t120 file
        n120_flipped;

    char inname[256],               // file name of input data file
         dirname[256],
         outname[256],
         blines[NUMFILS][3],        // null-terminated baselines list
         poltab [4][3] = {"LL", "RR", "LR", "RL"},
         lchan_id[5],
         rchan_id[5],
         buff[20];;
    
    double q_factor,                // quantization correction factor
           sb_factor[64];           // +1 | -1 for USB | LSB by channel

    FILE *fin,
         *fout[NUMFILS];
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
        float dummy[2058];          // reserve enough space for 1024 vis.
        } u;

                                    // function prototypes
    int get_vis (FILE *, int, vis_record *);
    void conv2date (double, struct date *);
    void write_t100 (struct type_100 *, FILE *);
    void write_t101 (struct type_101 *, FILE *);
    void write_t120 (struct type_120 *, FILE *);
    int RecordIsFlagged(vis_record *vr, const DifxJob *job);

                                    // initialize memory as necessary
                                    // quantization correction factor is pi/2 for
                                    // 1 bit, or 1.13 for 2 bit (see TMS, p.272)
    q_factor = (D->quantBits == 1) ? 1.57 : 1.13;
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
    t100.nindex = D->nFreq * D->nPolar;
    t100.nlags = nvis;
    strncpy (t100.rootname, rootname, 34);
    conv2date (D->scan->mjdStart, &t100.start);
    conv2date (D->scan->mjdEnd,   &t100.stop);
    if (opts->verbose > 0)
        fprintf (stderr, "mjdStart %g start %hd %hd %hd %hd %f\n", D->scan->mjdStart, 
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

                                    // form directory name for input file
    strcpy (dirname, baseFile);
    strcat (dirname, ".difx");
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
    do
        dent = readdir (pdir);
    while                           // ignore ".", "..", and pcal file names
        (strcmp (dent->d_name, ".") == 0 
      || strcmp (dent->d_name, "..") == 0
      || strncmp (dent->d_name, "PCAL", 4) == 0);

    strcpy (inname, dirname);
    strcat (inname, "/");
    strcat (inname, dent->d_name);

    fin = fopen (inname, "r");
    if (fin == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening input data file %s\n", inname);
        return (-1);
        }

    fprintf (stderr, "opened input file %s\n", inname);
                                    // loop over all records in input file
    while (TRUE)
        {
                                    // read a record from the input file
        if (get_vis (fin, nvis, &rec))
            break;                  // encountered a read error or EOF; stop looping

                                    // check if either antenna is flagged a priori
                                    // for this baseline. If so, disregard and move on
        nread++;
        if (RecordIsFlagged(&rec, D->job)) //only one job for now
            {
            nflagged++;
            continue;
            }
                                    // find the output file for this baseline
        for (n=0; n<NUMFILS; n++)
            {
            if (base_index[n] == rec.baseline)
                break;              // found baseline, exit loop
            else if (base_index[n] < 0)
                {                   // append new baseline to list
                base_index[n] = rec.baseline;
                                    // create name & open new output file
                                    // assume that site ID order is same as station order
                                    // probably not valid, though - THIS NEEDS WORK!!  
                strcpy (outname, node);
                strcat (outname, "/");
                k = (stns+rec.baseline/256-1)->dind;
                blines[n][0] = (stns+k)->mk4_id;
                k = (stns+rec.baseline%256-1)->dind;
                blines[n][1] = (stns+k)->mk4_id;
                blines[n][2] = 0;
                if (opts->verbose > 0)
                    fprintf (stderr, "rec.baseline %d blines <%s>\n", 
                             rec.baseline, blines[n]);
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
                fprintf (stderr, "created type 1 output file %s\n", outname);

                                    // construct and write type 000 record
                stat (inname, &attrib);
                mod_time = gmtime (&(attrib.st_mtime));
                sprintf (buff, "%4d%03d-%02d%02d%02d", mod_time->tm_year+1900,
                         mod_time->tm_yday, mod_time->tm_hour,
                         mod_time->tm_min,  mod_time->tm_sec);
                strcpy (t000.date, buff);
                strcpy (t000.name, outname);
                fwrite (&t000, sizeof (t000), 1, fout[n]);

                                    // construct and write type 100 record
                memcpy (t100.baseline, &blines[n][0], 2);
                write_t100 (&t100, fout[n]);

                                    // construct and write type 101 records
                t101.index = 0;
                for (i=0; i<D->nFreq; i++)
                    {
                                    // prepare ID strings for both pols, if there
                    ch = (D->nPolar > 1) ? 2 * i     : i;
                    sprintf (lchan_id, "C%02d?", ch);
                    lchan_id[3] = (D->freq+i)->sideband;
                                    //FIXME quick fix for X/S observations.
#ifdef XS_CONVENTION
			    if (D->freq[i].freq < 3000)
                                lchan_id[0] = 'S';
                            else
                                lchan_id[0] = 'X';
#endif
                    ch = (D->nPolar > 1) ? 2 * i + 1 : i;
                    sprintf (rchan_id, "C%02d?", ch);
                    rchan_id[3] = (D->freq+i)->sideband;
#ifdef XS_CONVENTION
			    if (D->freq[i].freq < 3000)
                                lchan_id[0] = 'S';
                            else
                                lchan_id[0] = 'X';
#endif
                                    // loop over 1, 2, or 4 pol'n. products
                    for (pol=0; pol<D->nPolar; pol++)
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
                                    // insert index# for this channel
        u.t120.index = D->nPolar * rec.freq_index + 1;
                                    // tack on offset that represents polarization
                                    // iff there is more than one polarization present
        if (D->nPolar > 1)
            for (i=0; i<4; i++)     
                if (strncmp (poltab[i], rec.pols, 2) == 0)
                    u.t120.index += i;
        u.t120.ap = n120[n] / t100.nindex;
                                    // write a type 120 record to the appropriate file
        write_t120 (&u.t120, fout[n]);
        n120[n]++;
        }
                                    // patch up each type 1 with correct # of records
    for (i=0; i<NUMFILS; i++)
        {
        if (base_index[i] < 0)      // bail out at end of list
            break;
        if (opts->verbose > 0)
            fprintf (stderr, "n120[%d] %d\n", i, n120[i]);
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
    fclose (fin);
                                    // print summary information
    fprintf (stderr, "%8d DiFX visibility records read\n", nread);
    fprintf (stderr, "%8d DiFX visibility records discarded (slew time)\n", nflagged);
    return (0);
    }
