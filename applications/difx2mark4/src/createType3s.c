// createType3s creates a type 3 fileset based upon the difx data structures
// there is one type 3 output file for each station in the difx scan
//
//  first created                          rjc  2010.2.23
//  added type 309 pcal record creation    rjc  2010.8.9
//  added type 303 az, el, pa, record      rjc  2012.2.21

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <glob.h>                   // for restart case
#include "difx2mark4.h"
#include "other.h"

#define D2M4_MERGE_PCAL_DATASTREAMS
#ifdef D2M4_MERGE_PCAL_DATASTREAMS
#include "d2m4_pcal_record.h"
#endif

#define LBUFF_SIZE 40 * NPC_TONES * NPC_FREQS + 256

char lbuff[LBUFF_SIZE];             // buffer for a single line of an input pcal file

int createType3s (DifxInput *D,     // difx input structure, already filled
                  struct fblock_tag *pfb,   // ptr to filled-in fblock table
                  int startJob,
                  int endJob,
                  int scanId,
                  char *node,       // directory for output fileset
                  char *rcode,      // 6 letter root suffix
                  struct stations *stns, // structure containing names of stations
                  struct CommandLineOptions *opts) // ptr to input options

    {
    int i,
        j,
        k,
        b,
        ochan,                       // output array channel #
        nochan,                      // number of output array channels
        l,
        m,
        n,
        jf,
        jfrec,
        jfdst,
        npol,
        nchan,
        ntones,
        np,
        nc,
        nt,
        nf,
        ri, //pcal record index
        counter, //counter
        tmpbuff_size, //for debug
        nstates,
        nrc,
        nchars,
        mchars,
        norm_corr,
        isb,
        record_chan = 0,
        once = FALSE,
        oncef = FALSE,
        refDay,
        configId,
        nclock,
        sourceId,
        ifreq,
        version = 0,                // version# of pcal file format (0 = legacy)
        dstr,                       // difx datastream index
        redstr,                     // (potentially) remapped datastream index
        lowerb,                     // lower bound for b loop
        upperb,                     // upper bound for b loop
        findex,
        found;



    double t,
           mjd,
           mjd_latest,
           tint,
           cable_delay,
           freq,
           f_rel,
           srate,
           cquad,
           squad,
           xtones[NPC_TONES],
           deltat,
           clock[6],
           geoc_lat,
           geod_lat,
           sha,
           cha,
           az,
           el,
           dec,
           freq_i,
           bw_i,
           opt_bw;

    char outname[256],
         pcal_filnam[256],
         ant[16],
         buff[5],
         *line,
         *pc,
         ds_pols[MAX_DS_RECBANDS],
         sideband_i,
         polar;

    DifxDatastream *pdds;

    FILE *fin;
    FILE *fout;
    glob_t pcalglob;                // for restart case

    struct type_000 t000;
    struct type_300 t300;
    struct type_301 t301;
    struct type_302 t302;
    struct type_303 t303;
    struct type_309 t309;

    #ifdef D2M4_MERGE_PCAL_DATASTREAMS
        //needed for merging p-cal data from multiple data streams
        struct d2m4_pcal_list_node* pcal_list = NULL;
        struct d2m4_pcal_list_node* merged_pcal_list = NULL;
        struct d2m4_pcal_list_node* pcal_tail = NULL;
    #endif

                                    // function prototypes
    void fill_ds_pols (DifxInput *, int, char *);
    int find_parent_recband(const DifxInput *, int, int);

                                    // initialize memory
    memset (&t000, 0, sizeof (t000));
    memset (&t300, 0, sizeof (t300));
    memset (&t301, 0, sizeof (t301));
    memset (&t302, 0, sizeof (t302));
    memset (&t303, 0, sizeof (t303));
    memset (&t309, 0, sizeof (t309));

                                    // fill in record boiler-plate and unchanging fields
    memcpy (t000.record_id, "000", 3);
    memcpy (t000.version_no, "01", 2);

    memcpy (t300.record_id, "300", 3);
    memcpy (t300.version_no, "00", 2);

    memcpy (t301.record_id, "301", 3);
    memcpy (t301.version_no, "00", 2);

    memcpy (t302.record_id, "302", 3);
    memcpy (t302.version_no, "00", 2);

    memcpy (t303.record_id, "303", 3);
    memcpy (t303.version_no, "00", 2);

    memcpy (t309.record_id, "309", 3);
    memcpy (t309.version_no, "01", 2);

    if (strlen (opts->bandwidth))   // note possible bw selection for later use
        opt_bw = atof (opts->bandwidth);
    else
        opt_bw = -1.0;
                                    // loop over all antennas in scan
                                    // doy of start of observation
    mjd2dayno((int)(D->mjdStart), &refDay);

    for (n = 0; n < D->nAntenna; n++)
        {                           // n incremented at bottom
                                    // and at every continue
        if (stns[n].invis == 0)
            continue;

                                    // fill polarization vector for this ant/datastream
        fill_ds_pols (D, n, ds_pols);

        strcpy (outname, node);     // form output file name
        strcat (outname, "/");

        outname[strlen(outname)+1] = 0;
        outname[strlen(outname)] = (stns+n)->mk4_id;
        strcat (outname, "..");
        strcat (outname, rcode);
                                    // now open the file just named
        fout = fopen (outname, "w");
        if (fout == NULL)
            {
            perror (outname);
            fprintf (stderr, "fatal error opening output type3 file %s\n", outname);
            return (-1);
            }
        printf ("      created type 3 output file %s\n", outname);
                                    // all files need a type 000 record
        strcpy (t000.date, "2001001-123456"); // FIXME: use real corrdate as in new_type1.c? or document why this strange fixed date is correct.
        strcpy (t000.name, outname);
        fwrite (&t000, sizeof (t000), 1, fout);

                                    // finish forming type 300 and write it
        t300.id = (stns+n)->mk4_id;
        memcpy (t300.intl_id, (stns+n)->intl_name, 2);
        memcpy (t300.name, (stns+n)->difx_name, 2);
        t300.name[2] = 0;           // null terminate to form string
                                    // check that model was read in OK
        if (D->scan[scanId].im == 0)
            {
            fprintf (stderr, "ERROR: problem accessing model array\n");
            fclose(fout);
            return (-1);
            }
        t = (**(D->scan[scanId].im+n))->mjd + (**(D->scan[scanId].im+n))->sec / 86400.0;
        conv2date (t, &t300.model_start);

        t300.model_interval = (float)(**(D->scan[scanId].im+n))->validDuration;
        t300.nsplines = (short int) D->scan[scanId].nPoly;
        write_t300 (&t300, fout);

                                    // construct type 301, 302, and 303's and write them
                                    // loop over channels
                                    // FIXME: What about polarizations?
                                    //        t30x get written for channels X01LX X02LX ..., ignoring X01LY X02LY ...
                                    //        Document why this DiFX-2.5/2.6 difx2mark4 behaviour works out ok(?) for HOPS!
        for (i=0; i<D->nFreq; i++)
            {
                                    // find matching freq channel
                                    // loop through whole fblock table
            found = FALSE;
            nf = -1;
            while (pfb[++nf].stn[0].ant >= 0 && !found) // check for end-of-table marker
                {

                for (k=0; k<2 && !found; k++)
                    {
                                    // check for mpifxcorr-used or mpifxcorr-outputted match to this frequency
                    if (pfb[nf].stn[k].ant      == n
                     && pfb[nf].stn[k].fmk4     >= 0
                     && ( pfb[nf].stn[k].find     == i
                          || pfb[nf].stn[k].fdest == i) )
                        {
                        strcpy (t301.chan_id, pfb[nf].stn[k].chan_id);
                        strcpy (t302.chan_id, pfb[nf].stn[k].chan_id);
                        strcpy (t303.chan_id, pfb[nf].stn[k].chan_id);
                        found = TRUE;
                        break;
                        }
                    }
                }
                                    // this freq not in table - skip it
            if (!found)
                continue;
                                    // loop over polynomial intervals
            for (j=0; j<D->scan[scanId].nPoly; j++)
                {
                                    // insert polynomial indices
                t301.interval = (short int) j;
                t302.interval = t301.interval;
                t303.interval = t301.interval;
                                    // units of difx are usec, ff uses sec
                                    // shift clock polynomial to start of model interval
                deltat = 8.64e4 * ((**(D->scan[scanId].im+n)+j)->mjd - (D->antenna+n)->clockrefmjd)
                                                   + (**(D->scan[scanId].im+n)+j)->sec;
                nclock = getDifxAntennaShiftedClock (D->antenna+n, deltat, 6, clock);
                                    // difx delay doesn't have clock added in, so
                                    // we must do it here; also apply sign reversal
                                    // for opposite delay convention
                for (l=0; l<6; l++)
                    {
                    t301.delay_spline[l]
                      = -1.e-6 * (**(D->scan[scanId].im+n)+j)->delay[l];

                    if (l < nclock) // add in those clock coefficients that are valid
                        t301.delay_spline[l] -= 1e-6 * clock[l];

                    t302.phase_spline[l] = t301.delay_spline[l] * (D->freq+i)->freq;
                                    // fill t303 with az and el polynomials
                    t303.azimuth[l] = (**(D->scan[scanId].im+n)+j)->az[l];
                    t303.elevation[l] = (**(D->scan[scanId].im+n)+j)->elgeom[l];
                    t303.u[l] = (**(D->scan[scanId].im+n)+j)->u[l];
                    t303.v[l] = (**(D->scan[scanId].im+n)+j)->v[l];
                    t303.w[l] = (**(D->scan[scanId].im+n)+j)->w[l];
                    }
                                    // par. angle from calc program is NYI
                                    // separate loop so l=1 values defined for az & el
                for (l=0; l<6; l++)
                    {
                    if (l == 0)     // for now, only constant term is non-zero
                        {
                                    // calculate geocentric latitude (rad)
                        geoc_lat = atan2 (D->antenna[n].Z,
                                          sqrt (D->antenna[n].X * D->antenna[n].X
                                              + D->antenna[n].Y * D->antenna[n].Y));
                                    // get declination for this source
                        sourceId = D->scan[scanId].pointingCentreSrc;
                        dec = D->source[sourceId].dec;
                                    // evaluate az & el at midpoint of spline interval
                        el = M_PI / 180.0 * (t303.elevation[0]
                                + 0.5 * t300.model_interval * t303.elevation[1]);
                        az = M_PI / 180.0 * (t303.azimuth[0]
                                + 0.5 * t300.model_interval * t303.azimuth[1]);
                                    // evaluate sin and cos of the local hour angle
                        sha = - cos(el) * sin(az) / cos(dec);
                        cha = (sin(el) - sin(geoc_lat) * sin(dec))
                            / (cos(geoc_lat) * cos(dec));
                                    // approximate (first order in f) conversion
                        geod_lat = atan(1.00674 * tan(geoc_lat));
                                    // finally ready for par. angle
                        t303.parallactic_angle[l] = 180 / M_PI *
                            atan2 (sha, (cos (dec) * tan(geod_lat) - sin(dec) * cha));
                        }
                    else
                        t303.parallactic_angle[l] = 0.0;
                    }

                write_t301 (&t301, fout);
                write_t302 (&t302, fout);
                write_t303 (&t303, fout);
                }
            }


#ifdef D2M4_MERGE_PCAL_DATASTREAMS /////////////////////////////////////////////

                                    // construct type 309 pcal records and write them
                                    // check to see if there is an input pcal file for this antenna
        mjd_latest = 0.0;
        for (j = startJob; j <= endJob; j++)
            {
            strncpy (pcal_filnam, D->job[j].outputFile, 242);
            strcat (pcal_filnam, "/PCAL_*");    // for restart case
            strcat (pcal_filnam, t300.name);
            pcalglob.gl_offs = 0;
            if (0 == glob(pcal_filnam, 0, 0, &pcalglob) && 1 == pcalglob.gl_pathc)
                fin = fopen ( pcalglob.gl_pathv[0], "r");
            else
                fin = NULL;
            // fin = fopen (pcal_filnam, "r");  // for restart case
            if (fin == NULL)
                printf ("      No input phase cal file %s for antenna %s\n",
                        pcal_filnam, t300.name);
            else
                {                       // input data is present - loop over records
                if (opts->verbose > 1)  // debug print
                    printf ("      getting pcal data from file %s\n", pcal_filnam);

                //HAVE PCAL DATA, SO READ AND MERGE THE DATASTREAMS FIRST...
                //THEN WE WILL LOOP OVER IT TO CREATE THE TYPE309s
                pcal_list = d2m4_pcal_create_list(fin);
                version = 1;  // FIXME: have d2m4_pcal_create_list() return the version info found in the PCAL file 'fin'
                merged_pcal_list = d2m4_pcal_merge_datastreams_in_list(pcal_list);
                pcal_tail = merged_pcal_list;

                counter = 0;
                while( pcal_tail != NULL && pcal_tail->pcal_record != NULL)            // read loop over all input records
                    {

                    //have to bump pcal ptr at begining of
                    //loop b/c of all the break statements
                    //but we don't do this on the very first pass

                    #ifdef D2M4_PCAL_DEBUG
                    printf("pcal list counter = %d\n", counter);
                    printf("pcal ptr = %p\n", pcal_tail);
                    #endif

                    if(counter != 0)
                        {
                        #ifdef D2M4_PCAL_DEBUG
                        printf("bumping ptr to %p\n", pcal_tail->next);
                        #endif
                        pcal_tail = pcal_tail->next;
                        }
                    counter++;

                    if(pcal_tail == NULL)
                        break;

                    if(pcal_tail->pcal_record == NULL)
                        break;

                    #ifdef D2M4_PCAL_DEBUG
                    printf("pcal ptr next = %p\n", pcal_tail->next);
                    #endif

                    nochan = 0;
                    //merging multiple-pcal data datastream
                    //THIS ONLY SUPPORTS PCAL VERSION 1 --
                    //VERSION 0 (last generated by DiFX version < r614:, mid-2014??)  is not supported
                    strcpy(ant, pcal_tail->pcal_record->antenna);
                    mjd = pcal_tail->pcal_record->mjd;
                    tint = pcal_tail->pcal_record->tint;
                    dstr = 0; //THE DATA STREAM IS SET AT THE INDIVIDUAL PHASOR RECORD LEVEL
                    nchan = pcal_tail->pcal_record->nchannels;
                    ntones = pcal_tail->pcal_record->ntones;
                    npol = 1;// d2m4_pcal_count_unique_polarizations(pcal_tail->pcal_record);       // for compatible np-loop control

                    #ifdef D2M4_PCAL_DEBUG
                    tmpbuff_size = d2m4_pcal_dump_record(pcal_tail->pcal_record, lbuff, LBUFF_SIZE);
                    lbuff[tmpbuff_size] = '\0';
                    printf("record dump = %s\n", lbuff);
                    #endif
                    t = mjd + refDay - (int)(D->mjdStart);

                    if (mjd < D->scan[scanId].mjdStart)
                                        // skip to next line
                        {
                        if (opts->verbose > 1)
                            printf("      pcal early %13.6f<%13.6f\n", mjd, D->scan[scanId].mjdStart);
                        continue;
                        }
                    if (mjd > D->scan[scanId].mjdEnd)
                        {
                        if (opts->verbose > 1)
                            printf("      pcal late %13.6f -> %13.6f > %13.6f\n",
                                    t, mjd, D->scan[scanId].mjdEnd);
                        break;
                        }
                                        // skip over any out of order mjd's
                    if (mjd <= mjd_latest)
                        {
                        if (opts->verbose > 1)
                            printf("      pcal out of order %13.6f<%13.6f\n", mjd, mjd_latest);
                        continue;
                        }

                                        // reject any pcal data for times in flag file
                    for (i=0; i < D->job[j].nFlag; i++)
                        {
                        if ((D->job+j)->flag[i].antennaId == n
                        && mjd > (D->job+j)->flag[i].mjd1
                        && mjd < (D->job+j)->flag[i].mjd2)
                            {
                            if (opts->verbose > 1)
                                printf("      pcal flagged at %13.6f for ant %d named %s\n",
                                       mjd, n, t300.name);
                            continue;
                            }
                        }
                    mjd_latest = mjd;   // new "high-water mark"

                                        // calculate and insert rot start time of record
                    t309.rot = 3.2e7 * 8.64e4 * (t - 1.0);
                                        // pcal integration period same as main AP
                    configId = D->scan[scanId].configId;
                    t309.acc_period =  D->config[configId].tInt;
                                        // debug print
                    if (opts->verbose > 2)
                        printf ("      pcal record ant %s t %lf tint %lf"
                              "\n      rot %lf acc_period %lf\n",
                                 ant, t, tint, t309.rot, t309.acc_period);
                                        // initialize list of next available tone location
                    for (i=0; i<NPC_TONES; i++)
                        xtones[i] = 0;
                    t309.ntones = 0;
                                        // clear record accumulators
                    //printf("memsetting ptr, size, itemsize = %p, %d, %d \n", &(t309.chan[0].acc[0][0]), NPC_FREQS * sizeof (t309.chan[0]),  sizeof (t309.chan[0]) );
                    memset( &(t309.chan[0]), 0, NPC_FREQS * sizeof (t309.chan[0]));

                    if (nchan > NPC_FREQS)
                        {   
                        static int warned = FALSE;
                        if (!warned)
                            {       
                            fprintf (stderr, "number of baseband channels %d exceeds limit of %d"
                                            " - ignoring the rest\n", nchan, NPC_FREQS);
                            warned = TRUE;
                            }
                        }

                                        // loop over tones within merged-datastream pcal record, add them into joint t309
                    for (ri=0; ri<(pcal_tail->pcal_record->nchannels * pcal_tail->pcal_record->ntones); ri++)
                        {
                        ifreq = pcal_tail->pcal_record->phasors[ri].frequency;
                        polar = pcal_tail->pcal_record->phasors[ri].polarization;
                        cquad = pcal_tail->pcal_record->phasors[ri].real;
                        squad = pcal_tail->pcal_record->phasors[ri].imag;

                        #ifdef D2M4_PCAL_DEBUG
                        printf("adding pcal record = %d, %c, %lf, %lf\n", ifreq, polar, cquad, squad);
                        #endif

                                        // skip over tones that weren't extracted
                        freq = ifreq;
                        if (freq < 0)
                            continue;
                                        // swap sign of imaginary part
                        squad *= -1;

                                        // find the pfb mk4 channel(s) into which the tone freq falls,
                                        // add channel name and phasor into the t309 fields
                        nf = -1;
                        found = FALSE;
                        while (pfb[++nf].stn[0].ant >= 0) // check for end-of-table marker
                            {
                            for (k=0; k<2; k++)
                                {
                                double flo, fhi;
                                int match = FALSE;

                                jfdst = pfb[nf].stn[k].fdest;
                                flo = D->freq[jfdst].freq - ((D->freq[jfdst].sideband == 'U') ? 0.0 : D->freq[jfdst].bw);
                                fhi = D->freq[jfdst].freq + ((D->freq[jfdst].sideband == 'U') ? D->freq[jfdst].bw : 0.0);
                                isb = (D->freq[jfdst].sideband == 'U') ? 1 : -1;
                                f_rel = freq - D->freq[jfdst].freq;

                                        // skip band if tone resides outside of it
                                if (freq < flo || freq > fhi)
                                    continue;

                                        // check if this channel matches, if so, look up t309 channel name
                                ochan = -1;
                                if (pfb[nf].stn[k].pol      == polar
                                 && pfb[nf].stn[k].ant      == n
                                 && pfb[nf].stn[k].fmk4     >= 0)
                                    {
                                    for (m=0; m<nochan; m++)
                                        if (strcmp (t309.chan[m].chan_name, pfb[nf].stn[k].chan_id) == 0)
                                            {
                                                        // found name - use this output channel#
                                            ochan = m;
                                            match = TRUE;
                                            break;
                                            }
                                                        // when name not in t309 yet, allocate
                                                        // new output channel #
                                    if (m == nochan)
                                        {
                                                        // trap potential array overwrites
                                        if (m >= NPC_FREQS)
                                            {
                                            printf ("skipping write for tone in channel %s - too many channels!\n",
                                                    pfb[nf].stn[k].chan_id);
                                            break;
                                            }

                                        ochan = m;
                                        strcpy (t309.chan[ochan].chan_name, pfb[nf].stn[k].chan_id);
                                        nochan++;// bump output channel #
                                        match = TRUE;
                                        }
                                    }

                                        // skip pfb entry if no match w tone freq and polzn
                                if (!match || ochan < 0)
                                    continue;

                                        // matching pfb entry, fill in corresponding t309 field
                                        // avoiding array overwrites for too many channels
                                if (ochan >= NPC_FREQS)
                                    continue;

                                jfrec = pfb[nf].stn[k].find;
                                if (pfb[nf].stn[k].zoom == TRUE)
                                    {
                                        jf = find_parent_recband(D, n, jfrec);
                                        if (jf<0)
                                            {
                                                fprintf(stderr, "Warning!! Zoom freq entry pfb[%d][%d] chan_id %s (DiFX freq id %d) at antenna %d has no parent band! Skipping.\n", nf, k, pfb[nf].stn[k].chan_id, jfrec, n);
                                                continue;
                                            }
                                        jfrec = jf;
                                    }
                                
                                    // change tones to usb if this corr is a mixed usb x lsb case
                                    // note: difx-2.5/2.6 d2m4 behaviour, baseline-dependent alteration of what is a station-based correction:
                                    //  if (pfb[nf].stn[k].sideband != D->freq[jf].sideband)
                                    //      {
                                    //      f_rel = D->freq[jf].bw + f_rel;
                                    //      isb = 1;
                                    //      }
                                    // note2: changed the above to a pure station-based check; if USB output originates from LSB recband, flip the LSB PCal to USB
                                if (D->freq[jfrec].sideband != D->freq[jfdst].sideband)
                                    {
                                    if (D->freq[jfrec].sideband == 'L')
                                        {
                                        f_rel = D->freq[jfdst].bw + f_rel;
                                        isb = 1;
                                        }
                                    }

                                // find out which tone slot this goes in
                                for (i=0; i<NPC_TONES; i++)
                                    {
                                                    // allow for round-off in comparing tone f's
                                    if (fabs (f_rel - xtones[i]) < 1e-9)
                                        break;
                                    else if (xtones[i] == 0.0)
                                        {           // not found - allocate a new slot
                                        xtones[i] = f_rel;
                                        t309.ntones++;
                                        break;
                                        }
                                    }

                                // did we run out of slots before finding tone?
                                if (i >= NPC_TONES)
                                    {
                                    if (!once)
                                        {
                                        fprintf (stderr, "more than %d baseband pcal tones"
                                                         " - ignoring the rest\n", NPC_TONES);
                                        once = TRUE;
                                        }
                                    continue;
                                    }

                                // FIXME ad hoc temporary fix for non-normalize pcal in zoom mode
                                if (fabs(cquad) > 10.0)
                                    cquad /= 6e7;
                                if (fabs(squad) > 10.0)
                                    squad /= 6e7;

                                // calculate sample rate (samples/s)
                                srate = 2e6 * D->freq[jfdst].bw * D->freq[jfdst].overSamp;
                                // renormalize correlations to those created in the DOM
                                norm_corr = - isb * floor (cquad * srate * t309.acc_period * 128.0 + 0.5);
                                memcpy (&t309.chan[ochan].acc[i][0], &norm_corr, 4);

                                norm_corr = floor (squad * srate * t309.acc_period * 128.0 + 0.5);
                                memcpy (&t309.chan[ochan].acc[i][1], &norm_corr, 4);

                                // tone freqs (in Hz) are spread through channel recs
                                t309.chan[i].freq = 1e6 * f_rel; // NB: chan[i] instead of chan[ochan] is on purpose,
                                                                 // cf. https://github.com/difx/difx/issues/43
                                found = TRUE;

#if 0
                                printf ("DBG: pcal %.3f t309 %c %s ochan %-2d tone %-2d - %+.6f %+.6f f_rel=%+6.2f - linked to jfrec=%d jfdst=%d pfb[%d].stn[%d] {.freq:%.3f %cSB .pol:%c-pol "
                                        ".bw:%.3f .chan_id:%s, .fmk4:%d, .pcal_int=%.3f} - stored %d tones overall\n",
                                    freq, t300.id, t309.chan[ochan].chan_name, ochan, i,
                                    (float)cquad, (float)squad, (float)f_rel,
                                    jfrec, jfdst, nf, k,
                                    (float)pfb[nf].stn[k].freq, pfb[nf].stn[k].sideband, pfb[nf].stn[k].pol, pfb[nf].stn[k].bw, pfb[nf].stn[k].chan_id, pfb[nf].stn[k].fmk4, pfb[nf].stn[k].pcal_int,
                                    i, t309.ntones);
#endif

                                }

                                        // DiFX-2.5/2.6 behaviour: quit after first matching mk4 pfb channel, ignore other channels
                                if (found)
                                    break;
                                
                            }
                        }

                                        // write output record
                    write_t309 (&t309, fout);
                    } // while (pcal file entries remain)

                fclose(fin);

                //free space allocated for the pcal data
                d2m4_pcal_free_list(pcal_list);
                d2m4_pcal_free_list(merged_pcal_list);
                pcal_list = NULL;
                merged_pcal_list = NULL;
                pcal_tail = NULL;


                }
            }

#else //////////////////////////////////////////////////////////////////////////

                                    // construct type 309 pcal records and write them
                                    // check to see if there is an input pcal file for this antenna
        mjd_latest = 0.0;
        for (j = startJob; j <= endJob; j++)
            {
            strncpy (pcal_filnam, D->job[j].outputFile, 242);
            strcat (pcal_filnam, "/PCAL_*");    // for restart case
            strcat (pcal_filnam, t300.name);
            pcalglob.gl_offs = 0;
            if (0 == glob(pcal_filnam, 0, 0, &pcalglob) && 1 == pcalglob.gl_pathc)
                fin = fopen ( pcalglob.gl_pathv[0], "r");
            else
                fin = NULL;
            // fin = fopen (pcal_filnam, "r");  // for restart case
            if (fin == NULL)
                printf ("      No input phase cal file %s for antenna %s\n",
                        pcal_filnam, t300.name);
            else
                {                       // input data is present - loop over records
                if (opts->verbose > 1)  // debug print
                    printf ("      getting pcal data from file %s\n", pcal_filnam);

                while (TRUE)            // read loop over all input records
                    {
                    nochan = 0;
                    line = fgets (lbuff, LBUFF_SIZE, fin);
                    if (line == NULL)   // EOF?
                        break;
                    else if (*line == '#')
                        {
                        pc = strstr (line, "File version =");
                        if (pc)         // get version, if present
                            sscanf (pc + 14, "%d", &version);
                        continue;       // skip over comment lines
                        }

                    if (version == 0)   // legacy is version 0
                        {
                        sscanf (line, "%s%lf%lf%lf%d%d%d%d%d%n", ant, &t, &tint, &cable_delay,
                                     &npol, &nchan, &ntones, &nstates, &nrc, &nchars);
                        mjd = t - refDay + (int)(D->mjdStart);
                        }
                    else                // by elimination, version must be 1
                        {
                        sscanf (line, "%s%lf%lf%d%d%d%n", ant, &mjd, &tint,
                                     &dstr, &nchan, &ntones, &nchars);
                                        // remap datastream if mapping used in this job
                        if ((D->job[j]).datastreamIdRemap)
                            redstr = *((D->job[j]).datastreamIdRemap + dstr);
                        else
                            redstr = dstr;
                        pdds = D->datastream + redstr;
                        npol = 1;       // for compatible np-loop control
                        t = mjd + refDay - (int)(D->mjdStart);
                        }


                    if (mjd < D->scan[scanId].mjdStart)
                                        // skip to next line
                        {
                        if (opts->verbose > 1)
                            printf("      pcal early %13.6f<%13.6f\n", mjd, D->scan[scanId].mjdStart);
                        continue;
                        }
                    if (mjd > D->scan[scanId].mjdEnd)
                        {
                        if (opts->verbose > 1)
                            printf("      pcal late %13.6f -> %13.6f > %13.6f\n",
                                    t, mjd, D->scan[scanId].mjdEnd);
                        break;
                        }
                                        // skip over any out of order mjd's
                    if (mjd <= mjd_latest)
                        {
                        if (opts->verbose > 1)
                            printf("      pcal out of order %13.6f<%13.6f\n", mjd, mjd_latest);
                        continue;
                        }

                                        // reject any pcal data for times in flag file
                    for (i=0; i < D->job[j].nFlag; i++)
                        {
                        if ((D->job+j)->flag[i].antennaId == n
                        && mjd > (D->job+j)->flag[i].mjd1
                        && mjd < (D->job+j)->flag[i].mjd2)
                            {
                            if (opts->verbose > 1)
                                printf("      pcal flagged at %13.6f for ant %d named %s\n",
                                       mjd, n, t300.name);
                            continue;
                            }
                        }
                    mjd_latest = mjd;   // new "high-water mark"

                                        // calculate and insert rot start time of record
                    t309.rot = 3.2e7 * 8.64e4 * (t - 1.0);
                                        // pcal integration period same as main AP
                    configId = D->scan[scanId].configId;
                    t309.acc_period =  D->config[configId].tInt;
                                        // debug print
                    if (opts->verbose > 2)
                        printf ("      pcal record ant %s t %lf tint %lf"
                              "\n      rot %lf acc_period %lf\n",
                                 ant, t, tint, t309.rot, t309.acc_period);
                                        // initialize list of next available tone location
                    for (i=0; i<NPC_TONES; i++)
                        xtones[i] = 0;
                    t309.ntones = 0;
                                        // clear record accumulators
                    memset( &(t309.chan[0]), 0, NPC_FREQS * sizeof (t309.chan[0]));
                    //memset (&(t309.chan[0].acc[0][0]), 0, NPC_FREQS * sizeof (t309.chan[0]));

                                        // loop over tones within record
                    for (np=0; np<npol; np++)
                        for (nc=0; nc<nchan; nc++)
                            {
                                        // skip over any channels in excess of the t309 size
                            if (nc >= NPC_FREQS)
                                {
                                if (!oncef)
                                    {
                                    fprintf (stderr, "more than %d baseband channels"
                                                     " - ignoring the rest\n", NPC_FREQS);
                                    oncef = TRUE;
                                    }
                                continue;
                                }
                            for (nt=0; nt<ntones; nt++)
                                {
                                        // identify channel and tone
                                if (version == 0)
                                    {
                                    sscanf (line + nchars, "%d%lf%lf%lf%n",
                                            &record_chan, &freq, &cquad, &squad, &mchars);
                                    nchars += mchars;
                                        // skip over channels which weren't recorded
                                    if (record_chan < 0)
                                        continue;
                                    }
                                else    // for now, version 1 is only alternative
#ifdef INTEGER_PC_FREQ
                                    {
                                    sscanf (line + nchars, "%d %c %lf%lf%n",
                                            &ifreq, &polar, &cquad, &squad, &mchars);
                                    nchars += mchars;
                                    freq = ifreq;
                                        // skip over tones that weren't extracted
                                    if (ifreq == -1)
                                        continue;
                                    }
#else /* INTEGER_PC_FREQ */
                                    {
                                    sscanf (line + nchars, "%lf %c %lf%lf%n",
                                            &freq, &polar, &cquad, &squad, &mchars);
                                    nchars += mchars;
                                        // skip over tones that weren't extracted
                                    if (freq < 0)
                                        continue;
                                    }
#endif /* INTEGER_PC_FREQ */
                                        // swap sign of imaginary part
                                squad *= -1;
                                        // b is channel index into the t309 record array
                                if (version == 0)
                                    {
                                    lowerb = 0;
                                    upperb = D->nFreq*npol;
                                    }
                                else    // version 1 doesn't require actual search
                                    {
                                    lowerb = nc;
                                    upperb = lowerb + 1;
                                    }
                                for (b=lowerb; b<upperb; b++)
                                    {
                                    if (version == 0)
                                        {
                                        jf = b / npol;
                                        // skip over non-matching polarizations
                                        if (np != b % npol)
                                            continue;
                                        }
                                    else // handle version 1
                                        {
                                        record_chan = nc;
                                        jf = *(pdds->recFreqId + *(pdds->recBandFreqId + nc));

                                        // if zoom band exists and parent band isn't the
                                        // desired bandwidth, overwrite with tone from zoom band
                                        if (pdds->nZoomBand > 0 && opt_bw > 0 && D->freq[jf].bw != opt_bw)
                                            for (i=0; i<pdds->nZoomBand; i++)
                                                {
                                                // skip over zoom bands with wrong polarization
                                                if ((*(pdds->zoomBandPolName + i)) != polar)
                                                    continue;
                                                findex = *(pdds->zoomFreqId + *(pdds->zoomBandFreqId + i));
                                                // zoom bands are always usb, is it in this one?
                                                if (freq - D->freq[findex].freq < D->freq[findex].bw
                                                 && freq - D->freq[findex].freq > 0.0)
                                                    {  // yes
                                                    jf = findex;
                                                    break;
                                                    }
                                                }
                                        }

                                    isb = (D->freq[jf].sideband == 'U') ? 1 : -1;
                                    f_rel = freq - D->freq[jf].freq;
                                        // is it within the jfth frequency band?
                                    if (isb > 0 && f_rel > 0.0 && f_rel < D->freq[jf].bw
                                     || isb < 0 && f_rel < 0.0 && f_rel >-D->freq[jf].bw);
                                        // yes, insert phasor info into correct slot
                                        {
                                        // find matching freq channel
                                        // loop through whole fblock table
                                        nf = -1;
                                        found = FALSE;
                                        while (pfb[++nf].stn[0].ant >= 0) // check for end-of-table marker
                                            {
                                            for (k=0; k<2; k++)
                                                {
                                                if (((pfb[nf].stn[k].freq     == D->freq[jf].freq
                                                   && pfb[nf].stn[k].sideband == D->freq[jf].sideband)
                                                 || ((pfb[nf].stn[k].freq + pfb[nf].stn[k].bw == D->freq[jf].freq)
                                                 && pfb[nf].stn[k].sideband == 'U' && D->freq[jf].sideband == 'L'))
                                                 && pfb[nf].stn[k].bw       == D->freq[jf].bw
                                                 && pfb[nf].stn[k].pol      == ds_pols[record_chan]
                                                 && pfb[nf].stn[k].ant      == n)
                                                    {
                                                                // this channel matches, is name
                                                                // already in the table?
                                                    for (m=0; m<nochan; m++)
                                                        if (strcmp (t309.chan[m].chan_name, pfb[nf].stn[k].chan_id) == 0)
                                                            {   // found it - use this output channel#
                                                            ochan = m;
                                                            found = TRUE;
                                                            break;
                                                            }
                                                    if (found)
                                                        break;  // 2nd part of double break

                                                                // on falling through, allocate
                                                                // new output channel #
                                                    if (m == nochan)
                                                        {
                                                                // trap potential array overwrites
                                                    if (m >= NPC_FREQS)
                                                        {
                                                        printf ("skipping write for tone in channel %s - too many channels!\n",
                                                                 pfb[nf].stn[k].chan_id);
                                                        break;
                                                        }

                                                        ochan = m;
                                                        strcpy (t309.chan[ochan].chan_name, pfb[nf].stn[k].chan_id);
                                                        nochan++;// bump output channel #
                                                        found = TRUE;
                                                        break;  // found freq, do double break
                                                        }
                                                    }
                                                }
                                            if (found)
                                                break;          // 2nd part of double break
                                            }
                                        // this freq not in table - skip it
                                        if (!found)
                                            continue;

                                        // avoiding array overwrites for too many channels
                                        if (m >= NPC_FREQS)
                                            continue;

                                        // change tones to usb if this corr is a mixed usb x lsb case
                                        if (pfb[nf].stn[k].sideband != D->freq[jf].sideband)
                                            {
                                            f_rel = D->freq[jf].bw + f_rel;
                                            isb = 1;
                                            }
                                        // find out which tone slot this goes in
                                        for (i=0; i<NPC_TONES; i++)
                                            {
                                        // allow for round-off in comparing tone f's
                                            if (fabs (f_rel - xtones[i]) < 1e-9)
                                                break;
                                            else if (xtones[i] == 0.0)
                                                {               // not found - allocate a new slot
                                                xtones[i] = f_rel;
                                                t309.ntones++;
                                                break;
                                                }
                                            }
                                        // did we run out of slots before finding tone?
                                        if (i == NPC_TONES)
                                            {
                                            if (!once)
                                                {
                                                fprintf (stderr, "more than %d baseband pcal tones"
                                                                 " - ignoring the rest\n", NPC_TONES);
                                                once = TRUE;
                                                }
                                            continue;
                                            }
                                        // FIXME ad hoc temporary fix for non-normalize pcal in zoom mode
                                        if (fabs(cquad) > 10.0)
                                            cquad /= 6e7;
                                        if (fabs(squad) > 10.0)
                                            squad /= 6e7;

                                        // calculate sample rate (samples/s)
                                        srate = 2e6 * D->freq[jf].bw * D->freq[jf].overSamp;
                                        // renormalize correlations to those created in the DOM
                                        norm_corr = - isb * floor (cquad * srate * t309.acc_period * 128.0 + 0.5);
                                        memcpy (&t309.chan[ochan].acc[i][0], &norm_corr, 4);

                                        norm_corr = floor (squad * srate * t309.acc_period * 128.0 + 0.5);
                                        memcpy (&t309.chan[ochan].acc[i][1], &norm_corr, 4);

                                        // tone freqs (in Hz) are spread through channel recs
                                        t309.chan[i].freq = 1e6 * f_rel;
                                        break;
                                        }
                                    }
                                }
                            }
                                        // write output record
                    write_t309 (&t309, fout);
                    }
                fclose(fin);
                }
            }

#endif //endif of D2M4_MERGE_PCAL_DATASTREAMS /////////////////////////////////

        fclose(fout);
        }
    return 0;
    }

// find vector of polarization labels for one antenna/datastream
// rjc 2013.7.15

void fill_ds_pols (DifxInput *D, int ant, char *ds_pols)
    {
    int i,
        j;
    DifxDatastream *pds;
                                    // initialize vector to all RCP
    for (j=0; j<MAX_DS_RECBANDS; j++)
        ds_pols[j] = 'R';

    for (i=0; i<D->nDatastream; i++)
        {
        pds = D->datastream + i;
        if (pds->antennaId == ant)
            {
            if (pds->nRecBand > MAX_DS_RECBANDS)
                printf ("WARNING!! Job antenna %d (%s) datastream %d has %d recorded bands, exceeds difx2mark4 limit of %d!\n",
                        ant, D->antenna[ant].name, i, pds->nRecBand, MAX_DS_RECBANDS);
            for (j=0; j<pds->nRecBand && j<MAX_DS_RECBANDS; j++)
                ds_pols[j] = pds->recBandPolName[j];
            break;
            }
        }
                                    // detect fall-through of loop, and complain
    if (i == D->nDatastream)
        printf ("WARNING!! antenna not found in datastreams, all pols set to R\n");
    }

    
// find a recband of an antenna that can host a given (zoom)freq Id

int find_parent_recband(const DifxInput *D, int ant, int freqId)
    {
    int i,
        j,
        recfq;
    double zflo,
        zfhi,
        rflo,
        rfhi;

    zflo = D->freq[freqId].freq - ((D->freq[freqId].sideband == 'U') ? 0.0 : D->freq[freqId].bw);
    zfhi = D->freq[freqId].freq + ((D->freq[freqId].sideband == 'U') ? D->freq[freqId].bw : 0.0);

    for (i=0; i<D->nDatastream; i++)
        {
        const DifxDatastream *pds = D->datastream + i;
        if (pds->antennaId == ant)
            {
            for (j=0; j<pds->nRecBand; j++)
                {
                recfq = *(pds->recFreqId + *(pds->recBandFreqId + j));
                rflo = D->freq[recfq].freq - ((D->freq[recfq].sideband == 'U') ? 0.0 : D->freq[recfq].bw);
                rfhi = D->freq[recfq].freq + ((D->freq[recfq].sideband == 'U') ? D->freq[recfq].bw : 0.0);
                if (zflo >= rflo && zfhi <= rfhi)
                    return recfq;
                }
            }
        }
        
    return -1;
    }

