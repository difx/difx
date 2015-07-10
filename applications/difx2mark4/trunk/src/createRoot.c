// createRoot creates a single root file from an input vex file
//
//  first created                          rjc  2010.2.23

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include "difx2mark4.h"
#include "other.h"
#define MAX_FS 10

int createRoot (DifxInput *D,           // difx input structure pointer
                struct fblock_tag *pfb, // ptr to filled-in fblock table
                int jobId,
                int scanId,
                char *node,             // directory for output fileset
                char *rcode,            // 6 letter root suffix
                struct stations *stns,  // station-relevant information
                struct CommandLineOptions *opts, // options pointer
                char *rootname)         // address to write root filename into
    {
    int i,
        j,
        k,
        n,
        match,
        current_block,
        numchan = 0,
        nsite = 0,
        nant = 0,
        scan_found = FALSE,
        itime,
        latest_start,
        earliest_stop,
        tarco = FALSE,              // true iff target_correlator = has been done
        sourceId,
        configId,
        delete_mode = FALSE,
        delete_freq = FALSE,
        yy, dd, hh, mm, ss,
        nfs,
        bps,
        ik,
        L_used = FALSE,             // true iff some LCP is present, etc.
        R_used = FALSE,
        X_used = FALSE,
        Y_used = FALSE;

    char s[256],
         *pst[50],
         source[DIFXIO_NAME_LENGTH], // for filename only, otherwise get from D
         current_def[32],
         current_scan[DIFXIO_NAME_LENGTH],
         fseq_list[MAX_FS][80],
         buff[256],
         hms[18],
         dms[18],
         *pchar,
         c,
         current_site[3],
         line[30000],
         def_block[30000];

    double freak,
           fract;

    struct date caltime,
                valtime;


    char *blocks[] = {"$NO_BLOCK", "$GLOBAL", "$EXPER", "$MODE", "$STATION", "$ANTENNA",
                      "$SITE", "$BBC", "$DAS", "$FREQ", "$HEAD_POS", "$IF",
                      "$PHASE_CAL_DETECT", "$PASS_ORDER", "$PROCEDURES", "$ROLL",
                      "$SCHEDULING_PARAMS", "$SCHED", "$SEFD", "$SOURCE", "$TRACKS",
                      "$EOP", "$CLOCK", "$TAPELOG_OBS", "END_LIST"};

    enum block_tokens {NO_BLOCK, GLOBAL, EXPER, MODE, STATION, ANTENNA,
                      SITE, BBC, DAS, FREQ, HEAD_POS, IF,
                      PHASE_CAL_DETECT, PASS_ORDER, PROCEDURES, ROLL,
                      SCHEDULING_PARAMS, SCHED, SEFD, SOURCE, TRACKS,
                      EOP, CLOCK, TAPELOG_OBS, END_LIST};
                                    // lines to be appended to output root file
    char *extra_lines[] = {"$EVEX_REV;\n", 
                           " rev = 1.0;\n", 
                           "$EVEX;\n",
                           " def 1234_std;\n", 
                           " corr_exp#   = 1234;\n",
                           " ovex_file   = dummy;\n",
                           " cvex_file   = dummy;\n",
                           " svex_file   = dummy;\n",
                           " AP_length   = 1.0 sec;\n",//FIXME
                           " speedup_factor = 1.0;\n",
                           " ref $CORR_CONFIG = CDUM;\n",
                           " ref $SU_CONFIG  = SDUM;\n",
                           " enddef;\n",
                           "$IVEX_REV;\n",
                           " rev = 1.0;\n",
                           "$CORR_INIT;\n",
                           " def INIT_DUMMY;\n",
                           " system_tempo = 1.00;\n",
                           " bocf_period =  8000000;\n",
                           " ref $PBS_INIT = PBS_DUMMY;\n",
                           " enddef;\n",
                           "$PBS_INIT;\n",
                           " def PBS_DUMMY;\n",
                           " enddef;\n",
                           "$LVEX_REV;\n",
                           " rev = 1.0;\n",
                           "$LOG;\n",
                           " def log_dummy;\n",
                           " enddef;\n",
                           "END_EXTRA"};
          

    FILE *fin,
         *fout;
                                    // function prototypes
    void fake_bocf_period(char [256], DifxConfig *);
    int isValidAntenna(const DifxInput *, char *, int);
    double frt (double, double, int);
    int fill_fblock (DifxInput *, struct CommandLineOptions *, struct fblock_tag *);

                                    // initialize memory as necessary
    current_def[0] = 0;
    current_scan[0] = 0;
    for (i=0; i<MAX_FS; i++)        // null out sequence names
        fseq_list[i][0] = 0;
    nfs = 0;

                                    // create scan identifier

                                    // source name
    sourceId = D->scan[scanId].pointingCentreSrc;
    if (sourceId < 0 || sourceId > D->nSource)
        {
        printf("sourceId %d out of range\n", sourceId);
        return (-1);
        }
    configId = D->scan[scanId].configId;
    if (configId < 0)
        {
        printf("No config for scan %d\n", scanId);
        return (-1);
        }

    strcpy (source, D->source[sourceId].name);

    if (opts->verbose > 0)
        printf ("      source %s\n", source);
                                    // modify source (change . to _) name
                                    // for filename only
    i = 0;
    while (source[i] != 0)
        {
        if (source[i] == '.')
            source[i] = '_';
        i++;
        }


    // FIXME inname for vexfile and antlist for job antennas
    
                                    // open input vex file
    printf("      Opening vex file <%s>\n", D->job[jobId].vexFile);
    fin = fopen (D->job[jobId].vexFile, "r");
    if (fin == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening input vex file %s for job %d\n", 
                 D->job[jobId].vexFile, jobId);
        return (-1);
        }

                                    // create the new root file name
    sprintf(rootname, "%s/%s.%s", node, source, rcode);
    printf ("      output rootfile: %s\n", rootname);
                                    // open output (root) file
    fout = fopen (rootname, "w");
    if (fout == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening output file %s\n", rootname);
        fclose (fin);
        return (-1);
        }
    fprintf (fout, "* correlated from input file %s\n*\n", D->job[jobId].inputFile);
    current_block = NO_BLOCK;
                                    // loop over all statements in input file
    while (fgets (line, 256, fin) != NULL)
        {
                                    // get all fields of this line
        strcpy (s, line);           // make a copy for (destructive) parsing
        //fprintf(stderr, "%s\n", line);
                                    // space-delimit any = sign
        if ((pchar = strchr (s, '=')) != NULL)
            {
            n = pchar - s;
            strcpy (pchar, " = ");
            strcpy (pchar+3, line+n+1);
            }


        for (i=0; i<50; i++)
            {
            pst[i] = strtok ((i>0) ? (char *) NULL : s, " :;");
            if (pst[i] == NULL)
                break;
            }
                                    // ensure that pointers are valid
        for (j=i; j<50; j++)
            pst[j] = "";

                                    // see if this is a block stmt
        match = FALSE;
        i = -1;
        while (strcmp (blocks[++i], "END_LIST"))
            if (strncmp (pst[0], blocks[i], strlen (blocks[i])) == 0)
                {
                match = TRUE;       // found it
                if (opts->verbose > 0)
                    printf ("      processing %s block\n", blocks[i]);
                break;
                }

        if (match)                  // yes, it was found, change current block
            current_block = i;

        if (strncmp ("def", pst[0], strlen (pst[0])) == 0)
            strncpy (current_def, pst[1], 32);

        if (strncmp ("scan", pst[0], strlen (pst[0])) == 0)
            {
            strncpy (current_scan, pst[1], 32);
            //fprintf(stderr, "scan=%s\n", current_scan);
            }
                                    // remember the site ID for later
        if (strncmp (pst[0], "site_ID", 7) == 0)
            {
            strncpy (current_site, pst[2], 3);
            //fprintf(stderr, "site=%s\n", current_site);
            }

                                    // switch to proper context for current block
        switch (current_block)
            {
            case ANTENNA:           // fix up the axis_offset stmt, iff necessary
                if (strncmp (pst[0], "axis_offset", 11) == 0
                 && strstr (line, "el:") == 0)
                    {
                    strcpy (buff, line);
                    pchar = strchr (line, '=') + 2;
                    strcpy (pchar, "el:");
                    strcpy (pchar + 3, strchr (buff, '=') + 1);
                    }
                else if (strncmp (pst[0], "antenna_motion", 14) == 0) 
                    line[0] = '*';  // comment out antenna motion command,
                                    // as it causes problems with vex parser
                break;

            case EXPER:             // modify target_correlator
                if (strcmp (pst[0], "target_correlator") == 0)
                    {
                    strcpy (line, "    target_correlator = difx;\n");
                    tarco = TRUE;
                    }
                else if (strncmp (pst[0], "enddef", 6) == 0 && tarco == FALSE)
                    {
                    strcpy (line, "    target_correlator = difx;\n  enddef;\n");
                    tarco = TRUE;
                    }
                break;

            case GLOBAL:
                                    // insert a dummy EOP ref (which is fine for fourfit)
                if (strncmp (pst[0], "$GLOBAL", 7) == 0)
                    strcat (line, "    ref $EOP = EOP_DIFX_INPUT;\n"); 
                else if (strncmp (pst[0], "ref", 3) == 0 
                      && strncmp (pst[1], "$SCHEDULING_PARAMS", 18) == 0)
                    line[0] = '*';  // comment out ref to deleted section 
                else if (strncmp (pst[0], "ref", 3) == 0 
                      && strncmp (pst[1], "$EOP", 4) == 0)
                    line[0] = '*';  // comment out ref to real EOP section
                break;

            case MODE:              // delete all modes other than current one
                if (strncmp (pst[0], "def", 3) == 0 
                 && strncmp (pst[1], D->scan[scanId].obsModeName, 30) != 0)
                    delete_mode = TRUE;
                if (delete_mode)
                    {
                    line[0] = 0;
                                    // exit delete mode at enddef
                    if (strncmp (pst[0], "enddef", 6) == 0)
                        delete_mode = FALSE;
                    }
                else                // mode is currently active
                    {
                    if (strncmp (pst[0], "ref", 3) == 0)
                        {
                        if (strncmp (pst[1], "$BBC", 4) == 0
                         || strncmp (pst[1], "$FREQ", 5) == 0
                         || strncmp (pst[1], "$IF", 3) == 0
                         || strncmp (pst[1], "$TRACKS", 7) == 0
                         || strncmp (pst[1], "$PHASE_CAL_DETECT", 17) == 0
                         || strncmp (pst[1], "$PROCEDURES", 11) == 0
                         || strncmp (pst[1], "$ROLL", 5) == 0
                         || strncmp (pst[1], "$HEAD_POS", 9) == 0
                         || strncmp (pst[1], "$PASS_ORDER", 11) == 0)
                            line[0] = 0; // delete original mode lines
                        }
                                    // synthesize new ref's at end of MODE
                    else if (strncmp (pst[0], "enddef", 6) == 0)
                        {
                                    // insert one freq line per used antenna
                        for (n = 0; n < D->nAntenna; n++)
                            if (D->scan[scanId].im != NULL 
                             && D->scan[scanId].im[n] != 0)
                                    // FIXME - generate antenna name from station
                                fprintf (fout, "    ref $FREQ = ant%02d:%c%c;\n",
                                      n, D->antenna[n].name[0], tolower (D->antenna[n].name[1]));
                        fprintf (fout, "    ref $BBC = bbcs;\n");
                        fprintf (fout, "    ref $IF = ifs;\n");
                        fprintf (fout, "    ref $TRACKS = trax;\n");
                        }
                    }
                break;
                
            case NO_BLOCK:          // insert ovex revision number
                if (strncmp (pst[0], "VEX_rev", 7) == 0)
                    strcpy (line, "$OVEX_REV;\nrev = 1.5;\n");
                break;

            case SCHED:             // insert fourfit reference time into sched block
                                    // don't change $SCHED line
                if (strncmp (pst[0], "$SCHED", 6) == 0)
                    break;
                                    // skip line copy if we're within the wrong scan
                else if (strcmp (current_scan, D->scan[scanId].identifier) != 0) 
                    {
                    line[0] = 0;
                    continue;
                    }
                                    // correct scan, insert one copy of ff ref time
                else if (strncmp (pst[0], "scan", 4) == 0)
                    {
                    scan_found = TRUE;
                                    // initialize earliest/latest to extrema
                    latest_start = 0;
                    earliest_stop = 9999;
                    }
                                    // Check that station is in this scan
                else if (strncmp (pst[0], "station", 7) == 0)
                    {
                                    // parse station line to find scan intersection
                    pchar = strchr (line, ':');
                    itime = atoi (pchar+1);
                    if (itime > latest_start)
                        latest_start = itime;
                    pchar = strchr (pchar+1, ':');
                    itime = atoi (pchar+1);
                    if (itime < earliest_stop)
                        earliest_stop = itime;

                    i = isValidAntenna(D, pst[2], scanId);
                    if(i < 0)
                        line[0] = 0;
                                    // this station participates, use difx start
                    else 
                        {
                        nant++;
                        stns[i].inscan = TRUE;
                        }
                    }
                                    // source name may have been changed in .v2d
                                    // set to difx input source name
                else if (strncmp (pst[0], "source", 6) == 0)
                    {
                    sprintf (buff, "  source = %s;",
                            D->source[sourceId].name);

                    if ((pchar = strchr (line, ';')) == NULL)
                        {
                        strcpy (line, buff);
                        strcat (line, "\n");
                        }
                    else            // copy rest of stmts from the start line
                        {
                        strcat (buff, pchar+1);
                        strcpy (line, buff);
                        }
                    }
                                    // process start
                else if (strncmp (pst[0], "start", 5) == 0)
                    {
                    sscanf (pst[2], "%dy%dd%dh%dm%ds", &yy, &dd, &hh, &mm, &ss);
                    fract = ((ss / 60.0 + mm) / 60.0 + hh ) / 24.0;
                                    // generate fresh scan start, in case it was overridden
                    conv2date (D->scan[scanId].mjdStart, &caltime);
                    sprintf (buff, "  start = %04hdy%03hdd%02hdh%02hdm%02ds;",
                         caltime.year, caltime.day, 
                         caltime.hour, caltime.minute, (int) caltime.second);
                    if ((pchar = strchr (line, ';')) == NULL)
                        {
                        strcpy (line, buff);
                        strcat (line, "\n");
                        }
                    else            // copy rest of stmts from the start line
                        {
                        strcat (buff, pchar+1);
                        strcpy (line, buff);
                        }
                    }

                else if (strncmp (pst[0], "endscan", 7) == 0)
                    {
                                    // end of the scan
                                    // generate and insert a fourfit ref time stmt
                                    // at the middle of the intersected scans
                    itime = (latest_start + earliest_stop) / 2;
                    conv2date (frt (D->scan[scanId].mjdStart, fract, itime), &caltime);

                    sprintf (buff, 
                             "    fourfit_reftime = %04hdy%03hdd%02hdh%02hdm%02ds;\n",
                             caltime.year, caltime.day, 
                             caltime.hour, caltime.minute, (int) caltime.second);
                                    // insert frt prior to line containing endscan
                    fputs (buff, fout); 
                    }
                                    // FIXME difxio scan start time is time when first
                                    // antenna comes on source. It may also be different
                                    // if mjdStart is set to the middle of a scan in
                                    // the .v2d file.
                break;

            case SCHEDULING_PARAMS: // the scheduling_params block is deleted
                line[0] = 0;
                break;

            case SITE:              // need to insert the single char site ID
                                    // and filter out unused sites
                if (strncmp (pst[0], "def", 6) == 0)
                    {               // start fresh block for this def
                    strcpy (def_block, line);
                    line[0] = 0;
                    }
                                    // if this site is in the correlation, it is
                                    // now time to insert the site ID
                else if (strncmp (pst[0], "enddef", 6) == 0)
                    {
                                    // if not in difx list, discard whole block
                    i = isValidAntenna(D, current_site, scanId);
                    if (i < 0)
                        {
                        if (opts->verbose > 0)
                            printf ("        intl_name %c%c difx_name -- mk4_id - "
                                             "difx site index --\n",
                                     current_site[0], current_site[1]);
                        line[0] = 0;
                        break;
                        }
                    c = single_code (current_site);
                    if (c == 0)
                        {
                        printf ("      All 52 codes used up, no code for %c%c\n",
                                 current_site[0], current_site[1]);
                        return (-1);
                        }
                    sprintf (buff, "  mk4_site_ID = %c;\n", c);
                    strcat (buff, line);
                    strcpy (line, def_block);
                    strcat (line, buff);
                                    // save 2-letter code for later use
                    memcpy ((stns+i)->intl_name, current_site, 2);
                                    // file single letter code for future use
                    (stns+i)->mk4_id = c;
                    
                    if (opts->verbose > 0)
                        printf ("        intl_name %c%c difx_name %c%c mk4_id %c "
                                         "difx site index %d\n",
                           *((stns+i)->intl_name), *((stns+i)->intl_name+1),
                           *((stns+i)->difx_name), *((stns+i)->difx_name+1),
                           (stns+i)->mk4_id, i);
                    nsite++;
                    }
                
                else if (strncmp (pst[0], "$SITE", 5))
                    {               // append lines onto this def's block
                                    // so long as it isn't the $SITE stmt
                    strcat (def_block, line);
                    line[0] = 0;
                    }
                break;

            case STATION:           // need to add ref to clock section
                                    // delete vex's clock and das
                if (strncmp (pst[1], "$CLOCK", 6) == 0
                 || strncmp (pst[1], "$DAS", 4) == 0)
                    line[0] = 0;

                else if (strncmp (pst[0], "enddef", 6) == 0)
                    {
                    strcpy (buff, "    ref $CLOCK = ");
                    strcat (buff, current_def);
                    strcat (buff, ";\n");
                    strcat (buff, line);
                    strcpy (line, buff);
                    }
                break;

                                    // edit out these sections that will be re-generated
            case BBC:
            case CLOCK:
            case EOP:
            case FREQ:
            case IF:
            case SOURCE:
            case TRACKS:
                line[0] = 0;
                break;
                                    // nothing special needs to be done for these blocks
                                    // but delete for clarity's sake
            case DAS:
            case HEAD_POS:
            case PASS_ORDER:
            case PHASE_CAL_DETECT:
            case PROCEDURES:
            case ROLL:
            case SEFD:
                line[0] = 0;
                break;
                                    // just copy any other sections to the output
            case TAPELOG_OBS:
            default:
                break;
            }
        fputs (line, fout); 
                                    // detect exit of def block and clear current def
        if (strncmp (pst[0], "enddef", 6) == 0)
            current_def[0] = 0;
        }

        if(scan_found == FALSE)
            {
            fprintf (stderr, "Error: scan with Id %d and identifier \"%s\" cannot "
                     "be found in vex file\n",
                     scanId, D->scan[scanId].identifier);
            fclose (fin);
            fclose (fout);
            return(-1);
            }
        printf ("      number of stations: %d\n", nsite);
        if (nsite != nant)
            printf ("Warning! Got %d stations ($SITE) but %d antennas ($STATION).\n", nsite, nant);
                                    // append extra statements to the end of the file

                                    // generate $FREQ block
        if (fill_fblock (D, opts, pfb) < 0)
            {
            printf ("error generating $FREQ block\n");
            return -1;
            }
        fprintf (fout, "$FREQ;\n");
        fprintf (fout, "* Generated from DiFX input by difx2mark4\n*\n");
        for (n = 0; n < D->nAntenna; n++)
            {
            fprintf (fout, "  def ant%02d;\n", n);
            i = -1;
            j = 0;
                                    // search through fblock for all ref to this antenna
            while (pfb[++i].stn[0].ant >= 0)
                if (pfb[i].stn[0].ant == n || pfb[i].stn[1].ant == n)
                    {
                    if (pfb[i].stn[0].ant == n && pfb[i].stn[0].first_time)
                        k = 0;
                    else if (pfb[i].stn[1].ant == n && pfb[i].stn[1].first_time)
                        k = 1;
                    else            // neither antenna is first use, skip to next product
                        continue;
                                    // print out a chan_def line
                    fprintf (fout, "   chan_def = %s : : %13.6f MHz : %c : %12.6f MHz"
                                   " : &Ch%02d : &BBC%c%1d;\n", 
                             pfb[i].stn[k].chan_id,
                             fabs (pfb[i].stn[k].freq),
                             pfb[i].stn[k].sideband,
                             pfb[i].stn[k].bw,
                             j, 
                             pfb[i].stn[k].pol,
                             pfb[i].stn[k].pcal_int);
                                    // keep track of polarizations that appear
                    L_used |= pfb[i].stn[k].pol == 'L';
                    R_used |= pfb[i].stn[k].pol == 'R';
                    X_used |= pfb[i].stn[k].pol == 'X';
                    Y_used |= pfb[i].stn[k].pol == 'Y';
                    
                    if (pfb[i].stn[k].pcal_int != 0
                     && pfb[i].stn[k].pcal_int != 1
                     && pfb[i].stn[k].pcal_int != 5
                     && pfb[i].stn[k].pcal_int != 10
                     && pfb[i].stn[k].pcal_int != 200)
                        printf ("Warning! Pcal interval of %d MHz has no BBC statement.\n",
                                pfb[i].stn[k].pcal_int);
                    ik = i;         // save i for which k applied
                    j++;
                    }
                
            fprintf (fout, "    sample_rate = %5.1f Ms/sec;\n", 2.0 * pfb[ik].stn[k].bw);
            fprintf (fout, "  enddef;\n");
            }
        fprintf (fout, "$BBC;\n");
        fprintf (fout, "  def bbcs;\n");
        if (L_used)
            {
            fprintf (fout, "    BBC_assign = &BBCL0  : 01 : &IFL0;\n");
            fprintf (fout, "    BBC_assign = &BBCL1  : 03 : &IFL1;\n");
            fprintf (fout, "    BBC_assign = &BBCL5  : 05 : &IFL5;\n");
            fprintf (fout, "    BBC_assign = &BBCL10 : 07 : &IFL10;\n");
            fprintf (fout, "    BBC_assign = &BBCL200: 09 : &IFL200;\n");
            }
        if (R_used)
            {
            fprintf (fout, "    BBC_assign = &BBCR0  : 02 : &IFR0;\n");
            fprintf (fout, "    BBC_assign = &BBCR1  : 04 : &IFR1;\n");
            fprintf (fout, "    BBC_assign = &BBCR5  : 06 : &IFR5;\n");
            fprintf (fout, "    BBC_assign = &BBCR10 : 08 : &IFR10;\n");
            fprintf (fout, "    BBC_assign = &BBCR200: 10 : &IFR200;\n");
            }
        if (X_used)
            {
            fprintf (fout, "    BBC_assign = &BBCX0  : 01 : &IFX0;\n");
            fprintf (fout, "    BBC_assign = &BBCX1  : 03 : &IFX1;\n");
            fprintf (fout, "    BBC_assign = &BBCX5  : 05 : &IFX5;\n");
            fprintf (fout, "    BBC_assign = &BBCX10 : 07 : &IFX10;\n");
            fprintf (fout, "    BBC_assign = &BBCX200: 09 : &IFX200;\n");
            }
        if (R_used)
            {
            fprintf (fout, "    BBC_assign = &BBCY0  : 02 : &IFY0;\n");
            fprintf (fout, "    BBC_assign = &BBCY1  : 04 : &IFY1;\n");
            fprintf (fout, "    BBC_assign = &BBCY5  : 06 : &IFY5;\n");
            fprintf (fout, "    BBC_assign = &BBCY10 : 08 : &IFY10;\n");
            fprintf (fout, "    BBC_assign = &BBCY200: 10 : &IFY200;\n");
            }
        fprintf (fout, "  enddef;\n");
        
        fprintf (fout, "$IF;\n");
        fprintf (fout, "  def ifs;\n");
                                   
        if (L_used)
            {
            fprintf (fout, "    if_def = &IFL0  : 1N : L : 10000.0 MHz : U :  0 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFL1  : 1N : L : 10000.0 MHz : U :  1 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFL5  : 1N : L : 10000.0 MHz : U :  5 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFL10 : 1N : L : 10000.0 MHz : U : 10 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFL200: 1N : L : 10000.0 MHz : U :200 MHz : 0 Hz;\n");
            }
        if (R_used)
            {
            fprintf (fout, "    if_def = &IFR0  : 2N : R : 10000.0 MHz : U :  0 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFR1  : 2N : R : 10000.0 MHz : U :  1 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFR5  : 2N : R : 10000.0 MHz : U :  5 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFR10 : 2N : R : 10000.0 MHz : U : 10 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFR200: 2N : R : 10000.0 MHz : U :200 MHz : 0 Hz;\n");
            }
        if (X_used)
            {
            fprintf (fout, "    if_def = &IFX0  : 1N : X : 10000.0 MHz : U :  0 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFX1  : 1N : X : 10000.0 MHz : U :  1 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFX5  : 1N : X : 10000.0 MHz : U :  5 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFX10 : 1N : X : 10000.0 MHz : U : 10 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFX200: 1N : X : 10000.0 MHz : U :200 MHz : 0 Hz;\n");
            }
        if (Y_used)
            {
            fprintf (fout, "    if_def = &IFY0  : 2N : Y : 10000.0 MHz : U :  0 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFY1  : 2N : Y : 10000.0 MHz : U :  1 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFY5  : 2N : Y : 10000.0 MHz : U :  5 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFY10 : 2N : Y : 10000.0 MHz : U : 10 MHz : 0 Hz;\n");
            fprintf (fout, "    if_def = &IFY200: 2N : Y : 10000.0 MHz : U :200 MHz : 0 Hz;\n");
            }
        fprintf (fout, "  enddef;\n");
        
        fprintf (fout, "$TRACKS;\n");
        fprintf (fout, "  def trax;\n");
                                    // FIXME - assumes global bits/sample
        fprintf (fout, "    bits/sample = %d;\n", pfb[0].stn[0].bs);
        fprintf (fout, "  enddef;\n");
                                   // generate $SOURCE section from difx header
        rad2hms (D->source[sourceId].ra,  hms);
        rad2dms (D->source[sourceId].dec, dms);

        fprintf (fout, "$SOURCE;\n");
        fprintf (fout, "* Generated from DiFX input by difx2mark4\n");
        fprintf (fout, "def %s;\n", D->source[sourceId].name);
        fprintf (fout, " source_name = %s;\n", D->source[sourceId].name);
        fprintf (fout, " ra = %s; dec =  %s; ref_coord_frame = J2000;\n",
                 hms,
                 dms);
        fprintf (fout, "enddef;\n\n");

        strcpy (line, "$CLOCK;\n");
        fputs (line, fout); 
        fprintf (fout, "* Generated from DiFX input by difx2mark4\n");
        
                                    // generate and put out clock_early statements
        for(n = 0; n < D->nAntenna; n++)
            {
            if (!stns[n].inscan)
                continue;
            conv2date ((D->antenna + n) -> clockrefmjd, &caltime);
                                    // calculation time is epoch of start of validity
            conv2date (D->mjdStart, &valtime);
                                    // note that the difx clock convention is
                                    // opposite that of vex, thus the minus signs
            fprintf (fout, " def %c%c; clock_early = %04hdy%03hdd%02hdh%02hdm%02ds :"
               "%6.3lf usec : %04hdy%03hdd%02hdh%02hdm%02ds : %le ; enddef;\n", 
                (stns + n)->intl_name[0], (stns + n)->intl_name[1], 
                valtime.year, valtime.day, valtime.hour,
                valtime.minute, (int)valtime.second, -(D->antenna+n)->clockcoeff[0],
                caltime.year, caltime.day, caltime.hour,
                caltime.minute, (int)caltime.second, -1e-6 * (D->antenna+n)->clockcoeff[1]);
            if (!isalnum((stns + n)->intl_name[0]))
                {
                printf ("Warning! Station '%c%c' has non-alphanumeric site_ID of 0x%02X 0x%02X in hex.\n",
                    (stns + n)->difx_name[0], (stns + n)->difx_name[1],
                    (stns + n)->intl_name[0], (stns + n)->intl_name[1]);
                }
            }

        //FIXME output difxio EOPs
        fprintf(fout, "\n$EOP;\n");
        fprintf (fout, "* Generated from DiFX input by difx2mark4\n");
        fprintf(fout, "def EOP_DIFX_INPUT;\nenddef;\n\n");

        i = 0;
        while (strncmp (extra_lines[i], "END_EXTRA", 9))
            {
            if (strncmp (extra_lines[i], " AP_length", 10) == 0)
                {
                sprintf (buff, " AP_length = %7.4lf sec;\n", D->config[configId].tInt);
                fputs (buff, fout);
                }
            else if (strncmp (extra_lines[i], " bocf_period", 12) == 0)
                {
                fake_bocf_period(buff, D->config + configId);
                fputs (buff, fout);
                }
            else
                fputs (extra_lines[i], fout);
            i++;
            }
                                    // close input and output files
        fclose (fin);
        fclose (fout);
        return (nant);
    }

char getband (double freq)
    {
    extern struct fbands fband[MAX_FBANDS];
    int i;
    char c = 'B';

    for (i=0; i<MAX_FBANDS; i++)
        if (fband[i].flo <= freq && freq <= fband[i].fhi)
            c = fband[i].code;
    return c;
    }

/*
 * fourfit requires that the AP (D->config[configId].tInt) be an integral
 * number of bocf periods.  There is no bocf period in the s/w
 * correlator, but the subintNS period is perhaps similar.
 * If it isn't suitable, we'll just quarter the AP and move on.
 *
 * ap_in_sysclks = rint ((double)param->acc_period * 32e6 / param->speedup);
 * ap_in_sysclks % param->bocf_period == 0 is required
 *
 * bocf units are 32e6/s == 32e-3/ns
 */
void fake_bocf_period(char buff[256], DifxConfig *config)
    {
    unsigned long ap_in_sysclks, bocf_period;
    ap_in_sysclks = rint((double)config->tInt * 32e6 / 1.0);
    bocf_period = (config->subintNS * 32) / 1000;
    if (ap_in_sysclks % bocf_period != 0)
    bocf_period = ap_in_sysclks / 4;

    sprintf (buff, " bocf_period = %lu;\n*subintNS = %d;\n",
    bocf_period, config->subintNS);
    }

int isValidAntenna(const DifxInput *D, char *antName, int scanId)
    {
    /* if named antenna is present in the specified scan, return
     * difxio antenna Id. Otherwise return -1
     */
    int antId, i;
    char antUpper[DIFXIO_NAME_LENGTH];

                                    //antenna in input?
    for(i = 0; i < strlen(antName); i++)
        antUpper[i] = toupper(antName[i]);
    antUpper[i] = '\0';
    antId = DifxInputGetAntennaId(D, antUpper);
    if (antId < 0)
        return(-1);
                                    //antenna in scan?
    if (D->scan[scanId].im != NULL && D->scan[scanId].im[antId] == 0)
        return(-1);

    return(antId);
    }

// calculates frt using difx start mjd, vex scheduled start as
// fraction of a day, and the offset off the scan intersection
// midpoint from the scheduled vex start
double frt (double mjd, double fract, int midpt_offset)
    {
    double start_delay,
           epoch;

    start_delay = fmod (mjd, 1.0) - fract;
    epoch = mjd - start_delay + midpt_offset / 86400.0;
                                    // if the difx start mjd is after the epoch of the
                                    // vex-calculated frt, adjust the frt
                                    // to be at the difx start time
    if (mjd - epoch > 0.0)
        epoch = mjd;
    return epoch;
    }
