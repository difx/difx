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

int createRoot (DifxInput *D,       // difx input structure pointer
                int jobId,
                int scanId,
                char *node,         // directory for output fileset
                char *rcode,        // 6 letter root suffix
                struct stations *stns, // station-relevant information
                struct CommandLineOptions *opts, // options pointer
                char *rootname)     // address to write root filename into
    {
    int i,
        j,
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
        yy, dd, hh, mm, ss;

    char s[256],
         *pst[50],
         source[DIFXIO_NAME_LENGTH], // for filename only, otherwise get from D
         current_def[32],
         current_scan[DIFXIO_NAME_LENGTH],
         current_freq[80],
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

                                    // initialize memory as necessary
    current_def[0] = 0;
    current_scan[0] = 0;
    current_freq[0] = 0;

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

            case CLOCK:             //the clock section will be replaced with new clock table
                    line[0] = 0;
                    break;

            case DAS:
                if (strncmp (pst[0], "electronics_rack_type", 21) == 0)
                    {               // patch up invalid rack types
                    if (strncmp (pst[2], "K4-2/M4", 7) == 0)
                         {
                         pchar = strstr (line, "K4-2/M4");
                         strcpy (pchar, "K4;\n");
                         }
                    else if (strncmp (pst[2], "K4-2", 4) == 0)
                         {
                         pchar = strstr (line, "K4-2");
                         strcpy (pchar, "K4;\n");
                         }
                    else if (strncmp (pst[2], "none", 4) == 0)
                         {
                         pchar = strstr (line, "none");
                         strcpy (pchar, "Mark4;\n");
                         }
                    }               
                else if (strncmp (pst[0], "recording_system_ID", 19) == 0)
                    {               // patch up invalid (K2) ID's
                    if (isalpha (*pst[2]))
                         {          // ID must be numeric, set to 0
                         pchar = strstr (line, pst[2]);
                         strncpy (pchar, "0000000", strlen (pst[2]));
                         }
                    }
                else if (strncmp (pst[0], "record_transport_type", 21) == 0)
                    {               // comment out invalid (K5) type
                    if (strncmp (pst[2], "K5", 2) == 0)
                         line[0] = '*';
                    }
                else if (strncmp (pst[0], "tape_motion", 11) == 0 
                      || strncmp (pst[0], "tape_length", 11) == 0)
                    line[0] = '*';  // comment out tape motion and tape
                                    // length commands, as S2 syntax
                                    // causes problems with vex parser
                else if (strncmp (pst[0], "headstack", 9) == 0 &&
                         strlen(pst[0]) == 9) 
                    line[0] = '*';  // NRAO SCHED produces invalid
                                    // headstack
                break;
            case EOP:            //the EOP section will be replaced from difx input
                    line[0] = 0;
                    break;

            case EXPER:             // modify target_correlator
                if (strcmp (pst[0], "target_correlator") == 0)
                    {
                    strcpy (line, "target_correlator = difx;\n");
                    tarco = TRUE;
                    }
                else if (strncmp (pst[0], "enddef", 6) == 0 && tarco == FALSE)
                    {
                    strcpy (line, "  target_correlator = difx;\n  enddef;\n");
                    tarco = TRUE;
                    }
                break;

            case FREQ:          
                if (strncmp (pst[0], "def", 3) == 0
                 && strncmp (pst[1], current_freq, 80) != 0)
                                    // enter deletion mode if this freq seq not active
                    delete_freq = TRUE;
                                    // delete all lines for unused freq def's
                if (delete_freq)
                    {
                    line[0] = 0;
                    if (strncmp (pst[0], "enddef", 6) == 0)
                        delete_freq = FALSE;
                    break;
                    }
                                    // start renumbering ch's for each freq seq
                if (strncmp (pst[0], "def", 3) == 0)
                    numchan = 0;
                                    // insert channel names in chan_def stmts
                else if (strncmp (pst[0], "chan_def", 8) == 0)
                    {
                    if(pst[2][0] == '&')
                        {
                        freak = atof (pst[3]);
                        c = *pst[5]; // sideband: 'U' or 'L'
                        }
                    else            // band_id field not present, indices different
                        {
                        freak = atof (pst[2]);
                        c = *pst[4];
                        }
                    sprintf (buff, "%c%02d%c :", getband (freak), numchan++, c);

                    strcat (buff, strchr (line, '=') + 1);
                                    // chop off line just after = sign
                    *(strchr (line, '=')+2) = 0;
                    strcat (line, buff); 
                                    // comment out freq channels that weren't correlated
                                    // look for matching frequency and sideband
                    for (i=0; i<D->nFreq; i++)
                        if (fabs (D->freq[i].freq - freak) < 1e-6
                         && D->freq[i].sideband == buff[3])
                            break;
                                    // if freq/sb not there, or is unused, comment it out
                    if (i == D->nFreq ||
                            D->config[D->scan[scanId].configId].freqIdUsed[i] <= 0)
                        line[0] = '*';
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
                    if (strncmp (pst[0], "ref", 3) == 0 
                     && strncmp (pst[1], "$FREQ", 5) == 0)
                        strncpy (current_freq, pst[3], 80);
                    if (strncmp (pst[0], "ref", 3) == 0 
                      && strncmp (pst[1], "$PASS_ORDER", 11) == 0)
                        line[0] = '*';  // comment out to avoid pass# problems
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
                    i = isValidAntenna(D, pst[2], scanId);
                    if(i < 0)
                        line[0] = 0;
                                    // this station participates, use difx start
                    else 
                        {
                        nant++;
                        stns[i].inscan = TRUE;
                                    // parse station line to find scan intersection
                        pchar = strchr (line, ':');
                        itime = atoi (pchar+1);
                        if (itime > latest_start)
                            latest_start = itime;
                        pchar = strchr (pchar+1, ':');
                        itime = atoi (pchar+1);
                        if (itime < earliest_stop)
                            earliest_stop = itime;
                                   
                                    // FIXME - is this override really necessary or optimal?
                        sprintf (buff, " 00 sec : %d sec : : : : 0 ; * overridden times\n", 
                                 D->scan[scanId].durSeconds);
                        pchar = strchr (line, ':');
                        strcpy (pchar+2, buff);
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

            case SOURCE:           // source block will be generated from the difx_input
                    line[0] = 0;
                    break;

            case STATION:           // need to add ref to clock section
                                    // comment out vex's clock
                if (strncmp (pst[1], "$CLOCK", 6) == 0)
                    line[0] = '*';

                else if (strncmp (pst[0], "enddef", 6) == 0)
                    {
                    strcpy (buff, "    ref $CLOCK = ");
                    strcat (buff, current_def);
                    strcat (buff, ";\n");
                    strcat (buff, line);
                    strcpy (line, buff);
                    }
                break;

            case TRACKS:
                                    // insert one copy of # bits/sample in each def
                if (strncmp (pst[0], "def", 3) == 0)
                    {
                    sprintf (buff, "  bits/sample = %d;\n", D->quantBits);
                    strcat (line, buff);
                    }
                else if (strncmp (pst[0], "S2_recording_mode", 17) == 0
                      || strncmp (pst[0], "S2_data_source", 14) == 0)
                    line[0] = '*';  // filter out unparseable lines
                break;

                                    // nothing special needs to be done for these blocks
            case BBC:
            case IF:
            case HEAD_POS:
            case PASS_ORDER:
            case PHASE_CAL_DETECT:
            case PROCEDURES:
            case ROLL:
            case SEFD:
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
                                    // append extra statements to the end of the file

                                    // generate source section from difx header
        rad2hms(D->source[sourceId].ra, hms);
        rad2dms(D->source[sourceId].dec, dms);
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
            sprintf (line, " def %c%c; clock_early = %04hdy%03hdd%02hdh%02hdm%02ds :"
               "%6.3lf usec : %04hdy%03hdd%02hdh%02hdm%02ds : %le ; enddef;\n", 
                (stns + n)->intl_name[0], (stns + n)->intl_name[1], 
                valtime.year, valtime.day, valtime.hour,
                valtime.minute, (int)valtime.second, -(D->antenna+n)->clockcoeff[0],
                caltime.year, caltime.day, caltime.hour,
                caltime.minute, (int)caltime.second, -1e-6 * (D->antenna+n)->clockcoeff[1]);
            fputs (line, fout); 
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
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
