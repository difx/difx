// createRoot creates a single root file from an input vex file
//
//  first created                          rjc  2010.2.23

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include "difx2mark4.h"

int createRoot (char *baseFile,     // common part of difx fileset name
                char *node,         // directory for output fileset
                char *rcode,        // 6 letter root suffix
                struct stations *stns, // station-relevant information
                DifxInput *D,       // difx input structure pointer
                struct CommandLineOptions *opts, // options pointer
                char *rootname)     // address to write root filename into
    {
    int i,
        k,
        n,
        ret,
        match,
        current_block,
        numchan,
        nsite = 0,
        durs,
        tarco = FALSE;              // true iff target_correlator = has been done

    char inname[256],
         v2dname[256],
         s[256],
         *pst[50],
         scan[32],
         mode[32],
         source[32],
         current_def[32],
         current_scan[32],
         buff[256],
         antlist[256],
         *pchar,
         c,
         current_site[3],
         line[30000],
         def_block[30000];

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
                           " AP_length   = 1.0 sec;\n",
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
          
    extern struct fbands fband[MAX_FBANDS];

    FILE *fin,
         *fout,
         *fv2d;
                                    // function prototypes
    char single_code (char *);
    void conv2date (double, struct date *);
    char getband (double);

                                    // initialize memory as necessary
    current_def[0] = 0;
    current_scan[0] = 0;
    inname[0] = 0;

                                    // check number of scans in job is 1
    if (D->nJob != 1)
        {
        perror ("difx2mark4");
        fprintf (stderr, "Error: Only one job may be selected and this must contain only one scan\n");
        return (-1);
        }
                                    // create scan identifier

                                    // source name
    strcpy (source, D->source->name);

    if (opts->verbose > 0)
        fprintf (stderr, "source %s\n", source);
                                    // modify source (change . to _) name
    i = 0;
    while (source[i] != 0)
        {
        if (source[i] == '.')
            source[i] = '_';
        i++;
        }
                                    // form input file name
    strcpy (v2dname, baseFile);
                                    // see if base file name ends in _xxx
    pchar = strrchr (v2dname, '_');
    if (pchar != NULL)
        *pchar = 0;                 // yes, truncate job# portion of name
    strcat (v2dname, ".v2d");

                                    // open vex2difx (.v2d) file
    fv2d = fopen (v2dname, "r");
    if (fv2d == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening v2d file %s\n", v2dname);
        return (-1);
        }
                                    // read v2d file pulling out vex file name
    while (fgets (line, 256, fv2d) != NULL)
        {
        if ((pchar = strstr (line, "vex = ")) != NULL)
            sscanf (pchar, "vex = %s", inname);
        else if ((pchar = strstr (line, "antennas = ")) != NULL)
            sscanf (pchar, "antennas = %s", antlist);
        }

    if (inname == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "Error reading vex file name from v2d file %s\n", v2dname);
        return (-1);
        }
    

    if (opts->verbose > 0)
        fprintf (stderr, "vex file name <%s>\n", inname);
                                    // open input vex file
    fin = fopen (inname, "r");
    if (fin == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening input vex file %s\n", inname);
        return (-1);
        }

                                    // create the new root file name
    strcpy (rootname, node);
    strcat (rootname, "/");
    strcat (rootname, source);
    strcat (rootname, ".");
    strcat (rootname, rcode);
    fprintf (stderr, "output rootfile: %s\n", rootname);
                                    // open output (root) file
    fout = fopen (rootname, "w");
    if (fout == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening output file %s\n", rootname);
        return (-1);
        }

    current_block = NO_BLOCK;
                                    // loop over all statements in input file
    while (fgets (line, 256, fin) != NULL)
        {
                                    // get all fields of this line
        strcpy (s, line);           // make a copy for (destructive) parsing
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
                                    // see if this is a block stmt
        match = FALSE;
        i = -1;
        while (strcmp (blocks[++i], "END_LIST"))
            if (strncmp (pst[0], blocks[i], strlen (blocks[i])) == 0)
                {
                match = TRUE;       // found it
                if (opts->verbose > 0)
                    fprintf (stderr, "processing %s block\n", blocks[i]);
                break;
                }

        if (match)                  // yes, it was found, change current block
            current_block = i;

        if (strncmp ("def", pst[0], strlen (pst[0])) == 0)
            strncpy (current_def, pst[1], 32);

        if (strncmp ("scan", pst[0], strlen (pst[0])) == 0)
            strncpy (current_scan, pst[1], 32);
                                    // remember the site ID for later
        if (strncmp (pst[0], "site_ID", 7) == 0)
            strncpy (current_site, pst[2], 3);

                                    // switch to proper context for current block
        switch (current_block)
            {
            case ANTENNA:           // fix up the axis_offset stmt
                if (strncmp (pst[0], "axis_offset", 11) == 0)
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
                break;
            case EOP:               // add dummy eop def
                if (strncmp (pst[0], "$EOP", 4) == 0)
                    strcat (line, "  def EOPXXX;\n  enddef;\n");
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
                                    // start renumbering ch's for each freq seq
                if (strncmp (pst[0], "def", 3) == 0)
                    numchan = 0;
                                    // insert channel names in chan_def stmts
                else if (strncmp (pst[0], "chan_def", 8) == 0)
                    {
                    sprintf (buff, "%c%02dU :", getband (atof (pst[3])), numchan++);
                    if (*pst[5] == 'L')
                        buff[3] = 'L';

                    strcat (buff, strchr (line, '=') + 1);
                                    // chop off line just after = sign
                    *(strchr (line, '=')+2) = 0;
                    strcat (line, buff); 
                    }
                break;
                                    
            case GLOBAL:
                                    // insert a dummy EOP ref (which is fine for fourfit)
                if (strncmp (pst[0], "$GLOBAL", 7) == 0)
                    strcat (line, "    ref $EOP = EOPXXX;\n"); 
                else if (strncmp (pst[0], "ref", 3) == 0 
                      && strncmp (pst[1], "$SCHEDULING_PARAMS", 18) == 0)
                    line[0] = '*';  // comment out ref to deleted section 
                else if (strncmp (pst[0], "ref", 3) == 0 
                      && strncmp (pst[1], "$EOP", 4) == 0)
                    line[0] = '*';  // comment out ref to real EOP section
                break;

            case MODE:
                if (strncmp (pst[0], "ref", 3) == 0 
                      && strncmp (pst[1], "$PASS_ORDER", 11) == 0)
                    line[0] = '*';  // comment out to avoid pass# problems
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
                else if (strcmp (current_scan, opts->scan) != 0) 
                    line[0] = 0;    
                                    // correct scan, insert one copy of ff ref time
                else if (strncmp (pst[0], "scan", 4) == 0)
                    {
                    //FIXME calculate this properly taking into account different scan lengths
                    //      for different antennas. See also stcodes/compute_reftime.c
                    conv2date (D->scan->mjdStart + D->scan->durSeconds / 172800.0, &caltime);
                    sprintf (buff, 
                             "    fourfit_reftime = %04hdy%03hdd%02hdh%02hdm%02ds;\n",
                             caltime.year, caltime.day, 
                             caltime.hour, caltime.minute, (int) caltime.second);
                    strcat (line, buff);
                    }
                                    // omit stations that didn't get correlated from scan
                else if (strncmp (pst[0], "station", 7) == 0)
                    {
                    if (strstr (antlist, pst[2]) == NULL)
                        line[0] = 0;
                                    // this station participates, use difx start
                    else 
                        {
                        sprintf (buff, " 00 sec : %d sec : : : : 0 ; * overridden times\n", 
                                 D->scan->durSeconds);
                        pchar = strchr (line, ':');
                        strcpy (pchar+2, buff);
                        }
                    }
                                    // process start
                else if (strncmp (pst[0], "start", 6) == 0)
                    {
                                    // generate fresh scan start, in case it was overridden
                    conv2date (D->scan->mjdStart, &caltime);
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

                                    // FIXME difxio scan start time is time when first
                                    // antenna comes on source. It may also be different
                                    // if mjdStart is set to the middle of a scan in
                                    // the .v2d file. For now we will copy the scan
                                    // verbatim from the vex file to avoid having to 
                                    // fiddle the individual antenna start and stop times
                           
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
                    if (strstr (antlist, current_site) == NULL)
                        {
                        if (opts->verbose > 0)
                            fprintf (stderr, "discarding unused def for site %s\n",
                                     current_site);
                        line[0] = 0;
                        break;
                        }
                    c = single_code (current_site);
                    if (c == 0)
                        {
                        fprintf (stderr, "All 52 codes used up, no code for %c%c\n",
                                 current_site[0], current_site[1]);
                        return (-1);
                        }
                    sprintf (buff, "  mk4_site_ID = %c;\n", c);
                    strcat (buff, line);
                    strcpy (line, def_block);
                    strcat (line, buff);
                                    // save 2-letter code for later use
                    memcpy ((stns+nsite)->intl_name, current_site, 2);
                                    // generate difx name by shifting to upper case
                    for (i=0; i<2; i++)
                        (stns+nsite)->difx_name[i] = toupper (current_site[i]);
                                    // find out difx antenna index for this site
                    for (i=0; i<D->nAntenna; i++)
                        if (memcmp ((stns+nsite)->difx_name, (D->antenna+i)->name, 2) == 0)
                            {
                                    // keep track of difx index for this site
                            (stns+nsite)->dind = i;
                            break;
                            }
                                    // bail out if antenna couldn't be found
                    if (i == D->nAntenna)
                        {
                        fprintf (stderr, "Couldn't find site %c%c in difx antenna list\n",
                                 (stns+nsite)->difx_name[0], (stns+nsite)->difx_name[1]);
                        return (-1);
                        }
                                    // file single letter code for future use
                    (stns+nsite)->mk4_id = c;
                    
                    if (opts->verbose > 0)
                        fprintf (stderr, "intl_name %c%c difx_name %c%c mk4_id %c "
                                         "difx site index %d\n",
                           *((stns+nsite)->intl_name), *((stns+nsite)->intl_name+1),
                           *((stns+nsite)->difx_name), *((stns+nsite)->difx_name+1),
                           (stns+nsite)->mk4_id, (stns+nsite)->dind);
                    nsite++;
                    (stns+nsite)->mk4_id = 0; // null-terminate list 
                    }
                
                else if (strncmp (pst[0], "$SITE", 5))
                    {               // append lines onto this def's block
                                    // so long as it isn't the $SITE stmt
                    strcat (def_block, line);
                    line[0] = 0;
                    }
                break;

            case SOURCE:
                if (strncmp (D->source->name, current_def, strlen (D->source->name)) != 0
                 && current_def[0] != 0)
                    line[0] = 0;    // inside of unwanted def, delete stmt
                break;

            case STATION:           // need to add ref to clock section
                if (strncmp (pst[0], "enddef", 6) == 0)
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

        fprintf (stderr, "number of stations: %d\n", nsite);
                                    // append extra statements to the end of the file
        strcpy (line, "$CLOCK;\n");
        fputs (line, fout); 
        
                                    // generate and put out clock_early statements
        n = 0;
        while ((stns+n)->mk4_id != 0 && n < MAX_STN)
            {
            k = (stns+n)->dind;     // index into difx arrays for this station #n
                                    // calculation time is ref epoch for linear model
            conv2date ((D->antenna + k) -> clockrefmjd, &caltime);
                                    // calculation time is epoch of start of validity
            conv2date (D->mjdStart, &valtime);
                                    // note that the difx clock convention is
                                    // opposite that of vex, thus the minus signs
            sprintf (line, " def %c%c; clock_early = %04hdy%03hdd%02hdh%02hdm%02ds :"
               "%6.3lf usec : %04hdy%03hdd%02hdh%02hdm%02ds : %le ; enddef;\n", 
                (stns + n)->intl_name[0], (stns + n)->intl_name[1], 
                valtime.year, valtime.day, valtime.hour,
                valtime.minute, (int)valtime.second, -(D->antenna+k)->clockcoeff[0],
                caltime.year, caltime.day, caltime.hour,
                caltime.minute, (int)caltime.second, -1e-6 * (D->antenna+k)->clockcoeff[1]);
            n++;
            fputs (line, fout); 
            }

        i = 0;
        while (strncmp (extra_lines[i], "END_EXTRA", 9))
            {
            if (strncmp (extra_lines[i], " AP_length", 10) == 0)
                {
                sprintf (buff, " AP_length = %5.2lf sec;\n", D->config->tInt);
                fputs (buff, fout);
                }
            else
                fputs (extra_lines[i], fout);
            i++;
            }
                                    // close input and output files
        fclose (fin);
        fclose (fout);
        return (1);
    }

char getband (double freq)
    {
    extern struct fbands fband[MAX_FBANDS];
    int i;
    char c;

    for (i=0; i<MAX_FBANDS; i++)
        if (fband[i].flo <= freq && freq <= fband[i].fhi)
            c = fband[i].code;
    return c;
    }
