// createRoot creates a single root file from an input vex file
//
//  first created                          rjc  2010.2.23

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
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
        nsite = 0;

    char inname[256],
         line[256],
         s[256],
         *pst[50],
         scan[32],
         mode[32],
         source[32],
         current_def[32],
         current_scan[32],
         buff[256],
         *pchar,
         c,
         current_site[2];

    double x[3];

    struct date caltime;


    char *blocks[] = {"$NO_BLOCK", "$GLOBAL", "$EXPER", "$MODE", "$STATION", "$ANTENNA",
                      "$SITE", "$BBC", "$DAS", "$FREQ", "$HEAD_POS", "$IF",
                      "$PHASE_CAL_DETECT", "$PASS_ORDER", "$ROLL",
                      "$SCHEDULING_PARAMS", "$SCHED", "$SOURCE", "$TRACKS",
                      "$EOP", "END_LIST"};

    enum block_tokens {NO_BLOCK, GLOBAL, EXPER, MODE, STATION, ANTENNA,
                      SITE, BBC, DAS, FREQ, HEAD_POS, IF,
                      PHASE_CAL_DETECT, PASS_ORDER, ROLL,
                      SCHEDULING_PARAMS, SCHED, SOURCE, TRACKS,
                      EOP, END_LIST};
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
    FILE *fin,
         *fout;
                                    // function prototypes
    char single_code (char *);
    void conv2date (double, struct date *);

                                    // initialize memory as necessary
    current_def[0] = 0;
    numchan = 0;
                                    // create scan identifier

                                    // source name
    strcpy (source, D->source->name);

    if (opts->verbose > 0)
        fprintf (stderr, "source %s\n", source);
                                    // form input file name
    strcpy (inname, baseFile);
                                    // see if base file name ends in _xxx
    pchar = strrchr (inname, '_');
    if (pchar != NULL)
        *pchar = 0;                 // yes, truncate job# portion of name
    strcat (inname, ".vex");

                                    // open input (.vex) file
    fin = fopen (inname, "r");
    if (fin == NULL)
        {
        perror ("difx2mark4");
        fprintf (stderr, "fatal error opening input file %s\n", inname);
        return (-1);
        }

                                    // create the new root file name
    strcpy (rootname, node);
    strcat (rootname, "/");
    strcat (rootname, D->source->name);
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
                break;

            case DAS:
                if (strncmp (pst[0], "electronics_rack_type", 21) == 0
                 && strncmp (pst[2], "K4-2/M4", 7) == 0)
                     {
                     pchar = strstr (line, "K4-2/M4");
                     strcpy (pchar, "K4;\n");
                     }
                break;
                                    
            case EOP:               // add dummy eop def
                if (strncmp (pst[0], "$EOP", 4) == 0)
                    strcat (line, "  def EOPXXX;\n  enddef;\n");
                break;

            case EXPER:             // modify target_correlator
                if (strcmp (pst[0], "target_correlator") == 0)
                    strcpy (line, "target_correlator = difx;\n");
                break;

            case FREQ:              // insert channel names in chan_def stmts
                if (strncmp (pst[0], "chan_def", 8) == 0)
                    {
                    sprintf (buff, "C%02dU :", numchan++);
                    if (*pst[5] == 'L')
                        buff[3] = 'L';
                    strcat (buff, strchr (line, '=') + 1);
                                    // chop off line just after = sign
                    *(strchr (line, '=')+2) = 0;
                    strcat (line, buff); 
                    }
                break;
                                    
            case GLOBAL:
                if (strcmp (pst[0], "ref") == 0
                 && strcmp (pst[1], "$SCHEDULING_PARAMS") == 0)
                    strcpy (line, "    ref $EOP = EOPXXX;\n"); 
                break;

            case NO_BLOCK:          // insert ovex revision number
                if (strncmp (pst[0], "VEX_rev", 7) == 0)
                    strcpy (line, "$OVEX_REV;\nrev = 1.5;\n");
                break;

            case SCHED:             // insert fourfit reference time into sched block
                                    // skip line copy if we're within the wrong scan
                if (strcmp (current_scan, opts->scan) 
                 && strncmp (pst[0], "$SCHED", 6) != 0)
                    line[0] = 0;    
                                    // correct scan, insert one copy of ff ref time
                else if (strncmp (pst[0], "scan", 4) == 0)
                    {
                    conv2date (D->scan->mjdStart + D->scan->durSeconds / 172800.0, &caltime);
                    sprintf (buff, 
                             "    fourfit_reftime = %04hdy%03hdd%02hdh%02hdm%02ds;\n",
                             caltime.year, caltime.day, 
                             caltime.hour, caltime.minute, (int) caltime.second);
                    strcat (line, buff);
                    }
                break;

            case SCHEDULING_PARAMS: // the scheduling_params block is deleted
                line[0] = 0;
                break;

            case SITE:              // need to insert the single char site ID
                                    // remember the site ID for later
                if (strncmp (pst[0], "site_ID", 7) == 0)
                    memcpy (current_site, pst[2], 2);
                
                                    // save site position for later
                else if (strncmp (pst[0], "site_position", 13) == 0)
                    for (i=0; i<3; i++)
                        sscanf (pst[2*i+2], "%lf", &x[i]);

                                    // if this site is in the correlation, it is
                                    // now time to insert the site ID
                else if (strncmp (pst[0], "enddef", 6) == 0)
                    {
                    c = single_code (current_site);
                    if (c == 0)
                        {
                        fprintf (stderr, "All 52 codes used up, no code for %c%c\n",
                                 current_site[0], current_site[1]);
                        return (-1);
                        }
                    sprintf (buff, "  mk4_site_ID = %c;\n", c);
                    strcat (buff, line);
                    strcpy (line, buff);
                                    // save 2-letter code for later use
                    memcpy ((stns+nsite)->intl_name, current_site, 2);
                                    // find difx name by comparing position vectors
                    memcpy ((stns+nsite)->difx_name, "--", 2);
                    
                    for (i=0; i<D->nAntenna; i++)
                        if (fabs((D->antenna+i)->X - x[0]) < 0.001
                         && fabs((D->antenna+i)->Y - x[1]) < 0.001
                         && fabs((D->antenna+i)->Z - x[2]) < 0.001)
                            {
                            memcpy ((stns+nsite)->difx_name, 
                                   (D->antenna+i)->name, 2);
                            break;
                            }
                                    // keep track of difx index for this site
                    (stns+nsite)->dind = i;
                                    // file single letter code for future use
                    (stns+nsite)->mk4_id = c;
                    
                    if (opts->verbose > 0)
                        fprintf (stderr, "intl_name %c%c difx_name %c%c mk4_id %c\n",
                           *((stns+nsite)->intl_name), *((stns+nsite)->intl_name+1),
                           *((stns+nsite)->difx_name), *((stns+nsite)->difx_name+1),
                           (stns+nsite)->mk4_id);
                    nsite++;
                    (stns+nsite)->mk4_id = 0; // null-terminate list 
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
                break;

                                    // nothing special needs to be done for these blocks
            case BBC:
            case IF:
            case HEAD_POS:
            case MODE:
            case PASS_ORDER:
            case PHASE_CAL_DETECT:
            case ROLL:
            default:
                break;
            }
        fputs (line, fout); 
                                    // detect exit of def block and clear current def
        if (strncmp (pst[0], "enddef", 6) == 0)
            current_def[0] = 0;
        }
                                    // append extra statements to the end of the file
        strcpy (line, "$CLOCK;\n");
        fputs (line, fout); 
        
                                    // establish link between difx numbering and
                                    // order within the stns array, and also
                                    // put out clock statements
        n = 0;
        while ((stns+n)->mk4_id != 0 && n < MAX_STN)
            {
            k = (stns+n)->dind;     // index into difx arrays for this station #n
            if (strncmp ((stns+n)->difx_name, "--", 2) != 0)
                {
                if (opts->verbose > 0)
                    fprintf (stderr, "n %d k %d mk4_id %c difx_name %c%c\n",
                             n, k, (stns+n)->mk4_id, (stns+n)->difx_name[0], 
                             (stns+n)->difx_name[1]);
                conv2date ((D->antenna + k) -> clockrefmjd, &caltime);
                }
            else                    // this station not used, make placeholder clock entry
                conv2date (54321.0, &caltime);

            sprintf (line, " def %c%c; clock_early = %04hdy%03hdd%02hdh%02hdm%02ds :"
               "%6.3lf usec : %04hdy%03hdd%02hdh%02hdm%02ds : %le ; enddef;\n", 
                (stns + n)->intl_name[0], (stns + n)->intl_name[1], 
                caltime.year, caltime.day, caltime.hour,
                caltime.minute, (int)caltime.second, (D->antenna+k)->clockcoeff[0],
                caltime.year, caltime.day, caltime.hour,
                caltime.minute, (int)caltime.second, 1e-6 * (D->antenna+k)->clockcoeff[1]);
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
