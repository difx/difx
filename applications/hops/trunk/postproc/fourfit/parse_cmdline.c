/*******************************************************************************/
/*                                                                             */
/* This handles everything to do with the command line.  It identifies and     */
/* acts upon UNIX-style flags of various descriptions, then attempts to fill   */
/* in the list of type corel filenames.  The user can specify either explicit  */
/* (possibly wildcarded) corel files directly, or specify 1 or more directories*/
/* which will be recursively searched, one by one, for corel files.  The end   */
/* result is a master list of corel files in the files structure array.        */
/*                                                                             */
/* Original version 9 April 1992 CJL, broken out from fourfit main routine     */
/* Modified February 2 1993 by CJL to use UTIL library routine get_filelist()  */
/* Modified January 1994 by CJL to support various new option flags.           */
/*    "     2006.4.27    by rjc to accommodate differing getopt's in linux/hppa*/
/*                                                                             */
/*******************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include "mk4_data.h"
#include "fstruct.h"
#include "control.h"
#include "refringe.h"
#include "param_struct.h"
#include "pass_struct.h"

#define FALSE 0
#define TRUE 1

//bumping control_filename and afile_name buffers up to 512 chars
//since 100 chars seems a little restrictive, jpb 12/20/16
char display_name[1024];
char control_filename[512];
char *control_string;
char afile_name[512];
int displayopt = FALSE;

parse_cmdline (
int argc,
char **argv,
fstruct **files,
bsgstruct **base_sgrp,
struct type_param *param)
    {
    char c, base_freq[21], *bf_string();
    int i, set, cslen, filenum;
    struct stat file_status;
    extern char *optarg;
    extern int optind, do_only_new, test_mode, plot_xpower,
    		   do_accounting;
    extern int write_xpower;
    extern int refringe, msglev, ap_per_seg, reftime_offset;
    int do_parse = FALSE, 
        bf_override = FALSE, 
        cs_too_big = FALSE;
    char *get_bfstring (char *);
                                        // local prototype
    int parse_polar (char *, short int *);

    strcpy (control_filename, "default");
                                        /* Default polarization */
    param->pol = POL_ALL;
    param->first_plot = 0;
    param->nplot_chans = 0;
                                        /* Interpret command line flags */
    while((c=getopt(argc,argv,"+ab:c:d:f:m:n:pr:s:tuxP:T:X")) != -1) 
        {
        switch(c) 
            {
            case 'a':
                do_accounting = TRUE;
                break;

            case 'b':
                bf_override = TRUE;
                strncpy (base_freq, optarg, sizeof(base_freq)-1);
                base_freq[sizeof(base_freq)-1] = 0;
                break;

            case 'c':
                do_parse = TRUE;
                strncpy (control_filename,optarg,sizeof(control_filename));
                break;

            case 'd':
                displayopt = TRUE;
                strncpy (display_name,optarg,sizeof(display_name));
                break;
            
            case 'f':
                param->first_plot = atoi(optarg);
                break;

            case 'm':
                if (sscanf (optarg, "%d", &msglev) != 1)
                    {
                    msg ("Invalid -m flag argument '%s'", 2, optarg);
                    msg ("Message level remains at %d", 2, msglev);
                    }
                 //1/27/17 jpb, updated for include '4' as a special parameter which only prints the name of the generated fringe file
                if (msglev > 4) msglev = 4;
                if (msglev < -3) msglev = -3;
                break;

            case 'n':
                param->nplot_chans = atoi(optarg);
                break;

            case 'p':
                displayopt = TRUE;
                strcpy (display_name, "psscreen");
                break;

            case 'r':
                refringe = TRUE;
                strncpy (afile_name, optarg, 100);
                break;

            case 's':
                if (sscanf (optarg, "%d", &ap_per_seg) != 1)
                    msg ("Invalid -s flag argument '%s'", 2, optarg);
                else if (ap_per_seg < 0)
                    {
                    msg ("Invalid -s flag argument '%s', ignoring", 2, optarg);
                    ap_per_seg = 0;
                    }
                break;

            case 't':
                test_mode = TRUE;
                break;

            case 'u':
                do_only_new = TRUE;
                break;

            case 'x':
                displayopt = TRUE;
                strcpy (display_name, "xwindow");
                break;

            case 'P':
                if (parse_polar (optarg, &param->pol))
                    msg ("Bad -P argument, doing all sequentially)", 2);
                break;

            case 'T':
                if (sscanf (optarg, "%d", &reftime_offset) != 1) reftime_offset = 0;
                if (reftime_offset <= 0)
                    {
                    msg ("Invalid -T flag argument '%s'", 2, optarg);
                    msg ("Using default reference time algorithm", 2);
                    }
                break;

            case 'X':
                write_xpower = TRUE;
                break;

            case '?':
            default:
                syntax();
                return (1);
            }
        }
                                        /* Check for silly states */
    if (refringe && do_only_new)
        {
        msg ("The -r and -u flags are mutually exclusive.", 2);
        return (1);
        }
                                        /* Read main user control file */
    if (do_parse)
        {
        if(parse_control_file (control_filename) != 0)
            {
            msg ("Fatal error parsing control file '%s'", 3, control_filename);
            return (1);
            }
        }
                                        /* Look for "set" argument and get the */
                                        /* control file text which follows it */
    for (set=optind; set<argc; set++) 
        if (strcasecmp (argv[set], "set") == 0) break;
                                        /* Make space, start with enough for -b option */
    cslen = 65;
    for (i=set+1; i<argc; i++) cslen += strlen (argv[i]) + 1;
    if ((control_string = malloc (cslen)) == NULL)
        {
        msg ("Could not allocate memory for -b and/or set override options", 3);
        return (1);
        }
                                        /* To be appended to control file, apply */
                                        /* to all if block.  parse_control_file()  */
                                        /* keys on this string */
    strcpy (control_string, "if ");
                                        /* Construct control string from command line */
    if (set < argc)
        {
        for (i=set+1; i<argc; i++)
            {
            strcat (control_string, argv[i]);
            strcat (control_string, " ");
            }
        }
                                        /* Decode -b option argument and append */
                                        /* corresponding control file syntax */
    if (bf_override) 
        strcat (control_string, get_bfstring (base_freq));
                                        /* Parse command line override information */
    if (strlen (control_string) > 3)
        {
        if(parse_control_file (control_string) != 0)
            {
            msg ("Fatal error parsing -b option and/or 'set' parameters", 3);
            msg ("Constructed string was '%s'", 3, control_string);
            return (1);
            }
        }
        else
        {
            //no set string was passed but we still need to make note of this in
            //the type_222 record, so allocate a small chunk to store an empty
            //string
            param->set_string_buff = (char*) malloc(2);
            param->set_string_buff[0] = ' ';
            param->set_string_buff[1] = '\0';
        }
                                        /* Make list of roots to process */
                                        /* If refringe mode, also attach list */
                                        /* of baselines/subgroups to each root */
    if (refringe)
        {
        if (refringe_list (afile_name, files, base_sgrp) != 0) 
            {
            msg ("Error figuring out data to refringe", 3);
            return (1);
            }
        }
                                        /* Otherwise retrieve all specified files of */
                                        /* type root (from UTIL library) */
    else if (get_filelist (set-optind, argv+optind, 0, files) != 0)
        {
        msg ("Error extracting list of files to process from command line args", 2);
        syntax();
        return (1);
        }
                                        /* Check to see if any data files */
                                        /* were specified */
    if ((*files)[0].order < 0)
        {
        msg ("No valid root files specified", 2);
        syntax();
        return (1);
        }

    return (0);
    }
                                        // parse polarization string into encoded int
int parse_polar (char *field, short *code)
    {
    int i,
        error,
        kode = -1,
        rc = 1;
    char buff[12];
    if (strcmp (field, "all") == 0)
        kode = POL_ALL;                 // we will do all present polarizations, one at a time
    else if (strcmp (field, "I") == 0)
        kode = POL_IXY;                 // form Stokes I for linear polarization data
    else if (strlen (field) < 12)
        {
        strcpy (buff, field);
                                        // replace all X's & Y's by L's & R's
        error = 0;
        for (i=0; i<strlen(buff); i++)
            if (buff[i] == 'X')
                buff[i] = 'L';
            else if (buff[i] == 'Y')
                buff[i] = 'R';
            else if (buff[i] == '+'
                    && (i==2 || i==5 || i==8))
                ;
            else if (buff[i] == 'L' || buff[i] == 'R')
                ;
            else
                {
                error = 1;
                break;
                }
                                        // only legal lengths are 2, 5, 8, or 11
        if ((strlen (buff) - 2) % 3)
            error = 1;
                                        // no syntax errors, now decode fields
        if (!error)
            {
            kode = 0;
            for (i=0; i<strlen(buff); i+=3)
                {
                if (strncmp (buff+i, "LL", 2) == 0)
                    kode |= POLMASK_LL;
                else if (strncmp (buff+i, "RR", 2) == 0)
                    kode |= POLMASK_RR;
                else if (strncmp (buff+i, "LR", 2) == 0)
                    kode |= POLMASK_LR;
                else if (strncmp (buff+i, "RL", 2) == 0)
                    kode |= POLMASK_RL;
                }
            }
        }

    if (kode != -1)                     // return without error only if kode is valid
        {
        *code = kode;
        rc = 0;
        }
    return rc;
    }
