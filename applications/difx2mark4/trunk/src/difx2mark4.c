/***************************************************************************
 *   Copyright (C) 2010 by Roger Cappallo                                  *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <glob.h>
#include <limits.h>
#include <sys/stat.h>
#include <time.h>
#include "difx2mark4.h"
#include "../config.h"

const char program[] = PACKAGE_NAME;
const char author[]  = PACKAGE_BUGREPORT;
const char version[] = VERSION;

static int usage (const char *pgm)
    {
    fprintf (stderr, "\n%s ver. %s   %s\n\n",
        program, version, author);
    fprintf (stderr, "A program to convert DiFX format data to "
        "mark4\n\n");
    fprintf (stderr, "Usage : %s [options] <baseFilename1> "
        "[<baseFilename2> ... ] \n\n", pgm);
    fprintf (stderr, "It assumed that SWIN format visibility file(s) "
        "to be converted live\n");
    fprintf (stderr, "in directory <baseFilename>.difx/\n");
    fprintf (stderr, "\nIt is also assumed that the following 3 additional "
        "files exist:\n");
    fprintf (stderr, "  <baseFilename>.input    DiFX input file\n");
    fprintf (stderr, "  <baseFilename>.im       Polynomial model and UVW\n");
    fprintf (stderr, "  <expFilename>.vex       Vex file for this expt.\n");
    fprintf (stderr, "where <expFilename> is <baseFilename w/o _<#> suffix\n");
        
    fprintf (stderr, "\nThe output fileset <outfile> will be written in "
        "mark4 format similar\n");
    fprintf (stderr, "to that created by mark4 HW correlators.\n");
    fprintf (stderr, "\nAvailable options are:\n");
    fprintf (stderr, "  -h or --help              Print this help message\n\n"); 
    fprintf (stderr, "  -v or --verbose           Be verbose.  -v -v for more!\n\n");
    fprintf (stderr, "  -d or --difx              Run on all .difx files in directory\n\n");
    fprintf (stderr, "  --override-version        Ignore difx versions\n\n");
    fprintf (stderr, "  -e or --experiment-number Set the experiment number (default 1234)\n");
    fprintf (stderr, "                            Must be a four-digit number\n\n");
    fprintf (stderr, "  -k or --keep-order        don't sort antenna order\n\n");
    fprintf (stderr, "  -r or --raw               use raw mode - suppresses normalization\n\n");
    fprintf (stderr, "  -p or --pretend           dry run\n\n");
    fprintf (stderr, "  -b <code> <flo> <fhi>     Override freq band codes\n");
    fprintf (stderr, "                            (can have multiple triplets)\n");
    fprintf (stderr, "  -s or --scode <file>      Specify new VEX to mk4 station code mappings\n");
    fprintf (stderr, "                            via a file with lines of the form:   X Xx\n");
    fprintf (stderr, "\n");

    return 0;
    }

                                    // global table of frequency bands
struct fbands fband[MAX_FBANDS] = {{'B',      0.0, 999999.9},  // default to band B
                                   {'I',    100.0,    150.0},
                                   {'G',    150.0,    225.0},
                                   {'P',    225.0,    390.0},
                                   {'L',    390.0,   1750.0}, // ITU: 1400-1722.2, EVN/arrays: 1150-1750
                                   {'S',   1750.0,   3900.0},
                                   {'C',   3900.0,   6200.0},
                                   {'X',   6200.0,  10900.0},
                                   {'K',  10900.0,  36000.0},
                                   {'Q',  36000.0,  46000.0},
                                   {'V',  46000.0,  56000.0},
                                   {'W',  56000.0, 100000.0},
                                   {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, 
                                   {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}};
                                   
  
int newScan(DifxInput *,  struct CommandLineOptions*, char *, int, int *);

int main(int argc, char **argv)
    {
    struct CommandLineOptions *opts;
    int nConverted = 0;
    int n, nScan = 0, nScanTot = 0;
                                    // function prototypes
    struct CommandLineOptions *parseCommandLine (int, char **);
    void deleteCommandLineOptions (struct CommandLineOptions *);
    int convertMark4 (struct CommandLineOptions *, int *, int *);

    if(argc < 2)
        {
        return usage (argv[0]);
        }

    if(getenv("DIFX_GROUP_ID"))
        {
        umask(2);
        }

    opts = parseCommandLine (argc, argv);
    if(opts == 0)
        return 0;

    /* merge as many jobs as possible and process */
    for(;;)
        {
        n = convertMark4(opts, &nScan, &nScanTot);
        if (n > 0)
            nConverted += n;
        else
            break;                  // all done, exit loop
        }

    printf ("%d of %d DiFX filesets converted to %d Mark4 filesets\n", nConverted,
        opts->nBaseFile, nScan);

    if(nConverted != opts->nBaseFile)
        printf ("\n*** Warning -- not all input files converted!\n");

    printf ("\n");
    
    deleteCommandLineOptions (opts);

    return 0;
    }

int convertMark4 (struct CommandLineOptions *opts, int *nScan, int *nScanTot)
    {
    DifxInput *D, *D1, *D2;
    // struct fitsPrivate outfile;
    char node[DIFXIO_FILENAME_LENGTH*2+1];
    int i, scanId, newScanId, oldJobId, err;
    int jobId = 0;
    int nConverted = 0;
    const char *difxVersion;
    struct stat stat_s;

    difxVersion = getenv ("DIFX_VERSION");
    if(!difxVersion)
        printf ("Warning: env. var. DIFX_VERSION is not set\n");

    D = 0;

    for(i = 0; i < opts->nBaseFile; i++)
        {
        if(opts->baseFile[i] == 0)
            continue;               // skip this one, it's already done

        if(opts->verbose > 1)
            printf ("Loading %s\n", opts->baseFile[i]);
             
        D2 = loadDifxInput(opts->baseFile[i]);
        if(!D2)
            {
            fprintf (stderr, "loadDifxInput failed on <%s>.\n",
                opts->baseFile[i]);
            opts->baseFile[i] = 0;
            continue; 
            }
        if(opts->specAvg)
            D2->specAvg = opts->specAvg;
             
        if(opts->nOutChan >= 1)
            D2->nOutChan = opts->nOutChan;
             
        else if(opts->nOutChan > 0.0) /* interpret in fractional sense */
            D2->nOutChan = D2->freq[0].nChan*opts->nOutChan/D->freq[0].specAvg;
             
        if(opts->startChan >= 1)
            D2->startChan = opts->startChan;
             
        else if(opts->startChan > 0.0)
            D2->startChan = (D2->freq[0].nChan*opts->startChan) + 0.5;
             

        if(D)
            {
            D1 = D;

            if(!areDifxInputsMergable(D1, D2) ||
               !areDifxInputsCompatible(D1, D2, FreqMergeModeStrict))
                {
                deleteDifxInput(D2);
                continue;
                }
            else if(opts->verbose > 1)
                {
                printf ("Merging %s\n", opts->baseFile[i]);
                }

            D = mergeDifxInputs(D1, D2, opts->verbose);

            deleteDifxInput(D1);
            deleteDifxInput(D2);

            if(!D)
                {
                fprintf (stderr, "Merging failed on <%s>.\n",
                    opts->baseFile[i]);
                opts->baseFile[i] = 0;
                continue; 
                }
            }
        else
            D = D2;
             
        opts->baseFile[i] = 0;
        nConverted++;
        if(opts->dontCombine)
            break;
             
        }
    if(!D)
        return 0;                   // no more work; force quit
         
    if(opts->verbose > 2)
        printDifxInput(D);

    D = updateDifxInput(D);

    if(!D)
        {
        printf ("updateDifxInput failed.  Aborting\n");
        opts->baseFile[i] = 0;
        return 0; 
        }

    if(difxVersion && D->job->difxVersion[0])
        {
        if(strncmp(difxVersion, D->job->difxVersion, 63))
            {
            fprintf (stderr, "Attempting to run difx2mark4, version %s,\n"
                             " on a job processed with version %s\n", 
                     difxVersion, D->job->difxVersion);
            if(opts->overrideVersion)
                {
                fprintf (stderr, " Continuing because of --override-version\n"
                                 "  but not setting a version.\n");
                D->job->difxVersion[0] = 0;
                }
            else
                {
                fprintf (stderr, " Not converting:  use --override-version\n"
                                 "  if you are sure it is safe to proceed.\n");
                deleteDifxInput(D);
                return 0; 
                }
            }
        }
    else if(!D->job->difxVersion[0])
        fprintf (stderr, "Warning -- working on unversioned job\n");

    if(opts->verbose > 1)
        printDifxInput(D);

    if(D->nIF <= 0 || D->nPolar <= 0)
        {
        fprintf (stderr, "Data geometry changes during obs\n");
        return 0; 
        }
                                // add in this batch of scans to total
    *nScanTot += D->nScan;


    if(!opts->pretend)
        {
                                 // this is not a drill, start conversion
        err = stat(opts->exp_no, &stat_s);
                                 // err true if file doesn't exist
        if(!err)
            {
            if(S_ISDIR(stat_s.st_mode))
                {
                                 // If directory already exists, use it!
                if(opts->verbose > 1)
                    printf ("Using existing directory %s\n", opts->exp_no);
                }
            else
                err = 1;
            }
        else if(err)
            {
                                // Otherwise create directory
                if(mkdir(opts->exp_no, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH))
                    {
                    fprintf (stderr, "Error creating output directory %s\n", opts->exp_no);
                    }
            }
        strcpy (node, opts->exp_no);

        if(!opts->keepOrder)
            DifxInputSortAntennas(D, opts->verbose);
             

        if(opts->verbose > 2)
            printDifxInput(D);

        scanId=0;
        oldJobId = -2;
        
        printf("\n");
        while (jobId < D->nJob)
            {
            if(oldJobId != jobId)
                {
                printf("Processing job %d/%d\n", jobId, D->nJob);
                oldJobId = jobId;
                }
            if(D->scan[scanId].identifier[0] == 0)
                {
                fprintf(stderr, "Developer Error (difxio) scanId %d has no scan identifier!\n", scanId);
                scanId++;
                return 0; 
                }
            printf("  Processing scan %d/%d: %s\n", scanId, D->nScan, D->scan[scanId].identifier);
                                                // convert scan
                                                // scanId and jobId can be incremented
                                                // by newScan
            newScanId = newScan(D, opts, node, scanId, &jobId);
            if(newScanId < 0)
                {
                printf ("error detected, attempting to proceed\n");
                jobId++;
                }
            else
                {
                *nScan += 1;
                if (*nScan > *nScanTot)
                    {
                    printf ("Error Detected! Output has more scans (%d) than exist (%d)!\n",
                            *nScan, *nScanTot);
                    return 0;
                    }
                scanId = newScanId;
                }
            }
        printf("%d of %d scans converted!\n", *nScan, D->nScan);
        deleteDifxInput(D);
        return jobId;
        }
    return 0;
    }
    
int newScan(DifxInput *D, struct CommandLineOptions *opts, char *node, int scanId, int *jobId)
{
    int startJobId,
        endJobId,
        nextScanId,
        i,
        err;
    static int first = TRUE, 
               year, day, hour, min, sec;
    time_t now;
    struct tm *t;
    struct stat stat_s;
    char *rcode,                    // six-letter timecode suffix
         rootname[DIFXIO_FILENAME_LENGTH],             // full root filename
         path[DIFXIO_FILENAME_LENGTH+5];

    struct stations stns[D->nAntenna];
    struct fblock_tag fblock[MAX_FPPAIRS];

    if (first)                  // on first time through get and save time
        {
        now = time ((time_t *) NULL);
        t = localtime (&now);
        year = t->tm_year;
        day = t->tm_yday+1;
        hour = t->tm_hour;
        min = t->tm_min;
        sec = t->tm_sec;
        first = FALSE;
        }
    else
        sec += 4;               // avoid collisions by moving to next code (4s later)

                                // generate 6-char rootcode timestamp
    rcode = root_id (year, day, hour, min, sec);
                 
                                // make scan directory
    snprintf(path, DIFXIO_FILENAME_LENGTH, "%s/%s", node, D->scan[scanId].identifier);
    err = stat(path, &stat_s);
    if(!err)
        {
        if(S_ISDIR(stat_s.st_mode))
            {
                             // If directory already exists, use it!
            if(opts->verbose > 1)
                printf ("Using existing directory %s\n", path);
            }
        else
            err = 1;
            }
    else if(err)
        {
                                // Otherwise create directory
        if(mkdir(path, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH))
            {
                fprintf (stderr, "Error creating output directory %s\n", path);
                return 0;
            }
        }
    
                                // initialise stns with all DiFX antennas
    for (i = 0; i < D->nAntenna; i++)
        {
        stns[i].inscan = 0;
        stns[i].invis = 0;
        stns[i].mk4_id = 0;
        strncpy (stns[i].difx_name, D->antenna[i].name, 2);
        }
                                // create root from vex file
    startJobId = *jobId;
    printf ("    Generating root file\n");
    if (createRoot (D, fblock, startJobId, scanId, path, rcode, stns, opts, rootname) < 0)
        {
        fprintf (stderr, "Could not create root file\n");
        return -1;
        }
                                // create type1 files for each baseline
    printf ("    Generating Type 1s\n");
    nextScanId = createType1s (D, fblock, jobId, scanId, path, rcode, stns, opts, rootname);
    if (nextScanId < 0)
        {
        fprintf (stderr, "Could not create type 1 files\n");
        return -1;
        }

                                // create type3 files for each station
    printf ("    Generating Type 3s\n");
    endJobId = (*jobId >= D->nJob) ? D->nJob - 1 : *jobId;
    if (createType3s (D, fblock, startJobId, endJobId, scanId, path, rcode, stns, opts) < 0)
        {
        fprintf (stderr, "Could not create type 3 files\n");
        return -1;
        }
    return(nextScanId);
}

struct CommandLineOptions *newCommandLineOptions()
    {
    struct CommandLineOptions *opts;

    opts = (struct CommandLineOptions *)calloc(1, 
        sizeof(struct CommandLineOptions));
    
    opts->sniffTime = 30.0;
    opts->jobMatrixDeltaT = 20.0;
    opts->phaseCentre = 0;
    opts->raw = 0;

    return opts;
    }

void deleteCommandLineOptions(struct CommandLineOptions *opts)
    {
    int i;

    if(opts)
        {
        if(opts->nBaseFile > 0)
            {
            for(i = 0; i < opts->nBaseFile; i++)
                {
                free(opts->baseFile[i]);
                }
            }
        free(opts->in);
        free(opts);
        }
    }

struct CommandLineOptions *parseCommandLine(int argc, char **argv)
    {
    struct CommandLineOptions *opts;
    int i, j, l;
    glob_t globbuf;

    opts = newCommandLineOptions();
                    // set default experiment number
    strcpy(opts->exp_no, "1234");

    for(i = 1; i < argc; i++)
        {
        if(argv[i][0] == '-')
            {
            if(strcmp (argv[i], "--quiet") == 0 ||
                    strcmp (argv[i], "-q") == 0)
                {
                opts->verbose--;
                }
            else if(strcmp (argv[i], "--difx") == 0 ||
                    strcmp (argv[i], "-d") == 0)
                {
                opts->doalldifx++;
                }
            else if(strcmp (argv[i], "--verbose") == 0 ||
                    strcmp (argv[i], "-v") == 0)
                {
                opts->verbose++;
                }
            else if(strcmp (argv[i], "--raw") == 0 ||
                strcmp (argv[i], "-r") == 0)
                {
                opts->raw = 1;
                }
            else if(strcmp (argv[i], "--pretend") == 0 ||
                    strcmp (argv[i], "-p") == 0)
                {
                opts->pretend = 1;
                }
            else if(strcmp (argv[i], "--help") == 0 ||
                    strcmp (argv[i], "-h") == 0)
                {
                usage (program);
                deleteCommandLineOptions(opts);
                return 0;
                }
            else if (strcmp (argv[i], "--keep-order") == 0 ||
                 strcmp (argv[i], "-k") == 0)
                {
                opts->keepOrder = 1;
                }
            else if (strcmp (argv[i], "--override-version") == 0)
                {
                opts->overrideVersion = 1;
                }
            else if (argv[i][1] == 'b')
                {                   // frequency band override
                if (argc < i + 2)
                    {
                    usage (program);
                    deleteCommandLineOptions(opts);
                    return 0;
                    }
               for (l=0; l<MAX_FBANDS; l++)
                   {
                   if (fband[l].code)
                       continue;
                   if (l == MAX_FBANDS)
                       {
                       fprintf (stderr, "too many freq bands\n");
                       deleteCommandLineOptions(opts);
                       return 0;
                       }
                   fband[l].code = argv[++i][0];
                   fband[l].flo = atof (argv[++i]);
                   fband[l].fhi = atof (argv[++i]);
                   break;
                   }
                }
            else if(i+1 < argc) /* one parameter arguments */
                {
                if (strcmp (argv[i], "--experiment-no") == 0 ||
                     strcmp (argv[i], "-e") == 0)
                    {
                    i++;
                    if(strlen(argv[i]) != 4)
                        {
                        printf("Warning: experiment number must be 4 digits.  Keeping default '1234'\n");
                        continue;
                        }
                            
                        for(j=0; j<4; j++)
                            {
                            if(!isdigit(argv[i][j]))
                                {
                                printf("Warning: experiment number must be numeric.  Keeping default '1234'\n");
                                //FIXME
                                j=0;
                                break;
                                }
                            }
                        if(j==4)
                            strncpy(opts->exp_no, argv[i], 4+1);
                        }
                else if(strcmp (argv[i], "--deltat") == 0 ||
                    strcmp (argv[i], "-t") == 0)
                    {
                    i++;
                    opts->jobMatrixDeltaT = atof(argv[i]);
                    }
                else if(strcmp (argv[i], "--average") == 0 ||
                        strcmp (argv[i], "-a") == 0)
                    {
                    i++;
                    opts->specAvg = atoi(argv[i]);
                    }
                else if(strcmp (argv[i], "--bin") == 0 ||
                    strcmp (argv[i], "-B") == 0)
                    {
                    i++;
                    opts->pulsarBin = atoi(argv[i]);
                    }
                else if(strcmp (argv[i], "--outchans") == 0 ||
                        strcmp (argv[i], "-o") == 0)
                    {
                    i++;
                    opts->nOutChan = atof(argv[i]);
                    }
                else if(strcmp (argv[i], "--scode") == 0 ||
                        strcmp (argv[i], "-s") == 0)
                    {
                    i++;
                    strncpy(opts->scodeFile, argv[i], PATH_MAX-1);
                    }
                else
                    {
                    printf ("Unknown param %s\n", argv[i]);
                    deleteCommandLineOptions(opts);
                    return 0;
                    }
                }
            else
                {
                printf ("Unknown param %s\n", argv[i]);
                deleteCommandLineOptions(opts);
                return 0;
                }
            }
        else
            {
            if(opts->nBaseFile >= MAX_INPUT_FILES)
                {
                printf ("Error -- too many input files!\n");
                printf ("Max = %d\n", MAX_INPUT_FILES);
                deleteCommandLineOptions(opts);
                return 0;
                }
            opts->baseFile[opts->nBaseFile] = 
                strdup(argv[i]);
            opts->nBaseFile++;
            }
        }

    if((opts->nBaseFile >  0 && opts->doalldifx >  0) ||
       (opts->nBaseFile == 0 && opts->doalldifx == 0))
        {
        deleteCommandLineOptions(opts);
        return 0;
        }

    if(opts->doalldifx)
        {
        glob("*.difx", 0, 0, &globbuf);
        if(globbuf.gl_pathc > MAX_INPUT_FILES)
            {
            printf ("Error -- too many input files!\n");
            printf ("Max = %d\n", MAX_INPUT_FILES);
            deleteCommandLineOptions(opts);
            return 0;
            }
        opts->nBaseFile = globbuf.gl_pathc;
        for(i = 0; i < opts->nBaseFile; i++)
            {
            opts->baseFile[i] = strdup(globbuf.gl_pathv[i]);
            }
        globfree(&globbuf);
        }

    /* if input file ends in .difx, trim it */
    for(i = 0; i < opts->nBaseFile; i++)
        {
        l = strlen (opts->baseFile[i]);
        if(l < 6)
            {
            continue;
            }
        if(strcmp (opts->baseFile[i]+l-5, ".difx") == 0)
            {
            opts->baseFile[i][l-5] = 0;
            }
        }
    opts->in = malloc(opts->nBaseFile*sizeof(FILE*));
    return opts;
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
