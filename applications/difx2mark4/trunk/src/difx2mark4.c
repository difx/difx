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
    fprintf (stderr, "\noptions can include:\n");
    fprintf (stderr, "  --help\n");
    fprintf (stderr, "  -h                  Print this help message\n"); 
    fprintf (stderr, "\n");
    fprintf (stderr, "  --verbose\n");
    fprintf (stderr, "  -v                  Be verbose.  -v -v for more!\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "  --difx\n");
    fprintf (stderr, "  -d                  Run on all .difx files in directory\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "  --override-version  Ignore difx versions\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "  --experiment-number\n");
    fprintf (stderr, "  -e                  Set the experiment number (default 1234)\n");
    fprintf (stderr, "                      Must be a four-digit number\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "  --pretend\n");
    fprintf (stderr, "  -p                  dry run\n");
    fprintf (stderr, "\n");

    return 0;
    }

                                    // global table of frequency bands
struct fbands fband[MAX_FBANDS] = {{'B',      0.0, 999999.9},  // default to band B
                                   {'I',    100.0,    150.0},
                                   {'G',    150.0,    225.0},
                                   {'P',    225.0,    390.0},
                                   {'L',    390.0,   1550.0},
                                   {'S',   1550.0,   3900.0},
                                   {'C',   3900.0,   6200.0},
                                   {'X',   6200.0,  10900.0},
                                   {'K',  10900.0,  36000.0},
                                   {'Q',  36000.0,  46000.0},
                                   {'V',  46000.0,  56000.0},
                                   {'W',  56000.0, 100000.0},
                                   {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, 
                                   {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}};
                                   
  
int newScan(DifxInput *,  struct CommandLineOptions*, char *, int, int *, FILE **, char *);

int main(int argc, char **argv)
    {
    struct CommandLineOptions *opts;
    int nConverted = 0;
    int n, nScan = 0;
                                    // function prototypes
    struct CommandLineOptions *parseCommandLine (int, char **);
    void deleteCommandLineOptions (struct CommandLineOptions *);
    int convertMark4 (struct CommandLineOptions *, int *);

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
        n = convertMark4(opts, &nScan);
        if(n <= 0)
            break;
        nConverted += n;
        }

    printf ("%d of %d DiFX filesets converted to %d Mark4 filesets\n", nConverted,
        opts->nBaseFile, nScan);

    if(nConverted != opts->nBaseFile)
        printf ("\n*** Warning -- not all input files converted!\n");

    printf ("\n");
    
    deleteCommandLineOptions (opts);

    return 0;
    }

int convertMark4 (struct CommandLineOptions *opts, int *nScan)
    {
    DifxInput *D, *D1, *D2;
    // struct fitsPrivate outfile;
    char node[DIFXIO_FILENAME_LENGTH*2+1];
    int i, scanId, newScanId, oldJobId, err;
    int jobId = 0;
    int nConverted = 0;
    const char *difxVersion;
    char corrdate[16];
    FILE *vis_file = 0;
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
            return 0;
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
               !areDifxInputsCompatible(D1, D2))
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
                return 0;
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
        return 0;
         
    if(opts->verbose > 2)
        printDifxInput(D);

    D = updateDifxInput(D);

    if(!D)
        {
        fprintf (stderr, "updateDifxInput failed.  Aborting\n");
        return 0;
        }

    if(difxVersion && D->job->difxVersion[0])
        {
        if(strncmp(difxVersion, D->job->difxVersion, 63))
            {
            fprintf (stderr, "Attempting to run difx2mark4 from version %s on a job make for version %s\n", difxVersion, D->job->difxVersion);
            if(opts->overrideVersion)
                {
                fprintf (stderr, "Continuing because of --override-version but not setting a version\n");
                D->job->difxVersion[0] = 0;
                }
            else
                {
                fprintf (stderr, "Not converting.\n");
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
        deleteDifxInput(D);
        return 0;
        }

    if(strcmp (D->job->taperFunction, "UNIFORM") != 0)
        {
        fprintf (stderr, "Taper func %s not supported.  "
            "Using UNIFORM.\n", D->job->taperFunction);
        strcpy(D->job->taperFunction, "UNIFORM");
        }



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
                    return 0;
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
                return -1;
                }
            printf("  Processing scan %d/%d: %s\n", scanId, D->nScan, D->scan[scanId].identifier);
                                                // convert scan
                                                // scanId and j can be incremented
                                                // by newScan
            newScanId = newScan(D, opts, node, scanId, &jobId, &vis_file, corrdate);
            //printf ("newScan file pointer %x\n", vis_file);
            if(newScanId < 0)
                break;
            else
                scanId = newScanId;
            *nScan += 1;
            }
        printf("%d of %d scans converted!\n", *nScan, D->nScan);
        deleteDifxInput(D);
        return jobId;
        }
    return 0;
    }
    
int newScan(DifxInput *D, struct CommandLineOptions *opts, char *node, int scanId, int *jobId, FILE **vis_file, char *corrdate)
{
    int startJobId,
        nextScanId,
        i,
        err;
    time_t now;
    struct tm *t;
    struct stat stat_s;
    char *rcode,                    // six-letter timecode suffix
         rootname[DIFXIO_NAME_LENGTH],             // full root filename
         path[DIFXIO_NAME_LENGTH+5];

    struct stations stns[D->nAntenna];

                                // make scan directory
    snprintf(path, DIFXIO_NAME_LENGTH, "%s/%s", node, D->scan[scanId].identifier);
    //strncat(node, "/", DIFXIO_NAME_LENGTH);
    //strncat(node, D->scan[scanId].identifier, DIFXIO_NAME_LENGTH);
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
    
                                // generate 6-char rootcode timestamp
    now = time((time_t *)NULL);
    t = localtime(&now);
    rcode = root_id (t->tm_year, t->tm_yday+1,
                     t->tm_hour, t->tm_min, t->tm_sec);
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
    if (createRoot (D, jobId, scanId, path, rcode, stns, opts, rootname) < 0)
        {
        fprintf (stderr, "Could not create root file\n");
        return -1;
        }
                                // create type1 files for each baseline
    printf ("    Generating Type 1s\n");
    nextScanId = createType1s (D, jobId, scanId, path, rcode, stns, opts, rootname, vis_file, corrdate);
    //printf ("newScan file pointer %x\n", *vis_file);
    if (nextScanId < 0)
        {
        fprintf (stderr, "Could not create type 1 files\n");
        return -1;
        }

                                // create type3 files for each station
    printf ("    Generating Type 3s\n");
    if (createType3s (D, startJobId, *jobId, scanId, path, rcode, stns, opts) < 0)
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
    
    opts->writemodel = 1;
    opts->sniffTime = 30.0;
    opts->jobMatrixDeltaT = 20.0;
    opts->phaseCentre = 0;

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
            if(strcmp (argv[i], "--no-model") == 0 ||
               strcmp (argv[i], "-n") == 0)
                {
                opts->writemodel = 0;
                }
            else if(strcmp (argv[i], "--quiet") == 0 ||
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
            else if(strcmp (argv[i], "--dont-sniff") == 0 ||
                strcmp (argv[i], "-x") == 0)
                {
                opts->sniffTime = -1.0;
                }
            else if(strcmp (argv[i], "--dont-combine") == 0 ||
                    strcmp (argv[i], "-1") == 0)
                {
                opts->dontCombine = 1;
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
