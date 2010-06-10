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
    fprintf (stderr, "Usage : %s -s <scan_id> [options] <baseFilename1> "
        "[<baseFilename2> ... ] [<outfile>]\n\n", pgm);
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
//  fprintf (stderr, "  --average <nchan>\n");
//  fprintf (stderr, "  -a        <nchan>   Average <nchan> channels\n");
//  fprintf (stderr, "\n");
//  fprintf (stderr, "  --beginchan <chan>\n");
//  fprintf (stderr, "  -b          <chan>  Skip <chan> correlated channels\n");
//  fprintf (stderr, "\n");
//  fprintf (stderr, "  --difx\n");
//  fprintf (stderr, "   -d                 Run on all .difx files in directory\n");
//  fprintf (stderr, "\n");
//  fprintf (stderr, "  --no-model\n");
//  fprintf (stderr, "  -n                  Don't write model (ML) table\n");
//  fprintf (stderr, "\n");
//  fprintf (stderr, "  --dont-combine\n");
//  fprintf (stderr, "  -1                  Don't combine jobs\n");
//  fprintf (stderr, "\n");
//  fprintf (stderr, "  --outchans <nchan>\n");
//  fprintf (stderr, "  -o         <nchan>  Write <nchan> channels\n");
//  fprintf (stderr, "\n");
//  fprintf (stderr, "  --deltat <deltat>\n");
//  fprintf (stderr, "  -t       <deltat>   Set interval (sec) in printing job matrix\n");
//  fprintf (stderr, "  --keep-order\n");
//  fprintf (stderr, "  -k                  Keep antenna order\n");
//  fprintf (stderr, "\n");
    fprintf (stderr, "  --verbose\n");
    fprintf (stderr, "  -v                  Be verbose.  -v -v for more!\n");
    fprintf (stderr, "\n");
//  fprintf (stderr, "  --override-version  Ignore difx versions\n");
//  fprintf (stderr, "\n");

    return 0;
    }

int main(int argc, char **argv)
    {
    struct CommandLineOptions *opts;
    int nConverted = 0;
    int n, nMark4 = 0;
                                    // function prototypes
    struct CommandLineOptions *parseCommandLine (int, char **);
    void deleteCommandLineOptions (struct CommandLineOptions *);
    int convertMark4 (struct CommandLineOptions *);

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
                                    // ensure that there is a scan option specified
    if (opts->scan == 0)
        {
        fprintf (stderr, "\nERROR - you must specify a scan_id!\n");
        return usage (argv[0]);
        }

    for(;;)
        {
        n = convertMark4 (opts);
        if(n <= 0)
            break;
             
        nConverted += n;
        nMark4++;
        }

    printf ("%d of %d DiFX filesets converted to %d Mark4 filesets\n", nConverted,
        opts->nBaseFile, nMark4);

    if(nConverted != opts->nBaseFile)
        printf ("\n*** Warning -- not all input files converted!\n");

    printf ("\n");
    
    deleteCommandLineOptions (opts);

    return 0;
    }

int convertMark4 (struct CommandLineOptions *opts)
    {
    DifxInput *D, *D1, *D2;
    // struct fitsPrivate outfile;
    char node[256],
         site_ids[50],              // ordered, null-terminated list of side id's
         stn_names[50][2];

    struct stations stns[MAX_STN];

    int i;
    int nConverted = 0;
    const char *difxVersion;
    char *rcode,                    // six-letter timecode suffix
         rootname[256];             // full root filename
    time_t now;
    struct tm *t;

                                    // prototypes
    char *root_id(int, int, int, int, int);
    int createRoot (char *, char *, char *, struct stations *, DifxInput *, 
                    struct CommandLineOptions *, char *);
    int createType1s (DifxInput *, char *, char *, char *, struct stations *,
                      struct CommandLineOptions *, char *);
    int createType3s (DifxInput *, char *, char *, struct stations *,
                      struct CommandLineOptions *, char *);

    difxVersion = getenv ("DIFX_VERSION");
    if(!difxVersion)
        printf ("Warning: env. var. DIFX_VERSION is not set\n");

    D = 0;
    opts->dontCombine = TRUE;       // for now, don't allow file combining

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
             
        // opts->baseFile[i] = 0;
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
        fprintf (stderr, "Data geometry changes during obs, cannot "
            "make into FITS.\n");
        deleteDifxInput(D);
        return 0;
        }

    if(strcmp (D->job->taperFunction, "UNIFORM") != 0)
        {
        fprintf (stderr, "Taper func %s not supported.  "
            "Using UNIFORM.\n", D->job->taperFunction);
        strcpy(D->job->taperFunction, "UNIFORM");
        }

    if(opts->fitsFile)
        strcpy(node, opts->fitsFile);
         
    else
        strcpy (node, ".");         // default to current node??

    if(!opts->pretend)
        {
        if(!opts->keepOrder)
            DifxInputSortAntennas(D, opts->verbose);
             

        if(opts->verbose > 2)
            printDifxInput(D);
             
                                    // generate 6-char rootcode timestamp
        now = time((time_t *)NULL);
        t = localtime(&now);
        rcode = root_id (t->tm_year, t->tm_yday+1,
                         t->tm_hour, t->tm_min, t->tm_sec);

                                    // create root from vex file
        if (createRoot (opts->baseFile[i], node, rcode, stns, D, opts, rootname) < 0)
            {
            deleteDifxInput(D);
            fprintf (stderr, "Could not create root file\n");
            return 0;
            }
                                    // create type1 files for each baseline
        if (createType1s (D, opts->baseFile[i], node, rcode, stns, opts, rootname) < 0)
            {
            deleteDifxInput(D);
            fprintf (stderr, "Could not create type 1 files\n");
            return 0;
            }
                                    // create type3 files for each station
        if (createType3s (D, node, rcode, stns, opts, rootname) < 0)
            {
            deleteDifxInput(D);
            fprintf (stderr, "Could not create type 3 files\n");
            return 0;
            }
/*
        if(fitsWriteOpen(&outfile, node) < 0)
            {
            deleteDifxInput(D);
            fprintf (stderr, "Cannot open output file\n");
            return 0;
            }

        if(DifxInput2FitsTables(D, &outfile, opts) == D)
            {
            printf ("\nConversion successful\n\n");
            }
        
        fitsWriteClose(&outfile);
*/      }

    deleteDifxInput(D);
    opts->baseFile[i] = 0;          // mark this as converted

    return nConverted;
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
        if(opts->fitsFile)
            {
            free(opts->fitsFile);
            }
        free(opts);
        }
    }

struct CommandLineOptions *parseCommandLine(int argc, char **argv)
    {
    struct CommandLineOptions *opts;
    int i, l;
    glob_t globbuf;

    opts = newCommandLineOptions();

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
            else if(i+1 < argc) /* one parameter arguments */
                {
                if(strcmp (argv[i], "--scan") == 0 ||
                   strcmp (argv[i], "-s") == 0)
                    {
                    i++;
                    opts->scan = strdup (argv[i]);
                    printf ("Processing scan %s\n", 
                        opts->scan);
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
                else if(strcmp (argv[i], "--beginchan") == 0 ||
                        strcmp (argv[i], "-b") == 0)
                    {
                    i++;
                    opts->startChan = atof(argv[i]);
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
            l = strlen (argv[i]);
            if(l > 5 && strcasecmp(argv[i]+l-5, ".FITS") == 0)
                {
                opts->fitsFile = strdup(argv[i]);
                }
            else
                {
                opts->baseFile[opts->nBaseFile] = 
                    strdup(argv[i]);
                opts->nBaseFile++;
                }
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

    if(opts->nBaseFile > 0 && opts->dontCombine && opts->fitsFile)
        {
        printf ("Error -- Cannot supply output filename for multiple output files\n");
        deleteCommandLineOptions(opts);
        return 0;
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

    return opts;
    }


