// get_vis opens and read one complete visibility file in Swinburne format 
//
//
//  first created                                       rjc  2010.3.11
//  modified to open and read whole Swinburne file      rjc  2011.5.18

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <difxio/difx_input.h>
#include <errno.h>
#include "difx2mark4.h"
#include "difxio/parsevis.h"
                                    // visibility allocation chunk size
#define CHUNK 1000

int get_vis (char *vf_name,                   // name of input file
             struct CommandLineOptions *opts, // ptr to input options
             int nvis,                        // number of vis per record
             int vrsize,                      // size of actual vis records in bytes
             vis_record **vrec,               // ptr to malloced array of vis. recs as read
             int *nvrec,                      // number of vis. records read
             char *corrdate)                  // modification date of input file
    {
    int err,
        vfile_status;

    FILE *vfile;
    struct tm *mod_time;
    struct stat attrib;
    vis_record *pv;                 // convenience pointers
    char *pch;

    vfile = fopen (vf_name, "r");
    if (vfile == NULL)
        {
        perror (vf_name);
        fprintf (stderr, "fatal error opening input data file %s\n", vf_name);
        return (-1);
        }

    printf ("      opened input file %s\n", vf_name);
    err = stat (vf_name, &attrib);
    if (err)
        {
        fprintf (stderr, "Warning: error stating file %s\n", vf_name);
        fprintf (stderr, "         t000.date will be set to 2000001-000000\n");
        sprintf (corrdate, "2000001-000000");
        }
    else
        {
        mod_time = gmtime (&(attrib.st_mtime));
        snprintf (corrdate, 16, "%4d%03d-%02d%02d%02d", 
                 mod_time->tm_year+1900,
                 mod_time->tm_yday+1, mod_time->tm_hour,
                 mod_time->tm_min,  mod_time->tm_sec);
        }

    *vrec = malloc (CHUNK * vrsize);
    pv = *vrec;
    pch = (char *) pv;
    *nvrec = 0;
                                    // loop over all records in file
    while (TRUE)
        {
                                    // read a header from the input file
                                    // first read sync word to identify header version
        vfile_status = fread (&pv->sync, sizeof (int), 1, vfile);
        if (vfile_status != 1)
            {
            if (feof (vfile))
                {
                                    //EOF in .difx file
                if (opts->verbose > 0)
                    printf ("        EOF in input file\n");
                fclose (vfile);
                return -1;
                }
            else 
                {
                fprintf (stderr, "unreadable input file %s status %d\n",
                         vf_name, vfile_status);
                                    //unreadable .difx file
                fclose (vfile);
                return -2;
                }
            }

        if (pv->sync == VISRECORD_SYNC_WORD_DIFX1) //old style ascii header
            {
            fprintf(stderr, "Error: difx2mark4 will not work with DiFX 1.x data\n");
            return -3;
            }
        else if (pv->sync == VISRECORD_SYNC_WORD_DIFX2) //new style binary header
            {
            fread (&pv->version, sizeof (int), 1, vfile);
            if(pv->version == 1) //new style binary header
                {
                fread (&pv->baseline,     sizeof (int),    1, vfile);
                fread (&pv->mjd,          sizeof (int),    1, vfile);
                fread (&pv->iat,          sizeof (double), 1, vfile);
                fread (&pv->config_index, sizeof (int),    1, vfile);
                fread (&pv->source_index, sizeof (int),    1, vfile);
                fread (&pv->freq_index,   sizeof (int),    1, vfile);
                fread (pv->pols,          sizeof (char),   2, vfile);
                fread (&pv->pulsar_bin,   sizeof (int),    1, vfile);
                fread (&pv->weight,       sizeof (double), 1, vfile);
                fread (pv->uvw,           sizeof (double), 3, vfile);
                fread (pv->comp,          sizeof (float),  2*nvis, vfile);

                if (opts->verbose > 2)
                    printf ("valid read bl %x time %d %13.6f config %d source %d freq %d, pol %c%c pb %d\n",
                    pv->baseline, pv->mjd, pv->iat, pv->config_index, pv->source_index, 
                    pv->freq_index, pv->pols[0], pv->pols[1], pv->pulsar_bin);
                }
            else
                {
                fprintf(stderr, "Error parsing Swinburne header: got a sync of %x and version"
                        " of %d in record %d\n", pv->sync, pv->version, *nvrec);
                return -4;
                }
            }
        else
            {
            fprintf (stderr, "Error parsing Swinburne header: got an unrecognized sync"
                    " of %x in record %d\n", pv->sync, *nvrec);
            return -5;
            }
                                    // allocate more memory and copy record in
        *nvrec += 1;                // bump the record counter
        pch += vrsize;
        pv = (vis_record *) pch;    // point to next record
                                    // if necessary, get another chunk's worth of ram
        if (*nvrec % CHUNK == 0)
            {
            if (opts->verbose > 1)
                {
                if (sizeof(size_t) == 4)
                    printf ("realloc another mem chunk for visibilities, nvrec %d size %lu\n",
                            *nvrec, (size_t) (*nvrec + CHUNK) * (long int) vrsize);
                else
                    printf ("realloc another Mem chunk for visibilities, nvrec %d size %llu\n",
                            *nvrec, (size_t) (*nvrec + CHUNK) * (long long int) vrsize);
                }
            *vrec = realloc (*vrec, (size_t) (*nvrec + CHUNK) * (long int) vrsize);
            if (*vrec == NULL)
                {
                printf ("error reallocating memory for %d records\n", *nvrec);
                return -2;
                }
            pch = (char *) *vrec + *nvrec * (long int) vrsize;
            pv = (vis_record *) pch;
            }
        }
    }                               // return path is always through EOF
