// get_vis opens and read one complete visibility file in Swinburne format 
//
//
//  first created                                       rjc  2010.3.11
//  modified to open and read whole Swinburne file      rjc  2011.5.18
//  handle variable nvis & vrsize                       rjc  2018.10.18

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
                                    // visibility allocation chunk size (bytes)
#define CHUNK 5000000

int get_vis (DifxInput *D,                    // ptr to difx input file data
             char *vf_name,                   // name of input file
             struct CommandLineOptions *opts, // ptr to input options
             int *nvrtot,                     // total number of vis. records read
             int *nvis,                       // array of #vis per record
             int *vrsize,                     // array of size of vis records in bytes
             vis_record **vrec,               // ptr to malloced array of vis. recs as read
             char *corrdate,                  // modification date of input file
             struct fblock_tag *pfb)          // ptr to filled-in fblock table
    {
    int err,
        vfile_status,
        vrsize_tot,
        allocated_tot,
        nskip=0,
        nvr,
        ipfb;

    FILE *vfile;
    struct tm *mod_time;
    struct stat attrib;
    vis_record *pv;                 // convenience pointers
    char *pch;
    DifxFreq *pfr;
                                    // local function prototypes
    int get_pfb_index (int, int, struct fblock_tag *);

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

    *vrec = malloc (CHUNK);
    allocated_tot = CHUNK;          // total amount of memory allocated
    pv = *vrec;
    pch = (char *) pv;
    nvr = 0;
    vrsize_tot = 0;
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
                *nvrtot = nvr;
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

                                    // determine #vis from input tables
                pfr = D->freq + pv->freq_index;
                nvis[nvr] = pfr->nChan / pfr->specAvg;
                                    // protect from array overrun
                if (nvis[nvr] > MAX_VIS) 
                    {
                    fprintf (stderr, 
                    "fatal error: # visibilities (%d) exceeds array dimension (%d)\n",
                    nvis, MAX_VIS);
                    return (-7);
                    }
                vrsize[nvr] = sizeof (vis_record) - sizeof (pv->comp)
                                 + nvis[nvr] * 2 * sizeof (float);
                fread (pv->comp,          sizeof (float),  2*nvis[nvr], vfile);

                                    // if baseline not in fblock - skip over data record
                ipfb = get_pfb_index (pv->baseline, pv->freq_index, pfb);
                if (ipfb < 0)
                    {
                    if (opts->verbose > 2)
                        fprintf (stderr, "Skipping data for index %d of baseline %d\n",
                                  pv->freq_index, pv->baseline);
                    nskip++;
                    continue;
                    }   
                if (opts->verbose > 2)
                    fprintf (stderr, "valid read bl %x time %d %13.6f %p config %d source %d "
                                     "freq %d, pol %c%c pb %d\n",
                    pv->baseline, pv->mjd, pv->iat, &(pv->iat),pv->config_index,
                    pv->source_index, pv->freq_index, pv->pols[0], pv->pols[1], pv->pulsar_bin);
                }
            else
                {
                fprintf(stderr, "Error parsing Swinburne header: got a sync of %x and version"
                        " of %d in record %d\n", pv->sync, pv->version, nvr);
                return -4;
                }
            }
        else
            {
            fprintf (stderr, "Error parsing Swinburne header: got an unrecognized sync"
                    " of %x in record %d\n", pv->sync, nvr);
            return -5;
            }
     
        vrsize_tot += vrsize[nvr];
        nvr += 1;                   // bump the record counter
                                    // protect from visibility array overruns
        if (nvr > NVRMAX) 
            {
            fprintf (stderr, 
            "fatal error: # visibility records (%d) exceeds array dimension (%d)\n",
            nvr, NVRMAX);
            return (-8);
            }
                                    // point to next record
        pch = (char *) *vrec + vrsize_tot;
        pv = (vis_record *) pch;
                                    // if necessary, get another chunk's worth of ram
                                    // trigger realloc when less than half of the
                                    // current chunk remains
        if (allocated_tot - vrsize_tot < 0.5 * CHUNK)
            {
            if (opts->verbose > 1)
                printf ("realloc another mem chunk for visibilities, nvr %d size %d bytes\n",
                        nvr, (int) CHUNK);
                 
            allocated_tot += CHUNK;
            *vrec = realloc (*vrec, (size_t) allocated_tot);
            if (*vrec == NULL)
                {
                printf ("error reallocating memory for %d records, requested %d bytes\n",
                        nvr, allocated_tot);
                return -2;
                }
                                    // recalculate pointers based on reallocated memory
            pch = (char *) *vrec + vrsize_tot;
            pv = (vis_record *) pch;
            }
        }
        if (opts->verbose > 1 && nskip > 0)
            fprintf (stderr, "total Swinburne records skipped %d\n", nskip);
    }                               // return path is always through EOF

// determine index into pfb table for baselne bl
int get_pfb_index (int baseline,    // difx-encoded baseline (100 * a1 + a2)
                   int freq_index,  // difx-generated freq index
                   struct fblock_tag *pfb)
    {
    int nf = -1,
        a1, 
        a2;

    a1 = baseline / 256 - 1;
    a2 = baseline % 256 - 1;
                                    
                                    // search through pfb for antennas matching baseline
    while (pfb[++nf].stn[0].ant >= 0) // check for end-of-table marker
        {
        if (256 * (pfb[nf].stn[0].ant + 1) + pfb[nf].stn[1].ant + 1 == baseline
          && (freq_index == pfb[nf].stn[0].find || freq_index == pfb[nf].stn[1].find))
            return nf;
                                    // baseline didn't match, if it's an auto-correlation
        if (a1 == a2                // need to match one antenna and freq index
         && (pfb[nf].stn[0].ant == a1 && freq_index == pfb[nf].stn[0].find 
          || pfb[nf].stn[1].ant == a1 && freq_index == pfb[nf].stn[1].find))
            return nf;
        }
         
    return -1;                      // signify not-found
    }
