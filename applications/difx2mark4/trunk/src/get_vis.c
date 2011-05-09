// get_vis reads one visibility record from the indicated SWIN file
//
//  first created                          rjc  2010.3.11

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2mark4.h"
#include "difxio/parsevis.h"

// there is one type 1 output file for each baseline in the difx scan
//     get_vis (FILE *, vis_record *);


int get_vis_header (FILE *fin,             // SWIN format file with visibilities
                    vis_record *pr) // pointer to data structure to be output 
    {
    int v;
                                    // test first and last reads for error or EOF
    //first of all, figure out what kind of header we are dealing with
    v = fread(&pr->sync, sizeof(int), 1, fin);
    if(v <= 0) //EOF
        return (-1);

    if(pr->sync == VISRECORD_SYNC_WORD_DIFX1) //old style ascii header
        {
        fprintf(stderr, "Error: difx2mark4 will not work with DiFX 1.x data\n");
        return -2;
        }
    else if(pr->sync == VISRECORD_SYNC_WORD_DIFX2) //new style binary header
        {
        fread (&pr->version, sizeof (int), 1, fin);
        if(pr->version == 1) //new style binary header
            {
            fread (&pr->baseline, sizeof (int), 1, fin);
            fread (&pr->mjd, sizeof (int), 1, fin);
            fread (&pr->iat, sizeof (double), 1, fin);
            fread (&pr->config_index, sizeof (int), 1, fin);
            fread (&pr->source_index, sizeof (int), 1, fin);
            fread (&pr->freq_index, sizeof (int), 1, fin);
            fread (pr->pols, sizeof (char), 2, fin);
            fread (&pr->pulsar_bin, sizeof (int), 1, fin);
            fread (&pr->weight, sizeof (double), 1, fin);
            fread (pr->uvw, sizeof (double), 3, fin);
            }
        else
            {
            fprintf(stderr, "Error parsing header: got a sync of %x and version of %d\n",
            pr->sync, pr->version);

            return -2;
            }
        }
        else
            {
            fprintf(stderr, "Error parsing header: got an unrecognized sync of %xd\n", pr->sync);

            return -2;
            }
        return (0);
    }
// vim: shiftwidth=4:softtabstop=4:expandtab:cindent:cinoptions={1sf1s^-1s
