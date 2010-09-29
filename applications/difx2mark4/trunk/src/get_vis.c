// get_vis reads one visibility record from the indicated SWIN file
//
//  first created                          rjc  2010.3.11

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx2mark4.h"

#define NUMFILS 30                  // max number of type 1 output files

// there is one type 1 output file for each baseline in the difx scan
//     get_vis (FILE *, vis_record *);


int get_vis (FILE *fin,             // SWIN format file with visibilities
             int nvis,              // number of visibilities in record
             vis_record *pr) // pointer to data structure to be output 
    {
    static int nrec = 0;
                                    // test first and last reads for error or EOF
    if (fread (&pr->sync, sizeof (int), 1, fin) <= 0)
        return (-1);
    fread (&pr->version, sizeof (int), 1, fin);
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

    if (fread (pr->comp, sizeof (float), 2 * nvis, fin) <= 0)
        return (-1);
    else
        return (0);
    }
