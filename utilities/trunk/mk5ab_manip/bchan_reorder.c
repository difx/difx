// bchan_reorder - reads a mk5b format file and shifts the channels around as desired
// note: user needs to insert a few lines of code to do the desired manipulation
// (see below)
//
// first version, based on bwrite      2012.4.24  rjc
// 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define _FILE_OFFSET_BITS 64    /* This has to be defined for LFS */
#define SYNC 0xabaddeed

#define TWOPI 6.28318530717958;

int main (int   argc, 
          char *argv[])
    {
    int i,
        nfr = 0;

    unsigned int data[2504];

    FILE *fin,
         *fout;

    if (argc !=  3)
        {
        printf ("Usage: bchan_reorder <input m5b fname> <output m5b fname>\n");
        return(1);
        }

    fin = fopen(argv[1], "r");

    if (fin == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("bchan_reorder");
        return (1);
        }

    fout = fopen(argv[2], "w");

    if (fout == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("bchan_reorder");
        return (1);
        }

                                    // loop over mk5b disk frames
    while (fread (data, sizeof(int), 2504, fin) == 2504)
        {
        if (data[0] != SYNC)        //tombstone on bad sync
            {
            printf ("invalid sync word read %8.8X at frame # %d, quitting!\n", data[0], nfr);
            for (i=0; i<16; i++)
                printf ("%x ", data[i]);
            printf ("\n");
            return (1);
            }

        for (i=4; i<2504; i++)
            {                       // bit manipulation code is inserted here
            data[i] >>= 10;
            }                       // end of desired bit manipulation
        fwrite (data, sizeof (int), 2504, fout);
        nfr++;
        }
    }

