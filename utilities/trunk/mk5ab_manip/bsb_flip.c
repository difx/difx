// bsb_flip - reads a mk5b format file and modifies each channel to change its
// sense from USB to LSB (or the reverse, if already LSB)
//
// first version, based on bchan_reorder      2013.8.30  rjc
// 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define _FILE_OFFSET_BITS 64    /* This has to be defined for LFS */
#define SYNC 0xabaddeed

#define TWOPI 6.28318530717958;

int main (int   argc, 
          char *argv[])
    {
    int i,
        nfr = 0;

    unsigned int data[2504],
                 flip_mask;

    FILE *fin,
         *fout;

    if (argc < 3 || argc > 4)
        {
        printf ("Usage: bsb_flip <input m5b fname> <output m5b fname> [<flipped channel mask>]\n");
        return(1);
        }

    fin = fopen(argv[1], "r");

    if (fin == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("bsb_flip");
        return (1);
        }

    fout = fopen(argv[2], "w");

    if (fout == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("bsb_flip");
        return (1);
        }

    if (argc == 4)                  // flip mask can be decimal or hex
        {
        flip_mask = (unsigned int) strtoul (argv[3], (char **) NULL, 0);
        printf ("flipping by channel using the hex mask %8x\n", flip_mask);
        }
    else
        flip_mask = 0xffffffff;     // default is to flip all channels

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

        for (i=4; i<2504; i+=2)
            {                       // bit manipulation code is inserted here
            data[i] ^= flip_mask;   // just negate every other real sample
            }                       // end of desired bit manipulation
        fwrite (data, sizeof (int), 2504, fout);
        nfr++;
        }
    }

