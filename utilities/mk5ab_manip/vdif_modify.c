// vdif_modify - reads a vdif file and change bits
//
// first version      2012.10.25 rjc
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

    unsigned int data[2056];

    FILE *fin,
         *fout;

    if (argc !=  3)
        {
        printf ("Usage: vdif_modify <input vdif fname> <output vdif fname>\n");
        return(1);
        }

    fin = fopen(argv[1], "r");

    if (fin == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("vdif_modify");
        return (1);
        }

    fout = fopen(argv[2], "w");

    if (fout == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("vdif_modify");
        return (1);
        }

                                    // loop over mk5b disk frames
    while (fread (data, sizeof(int), 2056, fin) == 2056)
        {
        if (data[7] != SYNC)        //tombstone on bad sync
            {
            printf ("invalid sync word read %8.8X at frame # %d, quitting!\n", data[0], nfr);
            for (i=0; i<16; i++)
                printf ("%x ", data[i]);
            printf ("\n");
            return (1);
            }
                                    // start bit fiddling
        data[3] = 0x84000000;
                                    // end bit fiddling
        fwrite (data, sizeof (int), 2056, fout);
        nfr++;
        }
    }

