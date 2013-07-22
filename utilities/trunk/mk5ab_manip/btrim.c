// btrim - ensures that a mk5b format file starts on a second tick by trimming it
//
// first version, based on bfind    2011.4.27   rjc
// 
#include <stdio.h>
#include <stdlib.h>

#define _FILE_OFFSET_BITS 64    /* This has to be defined for LFS */
#define SYNC 0xabaddeed
#define FRAME_MASK 0x00007fff

int main (int   argc, 
          char *argv[])
    {
    int i,
        nfr = 0;

    
    unsigned int data[2504];

    FILE *fin,
         *fout;

    if (argc != 3)
        {
        printf ("Usage: btrim <input m5b fname> <output m5b fname>\n");
        return(1);
        }

    fin = fopen(argv[1], "r");

    if (fin == NULL)                // tombstone on bad input file
        {
        printf ("quitting due to problem opening input file %s\n", argv[1]);
        perror ("btrim");
        return (1);
        }

                                    // read (and discard) file up to 1st sync that
                                    // is on a second tick
    for (i=0; i< 100000000; i++)
        {
        if ((fread (data, sizeof (int), 1, fin) != 1))
            {                       // read failed due to EOF or other problem
            i = 100000000;
            break;
            }
        
        if (data[0] == SYNC)
            {
            fread (data, sizeof (int), 1, fin);
            i++;
            if ((data[0] & FRAME_MASK) == 0)
                break;
            }
        }

    if (i == 100000000)             // tombstone on missing sync's
        {
        printf ("quitting due to problem finding sync in the input file %s\n", argv[1]);
        return (1);
        }
    printf ("trimmed off %d bytes\n", i * 4);

    fseek (fin, -8, SEEK_CUR);       // position back to start of frame
                                     // open output file
    fout = fopen(argv[2], "w");
    if (fout == NULL)                // tombstone on bad input file
        {
        printf ("quitting due to problem opening output file %s\n", argv[2]);
        perror ("btrim");
        return (1);
        }

                                    // loop over all input mk5b disk frames
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

        fwrite (data, sizeof (int), 2504, fout);
        nfr++;
        }
    fclose (fin);
    fclose (fout);

    printf ("btrim wrote %d frames of data\n", nfr);
    }

