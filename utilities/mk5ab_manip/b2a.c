// b2a - converts a mk5b format file into mk5a vlba format
//
// first version, based on T. Cappallo's mk5    2005.8.17   rjc
// 
#include <stdio.h>
#include <stdlib.h>

#define _FILE_OFFSET_BITS 64    /* This has to be defined for LFS */
#define SYNC 0xabaddeed

int main (int   argc, 
          char *argv[])
    {
    int i,
        j,
        nfr = 0,
        mode,
        df_tf,
        width;
    
    unsigned int data[2504],
                 mdata[2500];

    static unsigned int aux_data[64] = {0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0xcccccccc, 0xf0f0f0f0, // trk
                                        0xff00ff00, 0x00000000,
                                        0xffff0000, 0x55555555, // grp
                                        0x00000000, 0x00000000,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0},
                        ones[2]  = {0xffffffff, 0xffffffff},
                        zeros[2] = {0x00000000, 0x00000000};
 
    
    FILE *fa,
         *fb;

    if (argc != 4)
        {
        printf ("Usage: b2a <input m5b fname> <output m5a fname> <mode>\n");
        return(1);
        }

    mode = atoi (argv[3]);
                                    // precalculate useful mode-dependent quantities
    if (mode == 64)
        {
        df_tf = 16;                 // calculate # disk frames per tape frame
        width = 2;                  // width of each repeated output bit in words
        }
    else if (mode == 32)
        {
        df_tf = 8;
        width = 1;
        }
    else
        {
        printf ("invalid mode %d. currently supported modes are 32 and 64\n", mode);
        return (1);
        }

    fb = fopen(argv[1], "r");

    if (fb == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("b2a");
        return (1);
        }

    fa = fopen(argv[2], "w");

    if (fa == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("b2a");
        return (1);
        }

                                    // loop over mk5b disk frames
    while (fread (data, sizeof(int), 2504, fb) == 2504)
        {
        if (data[0] != SYNC)        //tombstone on bad sync
            {
            printf ("invalid sync word read %8.8X, quitting!\n", data[0]);
            return (1);
            }
        if (nfr % df_tf == 0)       // on every 8th (for mode 32) disk frame (20000
            {                       // samples) we write out VLBA-style header...
            for (i=0; i<32; i++)    // ...sync words
                fwrite (&ones, sizeof (int), width, fa);

            for (i=0; i<32; i++)    // ...JJJSSSSS
                if (data[2] >> (31 - i) & 1)
                    fwrite (ones, sizeof (int), width, fa);
                else
                    fwrite (zeros, sizeof (int), width, fa);
                    
            for (i=0; i<32; i++)    // ...ssssCRCC
                if (data[3] >> (31 - i) & 1)
                    fwrite (ones, sizeof (int), width, fa);
                else
                    fwrite (zeros, sizeof (int), width, fa);
            }
                                    // now write data
        if (mode == 32)
            fwrite (data+4, sizeof (int), 2500, fa);
        
                                    // if the mode is vlba64, we must multiplex the time
                                    // samples, while staying within the same 'headstack'
                                    // we do so by putting alternate time samples 16
                                    // tracks apart
        else if (mode == 64)
            {
            for (i=0; i<1250; i++)
                {                   // is this endian sense correct?
                mdata[2*i]   = (data[2*i+4] & 0xffff) | (data[2*i+5] & 0xffff) << 16;
                mdata[2*i+1] = (data[2*i+4] & 0xffff0000) >> 16 | (data[2*i+5] & 0xffff0000);
                }
            fwrite (mdata, sizeof (int), 2500, fa);
            }

                                    // ... aux data at end of track frame
        if (nfr % df_tf == df_tf - 1)
            {
            for (i=0; i<64; i++)
                for (j=0; j<width; j++)
                    fwrite (&aux_data[i], sizeof (int), 1, fa);
            }
        nfr++;
        }
    }
