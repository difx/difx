// b2vdif - reads a mk5b format file and converts it to vdif format
//
// first version, based on bwrite      2012.12.11  rjc
// 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SYNC 0xabaddeed
enum {FALSE, TRUE};

unsigned short int lookup[65536];

int main (int   argc, 
          char *argv[])
    {
    int i,
        nfr = 0,
        jjj,
        sssss,
        jdref,
        iyear,
        indref,
        delta,
        first = TRUE,
        word1,
        od,
        ev;
                     

    
    unsigned int m5data[2504],
                 vdata[2508];

    unsigned short int *pm5,
                       *pvd;

    FILE *fin,
         *fout;

                                    // table below is valid for year 2000.0 to 2032.0
                                    // and contains mjd on Jan 1 and Jul 1 for each year
    static int mjds[64] = 
        {
        544, 726, 910,  91, 275, 456, 640, 821,  // 2000-2003
          5, 187, 371, 552, 736, 917, 101, 282,  // 2004-2007
        466, 648, 832,  13, 197, 378, 562, 743,  // 2008-2011
        927, 109, 293, 474, 658, 839,  23, 204,  // 2012-2015
        388, 570, 754, 935, 119, 300, 484, 665,  // 2016-2019
        849,  31, 215, 396, 580, 761, 945, 126,  // 2020-2023
        310, 492, 676, 857,  41, 222, 406, 587,  // 2024-2027
        771, 953, 137, 318, 502, 683, 867,  48   // 2028-2031
        };

    if (argc != 4 && (argc !=  5 || strlen(argv[4]) != 2))
        {
        printf ("\nUsage: b2vdif <input m5b fname> <output vdif fname> <yyyy>\n");
        printf ("   or: b2vdif <input m5b fname> <output vdif fname> <yyyy> <xx>\n");
        printf ("\nwhere yyyy (e.g. 2012) is the year of the scan, and\n");
        printf ("xx (e.g. On) is an optional 2 character site ID\n");
        printf ("b2vdif converts the given mk5b file into vdif format\n");
        printf ("assuming that the mk5b input file has 16 x 2bit channels\n");
        return(1);
        }

                                    // generate the lookup table, which maps
                                    // 00:01:10:11 to 00:10:01:11, for all 2 bit pairs
    for (i=0; i<65536; i++)
        {
        od = i & 0xaaaa;            // odd bits
        ev = i & 0x5555;            // even bits
                                    // and now the magic part...
        lookup[i] = i ^ ((od^(ev<<1)) | (ev^(od>>1)));
        }

                                    // insert boilerplate portions of vdif header
    vdata[2] = 1254;
    vdata[3] = 0x04000000 | argv[4][1] << 8 | argv[4][0];

    fin = fopen(argv[1], "r");

    if (fin == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("b2vdif");
        return (1);
        }

    fout = fopen(argv[2], "w");

    if (fout == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("b2vdif");
        return (1);
        }

                                    // loop over mk5b disk frames
    while (fread (m5data, sizeof(int), 2504, fin) == 2504)
        {
        if (m5data[0] != SYNC)      //tombstone on bad sync
            {
            printf ("invalid sync word read %8.8X at frame # %d, quitting!\n", m5data[0], nfr);
            for (i=0; i<16; i++)
                printf ("%x ", m5data[i]);
            printf ("\n");
            return (1);
            }
                                    // decode bcd time fields
        jjj = 100 * ((m5data[2]             ) >> 28)
             + 10 * ((m5data[2] & 0x0f000000) >> 24)
             +      ((m5data[2] & 0x00f00000) >> 20);

        sssss = 10000 * ((m5data[2] & 0x000f0000) >> 16)
               + 1000 * ((m5data[2] & 0x0000f000) >> 12)
               +  100 * ((m5data[2] & 0x00000f00) >>  8)
               +   10 * ((m5data[2] & 0x000000f0) >>  4)
               +        ((m5data[2] & 0x0000000f)      );

                                    // determine julian date of ref epoch only once
        if (first)
            {
            iyear = atoi (argv[3]);
            if (iyear < 2000 || iyear > 2031)
                {
                printf ("year of %d is invalid!\n", iyear);
                return (1);
                }
            indref = 2 * (iyear - 2000) + 1;
            jdref = mjds[indref] % 1000;

            delta = jjj - jdref;
            if (delta < 0)
                delta += 1000;
                                    // 818..999 is in Jan..Jun
            if (delta >= 818 && delta <= 999)
                {
                indref--;
                jdref = mjds[indref] % 1000;
                }
            else if (delta > 183)   // 0..183 is in Jul..Dec
                {                   // which this isn't
                printf ("input file's mjd of %d inconsistent with year %d\n", jjj, iyear);
                return (1);
                }
            word1 = indref << 24;   // save constant part of word 1 of vdif header
            first = FALSE;
            }
                                    // insert seconds since ref epoch
        vdata[0] = (jjj - jdref) * 86400 + sssss;
                                    // insert frame sequence number within second
        vdata[1] = word1 | m5data[1] & 0x7fff;
                                    // copy 16 byte mk5b header
        memcpy ((char *)vdata + 16, (char *) m5data, 16);

        pvd = (short unsigned int *) vdata + 16;
        pm5 = (short unsigned int *) m5data + 8;
                                    // copy 10000 bytes of mk5b data
        for (i=0; i<1250; i++)
            {
                                    // coded inline for efficiency
            *pvd = lookup[*pm5];
            pvd++;
            pm5++;
            *pvd = lookup[*pm5];
            pvd++;
            pm5++;
            *pvd = lookup[*pm5];
            pvd++;
            pm5++;
            *pvd = lookup[*pm5];
            pvd++;
            pm5++;
            }
        fwrite (vdata, sizeof (int), 2508, fout);
        nfr++;
        }
    }

