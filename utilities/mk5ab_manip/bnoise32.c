// bnoise - writes 2 mk5b files, with a specified amount of correlated noise
//
// first version                            2010.12.16  rjc
// 32 bit version (16 independent channels) 2011.2.11   rjc
// 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_randist.h>

#define _FILE_OFFSET_BITS 64    /* This has to be defined for LFS */
#define SYNC 0xabaddeed

#define TWOPI 6.28318530717958
                                    // 2010y111d03h20m00s
#define T0 307.138888888888888 
                                    // sampler threshold in sigma units
#define THRESHOLD 1.00

int main (int   argc, 
          char *argv[])
    {
    int i,
        j,
        idur,
        isec,
        ifr,
        isamp,
        nfps,
        a2,
        b2;

    unsigned char ucdata[6];
    
    unsigned int adata[2504],
                 bdata[2504],
                 jjj,
                 sssss,
                 ssss,
                 crc16;

    static unsigned int bitreverse_table[65536];

    double fs,
           corr,
           fract,
           deltat,
           sigma,
           a,
           b,
           c,
           csigma,
           t = T0,
           f = 50.0;                // offset frequency for mixing with C's noise (Hz)

    FILE *fouta,
         *foutb;

    const gsl_rng_type *T;
    gsl_rng *r;
                                    // prototypes
    unsigned int bcd (unsigned int);
    unsigned int crcc (unsigned char *, int, int, int);
    void init_bitreverse (unsigned int *);


    if (argc !=  6)
        {
        printf ("Usage: bnoise32 <output m5b fname A> <output m5b fname B> <fs (MHz)> <dur (s)> <corr coeff>\n");
        printf ("e.g.:  bnoise32 filea.m5b fileb.m5b 32 5 0.1\n");
        return(1);
        }

    fs   = atof (argv[3]);
    idur = atoi (argv[4]);
    corr = atof (argv[5]);
                                    // housekeeping
    nfps = 400 * fs;                // calculate number of frames per second, time per frame
    deltat = 1.0 / (8.64e4 * nfps);
    csigma = sqrt (corr / (1.0 - corr));
    printf ("relative signal voltage %lf\n", csigma);
                                    // sigma of summed signal
    sigma = sqrt (1.0 + csigma * csigma);

    init_bitreverse (bitreverse_table);

    gsl_rng_env_setup ();
    T = gsl_rng_default;
    r = gsl_rng_alloc (T);

    fouta = fopen (argv[1], "w");

    if (fouta == NULL)
        {
        printf ("problem opening output file %s\n", argv[1]);
        perror ("bnoise");
        return (1);
        }

    foutb = fopen (argv[2], "w");

    if (foutb == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("bnoise");
        return (1);
        }
                                    // frame boiler plate
    adata[0] = 0xabaddeed;
    bdata[0] = adata[0];

                                    // loop over seconds of output data
    for (isec=0; isec<idur; isec++)
                                    // loop over frames within a second
        for (ifr=0; ifr<nfps; ifr++)
            {
                                    // generate headers
            adata[1] = 0xbead0000 | ifr;
            bdata[1] = adata[1];

            fract = fmod (t, 1.0);
            jjj = t - fract;
            sssss = 8.64e4 * fract + 1e-6;

            adata[2] = ((bcd (jjj) & 0xfff) << 20) | bcd (sssss);
            bdata[2] = adata[2];

            ssss = (8.64e4 * fract - sssss) * 1e4 + 0.5;
            adata[3] = bcd (ssss) << 16;
                                    // put in chars to keep Endian sense right
            ucdata[0] = (adata[2] & 0xff000000) >> 24;
            ucdata[1] = (adata[2] & 0x00ff0000) >> 16;
            ucdata[2] = (adata[2] & 0x0000ff00) >>  8;
            ucdata[3] =  adata[2] & 0x000000ff;
            ucdata[4] = (adata[3] & 0xff000000) >> 24;
            ucdata[5] = (adata[3] & 0x00ff0000) >> 16;

            crc16 = bitreverse_table [crcc (ucdata, 48, 0x4003, 16)];
            adata[3] |= crc16;
            bdata[3] = adata[3];

                                    // loop over samples within frame
            for (isamp=0; isamp<2500; isamp++)
              {
              adata[isamp+4]= 0;
              bdata[isamp+4]= 0;
              for (j=0; j<16; j++)
                {
                                    // generate correlated random samples
                c = gsl_ran_gaussian (r, csigma);
                a = gsl_ran_gaussian (r, 1.0);
                b = gsl_ran_gaussian (r, 1.0);
                
                a += c;
                b += c * 2.0 * sin (2.0 * M_PI * f * (t + isamp / 2500.0 * deltat) * 8.64e4); 
                                    // re-sample to 2 bits
                if (a < -THRESHOLD * sigma)
                    a2 = 0;
                else if (a < 0.0)
                    a2 = 2;
                else if (a < THRESHOLD * sigma)
                    a2 = 1;
                else
                    a2 = 3;

                if (b < -THRESHOLD * sigma)
                    b2 = 0;
                else if (b < 0.0)
                    b2 = 2;
                else if (b < THRESHOLD * sigma)
                    b2 = 1;
                else
                    b2 = 3;
                                    // insert samples into data words
                adata[isamp+4] = adata[isamp+4] << 2 | a2;
                bdata[isamp+4] = bdata[isamp+4] << 2 | b2;
                }
              }
                                    // write one output each for a & b
            fwrite (adata, sizeof (int), 2504, fouta);
            fwrite (bdata, sizeof (int), 2504, foutb);
                                    //update time by 1 frame
            t += deltat;
            }
                                    // close files and end
    fclose (fouta);
    fclose (foutb);
    return 0;
    }


// convert an integer into 8 digits of bcd
//
unsigned int bcd (unsigned int nbin)
    {
    unsigned int nbcd = 0;
    int i,
        idiv = 10000000;

    nbin %= 100000000;              // limit input binary number to 8 digits

    for (i=0; i<8; i++)
        {
        nbcd = (nbcd << 4) | (nbin / idiv);
        nbin -= (nbin / idiv) * idiv;
        idiv /= 10;
        }
    return nbcd;
    }

// crc subroutine - converts 48 bit input into a 16 bit code
unsigned int crcc (unsigned char *idata, 
                   int len, 
                   int mask, 
                   int cycl)
    {
    unsigned int istate = 0;
    int idbit;
    int q, ich, icb;

    for (idbit = 1; idbit <= len; idbit++)
        {
        q = istate & 1;
        ich = (idbit - 1) / 8;
        icb = 7 - (idbit - 1) % 8;
        if ((((idata[ich] >> icb) & 1) ^ q) == 0)
            istate &= -2;
        else {
            istate ^= mask;
            istate |= 1;
            }
        istate = istate >> 1 | (istate & 1) << cycl -1;
        }
    return (istate);
    }


// create a 2^16 entry table whose contents is the
// bit reversed address of each entry
void init_bitreverse (unsigned int *table)
    {
    unsigned int i;

    for (i=0; i<=65535; ++i)
        table[i] = ((i & 0x8000) >> 15)
              | ((i & 0x4000) >> 13)
              | ((i & 0x2000) >> 11)
              | ((i & 0x1000) >> 9)
              | ((i & 0x0800) >> 7)
              | ((i & 0x0400) >> 5)
              | ((i & 0x0200) >> 3)
              | ((i & 0x0100) >> 1)
              | ((i & 0x0080) << 1)
              | ((i & 0x0040) << 3)
              | ((i & 0x0020) << 5)
              | ((i & 0x0010) << 7)
              | ((i & 0x0008) << 9)
              | ((i & 0x0004) << 11)
              | ((i & 0x0002) << 13)
              | ((i & 0x0001) << 15);
    }
