// bpcal - extract phase cal tone info from a Mk5B file
//
// first version, based on bstate             2008.2.6   rjc
// added automatic sample rate determination  2008.5.2   rjc
//   - assumes that all 32 tracks recorded
// 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define _FILE_OFFSET_BITS 64    /* This has to be defined for LFS */
#define SYNC 0xabaddeed
#define FALSE 0
#define TRUE 1

int main (int   argc, 
          char *argv[])
    {
    int i,
        j,
        k,
        nfr = 0,
        nfrtot,
        ns = 0,
        amp,
        t,
        t0,
        t10,
        dt,
        twobit = TRUE;

    
    unsigned int data[2504],
                 datai;

    double pi = 3.14159265358979,
           freq,
           cos_acc[16],
           sin_acc[16],
           cos_sum[16],
           sin_sum[16],
           val_sum[16],
           c,
           s,
           v,
           cos_corr,
           sin_corr,
           value[4] = {-3.0, 1.0, -1.0, 3.0}, // scrambled order due to MS bit order on disk
           phase,
           phinc,
           fs;
    
    FILE *fb;
                                    // use default of 400 disk frames = 1 MSample
    if (argc == 3 || argc ==4 && strcmp (argv[3], "-1") == 0)                  
        nfrtot = 400;
    else if (argc == 4 && strcmp (argv[3], "-1") != 0)
        nfrtot = atoi (argv[3]);
    else if (argc != 5 || strcmp (argv[4], "-1") != 0)
        {
        printf ("Usage: bpcal <input m5b fname> <tone freq (KHz)> [-1]\n");
        printf ("   or: bpcal <input m5b fname> <tone freq (KHz)> <# frames> [-1]\n");
        return(1);
        }

    if (argc == 4 && strcmp (argv[3], "-1") == 0 
     || argc == 5 && strcmp (argv[4], "-1") == 0)
        twobit = FALSE;

    freq = atof (argv[2]);
    
    fb = fopen(argv[1], "r");

    if (fb == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("bfind");
        return (1);
        }

                                    // initialize quadrature accumulators
    for (j=0; j<16; j++)
        {
        cos_acc[j] = 0.0;
        sin_acc[j] = 0.0;
        cos_sum[j] = 0.0;
        sin_sum[j] = 0.0;
        val_sum[j] = 0.0;
        }
            
                                    // read 1st ten frames to determine sample rate
                                    // assuming all 32 tracks are active
    for (i=0; i<11; i++)
        {
        fread (data, sizeof(int), 2504, fb);
        if (data[0] != SYNC)        //tombstone on bad sync
            {
            printf ("invalid sync word read %8.8X, quitting!\n", data[0]);
            return (1);
            }
        t =  1000 * (data[3] >> 28 & 0xf)
            + 100 * (data[3] >> 24 & 0xf)
            +  10 * (data[3] >> 20 & 0xf)
            +       (data[3] >> 16 & 0xf);
        if (i == 0)
            t0 = t;
        else if (i == 10)
            t10 = t;
        }
    dt = t10 - t0;
                                    // assign sample rate based on elapsed time
    switch (dt)
        {
        case 3:
            fs = 64;
            break;
        case 7:
            fs = 32;
            break;
        case 15:
            fs = 16;
            break;
        case 31:
            fs = 8;
            break;
        case 62:
            fs = 4;
            break;
        case 125:
            fs = 2;
            break;
        case 250:
            fs = 1;
            break;
        default:
            printf ("unknown or unsupported sample rate, dt = %d in 10 frames; quitting!\n", dt);
            printf ("t0 %d t10 %d\n", t0, t10);
            return (1);
        }
    printf ("\nintegration time %6.3lf sec %d bits/sample\n\n", 
            2.5e-3 * nfrtot / fs, (twobit==TRUE)?2:1);
    rewind (fb);                    // position back to start

                                    // loop over mk5b disk frames
    while (fread (data, sizeof(int), 2504, fb) == 2504)
        {
        if (data[0] != SYNC)        //tombstone on bad sync
            {
            printf ("invalid sync word read %8.8X, quitting!\n", data[0]);
            return (1);
            }

        phase = 0.002 * pi * freq / fs * ns;
        phinc = 0.002 * pi * freq / fs;

        for (i=4; i<2504; i++)      // loop over data words
            {
            datai = data[i];
            c = cos (phase);
            s = sin (phase);   
                                    // loop over channels
            for (j=0; j<16; j++) 
                if (twobit)
                    {
                    v = value[datai & 3];   
                
                    cos_acc[j] += c * v;
                    sin_acc[j] += s * v;
                    cos_sum[j] += c * c;
                    sin_sum[j] += s * s;
                    val_sum[j] += v * v;
                    datai >>= 2;
                    }
                else                // one bit samples, must get 2 out of each word
                    for (k=0; k<2; k++)
                        {
                        v = value[datai & 1 + 1];   
                
                        cos_acc[j] += c * v;
                        sin_acc[j] += s * v;
                        cos_sum[j] += c * c;
                        sin_sum[j] += s * s;
                        val_sum[j] += v * v;
                        datai >>= 1;
                        }
            phase += phinc;
            }
        nfr++;
        if (twobit)
            ns += 2500;
        else
            ns += 5000;
                                    // do calculations and print when we've
                                    // read the requested amount of data
        if (nfr == nfrtot)
            {
            printf ("ch  amp   phase(dg)\n");
                                    // calculate amplitudes and phases
            for (j=0; j<16; j++)
                {
                cos_corr = cos_acc[j] / sqrt (cos_sum[j] * val_sum[j]);
                sin_corr = sin_acc[j] / sqrt (sin_sum[j] * val_sum[j]);

                amp = 1000 * sqrt (cos_corr*cos_corr + sin_corr*sin_corr) + 0.5;
                                    // imaginary part negated to match mk3 convention
                phase = 180.0 / pi * atan2 (-sin_corr, cos_corr);

                printf ("%2d  %5d  %7.1lf\n", j, amp, phase);
                }
                
            return (0);
            }
        }
    }



