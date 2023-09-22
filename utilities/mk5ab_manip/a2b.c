// a2b - converts a mk5a (with Mk4 track format) file into mk5b format
//
// first version, based on b2a           2006.5.12   rjc
// added 64 track mode 2                 2006.11.15  rjc
// 
#include <stdio.h>
#include <stdlib.h>

#define _FILE_OFFSET_BITS 64        // This used to be defined for LFS
#define SYNC 0xabaddeed
#define INVALID 0x11223344

int main (int   argc, 
          char *argv[])
    {
    int i,
        j,
        n,
        nfr,
        nfr_sec,
        mmode,
        srate,
        m5afact,
        m5bfact,
        formfact,
        syncing = 1,
        sync_size,
        frame_size,
        t0,
        t8,
        rc,
        nskipped,
        jjj,
        sssss,
        mmmm,
        yr, day, hr, min, sec, msec;
        
    
    unsigned int data,
                 ones = 0xffffffff,
                 frame[80000],
                 buff[40000],
                 m5b_header[4];
                                    // prototypes
    unsigned int bcd (int);
    void demult (unsigned int *, int, int, int *);
    void m5a_demux (unsigned int *, unsigned int *, int);
    void decode (unsigned int *, int, int *, int *, int *, int *, int *, int *);
    void m5b_mux (unsigned int *, int, int);
    int jdboy (int);
    
                                    // multiplexing map indexed by destination bit# 
                                    // and time sample; contains source track# (bit# + 2)
    int map[32][4];

    int mode_map[2][32][4] =      
        { 2,  4,  0,  0,            // output bit #0 of multiplex mode 1 map
         10, 12,  0,  0,
          6,  8,  0,  0,
         14, 16,  0,  0,
         18, 20,  0,  0,
         26, 28,  0,  0,
         22, 24,  0,  0,
         30, 32,  0,  0,
          3,  5,  0,  0,
         11, 13,  0,  0,
          7,  9,  0,  0,
         15, 17,  0,  0,
         19, 21,  0,  0,
         27, 29,  0,  0,
         23, 25,  0,  0,
         31, 33,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,
          0,  0,  0,  0,            // output bit #31
                                    // mode 2 is a 64 track mode
          2,  4,  0,  0,            // output bit #0
         10, 12,  0,  0,
          6,  8,  0,  0,
         14, 16,  0,  0,
         18, 20,  0,  0,
         22, 24,  0,  0,
         26, 28,  0,  0,
         30, 32,  0,  0,
          3,  5,  0,  0,
          7,  9,  0,  0,
         11, 13,  0,  0,
         15, 17,  0,  0,
         19, 21,  0,  0,
         23, 25,  0,  0,
         27, 29,  0,  0,
         31, 33,  0,  0,
          2,  4,  0,  0,
         10, 12,  0,  0,
          6,  8,  0,  0,
         14, 16,  0,  0,
         18, 20,  0,  0,
         22, 24,  0,  0,
         26, 28,  0,  0,
         30, 32,  0,  0,
          3,  5,  0,  0,
          7,  9,  0,  0,
         11, 13,  0,  0,
         15, 17,  0,  0,
         19, 21,  0,  0,
         23, 25,  0,  0,
         27, 29,  0,  0,
         31, 33,  0,  0};           // output bit #31

    int mode_m5afact[2]  = {1,      // 1 to 1 mk5a multiplexing in mode 1
                            0},     // 1 to 2  "        "       "   "   2 (depicted by 0)
        mode_formfact[2] = {2,      // 2 to 1 formatter multiplexing in mode 1
                            2},     // "  " "    "           "        "   "  2
        mode_m5bfact[2]  = {2,      // 2 to 1 output multiplexing on mk5b in mode 1
                            1};     // 1 to 1    "       "        "    "   "   "  2

    int sync_sizes[5] = {64, 32, 16, -1, 8},
        frame_sizes[5] = {40000, 20000, 10000, -1, 5000};

    FILE *fa,
         *fb;

    if (argc != 4)
        {
        printf ("Usage: a2b <input m5a fname> <output m5b fname> <mpx mode>\n");
        return(1);
        }

    mmode = atoi (argv[3]);
                                    // create mode map
    if (mmode == 1 || mmode == 2)
        {
        for (i=0; i<32; i++)
            for (j=0; j<4; j++)
                map[i][j] = mode_map[mmode-1][i][j] - 2;
        m5afact  = mode_m5afact[mmode-1];
        formfact = mode_formfact[mmode-1];
        m5bfact  = mode_m5bfact[mmode-1];
        printf ("mmode %d m5afact %d formfact %d m5bfact %d\n",
                 mmode, m5afact, formfact, m5bfact);
        }
    else
        {
        printf ("invalid multiplex mode %d. currently supported modes are 1 and 2\n", mmode);
        return (1);
        }

    fa = fopen(argv[1], "r");

    if (fa == NULL)
        {
        printf ("problem opening input file %s\n", argv[1]);
        perror ("a2b");
        return (1);
        }

    fb = fopen(argv[2], "w");

    if (fb == NULL)
        {
        printf ("problem opening output file %s\n", argv[2]);
        perror ("a2b");
        return (1);
        }
                                    // create a skeleton m5b header
    m5b_header[0] = SYNC;
    m5b_header[1] = 0xbead0000;
    m5b_header[2] = 0;
    m5b_header[3] = 0;

                                    // precalculate useful mode 
                                    // and sample rate dependent quantities

    sync_size = sync_sizes[m5afact];// size (in words) of a multiplexed sync word
    frame_size=frame_sizes[m5afact];// size (in words) of one multiplexed frame

                                    // find first sync word in m5a data stream
    n = 0;
    while (syncing && fread (&data, sizeof(int), 1, fa) == 1)
        {
        if (data == ones)           // possible start of sync, look for right # of 1's
            {
            for (i=0; i<sync_size-1; i++)
                {
                fread (&data, sizeof(int), 1, fa);
                if (data != ones)
                    break;
                }
            if (i == sync_size-1)   // we have found a sync! process it
                {
                syncing = 0;        // transition out of syncing state
                                    // reposition file to start of the sync block (at aux data)
                fseek (fa, -12 * sync_size, SEEK_CUR);
                }
            }
        n++;
        }
    printf ("found 1st sync block at byte location %x\n", 4*(n-1));  
                                    // read one frame of input data
    fread (buff, sizeof (int), frame_size, fa);
                                    // de-multiplex the buffer into frame
    m5a_demux (frame, buff, m5afact);

    printf ("buff:\n");
    for (i=0; i< 20; i++)
        {
        for (j=0; j< 8; j++)
            printf ("%08x ", buff[i*8+j]);
        printf ("\n");
        }

                                    // extract time information from sync block
    decode (frame, m5afact, &yr, &day, &hr, &min, &sec, &msec);
    printf ("0th frame time: %d:%d %02d:%02d:%02d.%03d\n", yr, day, hr, min, sec, msec);
    
    t0 = msec + 1000 * (sec + 60 * (min + 60 * hr));
        
                                    // read eigth frame of input data
    fseek (fa, 7 * frame_size * sizeof (int), SEEK_CUR);
    fread (buff, sizeof (int), frame_size, fa);
                                    // de-multiplex the buffer
    m5a_demux (frame, buff, m5afact);
                                    // extract time information from sync block
    decode (frame, m5afact, &yr, &day, &hr, &min, &sec, &msec);
    printf ("8th frame time: %d:%d %02d:%02d:%02d.%03d\n", yr, day, hr, min, sec, msec);
    t8 = msec + 1000 * (sec + 60 * (min + 60 * hr));

    if (t0 == t8)
        {                           // times same; tombstone
        printf ("Error! Times are the same; quitting.\n");
        return (-1);
        }
                                    // calculate sample rate
    srate = frame_size / (125 * (t8 - t0)); 
    printf ("inferred sample rate %d MHz\n", srate);
    if (srate != 32 && srate != 16 && srate != 8 && srate != 4 && srate != 2)
        {
        printf ("invalid value of sample rate!! ... quitting\n");
        return (-1);                // tombstone
        }

    nfr_sec = srate * 400;          // calculate # disk frames per second
                                    // move input to integral second if not already there
                                    // first, move back to first header that was read
    fseek (fa, -8 * frame_size * sizeof (int), SEEK_CUR);

    nskipped = 0;
    while (msec != 0)
        {
        rc = fread (buff, sizeof (int), frame_size, fa);
        if (rc <= 0)
            {
            printf ("error %d reading disk. quitting!\n", rc);
            return (-1);
            }
        
        if (++nskipped > 1600)
            {
            printf ("can't find header with 0 ms... quitting!\n");
            return (-1);            // tombstone
            }
                                    // de-multiplex the frame
        m5a_demux (frame, buff, m5afact);
                                    // extract time information from sync block
        decode (frame, m5afact, &yr, &day, &hr, &min, &sec, &msec);
        }
    printf ("\nfound start of second after skipping %d frames\n", nskipped);
                                    // now move back to start of frame on second boundary
    fseek (fa, - frame_size * sizeof (int), SEEK_CUR);
                                    // initialize output time quantities
    jjj = (jdboy(yr) + day - 1) % 1000;
    sssss = sec + 60 * (min + 60 * hr);
    mmmm = 0;
    nfr = 0;
    // printf ("fudging time by subtracting 1 second\n");
    // sssss -= 1;
    printf ("processing second: %03d:%05d\n", jjj, sssss);
                                    // loop over input frames until there are no more
    while (fread (buff, sizeof (int), frame_size, fa) == frame_size)
        {
                                    // de-multiplex m5a
        m5a_demux (frame, buff, m5afact);
                                    // de-multiplex data payload in place
        demult (frame, formfact, m5afact, &map[0][0]);
                                    // multiplex for mk5b, if necessary
        m5b_mux (frame, formfact, m5bfact);
        
                                    // write 8 * formfact / m5bfact disk frames per input frame
        for (j=0; j<8*formfact/m5bfact; j++)
            {
                                    // update disk frame header fields
            m5b_header[1] = (m5b_header[1] & 0xffff8000) | nfr;
            m5b_header[2] = bcd (jjj) << 20 | bcd (sssss);
            m5b_header[3] = bcd (mmmm) << 16;
            
                                    // write out mk5b disk frame header
            fwrite (m5b_header, sizeof (m5b_header), 1, fb);
                                    // write out 2500 word data packet
            fwrite (frame + j * 2500, sizeof (int), 2500, fb);
            nfr++;                  // update seconds and milliseconds
            if (nfr == nfr_sec)     // check for (and handle) second roll over
                {
                nfr = 0;
                sssss++;
                printf ("processing second: %03d:%05d\n", jjj, sssss);
                }
            mmmm = 10000 * nfr / nfr_sec;
            }
        }
    fclose (fa);                    // clean up before quitting
    fclose (fb);
    }



// demux a mk5a input buffer

void m5a_demux (unsigned int *frame, unsigned int *buff, int mfact)
    {
    int i, k;

    switch (mfact)                  // use separate cases for speed
        {
        case 0:
            for (i=0; i<40000; i++)
                frame[i] = buff[i];
            break;
            
        case 1:
            for (i=0; i<20000; i++)
                frame[i] = buff[i];
            break;

        case 2:
            for (i=0; i<10000; i++)
                {
                frame[k++] = buff[i] & 0xffff;
                frame[k++] = buff[i] >> 16;
                }
            break;
        
        case 4:
            for (i=0; i<5000; i++)
                {
                frame[k++] =  buff[i] & 0x000000ff;
                frame[k++] = (buff[i] & 0x0000ff00) >> 8;
                frame[k++] = (buff[i] & 0x00ff0000) >> 16;
                frame[k++] = (buff[i] & 0xff000000) >> 24;
                }
            break;
        }
    }




// demultiplex an input buffer according to the formatter map

void demult (unsigned int *frame, int mfact, int m5afact, int *map)
    {
    int i,
        m,
        t,                          // time sample index
        frn,
        frn_1,
        k = 20000 * mfact - 1,      // index into output frame words
        n = 19999;                  // index into input frame words
    
    if (m5afact == 0)               // double-size frame in case of 64 track input
        n = 39999;
        
    while (k >= 160 * mfact)
        {
        frn   = frame[n];           // save two input frames from overwriting
        frn_1 = frame[n-1];
        for (t=mfact-1; t>=0; t--)
            {                       // for this time point, insert bits into kth word of frame
            frame[k] = 0;
            for (i=31; i>=0; i--)
                {
                frame[k] <<= 1;
                m = map[4 * i + t];
                if (m >= 0)         // m will be -2 for null source bits
                    {
                    if (i>15 || m5afact != 0)
                        frame[k] |= (frn   >> m) & 1;
                    else
                        frame[k] |= (frn_1 >> m) & 1;
                    }
                }
            k--;
            }
        n = (m5afact) ? n-1 : n-2;  // for 64 tracks, decrement input by 2 words, else by 1
        }
                                    // samples displaced by header are invalid
    while (k >= 0)    
        frame[k--] = INVALID;
    }



// multiplex a frame to get ready for mk5b output


void m5b_mux (unsigned int *frame, int formfact, int m5bfact)
    {
    int i,
        k = 0,
        n;

    
    if (m5bfact == 4)               // 4:1 multiplexing
        {
        for (n=0; n<20000 * formfact / m5bfact; n++)
            {
            if (frame[k] == INVALID)
                frame[n] = INVALID;
                 
            else
                frame[n] = (frame[k+3] & 0xff) << 24
                         | (frame[k+2] & 0xff) << 16
                         | (frame[k+1] & 0xff) <<  8
                         | (frame[k]   & 0xff);

            k += 4;
            }
        }

    else if (m5bfact == 2)           // 2:1 multiplexing
        {
        for (n=0; n<20000 * formfact / m5bfact; n++)
            {
            if (frame[k] == INVALID)
                frame[n] = INVALID;
                 
            else
                frame[n] = (frame[k+1] & 0xffff) << 16
                         | (frame[k]   & 0xffff);

            k += 2;
            }
        }
                                    // no-op for 1:1 output multiplexing
    }




// decode a mk4 header bcd time into its constituents

void decode (unsigned int *frame,
             int m5afact,
             int *yr,
             int *day,
             int *hr,
             int *min,
             int *sec,
             int *msec)
    {
    int i,
        j,
        k,
        d,
        e;

    k = (m5afact) ? 96 : 192;       // initial bit position at start of time field
    
                                    // extract bit stream from bit 0 of data words
                                    // extract year
    d = 0;
    for (i=0; i<4; i++)
        {
        d <<= 1;
        d |= frame[k++] & 1;
        if (m5afact == 0)
            k++;                    // 64 bit words
        }
    *yr = 2010 + d;

                                    // extract day of year
    e = 0;
    for (j=0; j<3; j++)
        {
        d = 0;
        for (i=0; i<4; i++)
            {
            d <<= 1;
            d |= frame[k++] & 1;
            if (m5afact == 0)
                k++;                // 64 bit words
            }
        e = 10 * e + d;
        }
    *day = e;

                                    // extract hour
    e = 0;
    for (j=0; j<2; j++)
        {
        d = 0;
        for (i=0; i<4; i++)
            {
            d <<= 1;
            d |= frame[k++] & 1;
            if (m5afact == 0)
                k++;                // 64 bit words
            }
        e = 10 * e + d;
        }
    *hr = e;

                                    // extract minute
    e = 0;
    for (j=0; j<2; j++)
        {
        d = 0;
        for (i=0; i<4; i++)
            {
            d <<= 1;
            d |= frame[k++] & 1;
            if (m5afact == 0)
                k++;                // 64 bit words
            }
        e = 10 * e + d;
        }
    *min = e;

                                    // extract second
    e = 0;
    for (j=0; j<2; j++)
        {
        d = 0;
        for (i=0; i<4; i++)
            {
            d <<= 1;
            d |= frame[k++] & 1;
            if (m5afact == 0)
                k++;                // 64 bit words
            }
        e = 10 * e + d;
        }
    *sec = e;

                                    // extract milli-seconds
    e = 0;
    for (j=0; j<3; j++)
        {
        d = 0;
        for (i=0; i<4; i++)
            {
            d <<= 1;
            d |= frame[k++] & 1;
            if (m5afact == 0)
                k++;                // 64 bit words
            }
        e = 10 * e + d;
        }
    *msec = e;
    }

    

// convert an integer of 8 or fewer digits to bcd

unsigned int bcd (int n)
    {
    int i;
    unsigned int digs = 0;

    
    for (i=0; i< 8; i++)
        {
        digs >>= 4;
        digs |= (n % 10) << 28;
        n /= 10;
        }
    return digs;
    }
                                    // calculate the Julian day number of the beginning
                                    // of the specified year (Jan 1), truncate to 3 digits
                                    // year is full 4 digit year, e.g. 2005   
                                    // note: returns 372 for 2005. (2005.1.1 = jd 2453372)
                                    // source: Wikipedia            rjc 2005.12.20
                                    // 
                                    // NOTE! The above formulation was found to be wrong
                                    // by 1 (in the sense of returning a number 1 too large)
                                    // This problem arose due to the difference between MJD
                                    // and the JDN, which differ by 1. Now, the formula
                                    // will return 371 for 2005.      rjc 2006.4.20
int jdboy (int year)
    {
    int jd, y;

    y = year + 4799;

    jd = y * 365 + y / 4 - y / 100 + y / 400 - 31739;
   
    return (jd % 1000);
    }
