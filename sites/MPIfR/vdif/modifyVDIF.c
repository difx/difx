/********************************************************************************
 **
 ** modifyVDIF v1.02
 **
 ** Usage:    modifyVDIF <options> <file.vdif> [<outputfile.vdif>]
 **
 ** Changes the information in the VDIF headers. Does not change the header
 ** size nor the payload data, but can change Endianness of the header.
 **
 ** (C) 2014 Jan Wagner
 **
 *******************************************************************************/

#define NUM_BUFFERED_FRAMES   512 // should be small to fit in CPU cache

#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <malloc.h>
#include <math.h>
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifndef _BSD_SOURCE
    #define _BSD_SOURCE
#endif
#include <endian.h>

static struct option opts[] =
{
    {"legacy",    required_argument, 0, 'L'},
    {"invalid",   required_argument, 0, 'I'},
    {"version",   required_argument, 0, 'V'},
    {"refepoch",  required_argument, 0, 'E'},
    {"complex",   required_argument, 0, 'C'},
    {"nbits",     required_argument, 0, 'b'},
    {"nchannels", required_argument, 0, 'c'},
    {"framelen",  required_argument, 0, 'l'},
    {"station",   required_argument, 0, 'S'},
    {"thread",    required_argument, 0, 'T'},
    {"extract",   required_argument, 0, 'x'},
    {"filterinv", no_argument,       0, 'i'},
    {"starttime", required_argument, 0, 't'},
    {"deltatime", required_argument, 0, 'd'},
    {"b2lendian", no_argument,       0, 'e'},
    {"conj",      no_argument,       0, 'j'},
    {0, 0, 0, 0}
};

void usage(void)
{
    printf("\n"
           "ModifyVDIF v1.02  Jan Wagner 20200224\n\n"
           "Modifies the contents and headers of VDIF files, and can copy out single threads.\n\n"
           "Usage: modifyVDIF <options> <file.vdif> [<different output file.vdif>]\n"
           "\n"
           "   Available options:\n"
           "     --legacy=[0|1]    change Legacy flag (header size is not converted!)\n"
           "     --invalid=[0|1]   change Invalid flag\n"
           "     --version=<n>     change Version field\n"
           "     --refepoch=<n>    change Reference Epoch field\n"
           "     --complex=[0|1]   change Complex flag\n"
           "     --nbits=<n>       change Bits/sample (program converts it to VDIF 'nbits-1')\n"
           "     --nchannels=<n>   change Num of Channels (program convers it to VDIF log2(nchan))\n"
           "     --framelen=<n>    change Frame length in bytes (program converts it VDIF 8-byte units)\n"
           "     --station='xx'    change 2-letter Station code\n"
           "     --thread=old,new  change thread ID (oldId1,newId1,oldId2,newId2,...)\n"
           "\n"
           "     --b2lendian       convert Big Endian \"VDIF\" headers to VDIF before other modifications\n"
           "     --extract=<n,...> copies only frames from VDIF Thread(s) with ID(s) 'n'\n"
           "     --filterinv       copies only frames not marked as Invalid\n"
           "     --starttime=<n>   shift all timestamps so that the data starts at second 'n'\n"
           "     --deltatime=<+-n> shift all timestamps by 'n' seconds\n"
           "     --conj            complex conjugate the sample data; 2-bit complex only\n"
           "\n"
           "   Specifying an option means the respective VDIF header field will be modified.\n"
           "   Other unspecified VDIF header fields will not changed.\n"
           "   And output file is required when --extract'ing a single VDIF thread.\n"
           "\n"
    );
    return;
}

#define TIME_CHANGE_NONE  0
#define TIME_CHANGE_SET   1 // shift time (seconds field) to start from new absolute second
#define TIME_CHANGE_DELTA 2 // shift time (seconds field) by a fixed amount of seconds

void complex_conjugate_payload_2bit(uint32_t* payload, size_t nword32);

int main(int argc, char** argv)
{
    int     fdi, fdo, i;
    off64_t rdpos = 0, wrpos = 0;
    ssize_t nrd, nwr;
    size_t  framesize;

    unsigned char* buf = NULL;

    /* Modification-related locals */
    // Note:
    //   "fixed" modifications via: header = (header & ~mask) | (user header & mask)
    //   "running" modifications  : current frame# or current second,
    uint32_t hdr32[4]      = {0,0,0,0};
    uint32_t hdr32_user[4] = {0,0,0,0};
    uint32_t hdr32_mask[4] = {0,0,0,0};
    char     do_time_change = TIME_CHANGE_NONE;
    uint32_t t0_orig;
    uint32_t t0_new= 0;
    char     do_endian_change = 0;
    int      extract_thread[128]; // '1' at index of threadId: extract thread, '0': skip; todo: better method not limited to thread IDs 0..127?
    char     do_extract_threads = 0;
    char     do_relabel_thread = 0;
    char     do_discard_invalid = 0;
    char     do_complex_conjugate = 0;
    const uint32_t hdr32_threadId_mask = 0x03FF << 16;
    uint32_t hdr32_threadId_new[64] = {0};
    for (i=0; i<sizeof(hdr32_threadId_new)/sizeof(hdr32_threadId_new[0]); i++)
    {
        hdr32_threadId_new[i] = ((uint32_t)i) << 16;
    }
    memset(extract_thread, 0x00, sizeof(extract_thread));

    /* Arguments */
    while (1)
    {
        int bit0, c, optidx = 0;
        uint32_t val = 0;

        c = getopt_long (argc, argv, "L:I:V:E:C:b:c:l:S:x:d:t:eij", opts, &optidx);
        if (c == -1)
        {
            break;
        }

        if (optarg)
        {
            val = atol(optarg);
        }

        switch (c)
        {
            case 0:
                printf("option %s", opts[optidx].name);
                if (optarg)
                {
                    printf(" with arg %s", optarg);
                }
                printf("\n");
                break;
            case 'L': // Legacy
                bit0 = 30;
                hdr32_mask[0] |= (1 << bit0);
                hdr32_user[0] |= (val & 1) << bit0;
                break;
            case 'I': // Invalid
                bit0 = 31;
                hdr32_mask[0] |= (1 << bit0);
                hdr32_user[0] |= (val & 1) << bit0;
                break;
            case 'V': // Version
                bit0 = 29;
                hdr32_mask[2] |= (0x7 << bit0);
                hdr32_user[2] |= (val & 0x7) << bit0;
                break;
            case 'E': // Ref Epoch
                bit0 = 24;
                hdr32_mask[1] |= (0x3F << bit0);
                hdr32_user[1] |= (val & 0x3F) << bit0;
                break;
            case 'C': // Complex
                bit0 = 31;
                hdr32_mask[3] |= (1 << bit0);
                hdr32_user[3] |= (val & 1) << bit0;
                break;
            case 'b': // Num Bits minus 1
                bit0 = 26;
                hdr32_mask[3] |= (0x1F << bit0);
                hdr32_user[3] |= ((val-1) & 0x1F) << bit0;
                break;
            case 'c': // Log2(Num Channels)
                bit0 = 24;
                val = (uint32_t)(log2((double)val));
                hdr32_mask[2] |= (0x1F << bit0);
                hdr32_user[2] |= (val & 0x1F) << bit0;
                break;
            case 'l': // Frame length in 8-byte units (header size included)
                hdr32_mask[2] |= 0x00FFFFFF;
                hdr32_user[2] |= (val/8) & 0x00FFFFFF;
                break;
            case 'S': // Station ID
                if (strlen(optarg) != 2)
                {
                    printf("Warning: station ID must be 2 characters long (got '%s')!\n", optarg);
                    continue;
                }
                hdr32_mask[3] |= 0x0000FFFF;
                hdr32_user[3] |= ((uint32_t)optarg[0]) | (((uint32_t)optarg[1]) << 8);
                break;
            case 'T': // Thread ID
                do_relabel_thread = 1;
                {
                    char *tok1, *tok2;
                    tok1 = strtok(optarg, ",");
                    tok2 = strtok(NULL, ",");
                    while ((tok1 != NULL) && (tok2 != NULL))
                    {
                        int th_old = atoi(tok1), th_new = atoi(tok2);
                        printf("tokens: %d %d\n", th_old, th_new);
                        if (th_old >= sizeof(hdr32_threadId_new)/sizeof(hdr32_threadId_new[0]))
                        {
                            printf("Warning: thread ID %d exceeds internal limit %zu!\n", th_old, sizeof(hdr32_threadId_new)/sizeof(hdr32_threadId_new[0])-1);
                            th_old = sizeof(hdr32_threadId_new)/sizeof(hdr32_threadId_new[0]) - 1;
                        }
                        hdr32_threadId_new[th_old] = ((uint32_t)th_new) << 16;
                        tok1 = strtok(NULL, ",");
                        tok2 = strtok(NULL, ",");
                    }
                }
                break;
            case 'x': // Extract VDIF Thread(s) from multi-threaded VDIF
                do_extract_threads = 1;
                {
                    char *tok;
                    tok = strtok(optarg, ",");
                    while (tok != NULL)
                    {
                        int th = atoi(tok);
                        if (th < 0 || th >= sizeof(extract_thread)/sizeof(extract_thread[0]))
                        {
                            printf("Warning: thread ID %d exceeds internal range limit 0..%zu!\n", th, sizeof(extract_thread)/sizeof(extract_thread[0])-1);
                            continue;
                        }
                        extract_thread[atoi(tok)] = 1;
                        tok = strtok(NULL, ",");
                    }
                }
                break;
            case 't': // Time shift
                t0_new = val;
                do_time_change = TIME_CHANGE_SET;
                break;
            case 'd': // Delta-Time shift
                t0_new = val;
                do_time_change = TIME_CHANGE_DELTA;
                break;
            case 'e': // Big Endian change into VDIF Little Endian
                do_endian_change = 1;
                break;
            case 'i': // Discard invalid frames
                do_discard_invalid = 1;
                break;
            case 'j': // Complex conjugate the data
                do_complex_conjugate = 1;
                break;
            default:
                usage();
                exit(-1);
        }
    }
    if (do_endian_change)
    {
        printf("Endianness change enabled and will be done before header decoding.\n");
    }

    /* Open file to modify. Create different output file, if specified. */
    if ((optind == 1) || (optind != (argc-1) && (optind != (argc-2))))
    {
        usage();
        exit(-1);
    }
    fdi = open(argv[optind], (optind == (argc-2)) ? (O_RDONLY|O_LARGEFILE) : (O_RDWR|O_LARGEFILE));
    if (fdi < 0)
    {
        printf("Error opening file '%s': %s\n", argv[optind], strerror(errno));
        exit(-1);
    }
    if (optind == (argc-2))
    {
        fdo = open(argv[optind+1], O_RDWR|O_LARGEFILE|O_CREAT, 0644);
        if (fdo < 0)
        {
            printf("Error opening output file '%s': %s\n", argv[optind+1], strerror(errno));
            exit(-1);
        }
        printf("Mode        : input file will be copied to output file with modifications\n");
    }
    else
    {
        printf("Mode        : input file will be modified on the fly\n");
        fdo = fdi;
    }

    if (do_extract_threads && (fdo==fdi))
    {
        printf("Error: --extract=<n> requires an output file to be specified!\n");
        exit(-1);
    }
    if (do_extract_threads)
    {
        printf("Extract     : threads ");
        for (i=0; i<sizeof(extract_thread)/sizeof(extract_thread[0]); i++)
        {
            if (extract_thread[i]) { printf("%d ", i); }
        }
        printf("\n");
    }

    /* Check the first frame */
    nrd = read(fdi, (void*)&hdr32, sizeof(hdr32));
    lseek64(fdi, 0, SEEK_SET);
    if (do_endian_change)
    {
        hdr32[0] = be32toh(hdr32[0]);
        hdr32[2] = be32toh(hdr32[2]);
    }
    hdr32[0]  = (hdr32[0] & ~hdr32_mask[0]) | (hdr32_user[0] & hdr32_mask[0]);
    hdr32[2]  = (hdr32[2] & ~hdr32_mask[2]) | (hdr32_user[2] & hdr32_mask[2]);

    /* Get some infos from the first frame */
    t0_orig   = (hdr32[0] & 0x3FFFFFFF);
    framesize = (hdr32[2] & 0x00FFFFFF) * 8;
    if ((framesize > 4*1024*1024) || (framesize < 1024))
    {
        printf("Frame size in first header is suspicious (%zu bytes). Quitting!\n", framesize);
        exit(-1);
    }
    buf = (unsigned char*)memalign(16, framesize*NUM_BUFFERED_FRAMES);


    /* Make a modification pass over the file */
    printf("User header : w0:%08X w1:%08X w2:%08x w3:%08X\n", hdr32_user[0], hdr32_user[1], hdr32_user[2], hdr32_user[3]);
    printf("user mask   : w0:%08X w1:%08X w2:%08x w3:%08X\n", hdr32_mask[0], hdr32_mask[1], hdr32_mask[2], hdr32_mask[3]);
    printf("Start second: %u\n", t0_orig);
    printf("Frame size  : %zu\n", framesize);
    setbuf(stdout, NULL);
    while (1)
    {
        int nframes;

        lseek64(fdi, rdpos, SEEK_SET);
        nrd = read(fdi, (void*)buf, framesize*NUM_BUFFERED_FRAMES);
        if (nrd <= 0)
        {
            break;
        }
        rdpos += nrd;

        nframes = nrd / framesize;
        for (i=0; i<nframes; i++)
        {
            uint32_t* h32 = (uint32_t*)(buf + i*framesize);

            /* Do the "fixed" modifications */
            // printf("pre: w0:%08X  w1:%08X  w2:%08X  w3:%08X\n", h32[0], h32[1], h32[2], h32[3]);
            if (do_endian_change)
            {
                h32[0] = be32toh(h32[0]);
                h32[1] = be32toh(h32[1]);
                h32[2] = be32toh(h32[2]);
                h32[3] = be32toh(h32[3]);
            }
            h32[0] = (h32[0] & ~hdr32_mask[0]) | (hdr32_user[0] & hdr32_mask[0]);
            h32[1] = (h32[1] & ~hdr32_mask[1]) | (hdr32_user[1] & hdr32_mask[1]);
            h32[2] = (h32[2] & ~hdr32_mask[2]) | (hdr32_user[2] & hdr32_mask[2]);
            h32[3] = (h32[3] & ~hdr32_mask[3]) | (hdr32_user[3] & hdr32_mask[3]);
            // printf("pst: w0:%08X  w1:%08X  w2:%08X  w3:%08X\n", h32[0], h32[1], h32[2], h32[3]);

            /* Do the "running" modifications */
            if (do_time_change != TIME_CHANGE_NONE)
            {
                if (do_time_change == TIME_CHANGE_SET)
                {
                    uint32_t T_new = (h32[0] & 0x3FFFFFFF) - t0_orig + t0_new;
                    h32[0] = (h32[0] & ~0x3FFFFFFF) | (T_new & 0x3FFFFFFF);
                }
                else if (do_time_change == TIME_CHANGE_DELTA)
                {
                    uint32_t T_new = (h32[0] & 0x3FFFFFFF) + (int32_t)t0_new;
                    h32[0] = (h32[0] & ~0x3FFFFFFF) | (T_new & 0x3FFFFFFF);
                }
            }

            if (do_relabel_thread)
            {
                uint32_t th_new = hdr32_threadId_new[ (h32[3] & hdr32_threadId_mask) >> 16 ];
                h32[3] = (h32[3] & ~hdr32_threadId_mask) | th_new;
            }

            if (do_complex_conjugate)
            {
                complex_conjugate_payload_2bit(h32 + 4, (framesize - 4*sizeof(uint32_t))/sizeof(uint32_t));
            }
        }

        lseek64(fdo, wrpos, SEEK_SET);
        if (!do_extract_threads && !do_discard_invalid)
        {
            nwr = write(fdo, (void*)buf, nrd);
            wrpos += nwr;
        }
        else
        {
            for (i=0; i<nframes; i++)
            {
                uint32_t* h32 = (uint32_t*)(buf + i*framesize);
                uint32_t  th  = (h32[3] >> 16) & 0x3FF;
                uint32_t  inv = (h32[0] >> 31) & 0x01;
                if (do_extract_threads && (extract_thread[th] != 1))
                {
                    continue;
                }
                if (do_discard_invalid && (inv != 0))
                {
                    continue;
                }
                nwr = write(fdo, (void*)h32, framesize);
                wrpos += nwr;
            }
        }

        // putchar('.');
        // printf("\33[2K\r %.0f MByte", ((double)rwpos)/(1048576.0) );
        printf("\33[2K\r %.0f GByte", ((double)rdpos)*9.31322574615479e-10 ); // 1/2^30

    }

    printf("\nDone.\n");
    return 0;
}


void complex_conjugate_payload_2bit(uint32_t* payload, size_t nword32)
{
    size_t n;
    for (n=0; n<nword32; n++)
    {
        // Byte = two complex samples = [{2-bit re, 2-bit im} {2-bit re, 2-bit im}]
        // Conjugation: invert the sign of 'im'
        // Encoding: VDIF 0b00 = -vmax, 0b01 = -1.0, 0b10 = +1.0, 0b11 = +vmax ; XOR with 0b11 flips the "sign"
        //payload[n] ^= 0b00110011001100110011001100110011; // inverts 're' instead of 'im', strange...
        payload[n] ^= 0b11001100110011001100110011001100;
    }
}

