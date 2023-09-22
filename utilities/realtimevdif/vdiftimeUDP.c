////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//
//   VDIF UDP/IP Time Stamp Printout
//   (C) 2014 Jan Wagner
//
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//
// $ vdiftimeUDP [--offset=n] [--bigendian] [--saveto=filename] [--cpu=nr]
//               [--thread=n] <port>
//
// Listens for UDP on port number <port>. At the start of every integer second
// in the VDIF data the timestamp and the current computer system time are printed.
// The program can be tied to a certain CPU, if specified.
//
// The UDP packets may contain a packet sequence number (PSN), followed by the
// actual VDIF frame. The PSN must be removed with --offset=n (e.g. --offset=8
// in case of FILA10G UDP packets).
//
////////////////////////////////////////////////////////////////////////////////////

/*
  $ gcc -Wall -O3 -msse3 -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE \
        -D_LARGEFILE64_SOURCE -D_GNU_SOURCE vdiftimeUDP.c -o vdiftimeUDP
*/

// TODO: try recvmmsg() to receive multiple UDP in one call

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sched.h>
#include <arpa/inet.h>
#include <errno.h>
#include <getopt.h>
#include <malloc.h>
#include <memory.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/fcntl.h>
#include <time.h>
#include <unistd.h>

#ifndef _BSD_SOURCE
   #define _BSD_SOURCE
#endif
#include <endian.h>

#define MAX_RECEIVE_SIZE 65507 // max. expected UDP packet size containing a VDIF frame
#define RX_TIMEOUT_SEC   5     // timeout for listening to new UDP packets (is used to print a status)
#define VDIF_HEADER_SIZE 32
#define VDIF_LEGACY_HEADER_SIZE 16

void usage(void)
{
   printf("\n"
          "Usage: vdiftimeUDP [--offset=n|-o n] [--bigendian|-b] [--saveto=filename|-s filename]\n"
          "                   [--cpu=n|-c n] [--thread=n]  <port>\n"
          "  --offset=n  : remove the first n bytes in the UDP packet (e.g., hop over a 8-byte PSN)\n"
          "  --bigendian : specify for VDIF-like frames that have a Big Endian byte order (Japanese VOA)\n"
          "  --saveto=fn : at every 1-second change, store the single recent VDIF frame into given file\n"
          "  --cpu=n     : limit the program to run on CPU core n (0-31), can help to reduce packet loss\n"
          "  --thread=n  : show only VDIF thread <n>\n"
          "  port        : port to listen on for UDP\n"
          "\n"
    );
}

static uint32_t vdif_epoch_MJDs[] = {
    // VDIF Epoch 0 = 01/01/2000 UT 0h00min = MJD 51544,   VDIF Epoch 1 is 6 months later,  etc etc
    51544, 51726, 51910, 52091, 52275, 52456, 52640, 52821, 53005, 53187, 53371, 53552, 53736,
    53917, 54101, 54282, 54466, 54648, 54832, 55013, 55197, 55378, 55562, 55743, 55927, 56109,
    56293, 56474, 56658, 56839, 57023, 57204, 57388, 57570, 57754, 57935, 58119, 58300, 58484,
    58665, 58849, 59031, 59215, 59396, 59580, 59761, 59945, 60126, 60310, 60492, 60676, 60857,
    61041, 61222, 61406, 61587, 61771, 61953, 62137, 62318, 62502, 62683, 62867, 63048, 63232,
    63414, 63598, 63779, 63963, 64144, 64328, 64509, 64693, 64875, 65059, 65240, 65424, 65605,
    65789, 65970, 66154, 66336, 66520, 66701, 66885, 67066, 67250, 67431, 67615, 67797, 67981,
    68162, 68346, 68527, 68711, 68892, 69076, 69258, 69442, 69623, 69807, 69988
};

void vdif_time_to_tm(uint8_t vdifepoch, uint32_t frame_sec, struct tm* utc_vdif)
{
    uint32_t MJD, yearMJD;
    uint32_t epdays;
    double rem;
    int doy, year, evenvdifepoch;

    if (vdifepoch >= sizeof(vdif_epoch_MJDs)/sizeof(uint32_t))
    {
        vdifepoch = 0;
    }
    evenvdifepoch = 2*((int)(vdifepoch/2)); // ref.ep. 0, 2, 4, ... are the first day of a year (01/01/20xx)

    rem = (double)frame_sec;

    epdays = (uint32_t)(rem / (24.0*60.0*60.0));
    rem = rem - epdays*24.0*60.0*60.0;

    utc_vdif->tm_hour = (int)(rem / (60.0*60.0));
    rem = rem - utc_vdif->tm_hour*60.0*60.0;

    utc_vdif->tm_min = (int)(rem / 60.0);
    rem = rem - utc_vdif->tm_min*60.0;

    utc_vdif->tm_sec = (int)rem;

    MJD     = vdif_epoch_MJDs[vdifepoch] + epdays;     // 01/01-or-06/20xx + days
    yearMJD = vdif_epoch_MJDs[evenvdifepoch];          // 01/01/20xx
    doy     = (MJD - yearMJD) + 1;
    year    = 2000 + ((int)(vdifepoch/2));             // ref.ep. 0 is 01/01/2000, ref.ep. 2 is 01/01/2001, ...

    utc_vdif->tm_year = year - 1900;  // years since 1900
    utc_vdif->tm_yday = doy - 1;      // days since Jan 01 (0-365)
}

void die(const char* fmt, const char* msg)
{
    printf(fmt, msg);
    printf("\n");
    exit(-1);
}

void realtime_init(int cpu)
{
    // Bind to CPU that gave the best
    //   taskset -cp <cpu nr> <pid>
    //   e.g.  taskset -cp 2 3939
    cpu_set_t set;
    int rc;

    CPU_ZERO(&set);
    CPU_SET(cpu, &set);
    rc = sched_setaffinity(0, sizeof(set), &set);
    if (rc < 0)
    {
       printf("sched_setaffinity: could not set CPU affinity (maybe must run as root?)\n");
    }

    return;
}

static struct option long_options[] =
    {
        {"bigendian", no_argument,       0, 'b'},
        {"offset",    required_argument, 0, 'o'},
        {"cpu",       required_argument, 0, 'c'},
        {"saveto",    required_argument, 0, 's'},
        {"thread",    required_argument, 0, 't'},
        {0, 0, 0, 0}
    };


int main(int argc, char** argv)
{
   int c;
   int sd, fd=0;

   /* Args */
   int udp_vdif_offset = 0;
   int udp_port;
   int thread_to_show = -1;
   int header_is_bigendian = 0;

   /* Data frame buffer */
   unsigned char*  udp_frame;
   unsigned char*  vdif_frame;
   uint32_t*       vdif_w0;
   uint32_t*       vdif_w1;
   uint32_t*       vdif_w2;
   uint32_t*       vdif_w3;
   struct timeval  tv_prev, tv_curr, tv_timeout;

   /* Network */
   struct sockaddr_storage src_addr;
   socklen_t src_addr_len = sizeof(src_addr);
   struct addrinfo  hints;
   struct addrinfo* res = 0;

   size_t   framebytes_in_sec = 0;
   uint32_t frames_in_sec = 0;
   uint32_t maxframenr_in_sec = 0;
   uint32_t prev_frame_sec = 0;
   int      sec_count4printout = 0;
   int      threads_nr = 1;

   /* Command Line Arguments */
   while (1)
   {
      int option_index;
      c = getopt_long(argc, argv, "bo:s:c:t:", long_options, &option_index);
      if (c == -1) { break; }

      switch (c)
      {
           case 'b':
              header_is_bigendian = 1;
              break;
           case 'o':
              udp_vdif_offset = atoi(optarg);
              break;
           case 's':
              fd = open(optarg, O_WRONLY|O_CREAT|O_DSYNC, S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH);
              break;
           case 'c':
              realtime_init(atoi(optarg));
              break;
           case 't':
              thread_to_show = atoi(optarg);
              break;
           default:
              usage();
              return -1;
      }
   }

   // Main args:  ip   port   Mbps
   if ((argc - optind) != 1)
   {
       usage();
       return -1;
   }
   udp_port = atoi(argv[optind]);

   /* Allocate buffer for single UDP frame with VDIF */
   udp_frame  = (unsigned char*)memalign(128, MAX_RECEIVE_SIZE);
   vdif_frame = udp_frame + udp_vdif_offset;
   vdif_w0 = (uint32_t*)(vdif_frame + 0);
   vdif_w1 = (uint32_t*)(vdif_frame + 4);
   vdif_w2 = (uint32_t*)(vdif_frame + 8);
   vdif_w3 = (uint32_t*)(vdif_frame + 12);
   memset(udp_frame, 0x00, MAX_RECEIVE_SIZE);

   /* Receive socket */
   memset(&hints,0,sizeof(hints));
   hints.ai_family   = AF_UNSPEC;
   hints.ai_socktype = SOCK_DGRAM;
   hints.ai_protocol = 0;
   hints.ai_flags    = AI_PASSIVE | AI_ADDRCONFIG;
   getaddrinfo(NULL, argv[optind], &hints, &res);

   sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
   // sd = socket(PF_INET, SOCK_DGRAM, 0);
   if (bind(sd, res->ai_addr, res->ai_addrlen) == -1)
   {
      die("%s", strerror(errno));
   }
   freeaddrinfo(res);

   tv_timeout.tv_sec  = RX_TIMEOUT_SEC;
   tv_timeout.tv_usec = 0;
   setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, (char*)&tv_timeout, sizeof(struct timeval));

   c = 64*1024*1024;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, &c, sizeof(c));

   /* Receiving loop */
   gettimeofday(&tv_prev, NULL);
   while (1)
   {
      ssize_t nrd;
      uint32_t frame_no, frame_sec;
      int frame_thread_id;

      nrd = recvfrom(sd, udp_frame, MAX_RECEIVE_SIZE, 0, (struct sockaddr*)&src_addr, &src_addr_len);
      if (nrd == -1)
      {
          if (errno == EAGAIN)
          {
              printf("No UDP packets received in the last %d seconds...\n", RX_TIMEOUT_SEC);
              maxframenr_in_sec = 0;
              framebytes_in_sec = 0;
              continue;
          }
          else
          {
              die("%s", strerror(errno));
          }
      }

      if (nrd < (32 + udp_vdif_offset))
      {
         char host[NI_MAXHOST], service[NI_MAXSERV];
         int s = getnameinfo((struct sockaddr *)&src_addr,
                        src_addr_len, host, NI_MAXHOST,
                        service, NI_MAXSERV, NI_NUMERICSERV);
         if (s == 0)
         {
             printf("short UDP of %ld bytes from %s:%s\n", (long)nrd, host, service);
         }
         else
         {
             printf("short UDP of %ld bytes from unknown source (getnameinfo: %s)\n", (long)nrd, gai_strerror(s));
         }
         continue;
      }

      // Endian conversion: VDIF is Little Endian, host might be different,
      // and the japanese "VDIF" is mistakenly Big Endian
      if (header_is_bigendian)
      {
          *vdif_w0 = be32toh(*vdif_w0);
          *vdif_w1 = be32toh(*vdif_w1);
          *vdif_w2 = be32toh(*vdif_w2);
          *vdif_w3 = be32toh(*vdif_w3);
      }
      else
      {
          *vdif_w0 = le32toh(*vdif_w0); // no-op on Intel arch
          *vdif_w1 = le32toh(*vdif_w1);
          *vdif_w2 = le32toh(*vdif_w2);
          *vdif_w3 = le32toh(*vdif_w3);
      }

      // Timestamp of the VDIF frame
      frame_sec = (*vdif_w0) & 0x3FFFFFFF;
      frame_no  = (*vdif_w1) & 0x00FFFFFF;
      frame_thread_id = ((*vdif_w3) >> 16) & 0x3F;
      if (frame_thread_id > threads_nr) 
      { 
          threads_nr = frame_thread_id; 
      }

      // Single-thread
      if ((thread_to_show != -1) && (frame_thread_id != thread_to_show)) { continue; }

      // Bookkeeping
      framebytes_in_sec += nrd;
      framebytes_in_sec -= udp_vdif_offset;
      framebytes_in_sec -= (vdif_frame[0]&0x40) ? VDIF_LEGACY_HEADER_SIZE : VDIF_HEADER_SIZE;
      maxframenr_in_sec = (frame_no > maxframenr_in_sec) ? frame_no : maxframenr_in_sec;
      frames_in_sec++;

      // Compare newest VDIF timestamp and current computer time
      if (frame_sec != prev_frame_sec)
      {
         time_t    now;
         struct tm utc_pc;
         struct tm utc_vdif;
         double    dT_pc, diffT;
         int       vdifepoch = ((*vdif_w1) >> 24) & 0x3F;

         time(&now);
         gettimeofday(&tv_curr, NULL);
         gmtime_r(&now, &utc_pc);

         vdif_time_to_tm(vdifepoch, frame_sec, &utc_vdif);

         diffT = ((double)utc_vdif.tm_sec + 60.0*utc_vdif.tm_min + 3600.0*utc_vdif.tm_hour)
                -(1e-6*tv_curr.tv_usec + (double)utc_pc.tm_sec + 60.0*utc_pc.tm_min + 3600.0*utc_pc.tm_hour);

         dT_pc = (tv_curr.tv_sec - tv_prev.tv_sec) + 1e-6*(tv_curr.tv_usec - tv_prev.tv_usec);

         // Show a table header every once in a while
         if (sec_count4printout == 0 || sec_count4printout >= 40)
         {
             sec_count4printout = 0;
           //printf("VDIF frame#0      2014y286doy 03:28:05 : PC 2014y286doy 03:28:03.99 :  off +3.0s : 755470/800000 fps : 4x8192.00 Mbps : 7832.71 Mbps : 7832.04 Mbps\n");
             printf("----- VDIF Time ------------------------------ Computer Time ---------Time Delta----Frames total/max-----Rate peak------Rate nominal----Rate actual---\n");

         }

         // Print all available time and data rate information
         printf("VDIF frame#%-6u %04uy%03udoy %02d:%02d:%02d : PC %04dy%03ddoy %02d:%02d:%05.02f : ",
              frame_no,
              utc_vdif.tm_year+1900, // years since 1900
              utc_vdif.tm_yday+1,    // days since Jan 01 (0-365), yday=0 means Jan 01 or DOY 1
              utc_vdif.tm_hour, utc_vdif.tm_min, utc_vdif.tm_sec,
              utc_pc.tm_year+1900,
              utc_pc.tm_yday+1,
              utc_pc.tm_hour, utc_pc.tm_min, ((double)utc_pc.tm_sec) + 1e-6*tv_curr.tv_usec
         );
         if ((diffT >= -3600.0) || (diffT <= +3600.0))
         {
              printf(" off %+.3fs : ", diffT);
         }
         else
         {
              printf(" off>1h : ");
         }
         printf("%u/%u fps : %ux%7.2f Mbps : %7.2f Mbps : %7.2f Mbps\n",
              frames_in_sec,
              maxframenr_in_sec + 1,
              threads_nr,
              8e-6*((double)(maxframenr_in_sec + 1)*(double)(nrd-udp_vdif_offset-32)),
              8e-6*((double)framebytes_in_sec),
              8e-6*((double)framebytes_in_sec)/dT_pc
         );

         frames_in_sec = 0;
         framebytes_in_sec = 0;
         tv_prev = tv_curr;
         prev_frame_sec = frame_sec;
         sec_count4printout++;

         maxframenr_in_sec = 0;

         if (fd > 0)
         {
            lseek(fd, 0, SEEK_SET);
            write(fd, (void*)vdif_frame, nrd-udp_vdif_offset);
         }
      }

   }

   return 0;
}

