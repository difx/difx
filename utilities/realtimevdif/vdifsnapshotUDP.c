////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//
//   VDIF UDP/IP Snapshot in burst mode
//   (C) 2014 Jan Wagner
//
////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////
//
// $ vdifsnapshotUDP [--cpu=<0..31>] [--offset=<n bytes to skip in UDP>]
//                   <size in Mbyte> <port> <output file.vdif>
//
// Listens for UDP on port number <port>.
// The program can be tied to a certain CPU, if specified.
//
// The UDP packets may contain a packet sequence number (PSN), followed by the
// actual VDIF frame. The PSN must be removed with --offset=n (e.g. --offset=8
// in case of FILA10G UDP packets).
//
////////////////////////////////////////////////////////////////////////////////////

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <arpa/inet.h>
#include <errno.h>
#include <getopt.h>
#include <malloc.h>
#include <memory.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sched.h>
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

#ifdef HAVE_MPI
#include <mpi.h>
#endif

#define RX_TIMEOUT_SEC       10
#define RX_SOCKET_BUFSIZE_MB 64
#define MAX_RECEIVE_SIZE     65507
#define MIN_RECEIVE_SIZE     1000
#define TRIM_DURING_CAPTURE  1

void usage(void)
{
#ifdef HAVE_MPI
    printf("Usage: mpirun -np N -host host1,host2,..,hostN vdifsnapshotUDP [--cpu=<0..31>]\n"
           "           [--offset=<n bytes to skip in UDP>] <size in Mbyte>\n"
           "           <udpport_host1:udpport_host2:...:udpport_hostN> <output file.vdif>\n\n");
#else
    printf("Usage: vdifsnapshotUDP [--cpu=<0..31>] [--offset=<n bytes to skip in UDP>]\n"
           "                       <size in Mbyte> <port> <output file.vdif>\n\n");
#endif
}

void die(const char* fmt, const char* msg)
{
    printf(fmt, msg);
    printf("\n");
    exit(-1);
}

void realtime_init(int cpu)
{
    cpu_set_t set;
    int rc;

    CPU_ZERO(&set);
    CPU_SET(cpu, &set);
    rc = sched_setaffinity(0, sizeof(set), &set);
    if (rc < 0)
    {
       printf("sched_setaffinity: could not set CPU affinity (maybe must run as root)?\n");
    }
    else
    {
       printf("Bound to CPU#%d\n", cpu);
    }

    return;
}

int open_udp(const int port)
{
   int sd, c;
   struct addrinfo  hints;
   struct addrinfo* res = 0;
   struct timeval tv_timeout;
   char port_str[80];

   snprintf(port_str, sizeof(port_str)-1, "%d", port);

   memset(&hints,0,sizeof(hints));
   hints.ai_family   = AF_UNSPEC;
   hints.ai_socktype = SOCK_DGRAM;
   hints.ai_protocol = 0;
   hints.ai_flags    = AI_PASSIVE | AI_ADDRCONFIG;
   getaddrinfo(NULL, port_str, &hints, &res);

   sd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
   if (bind(sd, res->ai_addr, res->ai_addrlen) == -1)
   {
      die("%s", strerror(errno));
   }
   freeaddrinfo(res);

   tv_timeout.tv_sec  = RX_TIMEOUT_SEC;
   tv_timeout.tv_usec = 0;
   setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, (char*)&tv_timeout, sizeof(struct timeval));

   c = RX_SOCKET_BUFSIZE_MB*1024*1024;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, &c, sizeof(c));

   return sd;
}


static struct option long_options[] =
    {
        {"offset",    required_argument, 0, 'o'},
        {"cpu",       required_argument, 0, 'c'},
        {0, 0, 0, 0}
    };


int main(int argc, char** argv)
{
   size_t bufsize;
   size_t bufsize_MB;
   size_t buffilled;
   size_t bufmax;
   int    nskipfront = 0;
   int    fd, sd, c, port;
   char*  filename;
   char*  port_str;
   char*  buf = NULL;
   char*  wrbuf;
   ssize_t nrd, nwr;
   char*  minibuf = NULL;
   size_t framecount = 0;
#ifdef HAVE_MPI
   MPI_Comm mpicomm = MPI_COMM_WORLD;
   char   mpiprocessorname[MPI_MAX_PROCESSOR_NAME];
   int    mpinamelen;
   int    mpirank, n;
   char   filesuffix[50];
   char*  tok;
#endif

   /* Command Line Arguments */
   while (1)
   {
      int option_index;
      c = getopt_long(argc, argv, "o:c:", long_options, &option_index);
      if (c == -1) { break; }
      switch (c)
      {
          case 'o':
	      nskipfront = atoi(optarg);
              break;
          case 'c':
              realtime_init(atoi(optarg));
              break;
          default:
              usage();
              return -1;
      }
   }

   if ((argc - optind) != 3)
   {
       usage();
       return -1;
   }

   bufsize_MB = atol(argv[optind++]);
   port_str   = strdup(argv[optind++]);
   filename   = strdup(argv[optind++]);

   bufsize   = bufsize_MB * 1048576;
   buffilled = 0;
   bufmax    = bufsize - MAX_RECEIVE_SIZE;
   // buf       = malloc(bufsize);
   // minibuf   = malloc(nskipfront);
   posix_memalign((void**)&buf, 16, bufsize);
   posix_memalign((void**)&minibuf, 16, nskipfront);
   wrbuf     = buf;
   if (!buf)
   {
      printf("Could not allocate %zu bytes.\n", bufsize);
      return -1;
   }
   if (!minibuf)
   {
      printf("Could not allocate %d bytes.\n", nskipfront);
      return -1;
   }

#ifdef HAVE_MPI
   MPI_Init(&argc, &argv);
   MPI_Comm_rank(mpicomm, &mpirank);
   MPI_Get_processor_name(mpiprocessorname, &mpinamelen);
   n = mpirank;
   tok = strtok(port_str, ":");
   while (tok != NULL && (n > 0))
   {
      tok = strtok(NULL, ":");
      n--;
   }
   port_str = tok;
   snprintf(filesuffix, sizeof(filesuffix)-1, ".%s.%d", mpiprocessorname, atoi(port_str));
   filename = strcat(filename, filesuffix);
   printf("rank %d got port %s, using output file %s\n", mpirank, port_str, filename);
#endif

   port = atoi(port_str);
   sd = open_udp(port);
   if (sd == -1)
   {
      printf("Could not create UDP listener socket.\n");
      return -1;
   }

   fd = open(filename, O_CREAT|O_TRUNC|O_WRONLY, S_IWUSR|S_IRUSR|S_IRGRP);
   if (fd == -1)
   {
      printf("Could not open/create file %s.\n", filename);
      die("%s", strerror(errno));
   }

   setbuf(stdout, NULL);

#if HAVE_MPI
   MPI_Barrier(mpicomm);
#endif

   // Capture to memory
   printf("Capturing %zu MByte into memory", bufsize_MB);
   while (buffilled < bufmax)
   {
      nrd = recv(sd, wrbuf, MAX_RECEIVE_SIZE, 0);
      if (nrd == -1)
      {
          if (errno == EAGAIN)
          {
              printf("\nNo UDP packets received in the last %d seconds...\n", RX_TIMEOUT_SEC);
          }
          else
          {
              die("\n%s", strerror(errno));
          }
          break;
      }

      if (nrd < MIN_RECEIVE_SIZE)
      {
          putchar('x');
          continue;
      }

#if TRIM_DURING_CAPTURE
      if (nskipfront > 0) // assumes nrd>nskipfront !
      {
          if (buffilled == 0)
          {
              // first packet: discard the first N bytes via 'large' mem move
              memmove(wrbuf, wrbuf+nskipfront, nrd-nskipfront);
              wrbuf -= nskipfront;
          }
          else
          {
              // restore the 'small' data overwritten by previous recvfrom()
              memcpy(wrbuf, minibuf, nskipfront);
          }
          wrbuf     += (nrd-nskipfront);
          buffilled += (nrd-nskipfront);

          // back up end-of-frame data to be overwritten by next recvfrom()
          memcpy(minibuf, wrbuf, nskipfront);
      }
      else
      {
          wrbuf += nrd;
          buffilled += nrd;
      }
#else // !TRIM_DURING_CAPTURE
      wrbuf += nrd;
      buffilled += nrd;
#endif

      framecount++;
      if ((framecount % 10000) == 0) { putchar('.'); }
   }
   putchar('\n');

   // Write to file
   if (buffilled > 0)
   {
      printf("Memory capture ended, writing %zu bytes to file...\n", buffilled);
#if TRIM_DURING_CAPTURE
      while (buffilled > 0)
      {
          nwr = write(fd, buf, buffilled);
          printf("Wrote %zu bytes, now %zu bytes left.\n", nwr, buffilled-nwr);
          if (nwr < 0)
          {
             die("%s", strerror(errno));
          }
          buffilled -= nwr;
          buf += nwr;
      }
#else
      // use the last 'nrd' from recv() as the frame size
      wrbuf = buf;
      while (buffilled > 0)
      {
         nwr = write(fd, wrbuf+nskipfront, nrd-nskipfront);
         wrbuf += nrd;
         buffilled -= nrd;
      }
#endif
   }
   else
   {
      printf("Memory capture ended, no data received.\n");
   }

#if HAVE_MPI
   MPI_Finalize();
#endif

   return 0;
}
