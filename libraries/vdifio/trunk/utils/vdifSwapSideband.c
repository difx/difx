#ifdef __APPLE__

#define OSX

#define OPENREADOPTIONS O_RDONLY
#define OPENWRITEOPTIONS O_WRONLY|O_CREAT|O_TRUNC

#else

#define LINUX
#define _LARGEFILE_SOURCE 
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#define OPENREADOPTIONS O_RDONLY|O_LARGEFILE
#define OPENWRITEOPTIONS O_WRONLY|O_CREAT|O_TRUNC|O_LARGEFILE

#endif

#define BUFSIZE 10   //  MB
#define MAXCHAN 32
#define MAXSTR 256

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <strings.h>
#include <math.h>
#include <getopt.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include "vdifio.h"
#include "config.h"

#if HAVE_IPP
#include <ippcore.h>
#include <ipps.h>


#define IPPMALLOC(var,type,n)                                  \
  var = ippsMalloc_ ## type(n);                                \
  if (var==NULL) {                                             \
    fprintf(stderr, "Error allocating %d values for %s at line %d\n", n, #var, __LINE__); \
    exit(EXIT_FAILURE);                                        \
  }

#define MALLOC_8U(var, n) IPPMALLOC(var, 8u, n)

#define ZERO_8U(dest, length)                   ippsZero_8u(dest, length)
#define MEMSET_8U(dest, val, length)            ippsSet_8u(val, dest, length)
#define MEMCOPY(src, dest, length)              ippsCopy_8u(src, dest, length)
#define XORBUF(src, mask, length)               ippsXor_32u_I((Ipp32u*)mask, (Ipp32u*)src, length/4);

#else

typedef uint8_t   Ipp8u;
typedef uint16_t  Ipp16u;
typedef uint32_t  Ipp32u;
typedef int8_t    Ipp8s;
typedef int16_t   Ipp16s;
typedef int32_t   Ipp32s;
typedef float     Ipp32f;
typedef int64_t   Ipp64s;
typedef uint64_t  Ipp64u;
typedef double    Ipp64f;

//typedef float 32f;
//typedef uint8_t 8u;

#define MALLOC(var,type,n)                                  \
  var = malloc(n*sizeof(type));				       \
  if (var==NULL) {                                             \
    fprintf(stderr, "Error allocating %d bytes for %s at line %d\n", n*sizeof(type), #var, __LINE__); \
    exit(EXIT_FAILURE);                                        \
  }

#define MEMSET_8U(dest, val, length)                 memset(dest, val, length)
#define ZERO_8U(dest, length)                        memset(dest, 0, length)
#define MEMCOPY(src, dest, length)                   memcpy(dest, src, length)

#endif

void MEMSET_64U(Ipp64u *src, Ipp64u val, int length) {
  for (int i=0;i<length;i++) src[i] = val;
}

#define DEBUG(x) 

void kill_signal (int);
double tim(void);
int encodeShift(int *channels, int bits, int *shift);

int extract4chan32_2bit(char *in, char *out, int nbytes, int *shift);

// Globals needed for signal handling
volatile int time_to_quit = 0;
volatile int sig_received = 0;

const char program[] = "vdifSwapSideband";
const char author[]  = "Chris Phillips <Chris.Phillips@csiro.au>";
const char version[] = "0.1";
const char verdate[] = "20200803";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
  fprintf(stderr, "A program select a subset of channels from a VDIF file. Assumes all threads have the same # channels\n");
  fprintf(stderr, "\nUsage: %s -o <Output directory> <VDIF input file> [<VDIF output file> ...]\n", program);
  fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
  fprintf(stderr, "\n<Output directory> is the name of a directory to write all the files to\n");
  fprintf(stderr, "\nOptions:\n");
  fprintf(stderr, "\n-skip <bytes>    Skip <bytes> bytes a the start of each file\n");
}

int main (int argc, char * const argv[]) {
  int framesize=0, nfile, infile, outfile, opt, frameperbuf, nframe, i;
  int nread, status, nwrote, tmp, nchan=0, bits, isComplex=0, legacy=0, headersize=0, datasize=0;
  int first, bufsize=0;
  float ftmp;
  double t0;

  Ipp8u *buf, *mask;
  char  *slashptr, msg[MAXSTR+50];
  vdif_header *header;
  unsigned long long totalsent;

  int memsize = BUFSIZE;
  int offset = 0;
  int concat = 0;
  char outname[MAXSTR+1] = "";
  char outdir[MAXSTR+1] = "";

  struct option options[] = {
    {"outdir", 1, 0, 'o'},
    {"dir", 1, 0, 'o'},
    {"offset", 1, 0, 's'},
    {"skip", 1, 0, 's'},
    {"outfile", 1, 0, 'f'},
    {"concat", 0, 0, 'c'},
    {"bufsize", 1, 0, 'm'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  first = 1;
  outfile = 0;
  
#define CASEINT(ch,var)                                     \
  case ch:                                                  \
    status = sscanf(optarg, "%d", &tmp);                    \
    if (status!=1)                                          \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = tmp;                                            \
    break

#define CASEBOOL(ch,var)                                    \
  case ch:                                                  \
    var = 1;                                                \
    break  
  
  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "hp:o:f:H:", options, NULL);
    if (opt==EOF) break;

    switch (opt) {

      CASEINT('s', offset);
      CASEINT('m', memsize);
      CASEBOOL('c', concat);

    case 'o':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Outdir too long\n");
	return(1);
      }
      strcpy(outdir, optarg);
      if (outdir[strlen(outdir)-1] == '/')  {// Remove trailing "/"
	outdir[strlen(outdir)-1] = 0;
      }
      break;

    case 'f':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Outfile too long\n");
	return(1);
      }
      strcpy(outname, optarg);
      break;

    case 'h':
      usage();
      return(1);
    break;
    
    case '?':
    default:
      ;
    }
  }

  memsize *= 1024*1024;

  if (strlen(outdir)==0) {
    fprintf(stderr, "Must supply output dirtectory - aborting\n");
    return 1;
  }

  // Malloc enough data for one frame first
  MALLOC_8U(buf, VDIF_LEGACY_HEADER_BYTES);
  header = (vdif_header*)buf;

  if (strlen(outdir)>0) {
    printf("Writing data to %s\n", outdir);
  }

  /* Install a ^C catcher */
  signal (SIGINT, kill_signal);

  t0 = tim(); /* So we can time average write per file */
  totalsent = 0;
  
  first = 1;
  for (nfile=optind; nfile<argc; nfile++) {

    infile = open(argv[nfile], OPENREADOPTIONS);
    if (infile==-1) {
      sprintf(msg, "Failed to open input file (%s)", argv[nfile]);
      perror(msg);
      continue;
    }
    printf("Reading %s\n", argv[nfile]);

    if (offset>0) {
      off_t ss = lseek(infile, offset, SEEK_SET);
      if (ss<0) {
	sprintf(msg, "Trying to skip %d bytes at start of file", offset);
	perror(msg);
	close(infile);
	return(1);
      } else if (ss!=offset) {
	fprintf(stderr, "Could not skip %d bytes at start of file - aborting", offset);
	close(infile);
	return(1);
      }
    } else {
      offset = 0; // To be sure
    }
    

    if (first) {
      // Read first header
      nread = read(infile, buf, VDIF_LEGACY_HEADER_BYTES);
      if (nread==0) {  // EOF
	perror("Empty file - aborting\n");
	close(infile);
	break;
      } else if (nread==-1) {
	perror("Error reading file - aborting\n");
	close(infile);
	break;
      } else if (nread<VDIF_LEGACY_HEADER_BYTES) {
	perror("Header size too small - is this VDIF? Aborting\n");
	close(infile);
	break;
      }
      lseek(infile, offset, SEEK_SET); // Reset file
      
      nchan = getVDIFNumChannels(header);
      bits = getVDIFBitsPerSample(header);
      isComplex = getVDIFComplex(header);
      legacy = getVDIFLegacy(header);
      headersize = getVDIFHeaderBytes(header);
      framesize = getVDIFFrameBytes(header);
      datasize = framesize-headersize;
      //if (isComplex) bits *=2;

      free(buf);
      buf = NULL;

      frameperbuf = memsize/framesize;
      bufsize = frameperbuf*framesize;
      if (frameperbuf==0) {
	fprintf(stderr, "Large framesize (%d) not supported - aborting\n", framesize);
	close(infile);
	break;
      }
      MALLOC_8U(buf, bufsize);

      if (bits==2) {  // Need to create Mask
	MALLOC_8U(mask, bufsize);
	ZERO_8U(mask, bufsize);
	if (isComplex) { // Invert each complex sample
	  uint8_t  submask = 0xCC;
	  MEMSET_8U(mask+headersize, submask, datasize);
	} else {
	  Ipp64u subsubmask, submask;
	  int sampPerWord = 64/2/nchan;
	  if (nchan>16) {
	    fprintf(stderr, "Error: Do not support nchan=%d (line %d). Aborting\n", nchan, __LINE__);
	    exit(EXIT_FAILURE);
	    
	  }
	  subsubmask = (1<<(nchan*2))-1;  // Mask for bits
	  subsubmask <<= nchan*2;        // Shift it up for second time sample(s)
	  for (int i=0; i<sampPerWord/2; i++) {
	    submask |= subsubmask<<(nchan*2*i*2);
	  }
	  MEMSET_64U((Ipp64u*)(mask+headersize), submask, datasize/8); // Set whole data frame to to this mask

	  // Now duplicate frame to whole buffer, skipping headers
	  for (int i=1; i<frameperbuf; i++) {
	    MEMCOPY(mask+headersize, mask+headersize+framesize*i, datasize);
	  }
	}
      } else {
	fprintf(stderr, "Error: Do not support %d bits at line %d\n", bits, __LINE__);
	exit(EXIT_FAILURE);
      }
      
      first = 0;
    }

    // Output file name
      
    // File name contained in buffer
    slashptr = rindex(argv[nfile], '/');
      
    if (slashptr==NULL) { // Simple filename
      if (strlen(outdir)+strlen(outname)+1 > MAXSTR) {
	fprintf(stderr, "%s/%s too long. Increase \"MAXSTR(%d)\"\n", 
		outdir, outname, MAXSTR);
	return(1);
      }
      sprintf(outname, "%s/%s", outdir, argv[nfile]);
    } else {// File contains a "/"
      slashptr++;
      if (strlen(outdir)+strlen(slashptr)+1 > MAXSTR) {
	fprintf(stderr, "%s/%s too long. Increase \"MAXSTR(%d)\"\n", outdir, slashptr, MAXSTR);
	return(1);
      }
      sprintf(outname, "%s/%s", outdir, slashptr);
    }
  
    outfile = open(outname, OPENWRITEOPTIONS, S_IRWXU|S_IRWXG|S_IRWXO); 
    if (outfile==-1) {
      sprintf(msg, "Failed to open output file (%s)", outname);
      perror(msg);
      continue;
    }
    
    // Loop until EOF
    while (1) {

      if (time_to_quit) break;

      nread = read(infile, buf, bufsize);
      if (nread==0) {  // EOF
	break;
      } else if (nread==-1) {
	perror("Error reading file");
	time_to_quit = 1;
	break;
      } else if (nread%framesize!=0) {
	fprintf(stderr, "Warning: Read partial frame (%d/%d)\n\n", nread, framesize);
	nread = (nread / framesize) * framesize;
      }

      // Check headers
      header = (vdif_header*)buf;
      nframe = nread/framesize;
      for (i=0; i<nframe; i++) {
	if (nchan     != getVDIFNumChannels(header) ||
	    bits      != getVDIFBitsPerSample(header) ||
	    isComplex != getVDIFComplex(header) ||
	    legacy    != getVDIFLegacy(header) ||
	    framesize != getVDIFFrameBytes(header)) {
	  fprintf(stderr, "Error: VDIF frame parameters have changed - aborting\n");
	  time_to_quit = 1;
	  break;
	}
	header = (vdif_header*)((char*)header+framesize);
      }
      if (time_to_quit) break;

      if (bits==2) { // Sign change simple XOR operation. Use precomputed mask
	XORBUF(buf, mask, nframe*framesize);
      } else {
	fprintf(stderr, "Error: Do not support %d bits at line %d\n", bits, __LINE__);
	exit(EXIT_FAILURE);
      }
      // Write data
      nwrote = write(outfile, buf, nframe*framesize);
      if (nwrote==-1) {
	perror("Error writing outfile");
	time_to_quit = 1;
	break;
      } else if (nwrote!=nframe*framesize) {
	fprintf(stderr, "Warning: Did not write all bytes! (%d/%d)\n", nwrote, nframe*framesize);
	time_to_quit = 1;
	break;
      }
    
    } // Loop over input file

    /* Close the file */
    status = close(infile);
    if (status!=0) {
      perror("Error closing input file");
    }
    status = close(outfile);
    if (status!=0) {
      perror("Error closing output file");
    }

    if (time_to_quit) break;
  }

  /* A signal may have told us to quit. Raise this signal with the default
     handling */
    signal (sig_received, SIG_DFL);
  if (time_to_quit) raise (sig_received);
  
  return(0);
}

void kill_signal (int sig) {
  /* We may get called twice */
  if (time_to_quit) {
    fprintf(stderr, "kill_signal called second time\n");
    return;
  }
  time_to_quit = 1;
  sig_received = sig;

  signal (sig, kill_signal); /* Re-install ourselves to disable double signals */
}  

double tim(void) {
  struct timeval tv;
  double t;

  gettimeofday(&tv, NULL);
  t = (double)tv.tv_sec + (double)tv.tv_usec/1000000.0;

  return t;
}


