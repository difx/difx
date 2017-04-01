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

#define BUFSIZE 2000   // 2 MB
#define MAXCHAN 32
#define MAXSTR 256

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <strings.h>
#include <math.h>
#include <arpa/inet.h>
#include <getopt.h>
#include <netdb.h>  
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include "vdifio.h"

#define DEBUG(x) 

void kill_signal (int);
double tim(void);
int encodeShift(int *channels, int bits, int *shift);

int extract4chan32_2bit(char *in, char *out, int nbytes, int *shift);

// Globals needed for signal handling
volatile int time_to_quit = 0;
volatile int sig_received = 0;

const char program[] = "vdifChanSelect";
const char author[]  = "Chris Phillips <Chris.Phillips@csiro.au>";
const char version[] = "0.1";
const char verdate[] = "20150305";

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
  int framesize=0, nfile, infile, outfile, rate, opt, frameperbuf, nframe, i;
  int nread, status, nwrote, tmp, nchan=0, bits, isComplex=0, legacy=0, headersize=0, datasize=0;
  int nextract=0, first, sock, bufsize=0, odatasize=0, oframesize=0, obufsize, oheadersize=0;
  float ftmp;
  double t0;
  char outname[MAXSTR+1] = "";
  char outdir[MAXSTR+1] = "";

  char *buf, *obuf=0, *pin, *pout, *slashptr, msg[MAXSTR+50];
  vdif_header *header;
  unsigned long long totalsent;
  
  int offset = 0;
  int outlegacy = 0;
  int concat = 0;
  int net = 0;
  int server = 0;
  int port = 52100;     /* TCP port to use */
  int window_size = -1;	
  char hostname[MAXSTR+1] = ""; /* Host name to send data to */
  int (*extract)(char *in, char *out, int nbytes, int *shift);
  int channels[] = {0,1,2,3,-1};
  int shift[MAXCHAN]; // Hardcode for the moment

  struct option options[] = {
    {"outdir", 1, 0, 'o'},
    {"dir", 1, 0, 'o'},
    {"offset", 1, 0, 's'},
    {"skip", 1, 0, 's'},
    {"legacy", 0, 0, 'l'},
    {"outfile", 1, 0, 'f'},
    {"port", 1, 0, 'p'},
    {"host", 1, 0, 'H'},
    {"hostname", 1, 0, 'H'},
    {"window", 1, 0, 'w'},
    {"server", 0, 0, 'S'},
    {"concat", 0, 0, 'c'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  first = 1;
  outfile = 0;
  extract = NULL;
  
  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "hp:o:f:H:", options, NULL);
    if (opt==EOF) break;

    switch (opt) {

    case 's':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad offset value %s\n", optarg);
      else {
	offset = tmp;
      }
      break;

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

    case 'l':
      outlegacy = 1;
      break;

    case 'f':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Outfile too long\n");
	return(1);
      }
      strcpy(outname, optarg);
      break;

    case 'w':
      status = sscanf(optarg, "%f", &ftmp);
      if (status!=1)
	fprintf(stderr, "Bad window option %s\n", optarg);
      else 
	window_size = ftmp * 1024;
     break;
     
    case 'p':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad port option %s\n", optarg);
      else {
	port = tmp;
      }
      net = 1;
      break;
      
    case 'H':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Hostname too long\n");
	return(1);
      }
      strcpy(hostname, optarg);
      net = 1;
      break;

    case 'S':
      server = 1;
      net = 1;
      break;

    case 'c':
      concat = 1;
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

  if (strlen(outdir)==0) {
    fprintf(stderr, "Must supply output dirtectory - aborting\n");
    return 1;
  }

  // Malloc enough data for one frame first
  buf = malloc(VDIF_HEADER_BYTES);
  if (buf==NULL) {
    sprintf(msg, "Trying to allocate %d bytes", VDIF_HEADER_BYTES);
    perror(msg);
    return(1);
  }
  header = (vdif_header*)buf;

  if (strlen(hostname) == 0) {
    strcpy(hostname, "localhost");
  }

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
      if (isComplex) bits *=2;
      if (legacy && outlegacy)
	oheadersize = VDIF_LEGACY_HEADER_BYTES;
      else
	oheadersize = VDIF_HEADER_BYTES;

      free(buf);
      buf = NULL;
      if (outlegacy && !legacy) outlegacy = 0;
      
      nextract = encodeShift(channels, bits, shift);
      if (nextract==-1) {
	fprintf(stderr, "Unsupported channel selection\n");
      } else if (nextract>nchan) {
	fprintf(stderr, "Requested more channels than available - aborting\n");
	close(infile);
	break;
      }

      switch (bits) {
      case 2:
	switch (nchan) {
	case 32:
	  switch (nextract) {
	  case 4:
	    extract = extract4chan32_2bit;
	    break;
	  default:
	    ;
	  }
	  break;
	default:
	  ;
	}
	break;
      default:
	;
      }
      
      if (extract==NULL) {
	if (isComplex) {
	  fprintf(stderr, "Error: %d->%d channels, %dbits complex is not supported - aborting\n", nchan, nextract, bits/2);;
	} else {
	  fprintf(stderr, "Error: %d->%d channels, %dbits is not supported - aborting\n", nchan, nextract, bits);
	}
	close(infile);
	break;
      }

      frameperbuf = (BUFSIZE*1024)/framesize;
      bufsize = frameperbuf*framesize;
      if (frameperbuf==0) {
	fprintf(stderr, "Large framesize (%d) not supported - aborting\n", framesize);
	close(infile);
	break;
      }

      buf = malloc(bufsize);
      if (buf==NULL) {
	sprintf(msg, "Trying to allocate %d Kbytes", bufsize/1024);
	perror(msg);
	break;
      }

      odatasize = datasize/(nchan/nextract);
      oframesize = odatasize+oheadersize; 
      if (oframesize%8) { // Not multiple of 8
	fprintf(stderr, "Using output frame size of %d. This is not valid - aborting\n", oframesize);
	return(1);
      }
      obufsize = frameperbuf*oframesize;
      obuf = malloc(bufsize);
      if (obuf==NULL) {
	sprintf(msg, "Trying to allocate %d Kbytes", obufsize/1024);
	perror(msg);
	break;
      }
      printf("Reading in chunks of %d kB\n", bufsize/1024);

      if (!outlegacy) {
	// Initialize header
	pout = obuf + VDIF_LEGACY_HEADER_BYTES;
	for (i=0; i<frameperbuf; i++) {
	  bzero(pout, VDIF_LEGACY_HEADER_BYTES);
	  pout += oframesize;
	}
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
	fprintf(stderr, "Warning: Read partial frame (%d/%d)\n", nread, framesize);
	if (net) {
	  fprintf(stderr, " - aborting\n");
	  time_to_quit = 1;
	  break;
	} else {
	  fprintf(stderr, "\n");
	  nread = (nread / framesize) * framesize;
	}
      }

      header = (vdif_header*)buf;
      nframe = nread/framesize;
      pout = obuf;
      pin = buf + headersize;
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
	
	setVDIFFrameBytes(header, oframesize);
	setVDIFNumChannels(header, nextract);
	if (legacy && ! outlegacy) header->legacymode = 0;
	memcpy(pout, header, headersize);
	pout += oheadersize;
	extract(pin, pout, datasize, shift);
		
	header = (vdif_header*)((char*)header+framesize);
	pin += framesize;
	pout += odatasize;
      }
      if (time_to_quit) break;

      // Write data
      nwrote = write(outfile, obuf, nframe*oframesize);
      if (nwrote==-1) {
	perror("Error writing outfile");
	time_to_quit = 1;
	break;
      } else if (nwrote!=nframe*oframesize) {
	fprintf(stderr, "Warning: Did not write all bytes! (%d/%d)\n",
		nwrote, nframe*oframesize);
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

int encodeShift(int *channels, int bits, int *shift) {
  // Assumes channels are sorted - add explict sort later
  int nchan = 0;
  while (*channels!=-1) {
    if (nchan>=MAXCHAN) return -1;
    
    shift[nchan] = (*channels-nchan)*bits;
    nchan++;
    channels++;
  }
  return nchan;
}

int extract4chan32_2bit(char *in, char *out, int nbytes, int *shift) {
  // Assumes bound checks have already been run - e.g. arrays are or the right size and channel
  // numbers match frames sizes etc
  // nbytes is the number of bytes in the original frame
  // shift is the bitshift which corresponds to the selected channel
  int i;
  uint64_t *in64 = (uint64_t*)in;
  uint8_t *out8 = (uint8_t*)out;
  for (i=0; i<nbytes/8; i++) {
    *out8 = (*in64>>shift[0]&0x3) | (*in64>>shift[1]&0xC) | (*in64>>shift[2]&0x30) | (*in64>>shift[3]&0xC0);
    out8++;
    in64++;
  }
  return 0;
}
