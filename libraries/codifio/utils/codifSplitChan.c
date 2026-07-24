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

#define BUFSIZE 2048   // 2 MB
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
//#include <sys/types.h>
//#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>
#include "codifio.h"

#define DEBUG(x) 

void kill_signal (int);
double tim(void);

// Globals needed for signal handling
volatile int time_to_quit = 0;
volatile int sig_received = 0;

int split32(uint8_t *in, uint8_t **out, int nbytes, int nchunk);


const char program[] = "codifSplitChan";
const char author[]  = "Chris Phillips <Chris.Phillips@csiro.au>";
const char version[] = "0.1";
const char verdate[] = "20240507";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
  fprintf(stderr, "Split a codif file into multiple single channel files.\n");
  fprintf(stderr, "Assumes all frames have the same size. \n");
  fprintf(stderr, "\nUsage: %s  <CODIF input file> [<CODIF input file> ...]\n", program);
  fprintf(stderr, "\nOptions:\n");
  fprintf(stderr, "\n-skip <bytes>    Skip <bytes> bytes a the start of each file\n");
  fprintf(stderr, "-o <Output directory>  Existng Directory to write output files\n");
}

int main (int argc, char * const argv[]) {
  int frameSize=0, nfile, infile, *outfile, rate, opt, frameperbuf, nframe, i, n;
  int nread, status, nwrote, tmp, nchan=0, bits, sampleBits, isComplex=0, dataSize=0;
  int nextract=0, first, sock, bufsize=0, odatasize=0, oframesize=0, obufsize;
  int completeSample, splitBits, nout=0, result;
  float ftmp;
  double t0;
  char outname[MAXSTR+1] = "";
  uint8_t *buf, **obuf, **pout, *pin;
  char *slashptr, *dotptr, msg[MAXSTR+50], *infilename, *extptr;
  codif_header *header;
  unsigned long long totalsent;

  int (*split_func)(uint8_t *in, uint8_t **out, int nbytes, int nchunk);

  int offset = 0;
  int nGroup = 1;
  char *outdir = NULL;  
  
  struct option options[] = {
    {"outdir", 1, 0, 'o'},
    {"dir", 1, 0, 'o'},
    {"group", 1, 0, 'g'},
    {"skip", 1, 0, 's'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  first = 1;
  outfile = 0;
  split_func = NULL;
  
  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "ho:g:s:", options, NULL);
    if (opt==EOF) break;

    switch (opt) {

    case 's':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad skip value %s\n", optarg);
      else {
	offset = tmp;
      }
      break;

    case 'g':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1) {
	fprintf(stderr, "Bad group value %s\n", optarg);
      } else if  (tmp<=0) {
	fprintf(stderr, "-group must be positive\n");
      } else {
	nGroup = tmp;
      }
      break;

    case 'o':
      outdir = strdup(optarg);
      if (outdir[strlen(outdir)-1] == '/')  {// Remove trailing "/"
	outdir[strlen(outdir)-1] = 0;
      }
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

  // Malloc enough data for one frame first
  buf = malloc(CODIF_HEADER_BYTES);
  if (buf==NULL) {
    sprintf(msg, "Trying to allocate %d bytes", CODIF_HEADER_BYTES);
    perror(msg);
    return(1);
  }
  header = (codif_header*)buf;

  if (outdir!=NULL) {
    printf("Writing data to %s\n", outdir);
  }

  /* Install a ^C catcher */
  signal (SIGINT, kill_signal);

  t0 = tim(); /* So we can time average write per file */

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
      offset = 0; // To be sure not negative
    }

    if (first) {
      // Read first header
      nread = read(infile, buf, CODIF_HEADER_BYTES);
      if (nread==0) {  // EOF
	perror("Empty file - aborting\n");
	close(infile);
	break;
      } else if (nread==-1) {
	perror("Error reading file - aborting\n");
	close(infile);
	break;
      } else if (nread<CODIF_HEADER_BYTES) {
	perror("Header size too small - is this CODIF? Aborting\n");
	close(infile);
	break;
      }
      lseek(infile, offset, SEEK_SET); // Reset file
      
      nchan = getCODIFNumChannels(header);
      bits = getCODIFBitsPerSample(header);
      isComplex = getCODIFComplex(header);
      dataSize = getCODIFFrameBytes(header);
      frameSize = dataSize + CODIF_HEADER_BYTES;

      sampleBits = bits;
      if (isComplex) sampleBits *=2;
      free(buf);

      buf = NULL;
      
      completeSample = sampleBits*nchan;
      splitBits = sampleBits*nGroup;
      if (completeSample%splitBits!=0) {
	fprintf(stderr, "Error: Grouping of %d channels does not work with total channels of %d\n", nGroup, nchan);
	close(infile);
	exit(1);
      }
      nout = completeSample/splitBits;

      if (nout<=1) {
	fprintf(stderr, "Too many channels grouped - output file will be equal to input!\n");
	close(infile);
	break;
      }

      switch (splitBits) {
      case 32:
	split_func = split32;
	break;

      default:
	fprintf(stderr, "Do not support %d bits%s, %d channels with %d grouping (%d bits per group)\n",
		bits, isComplex ? " Complex" : "", nchan, nGroup, splitBits);
	close(infile);
	exit(1);
      }

      frameperbuf = (BUFSIZE*1024)/frameSize;
      bufsize = frameperbuf*frameSize;
      if (frameperbuf==0) {
	fprintf(stderr, "Large framesize (%d) not supported - aborting\n", frameSize);
	close(infile);
	break;
      }
      printf("Reading in chunks of %d kB\n", bufsize/1024);
      
      buf = malloc(bufsize);
      if (buf==NULL) {
	sprintf(msg, "Trying to allocate %d Kbytes", bufsize/1024);
	perror(msg);
	break;
      }

      odatasize = dataSize/nout;
      oframesize = odatasize+CODIF_HEADER_BYTES; 
      if (oframesize%8) { // Not multiple of 8
	fprintf(stderr, "Using output frame size of %d. This is not valid - aborting\n", oframesize);
	return(1);
      }
      obufsize = frameperbuf*oframesize;
      obuf = malloc(nout*sizeof(uint8_t*));
      if (obuf==NULL) {
	//sprintf(msg, "Trying to allocate %d Kbytes", obufsize/1024);
	sprintf(msg, "Trying to allocate pointers");
	perror(msg);
	break;
      }
      pout = malloc(nout*sizeof(uint8_t*));
      if (pout==NULL) {
	sprintf(msg, "Trying to allocate pointers");
	perror(msg);
	break;
      }

      outfile = malloc(nout*sizeof(int));
      if (outfile==NULL) {
	sprintf(msg, "Trying to allocate pointers");
	perror(msg);
	break;
      }

      for (i=0; i<nout; i++) {
	obuf[i] = malloc(obufsize);
	if (obuf[i]==NULL) {
	  sprintf(msg, "Trying to allocate %d Kbytes", obufsize/1024);
	  perror(msg);
	  close(infile);
	  exit(1);
	}
      }
      
      first = 0;
    }


    // Output file names
      
    // Are we writing to output directory and input filename is part of a path
    if (outdir!=NULL) {
      slashptr = rindex(argv[nfile], '/');
      if (slashptr==NULL) { // Simple filename
	infilename = argv[nfile];
      } else {
	infilename = slashptr+1;
      }
    } else {
      infilename = argv[nfile];
    }
    
    dotptr = rindex(infilename, '.');
    if (dotptr==NULL) {
      extptr = NULL;
    } else {
      extptr = dotptr+1;
      *dotptr = 0; 
    }

    for (i=0;i<nout;i++) {
      if (outdir==NULL) {
	if (extptr==NULL) 
	  result = snprintf(outname, MAXSTR, "%s-%d", infilename, i+1);
	else 
	  result = snprintf(outname, MAXSTR, "%s-%d.%s", infilename, i+1, extptr);
      } else {
	if (extptr==NULL) 
	  result = snprintf(outname, MAXSTR, "%s/%s-%d", outdir, infilename, i+1);
	else 
	  result = snprintf(outname, MAXSTR, "%s/%s-%d.%s", outdir, infilename, i+1, extptr);
      }
      if (result>=MAXSTR) {
	fprintf(stderr, "Error: output filename too long");
	close(infile);
	exit(1);
      }
	
      outfile[i] = open(outname, OPENWRITEOPTIONS, S_IRWXU|S_IRWXG|S_IRWXO); 
      if (outfile[i]==-1) {
	sprintf(msg, "Failed to open output file (%s)", outname);
	perror(msg);
	continue;
      }
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
      } else if (nread%frameSize!=0) {
	fprintf(stderr, "Warning: Read partial frame (%d/%d)\n", nread, frameSize);
	nread = (nread / frameSize) * frameSize;
      }

      header = (codif_header*)buf;
      nframe = nread/frameSize;
      for (i=0;i<nout;i++) {
	pout[i] = obuf[i];
      }

      pin = buf+CODIF_HEADER_BYTES;
      for (i=0; i<nframe; i++) {
	if (getCODIFSync(header)!=0xFEEDCAFE) {
	  fprintf(stderr, "Error: CODIF sync lost - aborting\n");
	  time_to_quit = 1;
	  break;
	}
	if (nchan     != getCODIFNumChannels(header) ||
	    bits      != getCODIFBitsPerSample(header) ||
	    isComplex != getCODIFComplex(header) ||
	    dataSize  != getCODIFFrameBytes(header)) {
	  fprintf(stderr, "Error: CODIF frame parameters have changed - aborting\n");
	  time_to_quit = 1;
	  break;
	}
	
	setCODIFFrameBytes(header, odatasize);
	setCODIFNumChannels(header, nGroup);

	for (n=0;n<nout;n++) {
	  memcpy(pout[n], header, CODIF_HEADER_BYTES);
	  pout[n] += CODIF_HEADER_BYTES;
	}
	split_func(pin, pout, dataSize, nout);
		
	header = (codif_header*)((uint8_t*)header+frameSize);
	pin += frameSize;
	for (n=0;n<nout;n++) 
	  pout[n] += odatasize;
      }
      if (time_to_quit) break;

      // Write data
      for (n=0; n<nout; n++) {
	nwrote = write(outfile[n], obuf[n], nframe*oframesize);
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
      }
    } // Loop over input file

    /* Close the file */
    status = close(infile);
    if (status!=0) {
      perror("Error closing input file");
    }
    for (i=0; i<nout; i++) {
      status = close(outfile[i]);
      if (status!=0) {
	perror("Error closing output file");
      }
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

int split32(uint8_t *in, uint8_t **out, int nbytes, int nchunk) {
  // Split the dataframe in chunks of 32 bits
  // Assumes bound checks have already been run - e.g. arrays are of
  // the right size etc
  int i, j, k;
  uint32_t *in32 = (uint32_t*)in;
  uint32_t **out32 = (uint32_t**)out;

  k = 0;
  for (i=0; i<nbytes/4/nchunk; i++) 
    for (j=0; j<nchunk; j++) { 
      out32[j][i] = in32[k];
      k++;
    }
  
  return 0;
}
