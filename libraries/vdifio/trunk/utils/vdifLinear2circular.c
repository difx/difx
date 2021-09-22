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
#define MAXSTR 256

#define TIMECONST 1   // TIME for amplitude changes to recorver - should set on command line
#define BANDWIDTH 128 // Need to know frames/sec

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

#include <ippcore.h>
#include <ipps.h>


#define IPPMALLOC(var,type,n)                                  \
  var = ippsMalloc_ ## type(n);                                \
  if (var==NULL) {                                             \
    fprintf(stderr, "Error allocating %d values for %s at line %d\n", n, #var, __LINE__); \
    exit(EXIT_FAILURE);                                        \
  }

#define MALLOC_8U(var, n) IPPMALLOC(var, 8u, n)

#define DEBUG(x) 

void kill_signal (int);
double tim(void);


void unpack8bit(Ipp8u *in, Ipp32f **out, int nchan, int iscomplex, int n, int offset);
void unpack16bit(Ipp16u *in, Ipp32f **out, int nchan, int iscomplex, int n, int offset);
int pack8bit(Ipp32f **in, Ipp8u *out, int nchan, int iscomplex, float mean, float stddev, float target, int len, int offset);
int pack16bit(Ipp32f **in, Ipp16u *out, int nchan, int iscomplex, float mean, float stddev, float target, int len, int offset);

// Globals needed for signal handling
volatile int time_to_quit = 0;
volatile int sig_received = 0;

const char program[] = "vdifLinear2circular";
const char author[]  = "Chris Phillips <Chris.Phillips@csiro.au>";
const char version[] = "0.1";
const char verdate[] = "20200817";

static void usage()
{
  fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
  fprintf(stderr, "A program to rotate dual linear polsarisations to circular in a VDIF file\n");
  fprintf(stderr, "  EXPERIMENTAL AND INCOMPLETE!!!!!!!!\n");
  fprintf(stderr, "  ONLY SUPPORTS 16BIT COMPLEX and 2 CHANNELS!!!!!!!!\n");
  fprintf(stderr, "\nUsage: %s [Options] <VDIF input file> [<VDIF input file> ...]\n", program);
  fprintf(stderr, "\n <VDIF input file> is the name of the VDIF file(s) to read\n");
  fprintf(stderr, "\nOptions:\n");
  fprintf(stderr, "  -o <Output directory>   Name of a directory to write all the files to (required if multiple input files)\n");
  fprintf(stderr, "  -f <Output filename>    Name of output file (only valid if single input file\n");
  fprintf(stderr, "  -p <Phase>              Phase to rotate data by\n");
  fprintf(stderr, "  -skip <bytes>           Skip <bytes> bytes a the start of each file\n");
}

int main (int argc, char * const argv[]) {
  int framesize=0, nfile, numfiles, infile, outfile, opt, frameperbuf, nframe, i, j;
  int nread, status, nwrote, tmp, nchan=0, bits, isComplex=0, legacy=0, datasize=0;
  int first, bufsize=0, cfact, completeSample, samplesperFrame, headersize, frameperSec, bufperSec;
  float ftmp;
  double t0, sfa, sfb, N;
  Ipp8u *buf, *data;
  Ipp32f *mean, *stddev, *scratch, *avPower, ratio=0;
  Ipp32fc **unpacked, **rotated, phaseC; 
  char  *slashptr, *filename, msg[MAXSTR+50], outfilename[MAXSTR+1];
  vdif_header *header;

  int memsize = BUFSIZE;
  int offset = 0;
  float phase = 0.0;
  char outname[MAXSTR+1] = "";
  char outdir[MAXSTR+1] = "";

  struct option options[] = {
    {"outdir", 1, 0, 'o'},
    {"dir", 1, 0, 'o'},
    {"offset", 1, 0, 's'},
    {"skip", 1, 0, 's'},
    {"phase", 1, 0, 'p'},
    {"outfile", 1, 0, 'f'},
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

#define CASEFLOAT(ch,var)                                   \
  case ch:                                                  \
    status = sscanf(optarg, "%f", &ftmp);                   \
    if (status!=1)                                          \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = ftmp;                                           \
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
      CASEFLOAT('p', phase);

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

  numfiles = argc - optind;

  if (numfiles==0) {
    usage();
  } else if (numfiles>1) {
    if (strlen(outdir)==0) {
      fprintf(stderr, "Must supply output directory with multiple files - aborting\n");
      exit(EXIT_FAILURE);
    }
    if (strlen(outname)>0) {
      fprintf(stderr, "Warning: Cannot set outname for numtiple files. Ignoring\n");
      strcpy(outname,"");
    }
  } else {  // 1 file
    if (strlen(outname)==0) {
      fprintf(stderr, "Must supply output file name - aborting\n");
      exit(EXIT_FAILURE);    
    }
  }

  if (strlen(outdir)>0) {
    printf("Writing data to %s\n", outdir);
  }
  
  memsize *= 1024*1024;

  // Malloc enough data for one frame first
  MALLOC_8U(buf, VDIF_LEGACY_HEADER_BYTES);
  header = (vdif_header*)buf;

  phase += 90;
  phaseC.re = cos(phase/360*2*M_PI);
  phaseC.im = sin(phase/360*2*M_PI);

  printf("DEBUG: Phase==>(%.3f,%.3fi)\n", phaseC.re, phaseC.im);
  
  /* Install a ^C catcher */
  signal (SIGINT, kill_signal);

  t0 = tim(); /* So we can time average write per file */
  
  first = 1;
  for (nfile=optind; nfile<argc; nfile++) {
    filename = argv[nfile];
    
    infile = open(filename, OPENREADOPTIONS);
    if (infile==-1) {
      sprintf(msg, "Failed to open input file (%s)", filename);
      perror(msg);
      continue;
    }
    printf("Reading %s\n", filename);

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
      cfact = (isComplex) ? 2 : 1;
      
      ippsFree(buf);
      buf = NULL;

      if (!isComplex) {
	fprintf(stderr, "Error: Only support complex (sampling) VDIF\n");
	exit(EXIT_FAILURE);
      }
      if (nchan!=2) {
	fprintf(stderr, "Error: Only support 2 channel VDIF\n");
	exit(EXIT_FAILURE);
      }
      if (bits!=16 && bits!=8) {  // Can be easily expanded with more unpackers, 2 bit is likely to lead to digital artifacts
	fprintf(stderr, "Error: Only support 8 or 16 bit VDIF\n");
	exit(EXIT_FAILURE);
      }

      frameperbuf = memsize/framesize;
      bufsize = frameperbuf*framesize;
      if (frameperbuf==0) {
	fprintf(stderr, "Large framesize (%d) not supported - aborting\n", framesize);
	close(infile);
	break;
      }
      MALLOC_8U(buf, bufsize);      

      completeSample = bits*nchan*cfact;
      samplesperFrame = datasize*8 / completeSample;
      
      frameperSec = BANDWIDTH * 1e6 * 2 / cfact / samplesperFrame;
      bufperSec = frameperSec / frameperbuf;

      // Smoothing constants for gain equalisation - should add this to command line options
      N =  TIMECONST*bufperSec/5; // Recovery time is about 5x N
      sfb = 1/((float)N+1);
      sfa = N*sfb;

      // Allocate memory to unpack frames into
      unpacked = malloc(nchan*sizeof(Ipp32fc*));
      if (unpacked==NULL) {
	fprintf(stderr, "Error: Failed to allocate memory\n");
	exit(EXIT_FAILURE);
      }
      for (i=0;i<nchan;i++) {
	IPPMALLOC(unpacked[i], 32fc, samplesperFrame*frameperbuf);
      }
      rotated = malloc(nchan*sizeof(Ipp32fc*));
      if (unpacked==NULL) {
	fprintf(stderr, "Error: Failed to allocate memory\n");
	exit(EXIT_FAILURE);
      }
      for (i=0;i<nchan;i++) {
	IPPMALLOC(rotated[i], 32fc, samplesperFrame*frameperbuf);
	ippsZero_32fc(rotated[i], samplesperFrame);
      }

      mean = malloc(nchan*sizeof(Ipp32f*));
      stddev = malloc(nchan*sizeof(Ipp32f*));
      avPower = malloc(nchan*sizeof(Ipp32f*));
      if (mean==NULL || stddev==NULL || avPower==NULL) {
	fprintf(stderr, "Error: Failed to allocate memory\n");
	exit(EXIT_FAILURE);
      }
      IPPMALLOC(scratch, 32f, samplesperFrame*frameperbuf);
      
      // TODO Initialise FFT

      first = 0;
    }

    // Output file name
    int outnamelen = strlen(outname);
    int outdirlen = strlen(outdir);
    
    if (outnamelen>0) { // Only possible with single file
      if (outdirlen>0) {
	if (outnamelen+outdirlen+1>MAXSTR) {
	  fprintf(stderr, "%s/%s too long. Increase \"MAXSTR(%d)\"\n", 
		  outdir, outname, MAXSTR);
	  exit(EXIT_FAILURE);
	}
	sprintf(outfilename, "%s/%s", outdir, outname);
      } else {
	strcpy(outfilename, outname);
      }
    } else {
      // File name contained in buffer
      slashptr = rindex(filename, '/');
      
      if (slashptr==NULL) { // Simple filename
	int filenamelen = strlen(outdir);

	if (outdirlen+filenamelen+1 > MAXSTR) {
	  fprintf(stderr, "%s/%s too long. Increase \"MAXSTR(%d)\"\n", outdir, filename, MAXSTR);
	  return(1);
	}
	sprintf(outfilename, "%s/%s", outdir, filename);
      } else {// File contains a "/"
	slashptr++;
	int filenamelen = strlen(slashptr);
	
	if (outdirlen+filenamelen+1 > MAXSTR) {
	  fprintf(stderr, "%s/%s too long. Increase \"MAXSTR(%d)\"\n", outdir, slashptr, MAXSTR);
	  return(1);
	}
	sprintf(outfilename, "%s/%s", outdir, slashptr);
      }
    }
    
    outfile = open(outfilename, OPENWRITEOPTIONS, S_IRWXU|S_IRWXG|S_IRWXO); 
    if (outfile==-1) {
      sprintf(msg, "Failed to open output file (%s)", outfilename);
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

      // Unpack all the frames
      data = buf + headersize;
      for (i=0; i<nframe; i++) {
	// Unpack to floating point
	if (bits==8) {
	  unpack8bit(data, (Ipp32f**)unpacked, nchan, isComplex, samplesperFrame, samplesperFrame*i);
	} else if (bits==16) {
	  unpack16bit((Ipp16u*)data, (Ipp32f**)unpacked, nchan, isComplex, samplesperFrame, samplesperFrame*i);
	} else {
	  fprintf(stderr, "Error: Do not support %d bits\n", bits);
	  exit(EXIT_FAILURE);
	}
	data += framesize;
      }

#if 0
      // Do some processing
      for (j=0;j<nchan;j++) {
	status = ippsPowerSpectr_32fc(unpacked[j], scratch, samplesperFrame*nframe);
	//status = ippsSqr_32f(unpacked[j], scratch, samplesperFrame*nframe);
	status = ippsSum_32f(scratch, samplesperFrame*nframe, &avPower[j], ippAlgHintFast);
	avPower[j] /= samplesperFrame*nframe;
	
	status = ippsMeanStdDev_32f((Ipp32f*)unpacked[j], samplesperFrame*cfact*nframe, &mean[j], &stddev[j], ippAlgHintFast);
	printf("Chan%02d: Power=%.2f Mean=%.2f  StdDev=%.2f\n", j, avPower[j], mean[j], stddev[j]);
      }
#endif
      // Figure out ratio of powers. This assumes pols are consecutive values
      for (j=0;j<nchan;j+=2) {
#if 0
	Ipp32f thisratio = avPower[j]/avPower[j+1];
	thisratio = sqrt(sqrt(thisratio));
	if (ratio==0.0)
	  ratio = thisratio;
	else
	  ratio = ratio * sfa + thisratio * sfb; // This should be expanded to handle multiple channels
	
	printf("Ratio=%.2f\n", ratio);

	status = ippsMulC_32f_I(1/ratio/sqrt(2), (Ipp32f*)unpacked[j], samplesperFrame*nframe*cfact);
	status = ippsMulC_32f_I(ratio/sqrt(2), (Ipp32f*)unpacked[j+1], samplesperFrame*nframe*cfact);
#endif
#if 1
	// Rotate second pol by given amount - includes 90deg 
	// This assumes complex data
	status = ippsMulC_32fc_I(phaseC, unpacked[j+1], samplesperFrame*nframe);
#endif
#if 1
	// Add and subtract to convert pols
	status = ippsAdd_32fc(unpacked[j], unpacked[j+1], rotated[j], samplesperFrame*nframe);
	status = ippsSub_32fc(unpacked[j], unpacked[j+1], rotated[j+1], samplesperFrame*nframe);
#else
	status = ippsCopy_32fc(unpacked[j], rotated[j], samplesperFrame*nframe);
	status = ippsCopy_32fc(unpacked[j+1], rotated[j+1], samplesperFrame*nframe);
#endif
      }

#if 0
      // Did this do anything?
      for (j=0;j<nchan;j++) {
	status = ippsPowerSpectr_32fc((Ipp32fc*)rotated[j], scratch, samplesperFrame*nframe);
	status = ippsSum_32f(scratch, samplesperFrame*nframe, &avPower[j], ippAlgHintFast);
	avPower[j] /= samplesperFrame*nframe;
	
	status = ippsMeanStdDev_32f((Ipp32f*)rotated[j], samplesperFrame*cfact*nframe, &mean[j], &stddev[j], ippAlgHintFast);
	printf("###  Chan%02d: Power=%.2f Mean=%.2f  StdDev=%.2f\n", j, avPower[j], mean[j], stddev[j]);
      }
#endif
      // Re-quantise to integer and put back in original frame
      data = buf + headersize;
      for (i=0; i<nframe; i++) {
	// Unpack to floating point
	//printf("*FRAME %d\n", i);
	if (bits==8) {
	  pack8bit((Ipp32f**)rotated, (Ipp8u*)data, nchan, isComplex, 0, 0, 10, samplesperFrame, samplesperFrame*i);
	} else if (bits==16) {
	  pack16bit((Ipp32f**)rotated, (Ipp16u*)data, nchan, isComplex, 0, 0, 10, samplesperFrame, samplesperFrame*i);
	} else {
	  fprintf(stderr, "Error: Do not support %d bits\n", bits);
	  exit(EXIT_FAILURE);
	}
	data += framesize;
      }

      if (time_to_quit) break;

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

void unpack8bit(Ipp8u *in, Ipp32f **out, int nchan, int iscomplex, int n, int offset) {
  // Unpack n *output* samples from 8 bit unsigned integers
  // Messes with input data
  Ipp8s *ins;
  IppStatus status;


  int cfact = 1;
  if (iscomplex) cfact = 2;
  offset *= cfact;

  // Convert unsigned to signed
  status = ippsXorC_8u_I(0x80, in, n*nchan*cfact);
  if (status!=ippStsNoErr) {
    fprintf(stderr, "ippsXorC_8u_I failed at one line %d!\n", __LINE__ -2);
    exit(EXIT_FAILURE);
  }
  
  ins = (Ipp8s*)in;
  if (nchan==1) {
    status = ippsConvert_8s32f(ins, &out[0][offset], n*cfact);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "ippsConvert_8s32f failed at line %d!\n", __LINE__ -2);
      exit(1);
    }
  } else if (nchan==2 && iscomplex) {
    int i, j=0;
    for (i=0; i<n; i++) {
      out[0][offset + i*2]   = ins[j++];
      out[0][offset + i*2+1] = ins[j++];
      out[1][offset + i*2]   = ins[j++];
      out[1][offset + i*2+1] = ins[j++];
    }
  } else {
    fprintf(stderr, "Error: Do not support %d channels 8bit in %s\n", nchan, __FUNCTION__);
    exit(1);
  }
  return;
}

void unpack16bit(Ipp16u *in, Ipp32f **out, int nchan, int iscomplex, int n, int offset) {
  // Unpack n *output* samples from 16 bit unsigned integers
  // Messes with input data
  Ipp16s *ins;
  IppStatus status;  
  int cfact = (iscomplex) ? 2 : 1;
  
  status = ippsXorC_16u_I(0x8000, in, n*nchan*cfact);
  if (status!=ippStsNoErr) {
    fprintf(stderr, "ippsXorC_8u_I failed at one line %d!\n", __LINE__ -2);
    exit(EXIT_FAILURE);
  }

  ins = (Ipp16s*)in;
  if (nchan==1) {
    status = ippsConvert_16s32f(ins, &out[0][offset], n*cfact);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "ippsConvert_16s32f failed at line %d!\n", __LINE__ -2);
      exit(1);
    }
  } else if (nchan==2 && iscomplex) {
    int i, j=0;
    for (i=0; i<n; i++) {
      out[0][offset + i*2]   = ins[j++];
      out[0][offset + i*2+1] = ins[j++];
      out[1][offset + i*2]   = ins[j++];
      out[1][offset + i*2+1] = ins[j++];
    }
  } else {
    fprintf(stderr, "Error: Do not support %d channels 8bit in %s\n", nchan, __FUNCTION__);
    exit(1);
  }
  return;
}


int pack8bit(Ipp32f **in, Ipp8u *out, int nchan, int iscomplex, float mean, float stddev, float target, int n, int offset) {
  Ipp8s *outs;
  IppStatus status;
  int cfact = (iscomplex) ? 2 : 1;
  int i;

  offset *= cfact;

  //printf("*OFFSET=%d\n", offset);
  
  outs = (Ipp8s*)out;
  for (i=0; i<nchan; i++) {
  // Subtract mean and scale to target
    if (mean!=0.0) 
      ippsSubC_32f_I(mean, &in[i][offset], n*cfact);
    
    if (stddev!=0)
      ippsMulC_32f_I(target/stddev, &in[i][offset], n*cfact);
  }

  if (nchan==1) {
    status = ippsConvert_32f8s_Sfs(&in[0][offset], outs, n*cfact, ippRndNear, 0);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error calling ippsConvert_32f8s_Sfs\n");
    }
  } else if (nchan==2) {
    int i, o=0;
    if (iscomplex) {
      for (i=0;i<n;i++) {
	//printf("* %4d-%4d: ", o,o+3);
	
        outs[o++] = in[0][offset + i*2];
        outs[o++] = in[0][offset + i*2+1];
        outs[o++] = in[1][offset + i*2];
        outs[o++] = in[1][offset + i*2+1];

	//for (int j=0;j<4; j++) printf(" %3u", out[o-4+j]^0x80);
	//printf("\n");

      }
    } else {
      for (i=0;i<n;i++) {
        outs[o++] = in[0][offset + i];
        outs[o++] = in[1][offset + i];
      }
    }
  } else {
    fprintf(stderr, "Error: Do not support %d channels 8bit\n", nchan);
    exit(1);
  }

  status = ippsXorC_8u_I(0x80, out, n*nchan*cfact);
  if (status!=ippStsNoErr) {
    fprintf(stderr, "ippsXorC_8u_I failed at one line %d!\n", __LINE__ -2);
    exit(EXIT_FAILURE);
  }
  
  return 0;
}

int pack16bit(Ipp32f **in, Ipp16u *out, int nchan, int iscomplex, float mean, float stddev, float target, int n, int offset) {
  Ipp16s *outs;
  IppStatus status;
  int cfact = (iscomplex) ? 2 : 1;
  int i;

  outs = (Ipp16s*)out;
  for (i=0; i<nchan; i++) {
    // Subtract mean and scale to target
    if (mean!=0.0) 
      ippsSubC_32f_I(mean, &in[i][offset], n*cfact);

    if (stddev!=0)
      ippsMulC_32f_I(target/stddev, &in[i][offset], n*cfact);
  }

  if (nchan==1) {
    status = ippsConvert_32f16s_Sfs(&in[0][offset], outs, n*cfact, ippRndNear, 0);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error calling ippsConvert_32f16s_Sfs\n");
    }
  } else if (nchan==2) {
    int i, o=0;
    if (iscomplex) {
      for (i=0;i<n;i++) {
        outs[o++] = in[0][offset + i*2];
        outs[o++] = in[0][offset + i*2+1];
        outs[o++] = in[1][offset + i*2];
        outs[o++] = in[1][offset + i*2+1];
      }
    } else {
      for (i=0;i<n;i++) {
        outs[o++] = in[offset + 0][i];
        outs[o++] = in[offset + 1][i];
      }
    }
  } else {
    fprintf(stderr, "Error: Do not support %d channels 16bit\n", nchan);
    exit(1);
  }

  status = ippsXorC_16u_I(0x8000, out, n*nchan*cfact);
  if (status!=ippStsNoErr) {
    fprintf(stderr, "ippsXorC_16u_I failed at one line %d!\n", __LINE__ -2);
    exit(EXIT_FAILURE);
  }
  
  return 0;
}
