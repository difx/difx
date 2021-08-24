#ifdef __APPLE__

#define OSX

#define OPENREADOPTIONS O_RDONLY
#define OPENWRITEOPTIONS O_WRONLY|O_CREAT|O_TRUNC

#else

#define LINUX
#define _LARGEFILE_SOURCE 
#define _LARGEFILE64_SOURCE
#define _FILE_OFsFSET_BITS 64

#define OPENREADOPTIONS O_RDONLY|O_LARGEFILE
#define OPENWRITEOPTIONS O_WRONLY|O_CREAT|O_TRUNC|O_LARGEFILE

#endif

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <strings.h>
#include <string.h>
#include <math.h>
#include <getopt.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <unistd.h>

#include "vdifio.h"


#define MAXTHREAD     1024     // 
#define MAXSTR        500
#define GOALBUFSIZE   2 // MBytes

#define DEBUG(x) 

double tim(void);
void kill_signal (int);

// Globals needed for signal handling
volatile int time_to_quit = 0;
volatile int sig_received = 0;

int main (int argc, char * const argv[]) {
  int framesize, nfile, infile, outfile[MAXTHREAD], opt, i, status, s, nFrame, thisThread;
  size_t bufsize, allocatedbytes;
  ssize_t nread, nwrote;
  double t0, ftmp;
  char outname[MAXSTR+1], outtemplate[MAXSTR+1];
  char outdir[MAXSTR+1] = "";
  char *dotptr, *fptr, msg[MAXSTR+50];
  unsigned char *buf, *bptr;
  vdif_header *header;
  
#define CASEINT(ch,var)                                     \
  case ch:                                                  \
    s = sscanf(optarg, "%d", &tmp);                         \
    if (s!=1)                                               \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = tmp;                                            \
    break

#define CASEFLOAT(ch,var)                                   \
  case ch:                                                  \
    s = sscanf(optarg, "%lf", &ftmp);                        \
    if (s!=1)                                               \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = ftmp;                                           \
    break

  struct option options[] = {
    {"outdir", 1, 0, 'o'},
    {"dir", 1, 0, 'o'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "ho:", options, NULL);
    if (opt==EOF) break;

    switch (opt) {
            
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

    case 'h':
      printf("Usage: convertTemplate [options] <file> [<file> ...]\n");
      printf("  -o/outdir/dir <DIR>   Output directory for converted data\n");
      printf("  -h/-help              This list\n");

      return(1);
    break;
    
    case '?':
    default:
      break;
    }
  }

  if (strlen(outdir)>0) {
    printf("Writing data to %s\n", outdir);
  }
  
  bufsize = 0;
  allocatedbytes = GOALBUFSIZE*1024*1024;

  buf = malloc(allocatedbytes);
  if (buf==NULL) {
    sprintf(msg, "Trying to allocate %lu bytes", allocatedbytes/1024/1024);
    perror(msg);
    return(1);
  }

  for (i=0;i<1024;i++) outfile[i] = 0;
  
  /* Install a ^C catcher */
  signal (SIGINT, kill_signal);
  
  t0 = tim(); /* So we can time average write per file */
  
  for (nfile=optind; nfile<argc; nfile++) {

    infile = open(argv[nfile], OPENREADOPTIONS);
    if (infile==-1) {
      sprintf(msg, "Failed to open input file (%s)", argv[nfile]);
      perror(msg);
      continue;
    }
    printf("Reading %s\n", argv[nfile]);

    nread = read(infile, buf, VDIF_LEGACY_HEADER_BYTES);
    if (nread==0) {  // EOF
      fprintf(stderr, "Empty file.  Skipping...\n");
      close(infile);
      continue;
    } else if (nread==-1) {
      perror("Error reading file: ");
      close(infile);
      continue;
    } else if (nread<VDIF_LEGACY_HEADER_BYTES) {
      fprintf(stderr, "Under-filled read %ld/%d from file. Skipping file\n", nread, VDIF_LEGACY_HEADER_BYTES);
      close(infile);
      continue;
    }
    // Rewind file
    lseek(infile, 0, SEEK_SET);    

    header = (vdif_header*)buf;
    
    framesize = getVDIFFrameBytes(header);

    nFrame = allocatedbytes/framesize;
    bufsize = nFrame*framesize;

    // Create output file name template

    // Should write to a directory?
    if (strlen(outdir)>0) {
      if (strlen(outdir)+1 > MAXSTR) { // +1 to allow for check below
	fprintf(stderr, "Error: Filenames too long. Increase MAXSTR\n");
	exit(1);
      }
      strcpy(outtemplate, outdir);
      if (outtemplate[strlen(outtemplate)-1]!='/') strcat(outtemplate, "/");

      // If writing to output directory, need to strip any leading path
      fptr = strrchr(argv[nfile], '/');
      if (fptr==NULL) { // None found
	fptr = argv[nfile];
      } else {
	fptr++; // Points to start of filename with no leading path
	if (*fptr==0) { // Advanced to end of string
	  fprintf(stderr, "Error: Input filename ends with '/'\n");
	  exit(1);
	}
      }
    } else {
      strcpy(outtemplate, "");
      fptr = argv[nfile]; // Just point directly to input file name
    }

    // Does filename contain a "."
    dotptr = strrchr(fptr, '.');
    if (dotptr!=NULL) {
      if (strlen(outtemplate)+(dotptr-fptr+1)+2+strlen(dotptr)>MAXSTR) {
	fprintf(stderr, "Error: Filenames too long. Increase MAXSTR\n");
	exit(1);
      }
      strncat(outtemplate, fptr, dotptr-fptr+1);
      strcat(outtemplate, "%d");
      strcat(outtemplate, dotptr);
    } else {
      if (strlen(outtemplate)+strlen(fptr)+3>MAXSTR) {
	fprintf(stderr, "Error: Filenames too long. Increase MAXSTR\n");
	exit(1);
      }
      strcat(outtemplate, fptr);
      strcat(outtemplate, ".%d");
    }
    printf("DEBUG: Template=%s\n", outtemplate);

    // Loop until EOF
    while (1) {
      
      if (time_to_quit) break;

      nread = read(infile, buf, bufsize);
      if (nread==0) {  // EOF
	break;
      } else if (nread==-1) {
	perror("Error reading file");
	break;
      } else if (nread<framesize) {
	fprintf(stderr, "  Under-filled frame %ld/%d bytes\n", nread, framesize);
      } else if (nread%framesize) {
	fprintf(stderr, "  Partial frame read %ld bytes\n", nread%framesize);
      }
      nFrame = nread/framesize;
      
      for (i=0; i<nFrame; i++) {
	bptr = &buf[i*framesize];
	header = (vdif_header*)bptr;

	if (getVDIFFrameBytes(header)!=framesize) {
	  fprintf(stderr, "Error: Framesize changes!\n");
	  exit(1);
	}
	
	thisThread = getVDIFThreadID(header);
	if (outfile[thisThread]==0) { // Have not seen this thread yet
	  sprintf(outname, outtemplate, thisThread);
	  printf("Writing thread %d to %s\n", thisThread, outname);
	  outfile[thisThread] = open(outname, OPENWRITEOPTIONS, S_IRWXU|S_IRWXG|S_IRWXO); 
	  if (outfile[thisThread]==-1) {
	    sprintf(msg, "Failed to open output file (%s)", outname);
	    perror(msg);
	    continue;
	  }
	}

	nwrote = write(outfile[thisThread], bptr, framesize);
	if (nwrote==-1) {
	  perror("Error writing outfile");
	  break;
	} else if (nwrote!=framesize) {
	  fprintf(stderr, "Warning: Did not write all bytes! (%ld/%d)\n", nwrote, framesize);
	  break;
	}
      }
    } // Loop over input file
    
    /* Close the file */
    for (i=0;i<MAXTHREAD;i++) {
      if (outfile[i]!=0) {
	status = close(outfile[i]);
	outfile[i] = 0;
	if (status!=0) {
	  perror("Error closing output file");
	}
      }
    }
    status = close(infile);
    if (status!=0) {
      perror("Error closing input file");
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

