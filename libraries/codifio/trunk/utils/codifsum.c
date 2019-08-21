#ifdef __APPLE__

#define OSX

#define OPENOPTIONS O_RDONLY

#else

#define LINUX
#define _LARGEFILE_SOURCE 
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#define OPENOPTIONS O_RDONLY|O_LARGEFILE

#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <getopt.h>

#include "codifio.h"

void mjd2cal(double mjd, int *day, int *month, int *year, double *ut);
int turns_to_string(double turns, char type, int dps, int nstr, char str[]);

int main (int argc, char **argv) {
  int i, file, day, month, year;
  double ut;
  uint16_t stationID;
  ssize_t nread;
  off_t sook;
  char msg[300], buf[CODIF_HEADER_BYTES], *stnCode, timestr[40];
  codif_header *header;

 struct option options[] = {
    {"skip", 1, 0, 's'},
    {"verbose", 0, 0, 'v'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

 int verbose = 0;
 int skip = 0;

 /* Read command line options */
  while (1) {
    int opt = getopt_long_only(argc, argv, "s:v", options, NULL);
    if (opt==EOF) break;
    
#define CASEINT(ch,var)                                     \
  case ch: {						    \
    int tmp;                                                \
    int status = sscanf(optarg, "%d", &tmp);		    \
    if (status!=1)                                          \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = tmp;                                            \
    break; }

#define CASEFLOAT(ch,var)                                   \
  case ch:                                                  \
    float ftmp;                                             \
    int status = sscanf(optarg, "%f", &ftmp);		    \
    if (status!=1)                                          \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = ftmp;                                           \
    break
    
    switch (opt) {

      CASEINT('s', skip);

      case 'v':
        verbose = 1;
        break;
    case 'h':
        printf("Usage: codifsum [options] <codiffile> [<codiffile> ...]\n");
        printf("  -s <N>                    Skip N bytes at start of file\n");
        printf("  -v                        Summary of codiffile\n");
        printf("  -h                        This list\n");
        return(1);
        break;
      
    case '?':
    default:
      break;
    }
  }

  header = (codif_header*)buf;

  for (i=optind; i<argc; i++) {
    printf("%s:\n", argv[i]);

    file = open(argv[i], OPENOPTIONS);
    if (file==-1) {
      sprintf(msg, "Failed to open input file (%s) [%d]", argv[i], errno);
      perror(msg);
      continue;
    }

    if (skip>0) {
      off_t sook = lseek(file, skip, SEEK_SET);
      if (sook==-1) {
	perror("Error seeking within file\n");
	close(file);
	exit(1);
      }
    }
    stnCode = (char*)&stationID;

    int first = 1;
    while (1) {
      nread = read(file, buf, CODIF_HEADER_BYTES);
      if (nread==0) { // EOF
        break;
      } else if (nread==-1) {
        perror("Error reading file");
        close(file);
        exit(1);
      } else if (nread != CODIF_HEADER_BYTES) {
	fprintf(stderr, "Undersized read\n");
	close(file);
	exit(1);
      }

      stationID = getCODIFStationID(header);

      int bits = getCODIFBitsPerSample(header);
      int nchan = getCODIFNumChannels(header);
      int iscomplex = getCODIFComplex(header);
      int period = getCODIFPeriod(header);
      int framesize = getCODIFFrameBytes(header)+CODIF_HEADER_BYTES;
      int sampleperframe = getCODIFFrameBytes(header)*8/(bits*nchan*(iscomplex+1));
      double framepersec = getCODIFTotalSamples(header)/(double)sampleperframe/period;
      double mjd = getCODIFFrameDMJD(header, framepersec);

      mjd2cal(mjd, &day, &month, &year, &ut);
      turns_to_string(ut, 'H', 6, 40, timestr);
      if (verbose) {
	if (first) {
	  char complextype[2] = "";
	  if (iscomplex) complextype[0] = 'C';
	  printf("CODIF%s/%d/%d/%d\n", complextype, period, framesize, bits);
	  printf(" %02d/%02d/%d  %s\n", day, month, year, timestr);       	
	} else {
	  printf(" %02d/%02d/%d  %s\n", day, month, year, timestr);       	
	}
      } else {
      
	printf("INVALID:     %d\n", getCODIFFrameInvalid(header));
	printf("COMPLEX:     %d\n", iscomplex);
	printf("\n");
	printf("SECONDS:     %d     %02d/%02d/%d  %s\n", getCODIFFrameEpochSecOffset(header),
	       day, month, year, timestr);
	printf("\n");
	printf("FRAME#:      %d\n", getCODIFFrameNumber(header));
	printf("\n");
	printf("VERSION:     %d\n", getCODIFVersion(header));
	printf("NBITS:       %d\n", bits);
	printf("FRAMELENGTH: %d\n", getCODIFFrameBytes(header));
	printf("\n");
	printf("EPOCH:       %d\n", getCODIFEpoch(header));
	printf("REPRESENT:   %d\n", getCODIFRepresentation(header));
	if(stnCode[0] >= ' ' && stnCode[0] <= 127 && (stnCode[1] >= ' ' || stnCode[1] == 0) && stnCode[1] <= 127) {
	printf("ANTID:       %c%c\n", stnCode[1], stnCode[0]);
	} else {
	  printf("ANTID:       %d\n", stationID);
	}
	printf("\n");
	printf("SAMPLEBLOCK: %d\n", getCODIFSampleblockLength(header));
	printf("NCHAN:       %d\n", nchan);
	printf("\n");
	printf("THREADID:    %d\n", getCODIFThreadID(header));
	printf("GROUPID:     %d\n", getCODIFGroupID(header));
	printf("\n");
	printf("PERIOD:      %d\n", period);
	printf("\n");
	printf("#SAMPLES:    %llu\n", getCODIFTotalSamples(header));
	printf("\n");
	printf("SYNC:        0X%X\n", getCODIFSync(header));
	
	printf("-------------------\n");
      }
      if (verbose) {
	if (first) {
	  first = 0;
	  // Want to seek to last frame of file
	  struct stat buf;
	  fstat(file, &buf);
	  off_t size = buf.st_size;
	  off_t lastframe = ((size - skip) - framesize)/framesize; // Offset in frames
	  lastframe *= framesize + skip;  // Convert back to bytes

	  sook = lseek(file, lastframe, SEEK_SET);
	  if (sook==-1) {
	    perror("Error seeking within file\n");
	    close(file);
	    exit(1);
	  }
	} else {
	  break;
	}
      } else {
	sook = lseek(file, getCODIFFrameBytes(header), SEEK_CUR);
	if (sook==-1) {
	  perror("Error seeking within file\n");
	  close(file);
	  exit(1);
	}
      }
    }
  }
  close(file);
}

void mjd2cal(double mjd, int *day, int *month, int *year, double *ut) {
  int jd, temp1, temp2;

  *ut = fmod(mjd,1.0);

  if (*ut<0.0) {
    *ut += 1.0;
    mjd -= 1;
  }

  jd = (int)floor(mjd + 2400001);

  // Do some rather cryptic calculations
  
  temp1 = 4*(jd+((6*(((4*jd-17918)/146097)))/4+1)/2-37);
  temp2 = 10*(((temp1-237)%1461)/4)+5;

  *year = temp1/1461-4712;
  *month =((temp2/306+2)%12)+1;
  *day = (temp2%306)/10+1;
}

int turns_to_string(double turns, char type, int dps, int nstr, char str[])
{

  int                 i, hours, sign, whole_turns, dp;
  unsigned long int   revunit, work, scale;
  int                 isec, imin, iunits, right_of_dp;
  char                out[20], text[20], fmt[5];

  /* Record conversion unit and enforce decimal point limits */
  if (type == 'H' || type == 'h') {
    /* Hours mode */
    hours = 1;
    revunit = 86400;
    dp = (dps > 6) ? 6 : dps;
  } else {
    /* Degrees mode */
    hours = 0;
    revunit = 1296000;
    dp = (dps > 6) ? 6 : dps;
  }
  if (dp < 0) dp = 0;
  if (dp != dps) printf("turns_to_string: Invalid number of d.p. requested, "
                        "enforcing %d\n", dp);

  /* Record and dispose of sign */
  sign = (turns >= 0) ? 1 : -1;
  turns = turns*(double)sign;

  /* Split into whole number of turns and fractional part. This allows us to
     achieve maximum precision in the seconds part since we can always add
     back a whole number of hours/degrees to the final result without worrying
     about propogation of rounding effects. */
  whole_turns = (int)turns;
  turns -= (double)whole_turns;

  /* Calculate the scaling factor for the conversion */
  i = dp;
  scale = 1.0;
  while (i > 0) {
    scale *= 10;
    i--;
  }
  
  /* Convert - the 0.5 ensures effective rounding, not truncation. */
  work = (unsigned long int)(0.5+(double)(revunit*scale)*turns);

  /* Split off the part to the right of the decimal place */
  right_of_dp = (int)(work - scale*(work/scale));

  /* Convert to seconds and split them off */
  work /= scale;
  isec = (int)(work - 60*(work/60));

  /* Convert to minutes and split them off */
  work /= 60;
  imin = (int)(work - 60*(work/60));

  /* Convert to either hours or degrees and restore whole number of turns */
  iunits = work/60;
  iunits += (hours ? 24 : 360)*whole_turns;

  /* Build the output string */
  out[0] = '\0';
  if (sign == -1) strcat(out, "-");
  sprintf(text, (iunits>99 ? "%d" : "%02d"), iunits);
  strcat(out, text);
  strcat(out ,":");
  sprintf(text, "%02d", imin);
  strcat(out, text);
  strcat(out, ":");
  sprintf(text, "%02d", isec);
  strcat(out, text);
  if (dp > 0) {
    strcat(out, ".");
    sprintf(fmt, "%%0%dd", dp);
    sprintf(text, fmt, right_of_dp);
    strcat(out, text);
  }

  /* Check to see if the output buffer is big enough, if it isn't, make sure
     that the output string is at least null terminated */
  i = strlen(out);
  if (i > nstr) {
    printf("turns_to_string: output string not large enough\n");
    out[nstr-1] = '\0';
    i = nstr-1;
  }
  while (i >= 0) {
    str[i] = out[i];
    i--;
  }

  return(strlen(str));
}
