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
#include <sys/stat.h>    /* for S_* modes on some flavors */

#endif

#define USEIPP

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <inttypes.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <vdifio.h>
#include <fcntl.h>
#include <sys/time.h>
#include <getopt.h>
#include <vdifio.h>

#include "config.h"

#if USE_CODIFIO
#include <codifio.h>
#endif


#ifdef USEIPP
#include <ippcore.h>
#include <ipps.h>

#define DEFAULTTAP 128

IppsRandGaussState_32f *pRandGaussState, *pRandGaussState2;
Ipp32f *scratch=NULL, *scratch2=NULL;
Ipp32fc *scratchHilbert=NULL;
Ipp32f phase, phase2;
#ifdef IPP9
Ipp32f *dly;
Ipp8u *buf;
IppsFIRSpec_32f *pSpec;
IppsFIRSpec_32fc *pcSpec;
IppsHilbertSpec* hSpec;
Ipp8u* hBuffer;
Ipp32fc *dsb_mult;
#else
typedef unsigned char  Ipp8u;
typedef unsigned short Ipp16u;
typedef unsigned int   Ipp32u;
typedef signed char    Ipp8s;
typedef signed short   Ipp16s;
typedef signed int     Ipp32s;
typedef float          Ipp32f;
typedef __INT64        Ipp64s;
typedef __UINT64       Ipp64u;
typedef double         Ipp64f;
typedef Ipp16s         Ipp16f;
#endif
#endif

#define SEED 48573

double currentmjd();
void mjd2cal(double mjd, int *day, int *month, int *year, double *ut);
float gaussrand();
int MeanStdDev(const float *src, int length, float *mean, float *StdDev);
int pack2bit1chan(Ipp32f **in, int off, Ipp8u *out, float mean, float stddev, int len);
int pack2bitNchan(Ipp32f **in, int nchan, int off, Ipp8u *out, float mean, float stddev, int len);
int pack2bitNchan_complex(Ipp32f **in, int nchan, int off, Ipp8u *out, float mean, float stddev, int len);
int pack8bitNchan(Ipp32f **in, int nchan, int off, Ipp8u *out, float mean, float stddev, int len, int iscodif, int iscomplex);
int pack16bitNchan(Ipp32f **in, int nchan, int off, Ipp16u *out, float mean, float stddev, int len, int iscodif, int iscomplex);
void dayno2cal (int dayno, int year, int *day, int *month);
double cal2mjd(int day, int month, int year);
double tm2mjd(struct tm date);

void generateData(Ipp32f **data, int nframe, int samplesperframe, int nchan, int iscomplex, int nobandpass, int noise,
		  double bandwidth, float tone, float amp, float tone2, float amp2, int lsb, int doublesideband, 
		  int hilbert, float *mean, float *stdDev);

#define MAXSTR        255
#define BUFSIZE        25  // MB
#define MAXPOS          3
#define SMALLPOS        2
#define SMALLNEG        1
#define MAXNEG          0

typedef enum {FLOAT, FLOAT8, INT} data_type;

#define IPPMALLOC(var,type,n)	                               \
  var = ippsMalloc_ ## type(n);                                \
  if (var==NULL) {                                             \
    fprintf(stderr, "Error allocating %d bytes for %s at line %d\n", n, #var, __LINE__); \
    exit(EXIT_FAILURE);                                        \
  }                                                            \

#define PRINTVARINT(var) printf("DEBUG: %s=%d\n", #var, var);
#define PRINTVARUINT64(var) printf("DEBUG: %s=%" PRIu64 "\n", #var, var);
#define PRINTVARFLOAT(var) printf("DEBUG: %s=%f\n", #var, var);

int main (int argc, char * const argv[]) {
  char *filename, msg[MAXSTR], *header;
  int i, frameperbuf, loopframes, status, outfile, opt, tmp, headerBytes=0;
  int complexOutput;
  uint64_t nframe, tmp64;
  float **data, ftmp, *stdDev, *mean;
  Ipp64f *taps64=0;
  Ipp32f *taps=0;
  Ipp32fc *tapsC=0;
  ssize_t nr;
  vdif_header vheader;
#if USE_CODIFIO
  codif_header cheader;
#endif
  Ipp8u *framedata;

  int memsize = BUFSIZE;
  int nbits = 2;
  double bandwidth = 64;
  int channels = 1;
  int seed = 0;
  int ntap = DEFAULTTAP;
  int framesize = 9000;
  int iscomplex = 0;
  int dohilbert = 0;
  int nobandpass = 0;
  int noise = 0;
  int lsb = 0;
  int doublesideband = 0;
  int usecodif = 0;
  int norescale = 0;
  int year = -1;
  int month = -1;
  int day = -1;
  int dayno = -1;
  int threadID = 0;
#if USE_CODIFIO
  int groupID = 0;
#endif
  int period = 1;
  uint64_t totalsamples=0;
  double mjd = -1;
  float tone =  0;  // MHz
  float tone2 = 0;  // MHz
  float amp = 0.1;
  float amp2 = 0.0;
  float duration = 0; // Seconds
  char *timestr = NULL;

  struct option options[] = {
    {"bandwidth", 1, 0, 'w'},
    {"channels", 1, 0, 'C'},
    {"day", 1, 0, 'd'},
    {"dayno", 1, 0, 'D'},
    {"month", 1, 0, 'm'},
    {"mjd", 1, 0, 'M'},
    {"year", 1, 0, 'y'},
    {"time", 1, 0, 't'},
    {"framesize", 1, 0, 'F'},
    {"duration", 1, 0, 'l'},
    {"amp", 1, 0, 'a'},
    {"amp2", 1, 0, 'A'},
    {"tone", 1, 0, 'T'},
    {"tone2", 1, 0, '2'},
    {"ntap", 1, 0, 'x'},
    {"bufsize", 1, 0, 'B'},
    {"nbits", 1, 0, 'b'},
    {"bits", 1, 0, 'b'},
    {"complex", 0, 0, 'c'},
    {"hilbert", 0, 0, 'H'},
    {"nobandpass", 0, 0, 'N'},
    {"noise", 0, 0, 'n'},
    {"seed", 1, 0, 's'},
    {"doublesideband", 0, 0, 'G'},
    {"lsb", 0, 0, 'L'},
    {"norescale", 0, 0, 'R'},
    {"threadid", 1, 0, 'X'},
    {"help", 0, 0, 'h'},
#if USE_CODIFIO
    {"codifio", 0, 0, 'Z'},
    {"groupid", 1, 0, 'Y'},
    {"period", 1, 0, 'p'},
    {"totalsamples", 1, 0, 'Q'},
#endif
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  srand(time(NULL));
  
  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "s:w:C:d:D:m:M:y:t:F:l:a:A:T:2:x:B:b:cNnZLhp:", options, NULL);
    if (opt==EOF) break;
    
#define CASEINT(ch,var)                                     \
  case ch:						    \
    status = sscanf(optarg, "%d", &tmp);		    \
    if (status!=1)					    \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = tmp;                                            \
    break

#define CASEINT64(ch,var)                                     \
  case ch:						    \
    status = sscanf(optarg, "%" PRIu64, &tmp64);		    \
    if (status!=1)					    \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = tmp64;                                            \
    break
    
#define CASEBOOL(ch,var)                                    \
  case ch:						    \
    var = 1;						    \
    break

#define CASEFLOAT(ch,var)                                   \
  case ch:						    \
    status = sscanf(optarg, "%f", &ftmp);		    \
    if (status!=1)					    \
      fprintf(stderr, "Bad %s option %s\n", #var, optarg);  \
    else                                                    \
      var = ftmp;                                           \
    break
    
    switch (opt) {

      CASEINT('w', bandwidth);
      CASEINT('d', day);
      CASEINT('D', dayno);
      CASEINT('m', month);
      CASEINT('M', mjd);
      CASEINT('y', year);
      CASEINT('b', nbits);
      CASEINT('x', ntap);
      CASEINT('F', framesize);
      CASEINT('C', channels);
      CASEINT('s', seed);
      CASEINT('X', threadID);
      CASEBOOL('c', iscomplex);
      CASEBOOL('H', dohilbert);
      CASEBOOL('N', nobandpass);
      CASEBOOL('n', noise);
      CASEBOOL('L', lsb);
      CASEBOOL('G', doublesideband);
      CASEBOOL('R', norescale);
#if USE_CODIFIO
      CASEBOOL('Z', usecodif);
      CASEINT('p', period);
      CASEINT('Y', groupID);
      CASEINT64('Q', totalsamples);
#endif
      CASEFLOAT('B', memsize);
      CASEFLOAT('l', duration);
      CASEFLOAT('T', tone);
      CASEFLOAT('2', tone2);
      CASEFLOAT('a', amp);
      CASEFLOAT('A', amp2);

      case 't':
	timestr = strdup(optarg);
	break;

      case 'h':
	printf("Usage: generateVDIF [options]\n");
	printf("  -w/-bandwidth <BANWIDTH>  Channel bandwidth in MHz (64)\n");
	printf("  -b/-nbits <N>             Number of bits/sample (default 2)\n");
	printf("  -c/-complex               Generate complex data\n");
	printf("  -C/-channels <N>          Number of if channels (default 1)\n");
	printf("  -F/-framesize <FRAMESIZE> VDIF framesize\n");
	printf("  -l/-duration <DURATION>   Length of output, in seconds\n");
	printf("  -T/-tone <TONE>           Frequency (MHz) of tone to insert\n");
	printf("  -ntap <TAPS>              Number of taps for FIR filter to create band shape\n");
	printf("  -a/-amp <amp>             Amplitude of tone\n");
	printf("  -A/-amp2 <amp2>d          Amplitude of second tone\n");
	printf("  -T/-tone <TONE>           Frequency of tone (MHz)\n");
	printf("  -2/-tone2 <TONE>          Frequency of second tone  (MHz)\n");
	printf("  -n/-noise                 Include (correlated) noise\n");
	printf("  -s/-seed                  Set seed of 'thermal' noise (default auto)\n");
	printf("  -lsb                      Generate LSB data\n");
	printf("  -doublesideband           Generate double sideband if complex (default single sideband)\n");
	printf("  -hilbert                  Generate real data and convert to complex via hilbert transform\n");
	printf("  -threadID <N>             Set threadID\n");
#if USE_CODIFIO	
	printf("  -codif                    Generate CODIF data\n");
	printf("  -groupID <N>              Set groupID\n");
	printf("  -p/-period                CODIF period (default 1))\n");
	printf("  -totalsamples             Number of samples/period. Default calculate from bandwidth\n");
#endif
	printf("  -day <DAY>                Day of month of start time (now)\n");
	printf("  -month <MONTH>            Month of start time (now)\n");
	printf("  -dayno <DAYNO>            Day of year of start time (now)\n");
	printf("  -year <YEAR>              Year of start time (now)\n");
	printf("  -time <HH:MM:SS>          Year of start time (now)\n");
	printf("  -mjd <MJD>                MJD of start time\n");
	printf("  -nobandpass               Don't apply bandpass filter\n");
	printf("  -R/-norescale             DOn't rescale the data\n");
	return(1);
	break;
      
    case '?':
    default:
      break;
    }
  }

  int nchan = channels;
  if (argc==optind) {
    if (usecodif) {
      filename = strdup("Test.cdf");
    } else {
      filename = strdup("Test.vdif");
    }
  } else {
    filename = strdup(argv[optind]);
  }

  memsize *= 1024*1024;
  
  int cfact = 1;
  if (iscomplex) cfact = 2;
  if (iscomplex && dohilbert) {
    fprintf(stderr, "Warning: Cannot hilbery transform complex data. Disabling hilbert\n");
    dohilbert = 0;
  }
  
  complexOutput = iscomplex || dohilbert;

  if (doublesideband && !complexOutput) {
    fprintf(stderr, "Warning: Cannot use doublesideband on real data!!\n");
    exit(1);
  }

  if (seed==0) seed=time(NULL);
  
  // Frame size and number of sampler/frame
  int completesample = nbits*cfact*nchan; // bits

  if  (totalsamples==0) {
    totalsamples = bandwidth*1e6*2/cfact;
    period=1;
  }  else {
#if USE_CODIFIO
    bandwidth = totalsamples/period/1e6/2*cfact;
#endif
  }

  framesize = (framesize/8)*8; // Round framesize multiple of 8
  // need integral number of frames/sec
  uint64_t bytesperperiod = completesample*totalsamples/8;
  // Need integral # frames/sec AND integral # completesamples/frame
  while (((bytesperperiod % framesize) != 0) || ((framesize*8 % completesample) !=0)) {
    framesize -=8;
    if (framesize <= 0) {
      printf("Could not select valid framesize. Aborting\n");
      printf("  nbit=%d, nchan=%d, iscomplex=%d, bandwidth=%d, completesample=%d bits\n", 
	     nbits, nchan, iscomplex, bandwidth, completesample);
      exit(1);
    }
  }
  printf("Using framesize=%d\n", framesize);
  int samplesperframe = framesize*8/completesample; // Treat Re/Im of complex as seperate samples
  int framesperperiod = bytesperperiod/framesize;

  frameperbuf = memsize/(samplesperframe*cfact*sizeof(Ipp32f));

  if (duration==0) { // Just create BUFSIZE bytes
    nframe = frameperbuf;
  } else {
    nframe = duration*framesperperiod/period;
  }

  if (tone==0) tone = bandwidth/4;
  if (tone2>0 && amp2==0) amp2=amp;
#ifdef IPP9
  data = (Ipp32f**)malloc(nchan*sizeof(Ipp32f*));
  if (data==NULL) {
    perror("Memory allocation problem\n");
    exit(1);
  }
  for (i=0; i<nchan; i++) {
    IPPMALLOC(data[i], 32f, frameperbuf*samplesperframe*cfact);
  }

  phase = phase2 = 0;

  if (doublesideband) {
    IPPMALLOC(dsb_mult, 32fc, frameperbuf*samplesperframe*cfact/2);
    for (i=0;i<frameperbuf*samplesperframe*cfact/2;i++) {
      if (i%2) {
	dsb_mult[i].re = -1;
      } else {
	dsb_mult[i].re = 1;
      }
      dsb_mult[i].im = 0;
    }
  }
  
#ifdef USEIPP
  int pRandGaussStateSize;
  ippsRandGaussGetSize_32f(&pRandGaussStateSize);
  pRandGaussState = (IppsRandGaussState_32f *)ippsMalloc_8u(pRandGaussStateSize);
  ippsRandGaussInit_32f(pRandGaussState, 0.0, 1.0, seed); // Mean 0.0, RMS=1
  if (noise) {
    pRandGaussState2 = (IppsRandGaussState_32f *)ippsMalloc_8u(pRandGaussStateSize);
    ippsRandGaussInit_32f(pRandGaussState2, 0.0, amp, SEED);  // Mean 0.0, RMS=amp
  }

  IPPMALLOC(scratch, 32f, frameperbuf*samplesperframe*cfact);
  if (amp2>0 && tone2!=0.0) {
    IPPMALLOC(scratch2, 32f, frameperbuf*samplesperframe*cfact);
  }
  if (dohilbert) {
   int sizeSpec, sizeBuf;

   IPPMALLOC(scratchHilbert, 32fc, frameperbuf*samplesperframe);
   status = ippsHilbertGetSize_32f32fc(frameperbuf*samplesperframe, ippAlgHintNone, &sizeSpec, &sizeBuf);

   hSpec = (IppsHilbertSpec*)ippMalloc(sizeSpec);
   IPPMALLOC(hBuffer, 8u, sizeBuf);
   status = ippsHilbertInit_32f32fc(frameperbuf*samplesperframe, ippAlgHintNone, hSpec, hBuffer);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error calling ippsHilbertInit (%s) (Line %d)\n", ippGetStatusString(status), __LINE__ -2);
      exit(1);
    }
  }

  int specSize;
  int bufsize, bufsize2;
#endif

  if (!nobandpass) {
    // Initialise FIR filter
    IPPMALLOC(taps64, 64f, ntap);
    IPPMALLOC(taps, 32f, ntap);
    IPPMALLOC(tapsC, 32fc, ntap);
    IPPMALLOC(dly, 32f, (ntap-1)*cfact); 
    ippsZero_32f(dly, (ntap-1)*cfact);

    status = ippsFIRGenGetBufferSize(ntap, &bufsize);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error calling ippsFIRGenGetBufferSize (%s)\n", ippGetStatusString(status));
      exit(1);
    }
    IPPMALLOC(buf, 8u, bufsize);
    status = ippsFIRGenBandpass_64f(0.02, 0.48, taps64, ntap, ippWinHamming, ippTrue, buf);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error generating tap coefficients (%s)\n", ippGetStatusString(status));
      exit(1);
    }
    ippsConvert_64f32f(taps64, taps, ntap);

    // Real FIR filter
    status = ippsFIRSRGetSize (ntap, ipp32f, &specSize, &bufsize);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error Getting filter initialisation sizes (%s)\n", ippGetStatusString(status));
      exit(1);
    }
    pSpec = (IppsFIRSpec_32f*)ippsMalloc_8u(specSize);
    status = ippsFIRSRInit_32f(taps, ntap, ippAlgAuto, pSpec);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error Initialising filter (%s)\n", ippGetStatusString(status));
      exit(1);
    }

    status = ippsFIRGenHighpass_64f(0.02, taps64, ntap, ippWinHamming, ippTrue, buf);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error generating tap coefficients (%s)\n", ippGetStatusString(status));
      exit(1);
    }
    for (i=0; i<ntap; i++) {
      tapsC[i].re = taps64[i];
      tapsC[i].im = 0;
    }
    ippsFree(taps64);
    ippsFree(buf);

    // Complex FIR Filter
    status = ippsFIRSRGetSize (ntap, ipp32fc, &specSize, &bufsize2);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error Getting filter initialisation sizes (%s)\n", ippGetStatusString(status));
      exit(1);
    }
    pcSpec = (IppsFIRSpec_32fc*)ippsMalloc_8u(specSize);
    status = ippsFIRSRInit_32fc(tapsC, ntap, ippAlgAuto, pcSpec);
    if (status != ippStsNoErr) {
      fprintf(stderr, "Error Initialising filter (%s)\n", ippGetStatusString(status));
      exit(1);
    }

    if (bufsize2 > bufsize) bufsize = bufsize2;
    IPPMALLOC(buf, 8u, bufsize);
  }
#endif

  IPPMALLOC(framedata, 8u, framesize);
  ippsSet_8u('Z', framedata, framesize);

  double thismjd = currentmjd();
  int thisday, thismonth, thisyear;
  double ut;
  mjd2cal(thismjd, &thisday, &thismonth, &thisyear, &ut);

  if (year==-1) year = thisyear;
  if (day==-1) day = thisday;
  if (month==-1) month = thismonth;
  if (dayno!=-1) dayno2cal(dayno, year, &day, &month);

  if (timestr!=NULL) {
    int hour, min, sec;
    status = sscanf(timestr, "%2d:%2d:%2d", &hour, &min, &sec);
    if (status==0) {
      fprintf(stderr, "Warning: Could not parse %s (%d)\n", timestr, status);
    } else {
      ut = ((sec/60.0+min)/60.0+hour)/24.0;
    }
  }

  mjd = cal2mjd(day, month, year)+ut;

  memsize *= 1024*1024;

  if (usecodif) {
    int sampleblock = 8*8; // bits
    while (sampleblock % completesample) sampleblock += 64;
    sampleblock /= 64;
    
#if USE_CODIFIO
    header = (char*)&cheader;
    headerBytes = CODIF_HEADER_BYTES;

    // Need to calculate a couple of these values
#ifdef CODIFV2
    status = createCODIFHeader(&cheader, framesize, threadID, groupID, nbits, nchan, sampleblock, period, totalsamples,
			       complexOutput, "Tt", 0);
#else
    status = createCODIFHeader(&cheader, framesize, threadID, groupID, nbits, nchan, sampleblock, period, totalsamples,
			       complexOutput, "Tt");
#endif
    if (status!=CODIF_NOERROR) {
      printf("Error creating CODIF header. Aborting\n");
      exit(1);
    }
 #ifdef CODIFV2
#ifdef TEXTURE
    // Whack some texture to the extended bytes
    cheader.meta1 = 0xAAAAAAAA;
    cheader.meta2 = 0xBBBBBBBB;  
    cheader.meta3 = 0xCCCCCCCC;
#else
    // Whack some texture to the extended bytes
    cheader.meta1 = 0x0;
    cheader.meta2 = 0x0;  
    cheader.meta3 = 0x0;
#endif
#else
#ifdef TEXTURE
    // Whack some texture to the extended bytes
    vheader.extended2 = 0xAAAAAAAA;
    vheader.extended3 = 0xBBBBBBBB;  
    vheader.extended4 = 0xCCCCCCCC;
#else
    // Whack some texture to the extended bytes
    vheader.extended2 = 0x0;
    vheader.extended3 = 0x0;  
    vheader.extended4 = 0x0;
#endif
#endif
    setCODIFEpochMJD(&cheader, mjd);
    setCODIFFrameMJDSec(&cheader, (uint64_t)floor(mjd*60*60*24));
    setCODIFFrameNumber(&cheader,0);
#endif
  } else {
    header = (char*)&vheader;
    headerBytes = VDIF_HEADER_BYTES;
    status = createVDIFHeader(&vheader, framesize, threadID, nbits, nchan, complexOutput, "Tt");
    if (status!=VDIF_NOERROR) {
      printf("Error creating VDIF header. Aborting\n");
      exit(1);
    }
#ifdef TEXTURE
    // Whack some texture to the extended bytes
    vheader.extended2 = 0xAAAAAAAA;
    vheader.extended3 = 0xBBBBBBBB;  
    vheader.extended4 = 0xCCCCCCCC;
#else
    // Whack some texture to the extended bytes
    vheader.extended2 = 0x0;
    vheader.extended3 = 0x0;  
    vheader.extended4 = 0x0;
#endif
    setVDIFEpochMJD(&vheader, mjd);
    setVDIFFrameMJDSec(&vheader, (uint64_t)floor(mjd*60*60*24));
    setVDIFFrameNumber(&vheader,0);
  }

  mean = malloc(sizeof(float)*nchan);
  stdDev = malloc(sizeof(float)*nchan);
  
  outfile = open(filename, OPENWRITEOPTIONS, S_IRWXU|S_IRWXG|S_IRWXO); 
  if (outfile==-1) {
    sprintf(msg, "Failed to open output (%s)", filename);
    perror(msg);
    exit(1);
  }
  printf("Writing %s\n", filename);

  while (nframe>0) {
    if (nframe>frameperbuf)
      loopframes = frameperbuf;
    else
      loopframes = nframe;

    
    generateData(data, loopframes, samplesperframe, nchan, iscomplex, nobandpass, noise, bandwidth, tone, amp, tone2, amp2, lsb, doublesideband, dohilbert, mean, stdDev);

    for (i=1;i<nchan;i++) {
      mean[0] += mean[i];
      stdDev[0] += stdDev[i];
    }
    mean[0] /= nchan;
    stdDev[0] /= nchan;

    if (norescale) {
      if (nbits==8)
	stdDev[0] = 10;
      else if (nbits==16)
	stdDev[0] = 40;
    }

    for (i=0; i<loopframes; i++) {

      if (nbits==2) {
	if (nchan==1) {
	  status = pack2bit1chan(data, i*samplesperframe*cfact, framedata,  mean[0], stdDev[0], samplesperframe*cfact);
	  if (status) exit(1);
	} else {
	  if (iscomplex)
	    status = pack2bitNchan_complex(data, nchan, i*samplesperframe*cfact, framedata,  mean[0], stdDev[0], samplesperframe*cfact);
	  else
	    status = pack2bitNchan(data, nchan, i*samplesperframe*cfact, framedata,  mean[0], stdDev[0], samplesperframe*cfact);
	  if (status) exit(1);
	}
      } else if (nbits==8) {
	status = pack8bitNchan(data, nchan, i*samplesperframe*cfact, framedata,  mean[0], stdDev[0], samplesperframe*cfact, usecodif, iscomplex);
      } else if (nbits==16) {
	status = pack16bitNchan(data, nchan, i*samplesperframe*cfact, (Ipp16u*)framedata,  mean[0], stdDev[0], samplesperframe*cfact, usecodif, iscomplex);
      } else {
	printf("Unsupported number of bits\n");
	exit(1);
      }

      nr = write(outfile, header, headerBytes);
      if (nr == -1) {
	sprintf(msg, "Writing to %s:", filename);
       
	perror(msg);
	exit(1);
      } else if (nr != headerBytes) {
	printf("Error: Partial write to %s\n", filename);
	exit(1);
      }
    
      nr = write(outfile, framedata, framesize); 
      if (nr == -1) {
	sprintf(msg, "Writing to %s:", filename);
	perror(msg);
	exit(1);
      } else if (nr != framesize) {
	printf("Error: Partial write to %s\n", filename);
	exit(1);
      }
      if (usecodif) {
#if USE_CODIFIO
	nextCODIFHeader(&cheader, framesperperiod);
#endif
      } else {
	nextVDIFHeader(&vheader, framesperperiod);
      }
      nframe--;
    }
  }
  close(outfile);

  free(filename);
  if (timestr!=NULL) free(timestr);
  for (i=0; i<nchan; i++) ippsFree(data[i]);
  free(data);
  ippsFree(scratch);
  if (scratch2!=NULL) ippsFree(scratch2);
  if (dohilbert) {
    ippsFree(scratchHilbert);
    ippsFree(hBuffer);
    ippFree(hSpec);
  }
  if (!nobandpass) {
    ippsFree(taps);
    ippsFree(tapsC);
    ippsFree(dly);
    ippsFree(buf);
    ippsFree(pSpec);
    ippsFree(pcSpec);
  }
  return(0);
}
  
float gaussrand() {
  static float V1, V2, S;
  static int phase = 0;
  float X;

  if (phase == 0) {
    do {
      float U1 = (float)rand() / RAND_MAX;
      float U2 = (float)rand() / RAND_MAX;
			
      V1 = 2 * U1 - 1;
      V2 = 2 * U2 - 1;
      S = V1 * V1 + V2 * V2;
    } while(S >= 1 || S == 0);

    X = V1 * sqrt(-2 * log(S) / S);
  } else
    X = V2 * sqrt(-2 * log(S) / S);
  
  phase = 1 - phase;
  
  return X;
}

double currentmjd () {
  struct tm *tim;
  struct timeval currenttime;

  setenv("TZ", "", 1); /* Force mktime to return gmt not local time */
  tzset();
  gettimeofday(&currenttime, NULL);

  tim = localtime(&currenttime.tv_sec);
  return(tm2mjd(*tim)+(currenttime.tv_usec/1.0e6)/(24.0*60.0*60.0));
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

int MeanStdDev(const float *src, int length, float *mean, float *StdDev) { 
  int i;
  double sum = 0;
  double sumsqr = 0;
  for(i=0;i<length;i++) {
    sum += src[i];
    sumsqr += src[i]*src[i];
  }
  *mean = sum/length;
  *StdDev = sqrt((sumsqr-(sum*sum/length))/(length-1));
  return 0; 
}

#define F2BIT(f,j) {						\
  if(f >= maxposThresh)  /* large positive */		\
    ch[j] = MAXPOS;					\
  else if(f <= maxnegThresh) /* large negative */	\
    ch[j] = MAXNEG;					\
  else if(f > mean)  /* small positive */		\
    ch[j] = SMALLPOS;					\
  else  /* small negative */				\
    ch[j] = SMALLNEG;					\
}


int pack2bit1chan(Ipp32f **in, int off, Ipp8u *out, float mean, float stddev, int len) {
  int i, j, ch[4];
  float maxposThresh, maxnegThresh, *f;
  
  if (len%4!=0) {
    printf("Can only pack multiple of 4 samples!\n");
    return(1);
  }

  maxposThresh = mean+stddev*0.95;
  maxnegThresh = mean-stddev*0.95;

  f = &in[0][off];
  j = 0;
  for (i=0;i<len;) {
    F2BIT(f[i],0);
    i++;
    F2BIT(f[i],1);
    i++;
    F2BIT(f[i],2);
    i++;
    F2BIT(f[i],3);
    i++;
    out[j] = (ch[0])|((ch[1]<<2) )|((ch[2]<<4) )|((ch[3]<<6) );
    j++;
  }

  return 0;
}

int pack2bitNchan_complex(Ipp32f **in, int nchan, int off, Ipp8u *out, float mean, float stddev, int len) {
  int i, j, n, k, ch[4];
  float maxposThresh, maxnegThresh;
  
  if (len*nchan%2!=0) {
    printf("Can only pack multiple of 2 samples*channels!\n");
    return(1);
  }

  maxposThresh = mean+stddev*0.95;
  maxnegThresh = mean-stddev*0.95;

  j = 0;
  k = 0;
  for (i=off;i<len+off;i+=2) {
    for (n=0; n<nchan; n++) {
      F2BIT(in[n][i],k);
      k++;
      F2BIT(in[n][i+1],k);
      k++;
      if (k==4) {
	out[j] = (ch[0])|((ch[1]<<2) )|((ch[2]<<4) )|((ch[3]<<6));
	j++;
	k = 0;
      }
    }
  }
  return 0;
}
int pack2bitNchan(Ipp32f **in, int nchan, int off, Ipp8u *out, float mean, float stddev, int len) {
  // Should check 31bit "off" offset is enough bits
  int i, j, n, k, ch[4];
  float maxposThresh, maxnegThresh;
  
  if (len*nchan%4!=0) {
    printf("Can only pack multiple of 4 samples*channels!\n");
    return(1);
  }

  maxposThresh = mean+stddev*0.95;
  maxnegThresh = mean-stddev*0.95;

  j = 0;
  k = 0;
  for (i=off;i<len+off;i++) {
    for (n=0; n<nchan; n++) {
      F2BIT(in[n][i],k);
      k++;
      if (k==4) {
	out[j] = (ch[0])|((ch[1]<<2) )|((ch[2]<<4) )|((ch[3]<<6));
	j++;
	k = 0;
      }
    }
  }
  return 0;
}

inline Ipp8s scaleclip(Ipp32f x, Ipp32f scale) {
  x *= scale;
  if (x>127) // Clip
    x = 127;
  else if (x<-128)
    x = -128;
  return lrintf(x);
}

inline Ipp16s scaleclip16(Ipp32f x, Ipp32f scale) {
  x *= scale;
  if (x>IPP_MAX_16S) // Clip  
    x = IPP_MAX_16S;
  else if (x<IPP_MIN_16S)
    x = IPP_MIN_16S;
  return lrintf(x);
}

int pack8bitNchan(Ipp32f **in, int nchan, int off, Ipp8u *out, float mean, float stddev, int len, int iscodif, int iscomplex) {
  // Should check 31bit "off" offset is enough bits
  int i, j, n;

  float scale = 10/stddev;

  j = 0;
  if (iscomplex) {
    for (i=off;i<len+off;i+=2) {
      for (n=0; n<nchan; n++) {
	out[j] = (Ipp8u)scaleclip(in[n][i], scale);
	if (!iscodif) out[j] ^= 0x80;
	j++;
	out[j] = (Ipp8u)scaleclip(in[n][i+1], scale);
	if (!iscodif) out[j] ^= 0x80;
	j++;
      }
    }
  } else {
    for (i=off;i<len+off;i++) {
      for (n=0; n<nchan; n++) {
	out[j] = (Ipp8u)scaleclip(in[n][i], scale);
	if (!iscodif) out[j] ^= 0x80;
	j++;
      }
    }
  }
  return 0;
}

int pack16bitNchan(Ipp32f **in, int nchan, int off, Ipp16u *out, float mean, float stddev, int len, int iscodif, int iscomplex) {
  // Should check 31bit "off" offset is enough bits
  int i, j, n;

  float scale = 40/stddev;

  j = 0;
  if (iscomplex) {
    for (i=off;i<len+off;i+=2) {
      for (n=0; n<nchan; n++) {
	out[j] = (Ipp16u)scaleclip16(in[n][i], scale);
	if (!iscodif) out[j] ^= 0x80;
	j++;
	out[j] = (Ipp16u)scaleclip16(in[n][i+1], scale);
	if (!iscodif) out[j] ^= 0x8000;
	j++;
      }
    }
  } else {
    for (i=off;i<len+off;i++) {
      for (n=0; n<nchan; n++) {
	out[j] = (Ipp16u)scaleclip16(in[n][i], scale);
	if (!iscodif) out[j] ^= 0x8000;
	j++;
      }
    }
  }
  return 0;
}



int leap (int year) {
  return (((!(year%4))&&(year%100))||(!(year%400)));
}

static int days[] = {31,28,31,30,31,30,31,31,30,31,30,31};

void dayno2cal (int dayno, int year, int *day, int *month) {
  int end;

  if (leap(year)) {
    days[1] = 29;
  } else {
    days[1] = 28;
  }

  *month = 0;
  end = days[*month];
  while (dayno>end) {
    (*month)++;
    end+= days[*month];
  }
  end -= days[*month];
  *day = dayno - end;
  (*month)++;

  return;
}

double cal2mjd(int day, int month, int year) {
  int m, y, c, x1, x2, x3;

  if (month <= 2) {
    m = month+9;
    y = year-1;
  } else {
    m = month-3;
    y = year;
  }

  c = y/100;
  y = y-c*100;

  x1 = 146097*c/4;
  x2 = 1461*y/4;
  x3 = (153*m+2)/5;

  return(x1+x2+x3+day-678882);
}

double tm2mjd(struct tm date) {
  int y, c;
  double dayfrac;

  if (date.tm_mon < 2) {
    y = date.tm_mon+1900-1;
  } else {
    y = date.tm_year+1900;
  }

  c = y/100;
  y = y-c*100;

  dayfrac = ((date.tm_hour*60.0+date.tm_min)*60.0+date.tm_sec)/(60.0*60.0*24.0);

  return(cal2mjd(date.tm_mday, date.tm_mon+1, date.tm_year+1900)+dayfrac);
}

#ifdef USEIPP

void generateData(Ipp32f **data, int nframe, int samplesperframe, int nchan, int iscomplex, int nobandpass,
		  int noise, double bandwidth, float tone, float amp, float tone2, float amp2, int lsb,
		  int doublesideband, int hilbert, float *mean, float *stdDev) {
  int n, nsamp, cfact;
  IppStatus status;
  if (iscomplex)
    cfact = 2;
  else
    cfact = 1;

  if (doublesideband && !(iscomplex || hilbert)) {
    fprintf(stderr, "Error: Cannot do doublesideband on real data in %s\n", __FUNCTION__);
    exit(1);
  }
  
  nsamp = nframe*samplesperframe;

  if (!noise) {
    if (iscomplex) {
      status = ippsTone_32fc((Ipp32fc*)scratch, nsamp, sqrt(amp), tone/bandwidth, &phase, ippAlgHintFast);
    } else {
      status = ippsTone_32f(scratch, nsamp, sqrt(amp), tone/(bandwidth*2), &phase, ippAlgHintFast);
    }
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error generating tone (%s)\n", ippGetStatusString(status));
      exit(1);
    }
  }

  if (amp2>0 && tone2!=0.0) {
    if (iscomplex) {
      status = ippsTone_32fc((Ipp32fc*)scratch2, nsamp, sqrt(amp2), tone2/bandwidth, &phase2, ippAlgHintFast);
    } else {
      status = ippsTone_32f(scratch2, nsamp, sqrt(amp2), tone2/(bandwidth*2), &phase2, ippAlgHintFast);
    }
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error generating second tone (%s)\n", ippGetStatusString(status));
      exit(1);
    }
  }

  for (n=0; n<nchan; n++) {
    mean[n] = 0;
    stdDev[n] = 0;

    status = ippsRandGauss_32f(data[n], nsamp*cfact, pRandGaussState);
    if (status!=ippStsNoErr) {
      fprintf(stderr, "Error generating Gaussian noise (%s)\n", ippGetStatusString(status));
      exit(1);
    }
#ifdef IPP9
    if (amp>0.0) {
      if (noise) {
	status = ippsRandGauss_32f(scratch, nsamp*cfact, pRandGaussState2);
	if (status==ippStsNoErr) status = ippsAdd_32f_I(scratch, data[n], nsamp*cfact);
	if (status!=ippStsNoErr) {
	  fprintf(stderr, "Error generating Gaussian noise2 (%s)\n", ippGetStatusString(status));
	  exit(1);
	}
      } else {
	status = ippsAdd_32f_I(scratch, data[n], nsamp*cfact);
	if (status!=ippStsNoErr) {
	  fprintf(stderr, "Error adding tone (%s)\n", ippGetStatusString(status));
	  exit(1);
	}
      }
      if (amp2>0 && tone2!=0.0) {
	status = ippsAdd_32f_I(scratch2, data[n], nsamp*cfact);
	if (status!=ippStsNoErr) {
	  fprintf(stderr, "Error adding second tone (%s)\n", ippGetStatusString(status));
	  exit(1);
	}
      }
    }
#endif
    if (!nobandpass) {
      if (iscomplex) {
	status = ippsFIRSR_32fc((Ipp32fc*)data[n], (Ipp32fc*)data[n], nsamp, pcSpec, (Ipp32fc*)dly, (Ipp32fc*)dly,  buf);
      } else {
	status = ippsFIRSR_32f(data[n], data[n], nsamp, pSpec, dly, dly, buf);
      }
      if (status!=ippStsNoErr) {
	fprintf(stderr, "Error applying FIR filter (%s) in %s\n", ippGetStatusString(status), __FUNCTION__);
	exit(1);
      }
    }

    if (hilbert) {
      int temp, hPhase = 0;
      status = ippsHilbert_32f32fc(data[n], scratchHilbert, hSpec, hBuffer);
      if (status!=ippStsNoErr) {
	fprintf(stderr, "Error applying Hilbert function (%s) in %s\n", ippGetStatusString(status), __FUNCTION__);
	exit(1);
      }
      status = ippsSampleDown_32fc(scratchHilbert, nsamp, (Ipp32fc*)data[n], &temp, 2, &hPhase);
      if (status!=ippStsNoErr) {
	fprintf(stderr, "Error applying ippsSampleDown (%s) in %s\n", ippGetStatusString(status), __FUNCTION__);
	exit(1);
      }
    }
    
    if (lsb) {
      status = ippsConj_32fc_I((Ipp32fc*)data[n], nsamp*cfact/2);
      if (status!=ippStsNoErr) {
	fprintf(stderr, "Error in ippsConj_32fc (%s) in %s\n", ippGetStatusString(status), __FUNCTION__);
	exit(1);
      }
    }

    if (doublesideband) {
      status = ippsMul_32fc_I(dsb_mult, (Ipp32fc*)data[n], nsamp*cfact/2);
      if (status!=ippStsNoErr) {
	fprintf(stderr, "Error in ippsMul_32fc (%s) in %s\n", ippGetStatusString(status), __FUNCTION__);
	exit(1);
      }
    }
    
    status = ippsMeanStdDev_32f(data[n], nsamp*cfact, &mean[n], &stdDev[n], ippAlgHintFast);
  }
}

#else 
void generateData(float **data, int nframe, int samplesperframe, int nchan,
		  int iscomplex, double bandwidth, float tone, float *mean, float *stdDev) {
  int i, n;
  float s;

  for (i=0; i<nframe*samplesperframe; i++) {
    s = sin(tone/bandwidth*i*M_PI)*0.05; // There may be a phase jump from buffer to buffer
    for (n=0; n<nchan; n++) {
      data[n][i] = gaussrand()+s;
    }
  }

  
  // Calculate RMS to allow thresholding
  *mean = 0;
  *stdDev = 0;
  for (n=0; n<nchan; n++) {
    float thismean, thisStdDev;
    MeanStdDev(data[n], nframe*samplesperframe, &thismean, &thisStdDev);
    *mean += thismean;
    *stdDev += thisStdDev;
  }
  *mean /= nchan;
  *stdDev /= nchan;
}
#endif


