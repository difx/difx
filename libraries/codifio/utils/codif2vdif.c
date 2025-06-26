/* Convert codif VLBI data to VDIF, using an FFT resampling approach
   ie Forward then backwads FFT

   Assume complex sampling - singe channel, single thread

*/

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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <getopt.h>
#include <sys/types.h>
#include <errno.h>
#include <complex.h>
#include <ipps.h>
#include <ippcore.h>
#include <vdifio.h>
#include <codifio.h>

#define CHUNKSIZE 4096 
#define MAXSTR        255
#define WHITESPACE " \t"
#define COMMENT_CHAR '#'

#define STDDEVTIMECONST 2  // Seconds

#define DEBUG(x) 

#define TIMESTR 50

//#define PRINTINT(x)   printf("DEBUG: " #x "=%d\n", x);
#define PRINTINT(x)
#define PRINTINT2(x) 

void convertdata(int infile, int outfile, char *buf, int offset, int dovalid);
int calculateVDIFframesize(int max, long long bandwidth, int nchan, int bits, int iscomplex);
int cal2mjd(int day, int month, int year);
int outfilename(char *infilename, char *outname, char* postfix, char *outdir);
void merge2vdif(Ipp8s *vdifframe, Ipp32fc **outchan, int offset, int nsample, Ipp32f *stddev);

int main (int argc, char * const argv[]) {
  int nfile, infile, outfile, opt;
  char *inbuf;
  char msg[MAXSTR+50];
  char outname[MAXSTR+1] = "";
  char *outdir = NULL;
  int outbits = 8;
  //int dovalid = 1;
  IppStatus status;
  double bandwidth;
  char postfix[MAXSTR+1] = "vdif";
  vdif_header vheader;
  codif_header cheader;
  IppsDFTSpec_C_32fc *specFwd, *specInv;
  
  struct option options[] = {
    {"postfix", 1, 0, 'p'},
    {"outdir", 1, 0, 'o'},
    {"dir", 1, 0, 'o'},
    {"bits", 1, 0, 'b'},
    {"novalid", 0, 0, 'v'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };


  /* Read command line options */
  while (1) {
    opt = getopt_long_only(argc, argv, "hp:b:v", options, NULL);
    if (opt==EOF) break;

    switch (opt) {

    case 'o':
      outdir = strdup(optarg);
      if (outdir[strlen(outdir)-1] == '/')  { // Remove trailing "/"
	outdir[strlen(outdir)-1] = 0;
      }
      break;

    case 'p':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Postfix too long\n");
	return(1);
      }
      strcpy(postfix, optarg);
      break;

    case 'b':
      outbits = atoi(optarg);
      break;

    case 'v':
      dovalid = 0;
      break;

    case 'h':
      printf("Usage: paf2vdif [options] <file> [<file> ...]\n");
      printf("  -o/outdir/dir <DIR>   Output directory for converted data\n");
      printf("  -p/postfix <postfix>  Postfix for output files\n");
      printf("  -b/-bits <bits >      Number of bits on output\n");
      //      printf("  -v/-novalid           Skip missing frames rather than adding invalid frames\n");
      printf("  -h/-help              This list\n");

      return(1);
    break;
    
    case '?':
    default:
      break;
    }
  }

  int outchan = 1; // Number of output channels
  int fftSize = 128*4; // Must be factor of 128
  int ifftSize = 0; // Calculated later
  unsigned long long targetBandwidth = 128; // MHz
  int threadid = 0;

  if (outdir!=NULL) {
    printf("Writing data to %s\n", outdir);
  }


  // Initialise VDIF header
  vdifframesize =  calculateVDIFframesize(9000, targetBandwidth*1e6, 1, outputBits, 1);  // This is excluding headersize
  vdifframeSamples = vdifframesize*8 / 1 / (outputBits*2); // 1 pol, complex, 8bit
  //bytesperOutputSample = 2 * 2; // 2pol, 8bit, complex
  PRINTINT(vdifframesize);
  PRINTINT(vdifframeSamples);

  status = createVDIFHeader(&vheader, vdifframesize, threadid, outputbits, 1, 1, "At");
  if (status!=VDIF_NOERROR) {
    fprintf(stderr, "Error creating VDIF header(%d). Aborting\n", status);
    exit(1);
  }

  // Whack some texture to the extended bytes
  vheader.extended2 = 0xAAAAAAAA;
  vheader.extended3 = 0xBBBBBBBB;  
  vheader.extended4 = 0xCCCCCCCC;

  vdifframeusec = vdifframesize*8 /(outputBits*2)/targetBandwidth; 
  vdifframepersec = 1e6/vdifframeusec;
  PRINTINT(vdifframepersec);

  bool first = true;
  for (nfile=optind; nfile<argc; nfile++) {

    status = outfilename(argv[nfile], outname, postfix, outdir);
    if (status) exit(1);

    infile = open(argv[nfile], OPENREADOPTIONS);
    if (infile==-1) {
      sprintf(msg, "Failed to open input file (%s)", argv[nfile]);
      perror(msg);
      exit(1);
    }

    outfile = open(outname, OPENWRITEOPTIONS, S_IRWXU|S_IRWXG|S_IRWXO); 
    if (outfile==-1) {
      sprintf(msg, "Failed to open output file (%s)", outname);
      perror(msg);
      close(infile);
      exit(1);
    }
    printf("Reading %s, writing %s\n", argv[nfile], outname);

    
    // Read the first frame and check sensible results
    ssize_t nread = read(infile, &cheader, CODIF_HEADER_BYTES);
    if (nread==0) {  // EOF
      fprintf(stderr, "Empty file.  Exiting\n");
      close(infile);
      close(outfile);
      exit(1);
    } else if (nread==-1) {
      perror("Error reading codif file: ");
      close(infile);
      close(outfile);
      exit(1);
    } else if (nread<CODIF_HEADER_BYTES) {
      fprintf(stderr, "Under-filled read %lu/%d from codif file. Aborting\n", nread, CODIF_HEADER_BYTES);
      close(infile);
      close(outfile);
      exit(1);
    }
    // Check
    int nchan = getCODIFNumChannels(&cheader);
    if (nchan!=1) {
      fprintf(stderr, "Unsupported # channels (%d)\n", nchan);
      close(infile);
      close(outfile);
      exit(1);
    }
    if (cheader->nbits!=8) {
      fprintf(stderr, "Unsupported # bits (%d)\n", cheader->nbits);
      close(infile);
      close(outfile);
      exit(1);
    }
    if (!cheader->iscomplex) {
      fprintf(stderr, "Not complex data\n");
      close(infile);
      close(outfile);
      exit(1);
    }

    // Rewind file
    int sought = lseek(infile, 0, SEEK_SET);
    if (sought==-1) {
      perror("Error rewinding file position");
      close(infile);
      close(outfile);
      exit(1);
    }

    if (first) {
      first = false;

      bandwidth = (double)getCODIFTotalSamples(&cheader)/(double)getCODIFPeriod(&cheader);
      printf("DEBUG: Bandwidth=%f\n", bandwidth);

      if (targetbandwidth*1.0e6*outchan>bandwidth) {
	fprintf(stderr, "Requested output bandwidth > input bandwidth. Quiting\n");
	close(infile);
	close(outfile);
	exit(1);
      }

      ifftSize = lround(targetBandwidth*1e6/bandwidth*fftSize);
      printf("DEBUG: Inverse FFT size = %d\n", ifftSize); // Should check sensible number is produced.

      status = allocMemory();
      if (status) {
	close(infile);
	close(outfile);
	exit(1);
      }
      
      // Initialised FFTs
      IppsDFTSpec_C_32fc *specFwd, *specInv;
      Ipp8u *workbufFwd, *workbufInv, *dftInitBuf;
      Ipp8s *vdifframe;
      Ipp16s *dataptr=NULL;
  
      // Initialise FFT
      ippsDFTGetSize_C_32fc(fftSize, IPP_FFT_DIV_INV_BY_N, ippAlgHintFast,  &sizeDFTSpec, &sizeDFTInitBuf, &wbufsize);
      specFwd = (IppsDFTSpec_C_32fc*)ippsMalloc_8u(sizeDFTSpec);
      dftInitBuf = ippsMalloc_8u(sizeDFTInitBuf);
      workbufFwd = ippsMalloc_8u(wbufsize);
      ippsDFTInit_C_32fc(CHANFFT, IPP_FFT_DIV_INV_BY_N, ippAlgHintFast, specFwd, dftInitBuf);
      if (dftInitBuf) ippFree(dftInitBuf);

      ippsDFTGetSize_C_32fc(ifftSize, IPP_FFT_DIV_INV_BY_N, ippAlgHintFast,  &sizeDFTSpec, &sizeDFTInitBuf, &wbufsize);
      specInv = (IppsDFTSpec_C_32fc*)ippsMalloc_8u(sizeDFTSpec);
      dftInitBuf = ippsMalloc_8u(sizeDFTInitBuf);
      workbufInv = ippsMalloc_8u(wbufsize);
      ippsDFTInit_C_32fc(ifftSize, IPP_FFT_DIV_INV_BY_N, ippAlgHintFast, specInv, dftInitBuf);
      if (dftInitBuf) ippFree(dftInitBuf);
    }

    setVDIFEpoch(&vheader, getCODIFEpoch(&cheader));

    // TODO Calculate offset and first header
    

    while (1) {
      readCODIFData(infile, codifData, codifValidity, FFTSize);  // Read and unpack
    
      // Use FFT to filter
      int startpoint = (fftSize-ifftSize)/2;
      // Do the forward FFT
      for (int i=0; i<nFFT; i++) {
	status = ippsDFTFwd_CToC_32fc(&dataBuf[fftSize*i], tmpFFT, specFwd, workbufFwd);

	// To support > 1 channel need to loop here
	status = ippsDFTInv_CToC_32fc(&tmpFFT[startPoint], dataBuf[ifftSize*i], specInv, workbufInv);
      }
    
      converttoVDIF(dataBuf, vdifBuf); // Requantise and convert to VDIF

      ssize_t nwrote = write(outfile, vdifframe, vdifbufsize); 
      if (nr == -1) {
	perror("Writing to output VDIF:");
	exit(1);
      } else if (nr != vdifbufsize) {
	printf("Error: Partial write to output VDIF\n");
	exit(1);
      }
    }
    close(infile);
    close(outfile);
  }

  return(0);
}
  
void flip_band(Ipp32fc  *data, uint64_t n) {
  ippsConj_32fc_I(data, n);
}


int allocMemory(Ipp8u **readFrames, Ipp32fc **dataBuf, Ipp8u **frameValidity, Ipp8u **fftValidity, Ipp32fc **tmpFFT,
		int fftSize, int ifftSize, int frameSize, int nFrame, int bits, int nchan) {

  int totalSamples = (frameSize-CODIF_HEADER_BYTES)*8/(nchan*bits*2)*snFrame;
  
  *readFrames = ippsMalloc_8u(frameSize*nFrame);
  int nValidity  = (totalSamples + fftSize -1) / fftSize; // Maximum number of FFTs which will fit
  *frameValidity = ippsMalloc_8u(nFrame);
  *fftValidity = ippsMalloc_8u(nValidity);
  *dataBuf = ippsMalloc_32fc(totalSamples); // unpacked 32bit floats
  *tmpFFT = ippsMalloc_32fc(fftsize);
  // allocate resampled data, with space for VDIF header
  
  if (*readFrames==NULL || *frameValidity==NULL || *fftValidity==NULL || *dataBuf==NULL || *tmpFFT==NULL) {
    fprintf(stderr, "Error allocating memory\n");
    return(1);
  }
  return(0);
};


// Read CODIF data and convert to floating point
int readCODIFData(int infile, Ipp8u *codifFrames, Ipp8u *frameValidity, Ipp8u *fftValidity, Ipp32_fc *dataBuf
		  int frameSize, int nFrame, uint64_t firstframe) {
  ssize_t nread;
  codif_header *cheader;
  int period, frameperperiod, samplesperframe, nchan, bits, datasize;
  uint64_t framesinceEpoch; // Count frames since start of Epoch. Need 41bits for 13.5usec frame length
  uint64_t frameOffset;
  IppStatus status;

  datasize = framesize-CODIF_HEADER_BYTES;
  
  status = ippsZero_8u(frameValidity, nFrame);
  //status = ippsZero_8u(fftValidity, ??);
  
  nread = read(infile, codifFrames, frameSize*nframe);
  if (nread==0) { // EOF
    return(0);
  } else if (nread==-1) {
    perror("Error reading file: ");
    return(1);
  } else if (nread%frameSize) { // Partial frame read
    fprintf(stderr, "Partial frame read - corrupt file?. Exiting\n");
    return(1);
  }
    
  int nloop = nread / framesize;

  // Load frames and unpack into floating point. This also sorts and handles missing packets
  bool first = true;
  for (int n=0; n<nloop; n++) {
    cheader = (codif_header*)&codifFrames[framesize*n];

    // Should check frame header consistency

    if (first) {
      first = false;
      period = getCODIFPeriod(cheader);
      frameperperiod = getCODIFFramePerPeriod(cheader);
      nchan = getCODIFNumChannels(cheader);
      bits =  getCODIFBitsPerSample(cheader);
      samplesperframe = datasize*8/(nchan*bits*2); // Assume complex
    }

    framesinceEpoch = getCODIFFrameEpochSecOffset(cheader)*frames + getCODIFgetCODIFFrameNumber(header);
    frameOffset = framesinceEpoch - firstframe;

    if (frameOffset<0) { // Skip
      printf("DEBUG: Skipping frame, too early\n");
      continue;
    } else if (frameOffset>=nFrame) {
      // Should be rewinding file
      printf("DEBUG: Skipping frame, too late\n");
      continue;
    }

    
    // Convert integer to floating point
    if (bits==8) {
      status =  ippsConvert_8s32f((Ipps8*)&codifFrames[framesize*n+CODIF_HEADER_BYTES], (Ipp32f*)&dataBuf[frameOffset*samplesperframe], samplesperframe);
    } else if (bits==16) {
      status =  ippsConvert_16s32f((Ipps8*)&codifFrames[framesize*n+CODIF_HEADER_BYTES], (Ipp32f*)&dataBuf[frameOffset*samplesperframe], samplesperframe);
    } else {
      fprintf(stderr, "Error: Unsupported # bits - how did you get here?\n");
      return(1);
    }
    frameValidity[frameOffset] = 1;
  }
  return(0);
}

int converttoVDIF(Ipp32_fc *dataBuf, Ipp8u *vdifData, vdif_header *vheader, int framesize, int nframe, int outputbits, Ipp32f target, int vdifframepersec) {
  IppsStatus status;
  int datasize = framesize - VDIF_HEADER_BYTES;
  Ipp32f mean, stddev, scale;

  int samplesperframe = datasize*8/(bits*2);
  
  // Get RMS of data - should add some smoothing

  status = ippsMeanStdDev_32f((Ipp32f*)dataBuf, samplesperframe*nframe*2, &mean, &stddev, ippAlgHintFast);

  scale = target/stddev;
  
  // Remove offset and scale by RMS
  status = ippsSubC_32f_I(mean, (Ipp32f*)dataBuf, samplesperframe*nframe*2);
  status = ippsSubC_32f_I(scale, dataBuf, samplesperframe*nframe);

  for (int i=0; i<nframe; i++) {
    status = ippsCopy_8u(vheader, &vdifData[framesize*i], VDIF_HEADER_BYTES);

    if (outputbits==8) {
      status = ippsConvert_32f8s_Sfs((Ipp32f*)&dataBuf[samplesperframe*2*i], vdifData[framesize*i+VDIF_HEADER_BYTES], samplesperframe*2, ippRndNear, 0);
    } else if (outputbits==16) {
      status = ippsConvert_32f16s_Sfs((Ipp32f*)&dataBuf[samplesperframe*2*i], vdifData[framesize*i+VDIF_HEADER_BYTES], samplesperframe*2, ippRndNear, 0);
    } else {
      fprintf(stderr, "Error - do not support %d bit output\n", outoutbits);
      exit(1);
    }
    nextVDIFHeader(vheader, vdifframepersec);
  }
  return(0);
}

int calculateVDIFframesize(int max, long long bandwidth, int nchan, int bits, int iscomplex) {
  // Max:        maximum number of data bytes/frame
  // bandwidth:  Channel bandwidth (Hz)
  // nchan:      Number of channels
  // bits:       Number bits per sample (per real/imag for complex)
  // iscomplex:  Is data complex sampling?
  
  int completesamplebits, sampleblock, framesize;
  unsigned long long rate;
  
  rate = nchan*bits*bandwidth*2;  // Nyquist or complex
  //printf("DEBUG: rate = %llu bits/sec\n", rate);
  
  if (max<8) {
  fprintf(stderr, "Frame size %d too small\n", max);
  return(-1);
  }
  
  completesamplebits = nchan*bits;
  if (iscomplex) completesamplebits *=2;

  if (completesamplebits < 64) {
    // An integral number of complete samples need to fit within 64bits
    if (64 % completesamplebits != 0) {
      fprintf(stderr, "Complete sample size of %d bits is not supported\n", completesamplebits);
      return(-1);
    }
    sampleblock = 8;
  } else {
    // A complete samples must be divisible by 64bits
    if (completesamplebits % 64 != 0) {
      fprintf(stderr, "Complete sample size of %d bits is not supported\n", completesamplebits);
      return(-1);
    }
    sampleblock = completesamplebits;
  }
  
  if (max<sampleblock) {
    fprintf(stderr, "Frame size %d too small\n", max);
    return(-1);
  }
  
  framesize = (max/sampleblock) * sampleblock;
  while ((framesize>=0) && (rate % (framesize*8)!=0)) {
    framesize -= sampleblock;
  }
  if (framesize==0) {
  fprintf(stderr, "Could not find suitable frame < %d bytes\n", max);
    framesize=-1;
  }
  return(framesize);
  
}

int cal2mjd(int day, int month, int year) {
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

Ipp8s scaleclip(Ipp32f x, Ipp32f scale) {
  x *= scale;
  if (x>127) // Clip
    x = 127;
  else if (x<-127)
    x = -127;
  return x;
}

void merge2vdif(Ipp8s *vdifframe, Ipp32fc **outchan, int offset, int nsample, Ipp32f *stddev) {
  int i, j;
  Ipp32f scaleA, scaleB;
  Ipp32fc *polA, *polB;

  // Target RMS 10
  scaleA = 10/stddev[0];
  scaleB = 10/stddev[1];

  //scaleA = 0.14;
  //scaleB = 10/stddev[1];

  polA = &outchan[0][offset];
  polB = &outchan[1][offset];

#if 0
  printf("DEBUGA:");
  for (i=0;i<nsample;i++) printf(" %.1f %.1fi", polA[i].re, polA[i].im);
  printf("\nDEBUGB:");
  for (i=0;i<nsample;i++) printf(" %.1f %.1fi", polB[i].re, polB[i].im);
  printf("\n");
#endif

  j = 0;
  for (i=0; i<nsample; i++) {

    vdifframe[j++] = scaleclip(polA[i].re,scaleA);
    vdifframe[j++] = scaleclip(polA[i].im,scaleA);
    vdifframe[j++] = scaleclip(polB[i].re,scaleB);
    vdifframe[j++] = scaleclip(polB[i].im,scaleB);
  }
  ippsXorC_8u_I(0x80, (Ipp8u*)vdifframe, nsample*2*2);
}

int outfilename(char *infilename, char *outname, char* postfix, char *outdir) {
  char *tmpstr, *dotptr;

  tmpstr = strdup(infilename);
  
  // File name contained in buffer
  dotptr = rindex(tmpstr, '.');
  if (dotptr!=NULL) {
    *dotptr = 0;
  }

  if ((outdir!=NULL) && (strlen(outdir))>0) {
    while ((strlen(outdir)>0) && outdir[strlen(outdir)]=='/') outdir[strlen(outdir)] = 0;
    // "/" as outdir is valid

      
    if (strlen(outdir)+1+strlen(tmpstr)+1+strlen(postfix)+1 > MAXSTR) {
      	fprintf(stderr, "%s/%s.%s too long. Increase \"MAXSTR(%d)\"\n", 
      		outdir, tmpstr, postfix, MAXSTR);
      	return(1);
    }
    sprintf(outname, "%s/%s.%s", outdir, tmpstr, postfix);

  } else {
    if (strlen(infilename)+strlen(postfix)+2 > MAXSTR) {
      	fprintf(stderr, "%s.%s too long. Increase \"MAXSTR(%d)\"\n", 
      		tmpstr, postfix, MAXSTR);
      	return(1);
    }
    sprintf(outname, "%s.%s", tmpstr, postfix);

  }

  free(tmpstr);
  return(0);
}

