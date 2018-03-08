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
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include "codifio.h"

int main (int argc, char **argv) {
  int i, file;
  uint16_t stationID;
  ssize_t nread;
  off_t sook;
  char msg[300], buf[CODIF_HEADER_BYTES], *stnCode;
  codif_header *header;

  header = (codif_header*)buf;
  
  for (i=1; i<argc; i++) {
    printf("Got %s\n", argv[i]);

    file = open(argv[i], OPENOPTIONS);
    if (file==-1) {
      sprintf(msg, "Failed to open input file (%s) [%d]", argv[i], errno);
      perror(msg);
      continue;
    }

    //sook = lseek(file, 8, SEEK_SET);
    //if (sook==-1) {
    //  perror("Error seeking within file\n");
    //  close(file);
    //  exit(1);
    //}

    stnCode = (char*)&stationID;

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
      printf("INVALID:     %d\n", getCODIFFrameInvalid(header));
      printf("COMPLEX:     %d\n", getCODIFComplex(header));
      printf("\n");
      printf("SECONDS:     %d\n", getCODIFFrameEpochSecOffset(header));
      printf("\n");
      printf("FRAME#:      %d\n", getCODIFFrameNumber(header));
      printf("\n");
      printf("VERSION:     %d\n", getCODIFVersion(header));
      printf("NBITS:       %d\n", getCODIFBitsPerSample(header));
      printf("FRAMELENGTH: %d\n", getCODIFFrameBytes(header));
      printf("\n");
      printf("EPOCH:       %d\n", getCODIFEpoch(header));
      printf("REPRESENT:   %d\n", getCODIFRepresentation(header));
      if(stnCode[0] >= ' ' && stnCode[0] <= 127 && (stnCode[1] >= ' ' || stnCode[1] == 0) && stnCode[1] <= 127)	{
	printf("ANTID:       %c%c\n", stnCode[1], stnCode[0]);
      }	else {
	printf("ANTID:       %d\n", stationID);
      }
      printf("\n");
      printf("SAMPLEBLOCK: %d\n", getCODIFSampleblockLength(header));
      printf("NCHAN:       %d\n", getCODIFNumChannels(header));
      printf("\n");
      printf("THREADID:    %d\n", getCODIFThreadID(header));
      printf("GROUPID:     %d\n", getCODIFGroupID(header));
      printf("\n");
      printf("PERIOD:      %d\n", getCODIFPeriod(header));
      printf("\n");
      printf("#SAMPLES:    %llu\n", getCODIFTotalSamples(header));
      printf("\n");
      printf("SYNC:        0X%X\n", getCODIFSync(header));
      
      printf("-------------------\n");
      
      sook = lseek(file, getCODIFFrameBytes(header), SEEK_CUR);
      if (sook==-1) {
	perror("Error seeking within file\n");
	close(file);
	exit(1);
      }
    }
    
    close(file);
  }
}
