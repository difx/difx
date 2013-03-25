/*
 * hrft2vdif
 *
 * Chris Phillips
 * 
 * Convert raw data from the HRFT VLBI sampler card into VDIF format
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <getopt.h>

#include <string>
#include <vector>
#include <iostream>

#include "vdifio.h"

#include "packetReader/CPacketReader.h"
#include "X3cPacket.h"
#include "CTimeFilter.h"
#include "projectManagement/Projects.h"

#ifdef __APPLE__

#define OSX

#define OPENOPTIONS O_WRONLY|O_CREAT|O_TRUNC

#else

#define LINUX
#ifndef _LARGEFILE_SOURCE
#define _LARGEFILE_SOURCE 
#endif
#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif
#define _FILE_OFFSET_BITS 64

#define OPENOPTIONS O_WRONLY|O_CREAT|O_TRUNC|O_LARGEFILE

#endif

#ifdef DEVELOPMENT_MACHINE
#define READ_BUFFER_SIZE 5
#else
#define READ_BUFFER_SIZE 128
#endif

#ifdef DEVELOPMENT_MACHINE
#define READ_BUFFER_CHUNK 2
#else
#define READ_BUFFER_CHUNK 16
#endif

void chomp (char* s) {
  int end = strlen(s) - 1;
  if (end >= 0 && s[end] == '\n')
    s[end] = '\0';
}

#define VDIF_PACKETSIZE 8192

#define SEQUENCE(x) ((x[0]>>32)&0xFFFFFFFF)
#define IFid(x) (x[0]&0xFF)

typedef enum {NONE=0, A, B, C, D, E, F, G, H} modeType;

int convert2VDIF(string project, string stream, string outname, 
		 modeType mode) {
  char *fileNames[16], msg[256];
  UINT64 *if1, *if2, *if3, *if4;
  int nFile = 0, istream, shift = 0;;
  UINT32 usec, sequence;
  char datestr[100];
  struct tm *date;
  int out;
  vdif_header header;
  string mnt ("/mnt/disk");
  string base ("/xcube");
  
  Projects projects(mnt, base, 16);
  vector <PROJECT> prjs = projects.pmGetProjects();
  projects.pmActivateProject(project);

  vector<STREAM> strms;
  if (projects.pmGetStreams(strms) != 0) {
    fprintf(stdout, "Error retrieving streams for current project\n");
    return -1;
  }

  vector<STREAM>::iterator it1;

  if (stream.size()==0) {

    // Count the number of streams
    int nstream = 0;
    for (it1 = strms.begin(); it1 < strms.end(); it1++) {
      nstream++;
    }
 
    if (nstream==0) {
      printf("No streams in this project\n");
      return(EXIT_FAILURE);
    } else if (nstream==1) {
      istream = 0;
    } else {

      printf("\n\nPlease select a stream:\n\n");
      int i=0;
      for (it1 = strms.begin(); it1 < strms.end(); it1++) {
	STREAM st = *it1;
	printf(" [%d] %s %d %llu\n", i, st.strmName.c_str(), st.strmType, st.strmSize);
	i++;
      }
      i--;
      char ret[10];
      while (1) {
	printf("\nStream [0]: ");
	fflush(stdout);
	if (fgets(ret,10,stdin)==NULL) {
	  printf("Error reading response\n");
	  return -1;
	}
	chomp(ret);
	printf("\n");
	if (strlen(ret)==0) {
	  istream = 0;
	  break;
	}
	int status = sscanf(ret, "%d", &istream);
	if (status!=1) {
	  printf("Did not understand \"%s\"\n", ret);
	  continue;
	}
	if (istream<0 || istream>nstream) {
	  printf(" %d out of range\n", istream);
	  continue;
	}
	break;
      }
    }
    stream = strms[istream].strmName;
    printf("Selected %s\n\n", strms[istream].strmName.c_str());
  }


  for (it1 = strms.begin(); it1 < strms.end(); it1++) {
    STREAM st = *it1;
    if (st.strmName.compare(stream) != 0)
      continue;

    vector<string> streamFiles;
    projects.pmGetStreamFiles(st, streamFiles);
    
    vector<string>::iterator it2;
    for (it2 = streamFiles.begin(); it2 < streamFiles.end(); it2++) {
      string dirName = *it2;
      // at this point, dirName has the full path/name of the stream
      fprintf(stdout, "Stream Names with path: %s\n", dirName.c_str());
      
      fileNames[nFile] = strdup(dirName.c_str());
      ++nFile;
    }
  }

  UINT8* framebytes = new UINT8[VDIF_PACKETSIZE];
  UINT64* frame64 = (UINT64*)framebytes;
  if (framebytes==NULL) {
    printf("Error: Could not allocate memory\n");
    return(EXIT_FAILURE);
  }
  frame64[0] = 1;
  frame64[1] = 1;
  frame64[2] = 1;
  frame64[3] = 1;
  frame64[4] = 1;

  int nchan;
  if (mode==NONE) 
    nchan = 64;
  else { 
    nchan = 4;
    if (mode==A) 
      shift = 0;
    else if (mode==B) 
      shift = 8;
    else if (mode==C) 
      shift = 16;
    else if (mode==D) 
      shift = 24;
    else if (mode==E) 
      shift = 32;
    else if (mode==F) 
      shift = 40;
    else if (mode==G) 
      shift = 48;
    else if (mode==H) 
      shift = 56;
  }

  int vdifwords = 0, vdifbytes = 0;
  int samplesperframe = (VDIF_PACKETSIZE*2/nchan);
  int samplespersec = 32e6; // Nquist
  int framespersec = samplespersec/samplesperframe;
  printf("Frame/sec = %d\n", framespersec);

  int nRetVal;
  // Create the packet reader
  CPacketReader *pktReader = new CPacketReader();

  /***
   * Create a time stamp filter.  This will be used for the start/end times of the
   * stream.
   *
   * If the stream has been indexed, it will also seek to that position.  Also included
   * are a couple of time sample tests.
   */

  CTimeFilter startFlt;
  startFlt.setStartTime(0x00000000, 0x14700);
  startFlt.setEndTime(0x99999999, 0x1200000);
  
  pktReader->setTimeFilter(startFlt);

  /**
   * initialize the readers.
   */
  pktReader->initialize(nFile, fileNames, (size_t) READ_BUFFER_SIZE, (size_t) READ_BUFFER_CHUNK);

  out = open(outname.c_str(), OPENOPTIONS,S_IRWXU|S_IRWXG|S_IRWXO); 


  if (out==-1) {
    sprintf(msg, "Failed to open output file (%s)", outname.c_str());
    perror(msg);
    return(EXIT_FAILURE);
  }

  bool first = true;
  try {
  
    nRetVal = pktReader->start();
    if(nRetVal ==0) {
      // skip to the correct frame
      int currentPacket = 0;
      bool isDone = false;
      while (currentPacket<500) {
	X3cPacket *pkt = pktReader->getPacket();
	if (NULL == pkt) {
	  fprintf(stdout, "Finished reading. packet <%d>\n", currentPacket);
	  break;
	} else {
	  
	  UINT64 nbytes = pkt->getLen();

	  //date = gmtime(&rsec);
	  //strftime(datestr, 99, "%F %H:%M:%S", date);

	  if (first) {
	    UINT32 nsec;
	    time_t sec, rsec;
	    sec = pkt->getHeader()->ts.tv_sec;
	    nsec = pkt->getHeader()->ts.tv_nsec;
	    rsec = sec; 
	    if (nsec>=500e6) rsec++;
	    createVDIFHeader(&header, VDIF_PACKETSIZE+VDIF_HEADER_BYTES, 0,  2, nchan, 1, "Tt");
	    setVDIFTime(&header, rsec);
	  }

	  const unsigned char *p = (unsigned char*)(pkt->getData());

	  for (int i=0; i<nbytes/(8192*4); i++) {

	    if1 = (UINT64*)&p[i*8192*4];
	    if2 = if1+1024;
	    if3 = if2+1024;
	    if4 = if3+1024;

	    // Check sensible sequence number
	    if (first) {
	      sequence = SEQUENCE(if1);
	      first = 0;
	    } else {
	      sequence++;
	      if (SEQUENCE(if1)!=sequence) {
		printf("Error: Sequence number did not match expected (%u/%u)\n", (unsigned int)sequence, (unsigned int)SEQUENCE(if1));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
	    }
	    if (SEQUENCE(if2) != sequence) {
	      printf("Error: IF2 Sequence does not match number did not match expected (%u/%u)\n", (unsigned int)sequence, (unsigned int)SEQUENCE(if2));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }
	    if (SEQUENCE(if3) != sequence) {
	      printf("Error: IF3 Sequence does not match number did not match expected (%u/%u)\n", (unsigned int)sequence, (unsigned int)SEQUENCE(if3));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }
	    if (SEQUENCE(if4) != sequence) {
	      printf("Error: IF4 Sequence does not match number did not match expected (%u/%u)\n", (unsigned int)sequence, (unsigned int)SEQUENCE(if3));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }

	    if (IFid(if1)!=0xF0) {
	      printf("Error: Unexpected IF1 id (0x%X/0xF0)\n", (unsigned int)IFid(if1));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }
	    if (IFid(if2)!=0xF1) {
	      printf("Error: Unexpected IF2 id (0x%X/0xF1)\n", (unsigned int)IFid(if2));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }
	    if (IFid(if3)!=0xF2) {
	      printf("Error: Unexpected IF3 id (0x%X/0xF2)\n", (unsigned int)IFid(if3));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }
	    if (IFid(if4)!=0xF3) {
	      printf("Error: Unexpected IF4 id (0x%X/0xF3)\n", (unsigned int)IFid(if4));
	      pktReader->freePacket(pkt);
	      return(EXIT_FAILURE);
	    }
	    
	    for (int j=1; j<1024; j++) {
	      if (mode==NONE) {
		// Merge the 4 streams
		frame64[vdifwords] = if1[j];
		vdifwords++;
		frame64[vdifwords] = if2[j];
		vdifwords++;
		frame64[vdifwords] = if3[j];
		vdifwords++;
		frame64[vdifwords] = if4[j];
		vdifwords++;
	      } else {
		// Take one byte from 2 IF only
		framebytes[vdifbytes] = (if1[j]>>shift)&0xFF;
		vdifbytes++;
		framebytes[vdifbytes] = (if2[j]>>shift)&0xFF;
		vdifbytes++;
	      }
	  
	      if (vdifwords>= VDIF_PACKETSIZE/8 || vdifbytes>=VDIF_PACKETSIZE) {
		// Write a VDIF packet
		ssize_t nwrote = write(out, &header, VDIF_HEADER_BYTES);
		if (nwrote==-1) {
		  perror("Error writing outfile");
		  close(out);
		  return(EXIT_FAILURE);
		} else if (nwrote!=VDIF_HEADER_BYTES) {
		  fprintf(stderr, "Warning: Did not write all bytes! (%d/%d)\n",
			  (int)nwrote, VDIF_HEADER_BYTES);
		  close(out);
		  return(EXIT_FAILURE);
		}
		nwrote = write(out, framebytes, VDIF_PACKETSIZE);
		if (nwrote==-1) {
		  perror("Error writing outfile");
		  close(out);
		  return(EXIT_FAILURE);
		} else if (nwrote!=VDIF_PACKETSIZE) {
		  fprintf(stderr, "Warning: Did not write all bytes! (%d/%d)\n",
			  (int)nwrote, VDIF_PACKETSIZE);
		  close(out);
		  return(EXIT_FAILURE);
		}
	      
		vdifwords = 0;
		vdifbytes = 0;
		nextVDIFHeader(&header, framespersec);
	      }
	    }
	  }
	  pktReader->freePacket(pkt);
	  currentPacket++;
	}
      }
      printf("Stopping pktReader\n");
      pktReader->stop();
      printf("Stopped pktReader\n");
    }
  }

  catch (std::exception ex) {
    return -1;
  }
  
  close(out);
  
  delete pktReader;
  
  return 0;
  }

void usage () {
  cerr << "Usage: hrft2vdif <project> <stream>" << endl;
}

int main(int argc, char **argv) {
  char *outname = NULL, *streamname = NULL;
  int opt;
  modeType mode = NONE;

  struct option options[] = {
    {"outname", 1, 0, 'o'},
    {"mode", 1, 0, 'm'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  while ((opt = getopt_long_only(argc, argv, "o:", options, NULL)) != EOF)
    switch (opt) {
      
    case 'm': // Channel selection mode
      if (strcmp(optarg,"A")==0)
	mode = A;
      else if (strcmp(optarg,"B")==0)
	mode = B;
      else if (strcmp(optarg,"C")==0)
	mode = C;
      else if (strcmp(optarg,"D")==0)
	mode = D;
      else if (strcmp(optarg,"E")==0)
	mode = E;
      else if (strcmp(optarg,"F")==0)
	mode = F;
      else if (strcmp(optarg,"G")==0)
	mode = G;
      else if (strcmp(optarg,"H")==0)
	mode = H;
      else {
	printf("Unknown mode %s. Aborting\n", optarg);
	exit(1);
      }   
      break;

    case 'o': // file to record
      outname=strdup(optarg);
      break;

    case 'h': // help
      usage();
      return EXIT_SUCCESS;
      break;
    }

  int narg = argc-optind;

  if (narg<1 || narg>2) {
    usage();
    return(EXIT_FAILURE);
  }
  
  if (outname==NULL) outname = strdup("vdif.out");

  if (narg>1) 
    streamname=argv[optind+1];
  else
    streamname=strdup("");

  return convert2VDIF(argv[optind],streamname, outname, mode);

}
