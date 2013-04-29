/*
 * hrftStats
 *
 * Chris Phillips
 * 
 * Calculate hrtf sampler stats directly from disk
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

#include "packetReader/CPacketReader.h"
#include "X3cPacket.h"
#include "CTimeFilter.h"
#include "projectManagement/Projects.h"

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

#define SEQUENCE(x) ((x[0]>>32)&0xFFFFFFFF)
#define IFid(x) (x[0]&0xFF)

int calcStats(string project, string stream, float time) {
  char *fileNames[16], msg[256];
  UINT64 *if1, *if2, *if3, *if4;
  UINT32 if1Stats[8][256], if2Stats[8][256], if3Stats[8][256], if4Stats[8][256];
  UINT32 if1Results[32][4], if2Results[32][4], if3Results[32][4], if4Results[32][4];
  int nFile = 0, istream, shift = 0, i,j;
  UINT32 usec, sequence, nsample, totalSample;
  char datestr[100];
  struct tm *date;
  string mnt ("/mnt/disk");
  string base ("/xcube");

  for (i=0; i< 256; i++) {
    for (j=0; j<8; j++) {
      if1Stats[j][i] = 0;
      if2Stats[j][i] = 0;
      if3Stats[j][i] = 0;
      if4Stats[j][i] = 0;
    }
  }
  for (i=0; i< 32; i++) {
    for (j=0; j<4; j++) {
      if1Results[i][j] = 0;
      if2Results[i][j] = 0;
      if3Results[i][j] = 0;
      if4Results[i][j] = 0;
    }
  }
  nsample = 0;

  totalSample = 32e6*time;
  printf("DEBUG: Processing %u samples\n", totalSample);

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
 
 int nRetVal;
  // Create the packet reader
  CPacketReader *pktReader = new CPacketReader();

  CTimeFilter startFlt;
  startFlt.setStartTime(0x00000000, 0x14700);
  startFlt.setEndTime(0x99999999, 0x1200000);
  
  pktReader->setTimeFilter(startFlt);

  /**
   * initialize the readers.
   */
  pktReader->initialize(nFile, fileNames, (size_t) READ_BUFFER_SIZE, (size_t) READ_BUFFER_CHUNK);

  bool first = true;
  try {
  
    nRetVal = pktReader->start();
    if(nRetVal ==0) {
      // skip to the correct frame
      int currentPacket = 0;
      bool isDone = false;
      while (nsample<totalSample) {
	X3cPacket *pkt = pktReader->getPacket();
	if (NULL == pkt) {
	  fprintf(stdout, "Finished reading. packet <%d>\n", currentPacket);
	  break;
	} else {
	  
	  UINT64 nbytes = pkt->getLen();

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
	      // Accumulate states
	      for (int k=0; k<8; k++) {
		if1Stats[k][(if1[j]>>(k*8))&0xFF]++;
		if2Stats[k][(if2[j]>>(k*8))&0xFF]++;
		if3Stats[k][(if3[j]>>(k*8))&0xFF]++;
		if4Stats[k][(if4[j]>>(k*8))&0xFF]++;
	      }
	    }
	    nsample += 1023;
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
  delete pktReader;

  printf("Processed %u samples\n", nsample);

  UINT32 totalcount = 0;
  // Analyze results
  for (i=0; i<256; i++) { // Go through each byte distribution
    for (j=0; j<8; j++) {

      if1Results[j*4][i&0x3] += if1Stats[j][i];
      if1Results[j*4+1][(i>>2)&0x3] += if1Stats[j][i];
      if1Results[j*4+2][(i>>4)&0x3] += if1Stats[j][i];
      if1Results[j*4+3][(i>>6)&0x3] += if1Stats[j][i];

      if2Results[j*4][i&0x3] += if2Stats[j][i];
      if2Results[j*4+1][(i>>2)&0x3] += if2Stats[j][i];
      if2Results[j*4+2][(i>>4)&0x3] += if2Stats[j][i];
      if2Results[j*4+3][(i>>6)&0x3] += if2Stats[j][i];

      if3Results[j*4][i&0x3] += if3Stats[j][i];
      if3Results[j*4+1][(i>>2)&0x3] += if3Stats[j][i];
      if3Results[j*4+2][(i>>4)&0x3] += if3Stats[j][i];
      if3Results[j*4+3][(i>>6)&0x3] += if3Stats[j][i];
	
      if4Results[j*4][i&0x3] += if4Stats[j][i];
      if4Results[j*4+1][(i>>2)&0x3] += if4Stats[j][i];
      if4Results[j*4+2][(i>>4)&0x3] += if4Stats[j][i];
      if4Results[j*4+3][(i>>6)&0x3] += if4Stats[j][i];
    }
  }

  // Print the results

  printf("IF1 Stats:\n");
  for (i=0; i<16; i++) {
    printf("Chan %2d  Re:", i);
    for (j=0; j<4; j++)  printf(" %5.2f%%", if1Results[i*2][j]/(double)nsample*100);
    printf("  Im:");
    for (j=0; j<4; j++)  printf(" %5.2f%%", if1Results[i*2+1][j]/(double)nsample*100);
    printf("\n");
  }

  printf("IF2 Stats:\n");
  for (i=0; i<16; i++) {
    printf("Chan %2d  Re:", i);
    for (j=0; j<4; j++)  printf(" %5.2f%%", if2Results[i*2][j]/(double)nsample*100);
    printf("  Im:");
    for (j=0; j<4; j++)  printf(" %5.2f%%", if2Results[i*2+1][j]/(double)nsample*100);
    printf("\n");
  }


  printf("IF3 Stats:\n");
  for (i=0; i<16; i++) {
    printf("Chan %2d  Re:", i);
    for (j=0; j<4; j++)  printf(" %5.2f%%", if3Results[i*2][j]/(double)nsample*100);
    printf("  Im:");
    for (j=0; j<4; j++)  printf(" %5.2f%%", if3Results[i*2+1][j]/(double)nsample*100);
    printf("\n");
  }

  printf("\nIF4 Stats:\n");
  for (i=0; i<16; i++) {
    printf("Chan %2d  Re:", i);
    for (j=0; j<4; j++)  printf(" %5.2f%%", if4Results[i*2][j]/(double)nsample*100);
    printf("  Im:");
    for (j=0; j<4; j++)  printf(" %5.2f%%", if4Results[i*2+1][j]/(double)nsample*100);
    printf("\n");
  }

  
  return 0;
}

void usage () {
  cerr << "Usage: hrftStats [-t <time>] <project> <stream>" << endl;
}

int main(int argc, char **argv) {
  char *streamname = NULL;
  float time = 1;
  int opt;

  struct option options[] = {
    {"time", 1, 0, 't'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  while ((opt = getopt_long_only(argc, argv, "t:h", options, NULL)) != EOF)
    switch (opt) {
      
    case 't': // Channel selection mode
      time = atof(optarg);
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
  
  if (narg>1) 
    streamname=argv[optind+1];
  else
    streamname=strdup("");

  return calcStats(argv[optind],streamname, time);

}
