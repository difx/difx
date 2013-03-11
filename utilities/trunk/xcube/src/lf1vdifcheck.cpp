/*
 * lf1vdifcheck.cpp
 *
 * Chris Phillips
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <arpa/inet.h>
 #include <sys/socket.h>
#include <netinet/in.h>

#include <iostream>
#include <string.h>
#include <string>
#include <vector>

#include "packetReader/CPacketReader.h"
#include "X3cPacket.h"
#include "CTimeFilter.h"
#include "projectManagement/Projects.h"

//#define DEVELOPMENT_MACHINE 1


/**
 * on a development computer, we might not have the
 * resources that a production system has.  the DEVELOPMENT_MACHINE
 * define cuts back on the size and number of memory bufs being
 * used by the reader.
 *
 * sizes are in MBytes
 */
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

int readLF1file(string project, string stream) {
  char *fileNames[16], stationid[3];
  int nFile = 0, sizeFilt=1000;
  char datestr[100];
  struct tm *date;
  struct in_addr addr;

  uint16_t IPv4val;
  IPv4val = htons(0x0800);

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
  for (it1 = strms.begin(); it1 < strms.end(); it1++) {
    STREAM st = *it1;
    if(st.strmName.compare(stream) != 0)
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
  startFlt.setStartTime(0x00000000, 0x0);
  startFlt.setEndTime(0x99999999, 0x1200000);
  pktReader->setTimeFilter(startFlt);

  /**
   * initialize the readers.
   */
  pktReader->initialize(nFile, fileNames, (size_t) READ_BUFFER_SIZE, (size_t) READ_BUFFER_CHUNK);

  /**
   * start reading in the packets
   */
  try {
    
    uint64_t npacket = 0;
    uint64_t expectedsequence = 0, thissequence;
    nRetVal = pktReader->start();
    if (nRetVal == 0) {
      bool isDone = false;
      while (!isDone) {
	
	X3cPacket *pkt = pktReader->getPacket();
	if (pkt == NULL) {
	  fprintf(stdout, "Finished reading. Got %lu packets\n", npacket);
	  isDone = true;
	} else {
	  npacket++;

	    
	  //UINT32 usec;
	  //time_t sec;
	  //sec = pkt->getHeader()->ts.tv_sec;
	  //usec = pkt->getHeader()->ts.tv_nsec/1000;
	  
	  //date = gmtime(&sec);
	  //strftime(datestr, 99, "%F %H:%M:%S", date);
	    
	  const unsigned char *p = (unsigned char *)(pkt->getData());

	  uint16_t ethertype = *((uint16_t*)&p[12]);
	  if (ethertype!=IPv4val) continue;  // Skip non-IPv4
	  if(pkt->getLen()<sizeFilt) continue; // Skip small packets

	  // VTP Sequence 
	  thissequence = *((uint64_t*)&p[42]);

	  if (thissequence != expectedsequence) {
	    if (thissequence!=0) { // Must have reset stream
	      if (thissequence==expectedsequence-1) {
		printf(" Got Duplicate sequence %llu\n", (unsigned long long)thissequence, (unsigned long long)expectedsequence);
	      } else {
		printf(" Got %llu expected %llu  <%lld>\n", (unsigned long long)thissequence, (unsigned long long)expectedsequence, (long long)(thissequence-expectedsequence));
	      }
	    }
	  }
	  expectedsequence = thissequence+1;

	  /**
	   * MAKE SURE TO DELETE the packet when finished!!!
	   */
	  pktReader->freePacket(pkt);
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

  return EXIT_SUCCESS;
}


int main(int argc, char **argv)
{
  if (argc!=3) {
    cerr << "Usage: lf1vdifcheck <project> <stream>" << endl;
    return(EXIT_FAILURE);
  }


  return readLF1file(argv[1],argv[2]);
}
