/*
 * lf1ReaderSample.cpp
 *
 * W Brisken modified version
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <getopt.h>

#include <iostream>
#include <string.h>
#include <string>
#include <vector>

#include "vdifio.h"

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

void getVDIFStationIDStr(vdif_header *header, char stationid[3])
{
  stationid[1] = header->stationid & 0xFF;
  stationid[0] = (header->stationid>>8) & 0xFF;
  stationid[2] = 0;
  return;
}

void chomp (char* s) {
  int end = strlen(s) - 1;
  if (end >= 0 && s[end] == '\n')
    s[end] = '\0';
}

int readLF1file(string project, string stream, int npkt, bool hrft) {
  char *fileNames[16], stationid[3];
  int nFile = 0;
  char datestr[100];
  struct tm *date;
  struct in_addr addr;
  vdif_header *header;

  uint16_t IPv4val, ARPval, LOOPval;
  IPv4val = htons(0x0800);
  ARPval = htons(0x0806);
  LOOPval = htons(0x9000);

  /***
   * This section of code shows how to use the Projects class.  From this, we can
   * eventually get the full stream path/name
   *
   * The mount string (mnt) has the path for the project.  the last directory should
   * be the directory that gets the disk/folder number appended to.  For
   * example:
   *    /mnt/disk
   *       would end up being /mnt/disk0, /mnt/disk1, /mnt/disk2,...
   *    /mnt/data_sets/vol2/disk
   *       would end up with .../vol2/disk0, .../vol2/disk1, ...
   *
   * The base string gets appended to the mount path.
   */
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
    printf("\n\nPlease select a stream:\n\n");
    int i=0;
    for (it1 = strms.begin(); it1 < strms.end(); it1++) {
      STREAM st = *it1;
      printf(" [%d] %s %d %llu\n", i, st.strmName.c_str(), st.strmType, st.strmSize);
      i++;
    }
    i--;
    char ret[10];
    int istream;
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
      if (istream<0 || istream>i) {
	printf(" %d out of range\n", istream);
	continue;
      }
      break;
    }
    printf("Selected %s\n\n", strms[istream].strmName.c_str());
    stream = strms[istream].strmName;
  }

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
    nRetVal = pktReader->start();
    if(0 == nRetVal) {
      // skip to the correct frame
      int currentPacket = 0;
      bool isDone = false;
      while (currentPacket < npkt)  { // way to limit number of packets
	X3cPacket *pkt = pktReader->getPacket();
	if (NULL == pkt) {
	  fprintf(stdout, "Finished reading. packet <%d>\n", currentPacket);
	  isDone = true;
	} else {

	  /**
	   * at this point, we have a packet.  now we need to
	   * do something with it.  In the sample code, we are just
	   * writing out the timestamp for the LF1 header.
	   */
	  UINT32 usec;
	  time_t sec;
	  sec = pkt->getHeader()->ts.tv_sec;
	  usec = pkt->getHeader()->ts.tv_nsec/1000;

	  date = gmtime(&sec);
	  strftime(datestr, 99, "%F %H:%M:%S", date);
	  
	  const unsigned char *p = (unsigned char *)(pkt->getData());

	  printf("Packet: %3d  %s.%06d  %4d ", currentPacket, datestr, usec, pkt->getLen());
	  currentPacket++;

	  if (hrft) {
	    uint32_t *q, *r;
	    for (int i=0; i<8; i++) {
	      if (i!=0) printf("%49s", " ");
	      q = (uint32_t*)&p[i*8192];
	      r = q+1;
	      printf("%02x %02x %04x %u\n", (*q)&0XFF,  ((*q)>>8)&0XFF, ((*q)>>16)&0XFFFF, *r);
	    }

	  } else {

	    // MAC destination
	    for(int i = 0; i < 6; ++i) printf("%s%02x", (i==0?"":":"), p[i]);
	    printf("  ");
	    // MAC source
	    for(int i = 6; i < 12; ++i) printf("%s%02x", (i==6?"":":"), p[i]);
	    
	    // Ethertype
	    uint16_t ethertype = *((uint16_t*)&p[12]); // Note network byte order
	    
	    bool IPv4 = false;
	    if (ethertype==IPv4val) {
	      printf("  IPv4 ");
	      IPv4 = true;
	    } else if (ethertype==ARPval) {
	      printf("   ARP ");
	    } else if (ethertype==LOOPval) {
	      printf("  Loop ");
	    } else {
	      printf(" 0x%04X", ntohs(ethertype));
	    }
	  
	    if (!IPv4) {
	      printf("\n");
	      continue;
	    }
	    
	    for(int i = 14; i < 26; ++i) printf("%s%02x", ((i-14)%4==0?" ":""), p[i]);
	  
	    // Source IP
	    addr.s_addr = *((in_addr_t*)&p[26]);
	    printf(" %s", inet_ntoa(addr));
	    // Destination IP
	    addr.s_addr = *((in_addr_t*)&p[30]);
	    printf(" %s", inet_ntoa(addr));
	    
	    // UDP headers 
	    uint16_t source = ntohs(*((uint16_t*)&p[34]));
	    uint16_t dest = ntohs(*((uint16_t*)&p[36]));
	    uint16_t len = ntohs(*((uint16_t*)&p[38]));
	    uint16_t checksum = ntohs(*((uint16_t*)&p[40]));
	    printf(" %d %d %d %5d ", source, dest, len, checksum);
	    
	    //printf("  VH =");
	    //for(int i = 0; i < 12; ++i) printf("%s%02x", (i%8==0?" ":""), p[i+42]);
	  
	    // VTP Sequence 
	    uint64_t sequence = *((uint64_t*)&p[42]);
	    printf(" 0x%llx", (unsigned long long)sequence);
	    
	    // VDIF Header
	    header = (vdif_header*)(&p[50]);
	    getVDIFStationIDStr(header, stationid);
	    printf(" %2s", stationid);
	    
	    printf(" %d",  getVDIFFrameBytes(header));
	    printf(" 0x%X",  getVDIFFullSecond(header));
	    printf(" %4d",  getVDIFFrameNumber(header));
	    printf(" %d",  getVDIFNumChannels(header));
	    printf(" %d  ",  getVDIFBitsPerSample(header));
	    
	    // Data ");
	    for(int i = 0; i < 16; ++i) printf("%s%02x", (i%8==0?" ":""), p[i+82]);
	    printf("\n");
	  }

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


int main(int argc, char **argv) {
  char *streamname;
  int opt;
  int npkt = 5;
  bool hrft = false;
  
  struct option options[] = {
    {"hrft", 0, 0, 'H'},
    {"npkt", 1, 0, 'n'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  while ((opt = getopt_long_only(argc, argv, "hHn:", options, NULL)) != EOF)
    switch (opt) {
      
    case 'H': 
      hrft = true;
      break;

    case 'n': 
      npkt = atoi(optarg);
      break;

    case 'h': // help
      printf("No help here\n");
      return EXIT_SUCCESS;
    
    }

  int narg = argc-optind;

  if (narg<1 || narg>3) {
    cerr << "Usage: lf1list <project> <stream>" << endl;
    return(EXIT_FAILURE);
  }


  if (narg==3) npkt = atoi(argv[optind+2]);
  if (narg>1) 
    streamname=argv[optind+1];
  else
    streamname=strdup("");

  return readLF1file(argv[optind],streamname, npkt, hrft);
}
