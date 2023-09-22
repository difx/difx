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
#include <netdb.h>
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

#define FLIP(x)  ((((x)>>1)&0x5555555555555555LL) | (((x)<<1)&0xAAAAAAAAAAAAAAAALL))

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

#define MAXSTR              200 

void chomp (char* s) {
  int end = strlen(s) - 1;
  if (end >= 0 && s[end] == '\n')
    s[end] = '\0';
}

#define MAX_PACKETSIZE 8192

#define SEQUENCE(x) ((x[0]>>32)&0xFFFFFFFF)
#define IFID(x) (x[0]&0xFF)
#define TYPE(x) ((x[0]>>8)&0xFF)

typedef enum {NONE=0, A, B, C, D, E, F, G, H} modeType;

UINT8 *lookup;
void initialize_lookup();
int diskwrite(int out, const void* buf, size_t len);
int netwrite(int sock, const void* buf, size_t len);
int setup_net(string hostname, int port, int window_size);

int convert2VDIF(string project, string stream, string outname, 
		 int sock, modeType mode) {
  char *fileNames[16], msg[256];
  UINT64 *if1, *if2, *if3, *if4;
  int nFile = 0, istream, shift = 0, status;
  int vdif_packetsize;
  int framespersec;
  UINT8 type, ifid, *framebytes;
  UINT32 usec, sequence;
  UINT64* frame64;
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
    cerr << "Error retrieving streams for current project" << endl;
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
      cout << "No streams in this project" << endl;
      return(EXIT_FAILURE);
    } else if (nstream==1) {
      istream = 0;
    } else {

      cout << endl << endl << "Please select a stream:" << endl << endl;
      int i=0;
      for (it1 = strms.begin(); it1 < strms.end(); it1++) {
	STREAM st = *it1;
	cout << " [" << i << "] " << st.strmName  << st.strmType << " " 
	     << st.strmSize << endl;
	i++;
      }
      i--;
      char ret[10];
      while (1) {
	cout << endl << "Stream [0]:" << flush;
	if (fgets(ret,10,stdin)==NULL) {
	  cout << "Error reading response" << endl;
	  return -1;
	}
	chomp(ret);
	cout << endl;
	if (strlen(ret)==0) {
	  istream = 0;
	  break;
	}
	int status = sscanf(ret, "%d", &istream);
	if (status!=1) {
	  cout << "Did not understand " << ret << endl;
	  continue;
	}
	if (istream<0 || istream>nstream) {
	  cout << " " <<  istream << "out of range" << endl;
	  continue;
	}
	break;
      }
    }
    stream = strms[istream].strmName;
    cout << "Selected " << strms[istream].strmName << endl;
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
      cout << "Stream Names with path: " << dirName << endl;;
      
      fileNames[nFile] = strdup(dirName.c_str());
      ++nFile;
    }
  }

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

  if (sock==0) {
    out = open(outname.c_str(), OPENOPTIONS,S_IRWXU|S_IRWXG|S_IRWXO); 

    if (out==-1) {
      sprintf(msg, "Failed to open output file (%s)", outname.c_str());
      perror(msg);
    return(EXIT_FAILURE);
    }
  }

  bool first = true;
  try {
  
    nRetVal = pktReader->start();
    if(nRetVal ==0) {
      // skip to the correct frame
      int currentPacket = 0;
      bool isDone = false;
      while (currentPacket<1000) {
      //while (1) {
	X3cPacket *pkt = pktReader->getPacket();
	if (NULL == pkt) {
	  cout << "Finished reading. packet " << currentPacket << endl;
	  break;
	} else {
	  
	  UINT64 nbytes = pkt->getLen();
	  UINT64 *p = (UINT64*)(pkt->getData());

	  //date = gmtime(&rsec);
	  //strftime(datestr, 99, "%F %H:%M:%S", date);

	  if (first) {

	    // Figure out time
	    UINT32 nsec;
	    time_t sec, rsec;
	    sec = pkt->getHeader()->ts.tv_sec;
	    nsec = pkt->getHeader()->ts.tv_nsec;
	    rsec = sec; 
	    if (nsec>=500e6) rsec++;

	    type = IFID(p);
	    if (type>=240) 
	      type = 1;
	    else
	      type = 0;
	    cout << "DEBUG: Type=" << type << endl;

	    // VDIF header values
	    int bits, samplespersec, bfactor;
	    if (type==0) {
	      bits = 8;
	      nchan = 1;
	      samplespersec = 1024e6; // 512 MHz real Nyquist
	      ifid = IFID(p);
	      bfactor = 1;
	    } else {
	      samplespersec = 32e6; // 32 MHz complex Nyquist
	      bits = 2; 
	      bfactor = 2;
	      // nchan previously set
	      initialize_lookup();
	    }
	    UINT64 rate = nchan*samplespersec*bits*bfactor/8; // Bytes/sec

	    vdif_packetsize = MAX_PACKETSIZE;
	    vdif_packetsize = (vdif_packetsize/8)*8; // Round down to multiple of 8 bytes

	    while (vdif_packetsize>0) {
	      if ((rate % vdif_packetsize) == 0) {
		cout << "Choosing VDIf framesize of " << vdif_packetsize << endl;
		break;
	      }
	      vdif_packetsize-=8;
	    }
	    if (vdif_packetsize<=0) {
	      cout << "Could not find appropriate VDIF header size for data rate " <<
		rate/1e6 << " Mbytes/sec" << endl;
	    }
	   
	    int samplesperframe = (vdif_packetsize*8)/(nchan*bits*bfactor);
	    framespersec = samplespersec/samplesperframe;
	    cout << "Frame/sec = " << framespersec << endl;

	    framebytes = new UINT8[vdif_packetsize];
	    frame64 = (UINT64*)framebytes;
	    if (framebytes==NULL) {
	      cout << "Error: Could not allocate memory" << endl;
	      return(EXIT_FAILURE);
	    }

	    createVDIFHeader(&header, vdif_packetsize+VDIF_HEADER_BYTES, 0,  bits, 
			     nchan, bfactor==2, "Tt");
	    setVDIFTime(&header, rsec);
	  }

	  if (type==1) {
	    for (int i=0; i<nbytes/(8192*4); i++) {

	      if1 = &p[i*1024*4];
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
		  cout << "Error: Sequence number did not match expected (" << sequence
		       << "/" << SEQUENCE(if1) << ")" << endl;
		  pktReader->freePacket(pkt);
		  return(EXIT_FAILURE);
		}
	      }
#if 0
	      // Only check the first IF for correct type
	      if (TYPE(if1)!=type) {
		printf("Error: Header type does not match that expected (%d/%d)\n", (int)type, (int)TYPE(if1));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
		
	      }
#endif
	      if (SEQUENCE(if2) != sequence) {
		printf("Error: IF2 Sequence does not match expected (%u/%u)\n", (unsigned int)sequence, (unsigned int)SEQUENCE(if2));
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
	      
	      if (IFID(if1)!=0xF0) {
		printf("Error: Unexpected IF1 id (0x%X/0xF0)\n", (unsigned int)IFID(if1));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
	      if (IFID(if2)!=0xF1) {
		printf("Error: Unexpected IF2 id (0x%X/0xF1)\n", (unsigned int)IFID(if2));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
	      if (IFID(if3)!=0xF2) {
		printf("Error: Unexpected IF3 id (0x%X/0xF2)\n", (unsigned int)IFID(if3));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
	      if (IFID(if4)!=0xF3) {
		printf("Error: Unexpected IF4 id (0x%X/0xF3)\n", (unsigned int)IFID(if4));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
	      
	      for (int j=1; j<1024; j++) {
		if (mode==NONE) {
		  // Merge the 4 streams
		  frame64[vdifwords] = FLIP(if1[j]);
		  vdifwords++;
		  frame64[vdifwords] = FLIP(if2[j]);
		  vdifwords++;
		  frame64[vdifwords] = FLIP(if3[j]);
		  vdifwords++;
		  frame64[vdifwords] = FLIP(if4[j]);
		  vdifwords++;
		} else {
		  // Take one byte from 2 IF only
		  framebytes[vdifbytes] = (if1[j]>>shift)&0xFF;
		  vdifbytes++;
		  framebytes[vdifbytes] = (if2[j]>>shift)&0xFF;
		  vdifbytes++;
		}
		
		if (vdifwords>= vdif_packetsize/8 || vdifbytes>=vdif_packetsize) {
		  // Write a VDIF packet
		  if (sock==0) 
		    status = diskwrite(out, &header, VDIF_HEADER_BYTES);
		  else 
		    status = netwrite(sock, &header, VDIF_HEADER_BYTES);
		  
		  if (status!=EXIT_SUCCESS)
		    return(status);
		

		  // Convert bit format
		  for (int n=0; n<vdif_packetsize; n++) {
		    framebytes[n] = lookup[framebytes[n]];
		  }

		  if (sock==0) {
		    status = diskwrite(out, framebytes, vdif_packetsize);
		  } else {
		    status = netwrite(sock, framebytes, vdif_packetsize);
		  }
		  if (status!=EXIT_SUCCESS) return(status);

		  vdifwords = 0;
		  vdifbytes = 0;
		  nextVDIFHeader(&header, framespersec);
		}
	      }
	    }

	  } else if (type==0) {
	    for (int i=0; i<nbytes/8192; i++) {
	      if1 = &p[i*1024];

	      // Check sensible sequence number
	      if (first) {
		sequence = SEQUENCE(if1);
		first = 0;
	      } else {
		sequence++;
		if (SEQUENCE(if1)!=sequence) {
		  printf("Error: Sequence number did not match expected (%u/%u)\n", 
			 (unsigned int)sequence, (unsigned int)SEQUENCE(if1));
		  pktReader->freePacket(pkt);
		  return(EXIT_FAILURE);
		}
	      }
#if 0
	      if (TYPE(if1)!=type) {
		printf("Error: Header type does not match that expected (%d/%d)\n", 
		       (int)type, (int)TYPE(if1));
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
#endif
	      if (IFID(if1)!=ifid) {
		printf("Error: Unexpected IF1 id (0x%X/0x%X)\n", 
		       (unsigned int)IFID(if1), ifid);
		pktReader->freePacket(pkt);
		return(EXIT_FAILURE);
	      }
	      
	      for (int j=1; j<1024; j++) {
		frame64[vdifwords] = if1[j] ^ 0x8080808080808080;
		vdifwords++;
		
		if (vdifwords>= vdif_packetsize/8) {
		  // Write a VDIF packet
		  if (sock==0)
		    status = diskwrite(out, &header, VDIF_HEADER_BYTES);
		  else
		    status = netwrite(sock, &header, VDIF_HEADER_BYTES);

		  if (status!=EXIT_SUCCESS) return(status);

		  if (sock==0)
		    status = diskwrite(out, framebytes, vdif_packetsize);
		  else 
		    status = netwrite(sock, framebytes, vdif_packetsize);

		  if (status!=EXIT_SUCCESS) return(status);

		  vdifwords = 0;
		  vdifbytes = 0;
		  nextVDIFHeader(&header, framespersec);
		}
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
  string streamname ("");
  int opt, tmp, status, sock;
  float ftmp;

  modeType mode = NONE;
  int port = 52100;     // Network port to use
  int window_size = -1;	// TCP window size
  string hostname (""); // Host name to send data to
  string outname (""); // Output file name 
  int donet = 0;       // Write to network rather than local file

  struct option options[] = {
    {"outname", 1, 0, 'o'},
    {"mode", 1, 0, 'm'},
    {"port", 1, 0, 'p'},
    {"host", 1, 0, 'H'},
    {"hostname", 1, 0, 'H'},
    {"window", 1, 0, 'w'},
    {"help", 0, 0, 'h'},
    {0, 0, 0, 0}
  };

  while ((opt = getopt_long_only(argc, argv, "p:H:o:hw:", options, NULL)) != EOF)
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
      outname = optarg;
      break;

    case 'w':
      status = sscanf(optarg, "%f", &ftmp);
      if (status!=1)
	fprintf(stderr, "Bad window option %s\n", optarg);
      else 
	window_size = ftmp * 1024;
     break;
     
    case 'p':
      status = sscanf(optarg, "%d", &tmp);
      if (status!=1)
	fprintf(stderr, "Bad port option %s\n", optarg);
      else {
	port = tmp;
      }
      donet = 1;
      break;
      
    case 'H':
      if (strlen(optarg)>MAXSTR) {
	fprintf(stderr, "Hostname too long\n");
	return(1);
      }
      hostname = optarg;
      donet = 1;
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
  
  if (donet) {
    if (hostname.length() == 0) hostname = "localhost";
    sock = setup_net(hostname, port, window_size);
    if (sock==0) exit(EXIT_FAILURE);

  } else {
    sock = 0;
    if (outname.length()==0) outname = "vdif.out";
  }

  if (narg>1) 
    streamname = argv[optind+1];

  return convert2VDIF(argv[optind], streamname, outname, sock, mode);

  if (sock!=0) close(sock);

}

void initialize_lookup() {
  int i;
  lookup = new UINT8[256];
  for (i=0; i<256; i++) {
    lookup[i] = ((i>>1)&0x55) | ((i<<1)&0xAA);
  }
}

int setup_net(string hostname, int port, int window_size) {
  int sock, status;
  unsigned long ip_addr;
  socklen_t client_len, winlen;
  struct hostent     *hostptr;
  struct sockaddr_in server, client;    /* Socket address */

  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)port); /* Which port number to use */

  /* Create the initial socket */
  sock = socket(AF_INET,SOCK_STREAM,0); 
  if (sock==-1) {
    perror("Error creating socket");
    return(0);
  }

  if (window_size>0) {
    status = setsockopt(sock, SOL_SOCKET, SO_SNDBUF,
			(char *) &window_size, sizeof(window_size));
    if (status!=0) {
      perror("Error setting socket send buffer");
      close(sock);
      return(0);
    } 

    status = setsockopt(sock, SOL_SOCKET, SO_RCVBUF,
			(char *) &window_size, sizeof(window_size));
    
    if (status!=0) {
      perror("Error setting socket receive buffer");
      close(sock);
      return(0);
    }

    /* Check what the window size actually was set to */
    winlen = sizeof(window_size);
    status = getsockopt(sock, SOL_SOCKET, SO_SNDBUF,
			(char *) &window_size, &winlen);
    if (status!=0) {
      close(sock);
      perror("Getting socket options");
      return(0);
    }
    printf("Sending buffersize set to %d Kbytes\n", window_size/1024);
    
  }

  hostptr = gethostbyname(hostname.c_str());
  if (hostptr==NULL) {
    cerr << "Failed to look up hostname " << hostname << endl;
    close(sock);
    return(0);
  }
  
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  server.sin_addr.s_addr = ip_addr;
  
  printf("Connecting to %s\n",inet_ntoa(server.sin_addr));

  status = connect(sock, (struct sockaddr *) &server, sizeof(server));
  if (status!=0) {
    perror("Failed to connect to server");
    close(sock);
    return(0);
  }

  return(sock);
}

int netwrite(int sock, const void* buf, size_t len) {
  ssize_t nwrote;
  char *poff;

  poff = (char*)buf;
  while (len>0) {
    nwrote = send(sock, poff, len, 0);
    if (nwrote==-1) {
      if (errno == EINTR) continue;
      perror("Error writing to network");
      return(EXIT_FAILURE);
    } else if (nwrote==0) {
      cerr << "Warning: Did not write any bytes!" << endl;
      return(EXIT_FAILURE);
    } else {
      len -= nwrote;
      poff += nwrote;
    }
  }
  return(EXIT_SUCCESS);
}


int diskwrite(int out, const void* buf, size_t len) {
  ssize_t nwrote = write(out, buf, len);
  if (nwrote==-1) {
    perror("Error writing outfile");
    close(out);
    return(EXIT_FAILURE);
  } else if (nwrote!=len) {
    cerr << "Warning: Did not write all bytes! (" << nwrote << "/" << len << ")" << endl;
    close(out);
    return(EXIT_FAILURE);
  }
  return(EXIT_SUCCESS);
}
