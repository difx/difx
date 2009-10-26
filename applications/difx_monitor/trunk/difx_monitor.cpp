/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <cpgplot.h>
#include <string.h>
#include <sstream>
#include "architecture.h"
#include "configuration.h"

using namespace std;

//prototypes
void plot_results();
void change_config();
void openstream(int portnumber, int tcpwindowsizebytes);
void closestream();
int readnetwork(int sock, char* ptr, int bytestoread, int* nread);

Configuration * config;
int port, socketnumber, currentconfigindex, maxresultlength, buffersize, atseconds, bufferindex;
int resultlength, numchannels;
double intseconds = 1;
//IppsFFTSpec_C_32fc* fftspec;
IppsFFTSpec_R_32f* fftspec;
cf32 ** resultbuffer;
f32  *phase, *amplitude;
f32 *lags;
//cf32 *lags;
//f32 *lagamp; 
f32 *xval;

int main(int argc, const char * argv[])
{
  int timestampsec, readbytes, status = 1;
  int numchan;

  if(argc < 3 || argc > 4)
  {
    cerr << "Error - invoke with difx_monitor <inputfile> <port> [int time (s)]" << endl;
    return EXIT_FAILURE;
  }

  //work out the config stuff
  config = new Configuration(argv[1], 0);
  config->loaduvwinfo(true);
  port = atoi(argv[2]);
  if(argc == 4)
    intseconds = atof(argv[3]);

  //open up the socket
  openstream(port, Configuration::MONITOR_TCP_WINDOWBYTES);

  //create the buffers etc we need
  maxresultlength = config->getMaxResultLength();
  buffersize = int((intseconds-1)/config->getIntTime(0)) + 2;
  for(int i=1;i<config->getNumConfigs();i++)
  {
    int b = int((intseconds-1)/config->getIntTime(i)) + 2; 
    if(b > buffersize)
      buffersize = b;
  }
  resultbuffer = new cf32*[buffersize];
  for(int i=0;i<buffersize;i++) 
    resultbuffer[i] = vectorAlloc_cf32(maxresultlength);

  phase = vectorAlloc_f32(maxresultlength);
  amplitude = vectorAlloc_f32(maxresultlength);
  xval = 0;
  lags = 0;
  //lagamp = 0;
  fftspec = 0;

  //get into the loop of receiving, processing and plotting!
  currentconfigindex = -1;
  atseconds = 0;

  umask(002);

  while(atseconds < config->getExecuteSeconds()  && status > 0)
  {
    //receive the timestamp
    cout << "About to get a visibility" << endl;
    status = readnetwork(socketnumber, (char*)(&timestampsec), sizeof(int), &readbytes);
    if (status==-1) { // Error reading socket
      cerr << "Error reading socket" << endl;
      exit(1);
    } else if (status==0) {  // Socket closed remotely
      cerr << "Socket closed remotely - aborting" << endl;
      break;
    } else if (readbytes!=sizeof(int)) { // This should never happen
      cerr<<"Read size error!!!"<<endl;
      break;
    }

    //if not skipping this vis
    if(!(timestampsec < 0))
    {
      int bufsize;
      //receive the buffersize
      status = readnetwork(socketnumber, (char*)(&bufsize), sizeof(int), &readbytes);
      if (status==-1) { // Error reading socket
	cerr << "Error reading socket" << endl;
	exit(1);
      } else if (status==0) {  // Socket closed remotely
	cerr << "Socket closed remotely - aborting" << endl;
	break;
      } else if (readbytes!=sizeof(int)) { // This should never happen
	cerr<<"Read size error!!!"<<endl;
	break;
      }

      //receive the number of channels
      status = readnetwork(socketnumber, (char*)(&numchan), sizeof(int), &readbytes);
      if (status==-1) { // Error reading socket
	cerr << "Error reading socket" << endl;
	exit(1);
      } else if (status==0) {  // Socket closed remotely
	cerr << "Socket closed remotely - aborting" << endl;
	break;
      } else if (readbytes!=sizeof(int)) { // This should never happen
	cerr<<"Read size error!!!"<<endl;
	break;
      }

      //if config has changed, zero everything and grab new parameters from config
      atseconds = timestampsec;
      if(config->getConfigIndex(atseconds) != currentconfigindex) 
	change_config();

      //receive the results into a buffer
      status = readnetwork(socketnumber, (char*)resultbuffer[bufferindex], resultlength*sizeof(cf32), &readbytes);
      if (status==-1) { // Error reading socket
        cerr << "Error reading socket" << endl;
        exit(1);
      } else if (status==0) {  // Socket closed remotely
        cerr << "Socket closed remotely - aborting" << endl;
        break;
      } else if (readbytes!=resultlength*(int)sizeof(cf32)) { // This should never happen
        cerr<<"Read size error!!!"<<endl;
        break;
      }

      //process - skip this step for now and just plot

      //plot
      if(bufferindex == 0) {
        plot_results();
	cout << "Finished plotting time " << atseconds << endl;
      }
    }
    bufferindex++;
    if (bufferindex > buffersize - 2) {
      bufferindex=0;
    }
  }

  //close the socket
  closestream();
}

void plot_results()
{
  char pgplotname[256];
  char polpair[3];
  char timestr[10];
  string sourcename;
  ostringstream ss;
  f32 temp;

  polpair[2] = 0;
  int status = vectorZero_cf32(resultbuffer[buffersize-1], resultlength);
  if(status != vecNoErr)
  {
    cerr << "Error trying to zero accumulation buffer - aborting!" << endl;
    exit(1);
  }

  for(int i=0;i<buffersize-1;i++)
  {
    status = vectorAdd_cf32_I(resultbuffer[i], resultbuffer[buffersize-1], resultlength);
    if(status != vecNoErr)
    {
      cerr << "Error trying to add to accumulation buffer - aborting!" << endl;
      exit(1);
    }
  }

  int binloop = 1;
  if(config->pulsarBinOn(currentconfigindex) && !config->scrunchOutputOn(currentconfigindex))
    binloop = config->getNumPulsarBins(currentconfigindex);

  // Calculate amplitude and phase
  status = vectorPhase_cf32(resultbuffer[buffersize-1], phase, resultlength);
  if(status != vecNoErr)
  {
    cerr << "Error trying to calculate phase - aborting!" << endl;
    exit(1);
  }
  status = vectorMulC_f32_I(180/M_PI, phase, resultlength);

  status = vectorMagnitude_cf32(resultbuffer[buffersize-1], amplitude, resultlength);
  if(status != vecNoErr)
  {
    cerr << "Error trying to calculate amplitude - aborting!" << endl;
    exit(1);
  }

  cout << "Plotting time " << atseconds << endl;
  int at = 0;
  //int sourceindex = config->getSourceIndex
  for(int i=0;i<config->getNumBaselines();i++)
  {
    int ds1index = config->getBDataStream1Index(currentconfigindex, i);
    int ds2index = config->getBDataStream2Index(currentconfigindex, i);
    //cout << "Baseline name is " << config->getDStationName(currentconfigindex, ds1index) << "-" <<  config->getDStationName(currentconfigindex, ds2index) << endl; 
    for(int j=0;j<config->getBNumFreqs(currentconfigindex,i);j++)
    {
      int freqindex = config->getBFreqIndex(currentconfigindex, i, j);
      //cout << "Frequency is " << config->getFreqTableFreq(freqindex) << endl;
      for(int b=0;b<binloop;b++)
      {
        for(int k=0;k<config->getBNumPolProducts(currentconfigindex, i, j);k++)
        {
          config->getBPolPair(currentconfigindex,i,j,k,polpair);
          //cout << "Polarisation is " << polpair << endl;
          //get the lagspace as well
#if COMPLEXLAG
	  ippsFFTFwd_CToC_32fc(&resultbuffer[buffersize-1][at], lags, fftspec, 0);  // Should supply buffer
	  status = vectorMagnitude_cf32(lags, lagamp, numchannels);
	  if(status != vecNoErr)
	  {
	    cerr << "Error trying to calculate amplitude - aborting!" << endl;
	    exit(1);
	  }
#else
	  ippsFFTInv_CCSToR_32f((Ipp32f*)&resultbuffer[buffersize-1][at], lags, fftspec, 0);
	  //rearrange the lags into order
	  for(int l=0;l<numchannels;l++) {
            temp = lags[l];
            lags[l] = lags[l+numchannels];
	    lags[l+numchannels] = temp;
          }
#endif

          //plot something - data is from resultbuffer[at] to resultbuffer[at+numchannels+1]
          //cout << "Plotting baseline " << i << ", freq " << j << ", bin " << b << ", polproduct " << k << endl;

	  sprintf(pgplotname, "lba-%d-f%d-p%d-b%d.png/png",
		  i, j, k, b);
	  status = cpgbeg(0,pgplotname,1,3);
	  if (status != 1) {
	    cout << "Error opening pgplot device: " << pgplotname << endl;
	  } else {
	    float max, min, delta;

	    cpgscr(0,1,1,1);
	    cpgscr(1,0,0,0);

            // Plot lags
	    ippsMax_32f(lags, numchannels*2, &max);
	    ippsMin_32f(lags, numchannels*2, &min);
	    delta = (max-min)*0.05;
	    if (delta==0) delta = 1;
	    min -= delta;
	    max += delta;

	    cpgsch(1.5);
	    cpgsci(1);
            cpgenv(0,numchannels*2,min,max,0,0);
            cpglab("Channel", "Correlation coefficient", "");
            cpgsci(2);
            cpgline(numchannels*2, xval, lags);

	    // Annotate

	    config->getUVW()->getSourceName(config->getStartMJD(), 
		     atseconds+config->getStartSeconds(),sourcename);

	    cpgsci(4);
	    cpgsch(2.5);
	    ss << config->getDStationName(currentconfigindex, ds1index) 
	       << "-" 
	       <<  config->getDStationName(currentconfigindex, ds2index)
	       << "  " << sourcename; 
	    cpgmtxt("T", -1.5,0.02, 0, ss.str().c_str());	    
	    ss.str("");

	    cpgmtxt("T", -1.5,0.97,1,polpair);

	    ss << config->getFreqTableFreq(freqindex) << " MHz";
	    cpgmtxt("T", -2.6,0.97,1,ss.str().c_str());	    
	    ss.str("");

	    int seconds = atseconds+config->getStartSeconds();
	    int hours = seconds/3600;
	    seconds -= hours*3600;
	    int minutes = seconds/60;
	    seconds %= 60;
	    sprintf(timestr, "%02d:%02d:%02d", hours, minutes, seconds);
	    cpgmtxt("T", -1.5,0.90,1,timestr);

	    //ss << hours << ":" << minutes << ":" << seconds;
	    //cpgmtxt("T", -1.5,0.90,1,ss.str().c_str());
	    //ss.str("");

	    cpgsch(1.5);
	    cpgsci(1);

	    // Plot Amplitude
	    ippsMax_32f(&amplitude[at], numchannels, &max);
	    ippsMin_32f(&amplitude[at], numchannels, &min);
	    delta = (max-min)*0.05;
	    if (delta==0) delta = 1;
	    min -= delta;
	    max += delta;

	    cpgsci(1);
	    cpgenv(0,numchannels,min,max,0,0);
	    cpglab("Channel", "Amplitude (Jy)", "");
	    cpgsci(2);
	    cpgline(numchannels, xval, &amplitude[at]);   

	    // Plot Phase
	    cpgsci(1);
	    cpgenv(0,numchannels,-180,180,0,0);
	    cpglab("Channel", "Phase (deg)", "");
	    cpgsci(2);
	    cpgsch(2);
	    cpgpt(numchannels, xval, &phase[at], 17);
	    cpgsch(1);


	    cpgend();
	  }
          at = at + numchannels + 1;
        }
      }
    }
  }

  int autocorrwidth = (config->getMaxProducts()>1)?2:1;
  for(int i=0;i<config->getNumDataStreams();i++)
  {
    //cout << "Doing autocorrelation for " << config->getDStationName(currentconfigindex, i) << endl;
    for(int j=0;j<autocorrwidth;j++)
    {
      for(int k=0;k<config->getDNumOutputBands(currentconfigindex, i); k++)
      {
        //keep plotting...
        //cout << "Frequency is " <<  config->getFreqTableFreq(config->getDFreqIndex(currentconfigindex, i, k)) << endl;
        //char firstpol = config->getDBandPol(currentconfigindex, i, k);

	if (j==0) {
	  sprintf(pgplotname, "lba-auto%d-b%d.png/png",
		  i, k);
	  status = cpgbeg(0,pgplotname,1,1);
	} else {
	  sprintf(pgplotname, "lba-autocross%d-b%d.png/png",
		  i, k);
	  status = cpgbeg(0,pgplotname,1,2);
	}
	if (status != 1) {
	  cout << "Error opening pgplot device: " << pgplotname << endl;
	} else {
	  float max, min;

	  cpgscr(0,1,1,1);
	  cpgscr(1,0,0,0);

	  // Plot Amplitude
	  max = amplitude[at];
	  min = max;
	  for (int n=at; n<at+numchannels; n++) {
	    if (amplitude[n] > max) max = amplitude[n];
	    if (amplitude[n] < min) min = amplitude[n];
	  }
	  if (min==max) {
	    min-=1;
	    max+=1;
	  }

	  cpgsci(1);
	  cpgenv(0,numchannels+1,min,max,0,0);
	  cpglab("Channel", "Amplitude (Jy)", "");
	  cpgsci(2);
	  cpgline(numchannels, xval, &amplitude[at]);

	  if (j!=0) {
	    // Plot Phase
	    cpgsci(1);
	    cpgenv(0,numchannels,-180,180,0,0);
	    cpglab("Channel", "Phase (deg)", "");
	    cpgsci(2);
	    cpgline(numchannels, xval, &phase[at]);
	  }
	  cpgend();
	}

	at = at + numchannels + 1;
      }
    }
  }
}

void change_config()
{
  int status;


  currentconfigindex = config->getConfigIndex(atseconds);
  numchannels = config->getNumChannels(currentconfigindex);

  cout << "New config " << currentconfigindex << " at " << atseconds << endl;

  if(xval)
    vectorFree(xval);
  if(lags)
    vectorFree(lags);
  //if(lagamp)
  //  vectorFree(lagamp);
  if(fftspec)
    ippsFFTFree_R_32f(fftspec);
  //ippsFFTFree_C_32fc(fftspec);

  //xval = vectorAlloc_f32(numchannels*2);
  xval = vectorAlloc_f32(numchannels*2);
  for(int i=0;i<numchannels*2;i++)
    xval[i] = i;

  lags = vectorAlloc_f32(numchannels*2);
  //lags = vectorAlloc_cf32(numchannels);
  //lagamp = vectorAlloc_f32(numchannels);

  int order = 0;
  //while(((numchannels) >> order) > 1)
  while(((numchannels*2) >> order) > 1)
    order++;

  //ippsFFTInitAlloc_C_32fc(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
  ippsFFTInitAlloc_R_32f(&fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast);
  bufferindex = 0;
  buffersize = int((intseconds-1)/config->getIntTime(currentconfigindex)) + 2;
  cout << "BUFFERSIZE" << buffersize << endl;
  resultlength = config->getResultLength(currentconfigindex);
  for(int i=0;i<buffersize;i++)
  {
    status = vectorZero_cf32(resultbuffer[i], resultlength);
    if(status != vecNoErr) {
      cerr << "Error trying to zero buffer entry " << i << " - aborting!" << endl;
      exit(1);
    }
  }
}

void openstream(int portnumber, int tcpwindowsizebytes)
{
  int serversock, status;
  socklen_t client_len;
  struct linger      linger = {1, 1};
  struct sockaddr_in server, client;    /* Socket address */

  /* Open a server connection for reading */

  /* Initialise server's address */
  memset((char *)&server,0,sizeof(server));
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = htonl(INADDR_ANY); /* Anyone can connect */
  server.sin_port = htons((unsigned short)portnumber); /* Which port number to use */

  /* Create a server to listen with */
  serversock = socket(AF_INET,SOCK_STREAM,0); 
  if (serversock==-1) 
    cerr << "Error creating socket" << endl;

  /* Set the linger option so that if we need to send a message and
     close the socket, the message shouldn't get lost */
  status = setsockopt(serversock, SOL_SOCKET,SO_LINGER, (char *)&linger,
		      sizeof(struct linger));
  if (status!=0) {
    cerr << "Error setting socket options" << endl;
    close(serversock);
  } 
  
  /* Set the TCP window size */
  setsockopt(serversock, SOL_SOCKET, SO_SNDBUF,
	     (char *) &tcpwindowsizebytes, sizeof(tcpwindowsizebytes));
  if (status!=0) {
    cerr << "Error setting socket options" << endl;
    close(serversock);
  } 

  setsockopt(serversock, SOL_SOCKET, SO_RCVBUF,
	     (char *) &tcpwindowsizebytes, sizeof(tcpwindowsizebytes));

  if (status!=0) {
    cerr << "Error setting socket options" << endl;
    close(serversock);
  } 
  
  status = bind(serversock, (struct sockaddr *)&server, sizeof(server));
  if (status!=0) {
    cerr << "Error binding socket" << endl;
    close(serversock);
  } 
  
  /* We are willing to receive conections, using the maximum
     back log of 1 */
  status = listen(serversock,1);
  if (status!=0) {
    cerr << "Error binding socket" << endl;
    close(serversock);
  }

  cout << "Waiting for connection" << endl;

  /* Accept connection */
  client_len = sizeof(client);
  socketnumber = accept(serversock, (struct sockaddr *)&client, &client_len);
  if (socketnumber == -1) {
    cerr << "Error connecting to client" << endl;
    close(serversock);
  }

  cout << "Got a connection from " << inet_ntoa(client.sin_addr) << endl;
}

int readnetwork(int sock, char* ptr, int bytestoread, int* nread)
{
  int nr;

  *nread = 0;

  while (bytestoread>0)
  {
    nr = recv(sock,ptr,bytestoread,0);

    if (nr==-1) { // Error reading socket
      if (errno == EINTR) continue;
      cout << "Only read " << *nread << " out of " << bytestoread+*nread << endl;
      perror("");
      return(nr);
    } else if (nr==0) {  // Socket closed remotely
      return(nr);
    } else {
      ptr += nr;
      *nread += nr;
      bytestoread -= nr;
    }
  }
  return(1);
}

void closestream()
{
  //closes the socket
  int status;

  status = close(socketnumber);
  if (status!=0) 
    cerr << "Error closing socket" << endl;
}


