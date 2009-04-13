/***************************************************************************
 *   Copyright (C) 2009 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id:  $
// $HeadURL:  $
// $LastChangedRevision:  $
// $Author: $
// $LastChangedDate:  $
//
//============================================================================

#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "alert.h"
#include "vdif_server.h"

int main(int argc, char *argv[])
{
  if(argc != 2) {
    cfatal << startl << "VDIF_Server must be invoked with one argument only (configfilename) - aborting!" << endl;
    exit(1);
  }

  //create the VDIF_Server
  string filename = string(argv[1]);
  VDIF_Server * vds = new VDIF_Server(filename);
  if(!vds->initialisedOK()) {
    cfatal << startl << "Problem iniialising VDIF_Server - aborting!" << endl;
    exit(1);
  }

  //Execute
  cinfo << startl << "VDIF_Server is commencing data farming" << endl;
  bool success = vds->serve_data();
  if(success) 
    cinfo << startl << "VDIF_Server has completed successfully" << endl;
  else
    cinfo << startl << "VDIF_Server failed during execution - sorry." << endl;
}

const char * VDIF_Server::DS_NETWORK_STR = "NETWORK";
const char * VDIF_Server::DS_MODULE_STR = "MODULE";
const char * VDIF_Server::DS_FILE_STR = "FILE";

VDIF_Server::VDIF_Server(string configfilename)
{
  //parse the input file
  inputparms = newDifxParametersfromfile(configfilename.c_str());
  init_ok = parseCommonTable(inputparms);
  if(init_ok)
    init_ok = parseServerTable(inputparms);
  if(init_ok)
    init_ok = parseDatasourceTable(inputparms);

  //if ok, create the read buffer and bufferinfo
  if(init_ok) {
    readbuffer = (char*)malloc(readbuffersegmentbytes*numreadbuffersegments);
    if(readbuffer == NULL) {
      cfatal << startl << "Error allocating read buffer - aborting!" << endl;
      init_ok = false;
    }
    readbufinfo = new readsegmentinfo[numreadbuffersegments];
    for(int i=0;i<numreadbuffersegments;i++)
      pthread_mutex_init(&(readbufinfo[i].lock), NULL);
  }

  //if still ok, create the thread buffers and bufferinfo
  if(init_ok) {
    threadbuffers = new char*[maxthreads];
    socket_handles = new int*[maxthreads];
    threadbufinfo = new threadsegmentinfo*[maxthreads];
    for(int i=0;i<maxthreads;i++) {
      threadbuffers[i] = (char*)malloc(threadbuffersegmentbytes*numthreadbuffersegments);
      if(threadbuffers[i]==NULL) {
	cfatal << startl << "Error allocating thread buffer " << i << "- aborting!" << endl;
	init_ok = false;
      }
      threadbufinfo[i] = new threadsegmentinfo[numthreadbuffersegments];
      for(int j=0;j<numreadbuffersegments;j++)
	pthread_mutex_init(&(threadbufinfo[i][j].lock), NULL);
    }
  }

  //if still ok, populate the settings for each second of the experiment and check consistency
  if(init_ok) {
    populateTimeSettings();
    init_ok = consistencyCheck();
  }
}

VDIF_Server::~VDIF_Server()
{}

bool VDIF_Server::serve_data()
{
  int lastsendsegment, perr;

  //initialise the read and thread buffers
  initialiseBuffers();

  //send data until we're done
  while(remainingData()) {
    lastsendsegment = sendsegment;
    sendsegment = (sendsegment + 1)%numthreadbuffersegments;
    lockThreadSegment(sendsegment);
    unlockThreadSegment(lastsendsegment);
    if(remainingData())
      sendData(sendsegment);
  }

  //close the TCP connections
  closeSockets();

  //Shut down and join the read and copy threads
  joinThreads();
}

void VDIF_Server::sendData(int threadsegment)
{
  int sec;

  for(int i=0;i<maxthreads;i++) {
    //check if there is data to send for this thread
    if(threadbufinfo[i][threadsegment].framesfilled > 0) {
      sec = threadbufinfo[i][threadsegment].jobsecond;

      //if the TCP connection is not open, try to open it
      if(!socketOpen(i))
	openSocket(i, &(serversettings[setting_indices[sec]]));
  
      //send the data
      if(socketOpen(i))
	sendToSocket(&(threadbuffers[i][threadsegment]), i, threadbuffersegmentbytes);
      else
	cerror << startl << "Could not open socket for thread " << i << " - " << threadbufinfo[i][threadsegment].framesfilled << " frames have been lost!" << endl;
    }
  }
}

void VDIF_Server::initialiseBuffers()
{
  int perr;

  //lock the first segment and the last segment for the read buffer
  lockReadSegment(0);
  lockReadSegment(numreadbuffersegments-1);

  //lock the last segment of the thread buffer
  sendsegment = numthreadbuffersegments-1;
  lockThreadSegment(sendsegment);

  //start the readthread, wait for it to clear the first two segments
  readthreadstarted = false;
  perr = pthread_create(&readthread, NULL, VDIF_Server::launchNewReadThread, (void *)(this));
  if(perr != 0)
    csevere << startl << "Error in launching VDIF_Reader thread!!!" << endl;
  while(!readthreadstarted)
  {
    perr = pthread_cond_wait(&readinitcond, &(readbufinfo[0].lock));
    if (perr != 0)
      csevere << startl << "Error waiting on readthreadstarted condition!!!!" << endl;
  }

  //start the copythread, wait for it to clear the first segment
  copythreadstarted = false;
  perr = pthread_create(&copythread, NULL, VDIF_Server::launchNewCopyThread, (void *)(this));
  if(perr != 0)
    csevere << startl << "Error in launching copy thread!!!" << endl;
  while(!copythreadstarted)
  {
    perr = pthread_cond_wait(&copyinitcond, &(readbufinfo[0].lock));
    if (perr != 0)
      csevere << startl << "Error waiting on copythreadstarted condition!!!!" << endl;
  }

  //unlock the two mutexes main thread holds on the readbuffer
  unlockReadSegment(0);
  unlockReadSegment(numreadbuffersegments-1);
}

void VDIF_Server::joinThreads()
{
  int perr;

  //tell the two threads they're done
  vdr->stopReading();
  vdc->stopCopying();

  //release the lock that main thread holds
  unlockThreadSegment(sendsegment);

  //join the threads
  perr = pthread_join(readthread, NULL);
  if(perr != 0)
    csevere << startl << "Error in closing readthread!!!" << endl;
  perr = pthread_join(copythread, NULL);
  if(perr != 0)
    csevere << startl << "Error in closing copythread!!!" << endl;
}

bool VDIF_Server::remainingData()
{
  //if we are past the end of the execution time, we're done
  if (threadbufinfo[0][sendsegment].jobsecond >= executeseconds)
    return false;
  
  //or if the VDIF_Reader is dead and we have no data in our thread buffer
  if (!vdr->stillReading()) {
    bool empty = true;
    for(int i=0;i<maxthreads;i++) {
      if (threadbufinfo[i][sendsegment].framesfilled != 0)
	empty = false;
    }
    return !empty;
  }

  //if we get to here job time is ok and we still have some left data to send
  return true;
}

bool VDIF_Server::parseCommonTable(DifxParameters * inputparms)
{
  const char commonKeys[][MAX_DIFX_KEY_LEN] = {
    "START MJD",
    "START SECONDS",
    "EXECUTION SECONDS",
    "INPUT FILE",
    "RBUFFER SEG BYTES",
    "NUM RBUFFER SEGS",
    "TBUFFER SEG BYTES",
    "NUM TBUFFER SEGS"
  };

  const int N_COMMON_ROWS = sizeof(commonKeys)/sizeof(commonKeys[0]);
  int nfound;
  int rows[N_COMMON_ROWS];
  
  //locate the common keys in the input file
  nfound = DifxParametersbatchfind(inputparms, 0, commonKeys, N_COMMON_ROWS, rows);
  if(nfound < N_COMMON_ROWS)
  {
    cfatal << startl << "VDIF_SERVER: error parsing common table, only found " << nfound << " of " << N_COMMON_ROWS << " required rows - aborting!" << endl;
    return false;
  }
  
  //populate our local variables
  startmjd = atoi(DifxParametersvalue(inputparms, rows[0]));
  startseconds = atoi(DifxParametersvalue(inputparms, rows[1]));
  executeseconds = atoi(DifxParametersvalue(inputparms, rows[2]));
  inputfile = string(DifxParametersvalue(inputparms, rows[3]));
  readbuffersegmentbytes = atoi(DifxParametersvalue(inputparms, rows[4]));
  numreadbuffersegments = atoi(DifxParametersvalue(inputparms, rows[5]));
  threadbuffersegmentbytes = atoi(DifxParametersvalue(inputparms, rows[6]));
  numthreadbuffersegments = atoi(DifxParametersvalue(inputparms, rows[7]));
  config = new Configuration(inputfile.c_str(), -1);
  model = config->getModel();

  return true;
}

bool VDIF_Server::parseServerTable(DifxParameters * inputparms)
{
  int row;
  maxthreads = 0;
  
  //find the first line of the server settings table
  row = DifxParametersfind(inputparms, 0, "NUM SETTINGS");
  if(row < 0) {
    cfatal << startl << "Could not locate server settings table - aborting!" << endl;
    return false;
  }
  numserversetups = atoi(DifxParametersvalue(inputparms, row));
  serversettings = new serversetting[numserversetups];

  //populate the local variables for each server setting
  for(int i=0;i<numserversetups;i++) {
    row = DifxParametersfind1(inputparms, row+1, "SET %d SRC NAME", i);
    if(row<0) break;
    serversettings[i].srcname = string(DifxParametersvalue(inputparms, row));
    row = DifxParametersfind1(inputparms, row+1, "SET %d SRC CALCODE", i);
    if(row<0) break;
    serversettings[i].srccalcode = DifxParametersvalue(inputparms, row)[0];
    row = DifxParametersfind1(inputparms, row+1, "SET %d SRC QUAL", i);
    if(row<0) break;
    serversettings[i].srcqual = atoi(DifxParametersvalue(inputparms, row));
    row = DifxParametersfind1(inputparms, row+1, "SET %d SRC MODE", i);
    if(row<0) break;
    serversettings[i].obsmode = string(DifxParametersvalue(inputparms, row));
    row = DifxParametersfind1(inputparms, row+1, "SET %d SCAN", i);
    if(row<0) break;
    serversettings[i].scan = atoi(DifxParametersvalue(inputparms, row));
    row = DifxParametersfind1(inputparms, row+1, "SET %d FRAME BYTES", i);
    if(row<0) break;
    serversettings[i].framebytes = atoi(DifxParametersvalue(inputparms, row));
    row = DifxParametersfind1(inputparms, row+1, "SET %d FRAMES/SEC", i);
    if(row<0) break;
    serversettings[i].framespersec = atoi(DifxParametersvalue(inputparms, row));
    row = DifxParametersfind1(inputparms, row+1, "SET %d NUM THREADS", i);
    if(row<0) break;
    serversettings[i].nthreads = atoi(DifxParametersvalue(inputparms, row));
    if(serversettings[i].nthreads > maxthreads)
      maxthreads = serversettings[i].nthreads;
    serversettings[i].threadtcpwins = new int[serversettings[i].nthreads];
    serversettings[i].threadports = new int[serversettings[i].nthreads];
    for(int j=0;j<serversettings[i].nthreads;j++) {
      row = DifxParametersfind2(inputparms, row+1, "SET %d T%d PORT", i, j);
      if(row<0) break;
      serversettings[i].threadports[j] = atoi(DifxParametersvalue(inputparms, row));
      row = DifxParametersfind2(inputparms, row+1, "SET %d T%d TCPWIN B", i, j);
      if(row<0) break;
      serversettings[i].threadtcpwins[j] = atoi(DifxParametersvalue(inputparms, row));
      row = DifxParametersfind2(inputparms, row+1, "SET %d T%d HOSTNAME", i, j);
      if(row<0) break;
      serversettings[i].hostnames[j] = string(DifxParametersvalue(inputparms, row));
    }
  }
  if(row < 0) {
    cfatal << startl << "Could not locate a row in the server settings table - aborting!" << endl;
    return false;
  }
  return true;
}

bool VDIF_Server::parseDatasourceTable(DifxParameters * inputparms)
{
  int row;

  //find the first line of the Datasource Table
  row = DifxParametersfind(inputparms, 0, "DATA SOURCE");
  if(row < 0) {
    cfatal << startl << "Could not locate data source table - aborting!" << endl;
    return false;
  }
  if(strcmp(DifxParametersvalue(inputparms, row), DS_MODULE_STR) == 0)
    datasrc = MODULE;
  else if(strcmp(DifxParametersvalue(inputparms, row), DS_FILE_STR) == 0)
    datasrc = FILE;
  else if(strcmp(DifxParametersvalue(inputparms, row), DS_NETWORK_STR) == 0)
    datasrc = NETWORK;
  else {
    cerror << startl << "Could not identify data source " << DifxParametersvalue(inputparms, row) << "; setting source type to FILE!" << endl;
    datasrc = FILE;
  }
  
  //read the contents of the table, differs depending on the source
  switch(datasrc) {
    case MODULE:
      row = DifxParametersfind(inputparms, row+1, "MODULE NAME");
      if(row < 0) {
	cfatal << startl << "Could not find data source module - aborting!" << endl;
	return false;
      }
      modulename = string(DifxParametersvalue(inputparms, row));
      break;
    case FILE:
      row = DifxParametersfind(inputparms, row+1, "NUM FILES");
      if(row < 0) {
	cfatal << startl << "Could not find num data files line - aborting!" << endl;
	return false;
      }
      numfiles = atoi(DifxParametersvalue(inputparms, row));
      filenames = new string[numfiles];
      for(int i=0;i<numfiles;i++) {
	row = DifxParametersfind(inputparms, row+1, "FILENAME");
	if(row < 0) {
	  cfatal << startl << "Could not find a source data file - aborting!" << endl;
	  return false;
	}
	filenames[i] = string(DifxParametersvalue(inputparms, row));
      }
      break;
    case NETWORK:
      row = DifxParametersfind(inputparms, row+1, "DATASRC PORT");
      if(row < 0) {
	cfatal << startl << "Could not find data source port line - aborting!" << endl;
	return false;
      }
      srcport = atoi(DifxParametersvalue(inputparms, row));
      row = DifxParametersfind(inputparms, row+1, "DATASRC TCP WIN (B)");
      if(row < 0) {
	cfatal << startl << "Could not find data source TCP window size - aborting!" << endl;
	return false;
      }
      srctcpwin = atoi(DifxParametersvalue(inputparms, row));
  }
}  

void VDIF_Server::populateTimeSettings()
{
  bool found;
  setting_indices = new int[executeseconds];

  //work out which setting applies at every second of the job
  for(int i=0;i<executeseconds;i++) {
    found = false;
    setting_indices[i] = -1;
    for(int j=0;j<numserversetups;j++) {
      if(matchingSetup(i, serversettings[j])) {
	if(found) { //already found a matching setup
	  if(serversettings[setting_indices[i]].nthreads != serversettings[j].nthreads)
	    init_ok = false;
	  if(serversettings[setting_indices[i]].framebytes != serversettings[j].framebytes)
	    init_ok = false;
	  if(serversettings[setting_indices[i]].framespersec != serversettings[j].framespersec)
	    init_ok = false;
	  for(int k=0;k<serversettings[j].nthreads;k++) {
	    if(serversettings[setting_indices[i]].threadtcpwins[k] != serversettings[j].threadtcpwins[k])
	      init_ok = false;
	    if(serversettings[setting_indices[i]].threadports[k] != serversettings[j].threadports[k])
	      init_ok = false;
	    if(serversettings[setting_indices[i]].hostnames[k] != serversettings[j].hostnames[k])
	      init_ok = false;
	  }
	}
	else
	  setting_indices[i] = j;
      }
    }
    if(!init_ok) {
      cfatal << startl << "Inconsistent setup found at time " << i << " seconds into execution - two inconsistent server setups match! Aborting." << endl;
      break; //no point continuing
    }
  }
}

bool VDIF_Server::consistencyCheck()
{
  cout << "Consistency check not yet properly implemented" << endl;

  //check that nbytes is a multiple of frames, for all frame sizes

  //check that there is an even number of segments in a second, always

  //passed all tests, must be ok
  return true;
}

bool VDIF_Server::matchingSetup(int executesec, serversetting s)
{
  cout << "MatchingSetup not yet properly implemented" << endl;

  //see if it is disqualified by any condition

  //if not, must be matching
  return true;
}

void VDIF_Server::lockThreadSegment(int segment)
{
  int perr;
  for(int i=0;i<maxthreads;i++) {
    perr = pthread_mutex_lock(&(threadbufinfo[i][segment].lock));
    if(perr != 0)
      csevere << startl << "Error trying to lock thread " << i << " buffer index " << segment << endl;
  }
}

void VDIF_Server::unlockThreadSegment(int segment)
{
  int perr;
  for(int i=0;i<maxthreads;i++) {
    perr = pthread_mutex_unlock(&(threadbufinfo[i][segment].lock));
    if(perr != 0)
      csevere << startl << "Error trying to unlock thread " << i << " buffer index " << segment << endl;
  }
}

void VDIF_Server::updateThreadSegment(int segment)
{
  int prevsegment, prevsecond, prevfirstframe, prevnumframes, prevframespersec;

  prevsegment = (segment+numthreadbuffersegments-1)%numthreadbuffersegments;

  for(int i=0;i<maxthreads;i++) {
    prevsecond       = threadbufinfo[i][prevsegment].jobsecond;
    prevfirstframe   = threadbufinfo[i][prevsegment].firstframenumber;
    prevnumframes    = threadbufinfo[i][prevsegment].framesrequired;
    prevframespersec = serversettings[setting_indices[prevsecond]].framespersec;

    threadbufinfo[i][segment].framesfilled = 0;
    threadbufinfo[i][segment].jobsecond = prevsecond;
    threadbufinfo[i][segment].framesrequired = threadbuffersegmentbytes/serversettings[setting_indices[threadbufinfo[i][segment].jobsecond]].framebytes;
    threadbufinfo[i][segment].firstframenumber = prevfirstframe + prevnumframes;
    if(threadbufinfo[i][segment].firstframenumber >= prevframespersec) {
      threadbufinfo[i][segment].firstframenumber -= prevframespersec;
      threadbufinfo[i][segment].jobsecond++;
    }
  }
}

void VDIF_Server::lockReadSegment(int segment)
{
  int perr;
  perr = pthread_mutex_lock(&(readbufinfo[segment].lock));
  if(perr != 0)
    csevere << startl << "Error trying to lock readbuffer index " << segment << endl;
}

void VDIF_Server::unlockReadSegment(int segment)
{
  int perr;
  perr = pthread_mutex_unlock(&(readbufinfo[segment].lock));
  if(perr != 0)
    csevere << startl << "Error trying to unlock readbuffer index " << segment << endl;
}

/* Thread launchers */
void * VDIF_Server::launchNewReadThread(void * thisserver)
{
  VDIF_Server * vs = (VDIF_Server*)thisserver;

  switch(vs->datasrc) {
  case MODULE:
    vs->vdr = new VDIF_Module_Reader(vs, vs->modulename);
    break;
  case FILE:
    vs->vdr = new VDIF_File_Reader(vs, vs->numfiles, vs->filenames);
    break;
  case NETWORK:
    vs->vdr = new VDIF_Network_Reader(vs, vs->srctcpwin, vs->srcport);
    break;
  }

  vs->vdr->loopread();
}

void * VDIF_Server::launchNewCopyThread(void * thisserver)
{
  VDIF_Server * vs = (VDIF_Server*)thisserver;
  vs->vdc = new VDIF_Copier(vs);
  vs->vdc->loopcopy();
}

/* VDIF_Copier */
VDIF_Server::VDIF_Copier::VDIF_Copier(VDIF_Server * p)
  : parent(p)
{
  int perr;
  numreadsegments = parent->numreadbuffersegments;
  numthreadsegments = parent->numthreadbuffersegments;
  readsegment = 0;
  oldestthreadsegment = 0;
  newestthreadsegment = 0;
  parent->lockReadSegment(readsegment);
  parent->lockThreadSegment(oldestthreadsegment);
  drainsegment(readsegment++);
  parent->lockReadSegment(readsegment);
  parent->unlockReadSegment(readsegment-1);

  perr = pthread_cond_signal(&(parent->copyinitcond));
  if(perr != 0)
    csevere << startl << "Copy error trying to signal main thread to wake up!!!" << endl;
}

VDIF_Server::VDIF_Copier::~VDIF_Copier() {}

void VDIF_Server::VDIF_Copier::loopcopy() {
  int nextsegment;
  while(stillcopying) {
    drainsegment(readsegment);
    parent->readbufinfo[readsegment].framesfilled = 0;
    nextsegment = (readsegment + 1)%numreadsegments;
    parent->lockReadSegment(nextsegment);
    parent->unlockReadSegment(readsegment);
    readsegment = nextsegment;
  }
}

void VDIF_Server::VDIF_Copier::drainsegment(int segment) {
  int numframes, framebytes, framethread, framemjd, framesecond, framenumber;
  int jobsecond, threadbufferindex, bufferframenum;
  char * buffer;
  char * destination;
  threadsegmentinfo * destinfo;

  numframes = (parent->readbufinfo[segment]).framesfilled;
  buffer = &((parent->readbuffer)[parent->readbuffersegmentbytes*segment]);
  framebytes = getVDIFFrameBytes(buffer);

  for(int i=0;i<numframes;i++) {
    framethread = getVDIFThreadID(buffer);
    framemjd    = getVDIFFrameMJD(buffer);
    framesecond = getVDIFFrameSecond(buffer);
    framenumber = getVDIFFrameNumber(buffer);
    jobsecond   = (framemjd-parent->startmjd)*86400 + framesecond - parent->startseconds;
    threadbufferindex = getThreadBufferIndex(jobsecond, framenumber);
    if(threadbufferindex >= 0) {
      destination = &((parent->threadbuffers)[framethread][threadbufferindex]);
      destinfo = &((parent->threadbufinfo)[framethread][threadbufferindex]);
      bufferframenum = framenumber - destinfo->firstframenumber;
      memcpy(&(destination[bufferframenum*framebytes]), buffer, framebytes);
    }
    buffer += framebytes;
  }
}

int VDIF_Server::VDIF_Copier::getThreadBufferIndex(int second, int framenum)
{
  threadsegmentinfo ** tbi = parent->threadbufinfo;
  int startsegment, currentsegment;

  //check that this data is not too old
  if(tbi[oldestthreadsegment]->jobsecond > second || (tbi[oldestthreadsegment]->jobsecond == second && tbi[oldestthreadsegment]->firstframenumber > framenum)) { //too old
    cerror << startl << "Attempted to copy stale data from time " << second << " seconds, framenum " << framenum << " - will be discarded!" << endl;
    return -1;
  }

  startsegment = oldestthreadsegment;
  //make sure our locked range covers the data
  while(tbi[newestthreadsegment]->jobsecond < second || (tbi[newestthreadsegment]->jobsecond == second && tbi[newestthreadsegment]->firstframenumber > framenum)) {
    //we will have to lock the next newest segment
    //check if we already hold the maximum number
    if((newestthreadsegment + numthreadsegments - oldestthreadsegment)%numthreadsegments >= numthreadsegments/2) {
      cerror << "Attempting to copy data (from time " << second << " seconds, frame " << framenum << " which is too recent! Will drop old data" << endl;
      parent->unlockThreadSegment(oldestthreadsegment++);
      for(int i=0;i<(newestthreadsegment+numthreadsegments-oldestthreadsegment)%numthreadsegments;i++) {
	if(threadSegmentComplete((oldestthreadsegment+i)%numthreadsegments))
	  parent->unlockThreadSegment(oldestthreadsegment++);
	else
	  break;
      }
    }
    parent->lockThreadSegment(++newestthreadsegment);
    parent->updateThreadSegment(newestthreadsegment);
    startsegment = newestthreadsegment;
  }

  //figure out which segment the data comes from
  for(int i=0;i<newestthreadsegment-oldestthreadsegment;i++) {
    currentsegment = (startsegment+i)%numthreadsegments;
    if(tbi[currentsegment]->jobsecond == second && tbi[currentsegment]->firstframenumber < framenum && framenum - tbi[currentsegment]->firstframenumber < tbi[currentsegment]->framesrequired)
      return currentsegment;
  }

  csevere << startl << "Somehow did not find correct location to copy data from time " << second << " seconds, framenum " << framenum << "; will ignore it!" << endl;
  return -1;
}

bool VDIF_Server::VDIF_Copier::threadSegmentComplete(int segment)
{
  bool done = true;
  for(int i=0;i<parent->maxthreads;i++) {
    if(parent->threadbufinfo[i][segment].framesfilled < parent->threadbufinfo[i][segment].framesrequired)
      done = false;
  }
  return done;
}

/* VDIF_Reader */
VDIF_Server::VDIF_Reader::VDIF_Reader(VDIF_Server * p)
  : parent(p)
{
  int perr;

  atsegment = 0;
  stillreading = true;
  parent->lockReadSegment(atsegment);
  fillsegment(atsegment++);
  parent->lockReadSegment(atsegment);
  parent->unlockReadSegment(atsegment-1);
  fillsegment(atsegment++);
  parent->lockReadSegment(atsegment);
  parent->unlockReadSegment(atsegment-1);

  perr = pthread_cond_signal(&(parent->readinitcond));
  if(perr != 0)
    csevere << startl << "Read error trying to signal main thread to wake up!!!" << endl;
}

VDIF_Server::VDIF_Reader::~VDIF_Reader() {}

void VDIF_Server::VDIF_Reader::loopread()
{
  int nextsegment;
  while(stillreading) {
    fillsegment(atsegment);
    nextsegment = (atsegment + 1)%parent->numreadbuffersegments;
    parent->lockReadSegment(nextsegment);
    parent->unlockReadSegment(atsegment);
    atsegment = nextsegment;
  }
}

VDIF_Server::VDIF_File_Reader::VDIF_File_Reader(VDIF_Server * p, int nfiles, string * fnames)
  : VDIF_Reader(p), numfiles(nfiles), filenames(fnames), atfile(0), fileopen(false)
{}


void VDIF_Server::VDIF_File_Reader::fillsegment(int segment)
{
  readsegmentinfo info = parent->readbufinfo[segment];
  int framemjd, framesecond, readsecond;
  char * writeto = &(parent->readbuffer[segment*parent->readbuffersegmentbytes]);
  while(!fileopen) {
    if(atfile >= numfiles) {
      stillreading = false;
      return; //note exit here! No more files to process
    }
    openfile(atfile++);
  }
  input.read(writeto, parent->readbuffersegmentbytes);
  framemjd = getVDIFFrameMJD(writeto);
  framesecond = getVDIFFrameSecond(writeto);
  readsecond = (framemjd-parent->startmjd)*86400 + (framesecond - parent->startseconds);
  info.framesfilled = input.gcount()/parent->serversettings[parent->setting_indices[readsecond]].framebytes;
  if(input.eof() || input.peek() == EOF)
    fileopen = false;
}

void VDIF_Server::VDIF_File_Reader::openfile(int filenum)
{
  if(input.fail())
    input.clear(); //get around EOF problems caused by peeking
  input.open(filenames[filenum].c_str(),ios::in);
  fileopen = true;
  if(!input.is_open() || input.bad() || input.peek() == EOF)
    fileopen = false;
}

/* Networking stuff below... */

bool VDIF_Server::socketOpen(int thread)
{
  return (*socket_handles[thread]>=0)?true:false;
}

void VDIF_Server::openSocket(int thread, serversetting * setting)
{
  int status, saveflags, getopt_status, back_err;
  unsigned long ip_addr;
  struct hostent * hostptr;
  struct sockaddr_in server;    /* Socket address */
  fd_set fd_w;
  struct timeval timeout;

  timeout.tv_sec = 0;
  timeout.tv_usec = 10000;

  hostptr = gethostbyname(setting->hostnames[thread].c_str());
  if (hostptr==NULL) {
    cwarn << startl << "Failed to look up hostname " << setting->hostnames[thread] << " for thread " << thread << endl;
    return;
  }
  
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)(setting->threadports[thread])); 
  server.sin_addr.s_addr = ip_addr;
  
  /* Allocate the socket handle */
  cinfo << startl << "Connecting to " << inet_ntoa(server.sin_addr) << " for thread " << thread << endl;
  *socket_handles[thread] = socket(AF_INET, SOCK_STREAM, 0);
  if (*socket_handles[thread]==-1) {
    cerror << startl << "Failed to allocate socket for thread " << thread << endl;
    return;
  }

  /* Set the window size */
  status = setsockopt(*socket_handles[thread], SOL_SOCKET, SO_SNDBUF,
                      (char*) &(setting->threadtcpwins[thread]), sizeof(setting->threadtcpwins[thread]));
  if (status!=0) {
    close(*socket_handles[thread]);
    cerror << startl << "Error setting window size for thread " << thread << endl;
    return;
  }

  /* Get current flags */
  saveflags=fcntl(*socket_handles[thread],F_GETFL,0);
  if(saveflags<0) {
    cerror << startl << "Error getting flags for thread " << thread << endl;
    return;
  }

  /* Set non blocking */
  if(fcntl(*socket_handles[thread],F_SETFL,saveflags|O_NONBLOCK)<0) {
    cerror << startl << "Error setting flags to non-blocking for thread " << thread << endl;
    return;
  }

  // try to connect    
  status = connect(*socket_handles[thread], (struct sockaddr *) &server, sizeof(server));
  back_err=errno;

  /* restore flags */
  if(fcntl(*socket_handles[thread],F_SETFL,saveflags)<0) {
    cerror << startl << "Error restoring flags for thread " << thread << endl;
    return;
  }

  /* return unless the connection was successful or the connect is
           still in progress. */
  if(status<0) {
    if (back_err!=EINPROGRESS) {
      cerror << startl << "Error in connection for thread " << thread << endl;
      return;
    } 
    else {
      FD_ZERO(&fd_w);
      FD_SET(*socket_handles[thread],&fd_w);

      status = select(FD_SETSIZE,NULL,&fd_w,NULL,&timeout);
      if(status < 0) {
	cerror << startl << "Error in select for thread " << thread << endl;
	return;
      }

      /* 0 means it timed out & no fds changed */
      if(status==0) {
	close(*socket_handles[thread]);
	return;
      }

      /* Get the return code from the connect */
      socklen_t len=sizeof(getopt_status);
      status=getsockopt(*socket_handles[thread],SOL_SOCKET,SO_ERROR,&getopt_status,&len);
      if(status<0 || getopt_status != 0) {
	cerror << startl << "Error getting socket options for thread " << thread << endl;
	return;
      }
    }
  }

  return;
}

void VDIF_Server::sendToSocket(char * buffer, int thread, int sendbytes)
{
  int byteswrote, bytesremaining;
  char * ptr = buffer;
  int * socket_handle = socket_handles[thread];

  bytesremaining = sendbytes;
  while (bytesremaining>0) {
    byteswrote = send(*socket_handle, ptr, bytesremaining, 0);
    if(errno == EPIPE) {
      cwarn << startl << "Network seems to have dropped out for thread " << thread 
	    << ".  Will try to reconnect shortly...!" << endl;
      return; //note exit here
    }
    if (byteswrote==-1) {
      if (errno == EINTR) continue;
      cerror << startl << "Error writing to network for thread " << thread << endl;
      return; //note exit here
    } 
    else if (byteswrote==0) {
      cwarn << startl << "Warning: Did not write any bytes for thread " << thread << "!" << endl;
      return; //note exit here
    } 
    else {
      bytesremaining -= byteswrote;
      ptr += byteswrote;
    }
  }
}

void VDIF_Server::closeSockets()
{
  for(int i=0;i<maxthreads;i++) {
    if(*socket_handles[i] >= 0) {
      close(*socket_handles[i]);
      *socket_handles[i] = -1;
    }
  }
}
