/***************************************************************************
 *   Copyright (C) 2006-2020 by Adam Deller                                *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <mpi.h>
#include <iostream>
#include <cstdlib>
#include <unistd.h>
#include <signal.h>
#include "configuration.h"
#include "fxmanager.h"
#include "core.h"
#include "datastream.h"
#include "mk5.h"
#include "nativemk5.h"
#include "mark5bfile.h"
#include "mark5bmark5.h"
#include "vdiffile.h"
#include "vdifnetwork.h"
#include "vdiffake.h"
#ifdef HAVE_MARK6SG
#include "mark5bmark6_datastream.h"
#include "vdifmark6_datastream.h"
#endif
#include <sys/utsname.h>
//includes for socket stuff - for monitoring
#include "string.h"
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <difxmessage.h>
#include <unistd.h>
#include "alert.h"
#include <errno.h>

//act on an XML command message which was received
bool actOnCommand(Configuration * config, DifxMessageGeneric * difxmessage) {
  string paramname, paramvalue;

  //Only act on parameter setting commands
  //cout << "received a message" << endl;
  if (difxmessage->type == DIFX_MESSAGE_PARAMETER) {
    bool toMatches = true;
    DifxMessageParameter * pmessage = &((difxmessage->body).param);
    paramname = string(pmessage->paramName);
    paramvalue = string(pmessage->paramValue);
    cdebug << startl << "Received a parameter message for parameter " << paramname << " and value " << paramvalue << ", targetmpiid is " << pmessage->targetMpiId << endl;

    if(difxmessage->nTo > 0)
    {
      toMatches = false;
      for(int t = 0; t < difxmessage->nTo; ++t)
      {
        if(strcmp(difxmessage->to[t], getDifxMessageIdentifier()) == 0)
        {
          toMatches = true;
        }
      }
    }

    //is it for me
    if (toMatches && (
        (pmessage->targetMpiId == config->getMPIId()) || 
        (pmessage->targetMpiId == DIFX_MESSAGE_ALLMPIFXCORR) || 
        ((pmessage->targetMpiId == DIFX_MESSAGE_ALLCORES) && config->isCoreProcess()) ||
        ((pmessage->targetMpiId == DIFX_MESSAGE_ALLDATASTREAMS) && config->isDatastreamProcess()))) {
      //is it a shutdown message?
      if (paramname == "keepacting" && paramvalue == "false")
        return false;
      //otherwise set a config parameter if we know how
      if (paramname == "dumpsta")
        config->setDumpSTAState((paramvalue == "true") || (paramvalue == "True"));
      else if (paramname == "dumplta")
        config->setDumpLTAState((paramvalue == "true") || (paramvalue == "True"));
      else if (paramname == "dumpkurtosis")
        config->setDumpKurtosisState((paramvalue == "true") || (paramvalue == "True"));
      else if (paramname == "stachannels")
        config->setSTADumpChannels(atoi(pmessage->paramValue));
      else if (paramname == "ltachannels")
        config->setLTADumpChannels(atoi(pmessage->paramValue));
      else if (paramname == "clockupdate")
        config->updateClock(paramvalue);
      else if (paramname == "stopat")
      {
        config->setStopTimeUnixTime(atoll(pmessage->paramValue)); // expects unix time; datastreams result in no more data when their timestamps reach this number.
        cinfo << startl << config->getMPIId() << ": received message requesting stop at Unix data observe time " << paramvalue << ".  executeseconds changed to " << config->getExecuteSeconds() << endl;
      }
      else {
        cwarn << startl << config->getMPIId() << ": warning - received a parameter instruction regarding " <<  pmessage->paramName << " which cannot be honored and will be ignored!" << endl;
      }
    }
  }
  else if(difxmessage->type == DIFX_MESSAGE_STOP) {
    if(difxmessage->mpiId == 0 && config->getMPIId() == 0) {
      if(strcmp(difxmessage->identifier, getDifxMessageIdentifier()) == 0) {
         kill(getpid(), SIGINT);
      }
    }
  }
  return true;
}

//setup message receive thread
void * launchCommandMonitorThread(void * c) {
  Configuration * config = (Configuration*) c;
  int socket, bytesreceived;
  char message[DIFX_MESSAGE_LENGTH+1];
  char sendername[DIFX_MESSAGE_HOSTNAME_LENGTH+1];
  bool keepacting = true;
  DifxMessageGeneric genericmessage;

  socket = difxMessageReceiveOpen();
  cinfo << startl << "Receive socket opened; socket is " << socket << endl;
  if (socket < 0) {
    cwarn << startl << "Could not open command monitoring socket! Aborting message receive thread." << endl;
    config->setCommandThreadFailed();
  }
  else {
    config->setCommandThreadInitialised();
    //cinfo << startl << "Command thread initialised has been set" << endl;
    while (keepacting) {
      bytesreceived = difxMessageReceive(socket, message, DIFX_MESSAGE_LENGTH, sendername);
      if(bytesreceived > 0) {
        message[bytesreceived] = 0;
        difxMessageParse(&genericmessage, message);
        keepacting = actOnCommand(config, &genericmessage);
      }
    }
    if(socket >= 0) {
      difxMessageReceiveClose(socket);
    }
    //cinfo << startl << "Command monitor thread shutting down" << endl;
  }
  return 0;
}

//setup monitoring socket
int setup_net(const char *monhostname, int port, int window_size, int *sock) {
  int status;
  unsigned long ip_addr;
  struct hostent     *hostptr;
  struct linger      linger = {1, 1};
  struct sockaddr_in server;    /* Socket address */

  hostptr = gethostbyname(monhostname);
  if (hostptr==NULL) {
    cerror << startl << "Failed to look up monhostname " << monhostname << endl;
    return 1;
  }
  
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)port); 
  server.sin_addr.s_addr = ip_addr;
  
  cinfo << startl << "Connecting to " << inet_ntoa(server.sin_addr) << endl;
    
  *sock = socket(AF_INET, SOCK_STREAM, 0);
  if (*sock==-1) {
    perror("Failed to allocate socket");
    return(1);
  }

  /* Set the linger option so that if we need to send a message and
     close the socket, the message shouldn't get lost */
  status = setsockopt(*sock, SOL_SOCKET, SO_LINGER, (char *)&linger,
                      sizeof(struct linger)); 
  if (status!=0) {
    close(*sock);
    perror("Setting socket options");
    return(1);
  }

  /* Set the window size to TCP actually works */
  status = setsockopt(*sock, SOL_SOCKET, SO_SNDBUF,
                      (char *) &window_size, sizeof(window_size));
  if (status!=0) {
    close(*sock);
    perror("Setting socket options");
    return(1);
  }
  status = setsockopt(*sock, SOL_SOCKET, SO_RCVBUF,
                      (char *) &window_size, sizeof(window_size));
  if (status!=0) {
    close(*sock);
    perror("Setting socket options");
    return(1);
  }
    
  status = connect(*sock, (struct sockaddr *) &server, sizeof(server));
  if (status!=0) {
    perror("Failed to connect to server");
    return(1);
  }
  return(0);
} /* Setup Net */

static void generateIdentifier(const char *inputfile, char *identifier)
{
  int i, s=0;

  for(i = 0; inputfile[i]; i++)
  {
    if(inputfile[i] == '/')
    {
      s = i+1;
    }
  }

  if(inputfile[s] == 0)
  {
    s = 0;
  }

  for(i=0;i<DIFX_MESSAGE_PARAM_LENGTH-1;i++)
  {
    if(strcmp(&(inputfile[s+i]), ".input") == 0)
      break;
    identifier[i] = inputfile[s+i];
  }
  identifier[i] = 0;
  if(i == DIFX_MESSAGE_PARAM_LENGTH-1) //job name was too long!
  {
    cwarn << startl << "Job name was too long to serve as difxmessage identifier; identifier was truncated to " << identifier << endl;
  }
}

//main method - run by everyone
int main(int argc, char *argv[])
{
  MPI_Comm world, return_comm;
  int numprocs, myID, numdatastreams, numcores, perr, perc, prv = -1;
  void *prvp = &prv;
  double t1, t2;
  Configuration * config;
  FxManager * manager = 0;
  Core * core = 0;
  DataStream * stream = 0;
  int * coreids;
  int * datastreamids;
  bool monitor = false;
  bool nocommandthread = false;
  string monitoropt;
  pthread_t commandthread;
  //pthread_attr_t attr;
  char monhostname[512];
  int port=0, monitor_skip=0, namelen;
  double restartseconds = 0.0;
  char processor_name[MPI_MAX_PROCESSOR_NAME];
  char difxMessageID[DIFX_MESSAGE_PARAM_LENGTH];

  char myhostname[200];
  gethostname(myhostname, sizeof(myhostname)-1);
#if(ARCH == INTEL)
  ippSetNumThreads(1);
#endif
  cout << "About to run MPIInit on node " << myhostname << endl;

  MPI_Init(&argc, &argv);
  world = MPI_COMM_WORLD;
  MPI_Comm_size(world, &numprocs);
  MPI_Comm_rank(world, &myID);
  MPI_Comm_dup(world, &return_comm);
  MPI_Get_processor_name(processor_name, &namelen);

  if(argc < 2 || argc > 5)
  {
    cerr << "Error: invoke with mpifxcorr <inputfilename> [-M<monhostname>:port[:monitor_skip]] [-rNewStartSec] [--nocommandthread]" << endl;
    MPI_Barrier(world);
    MPI_Finalize();
    return EXIT_FAILURE;
  }

  //setup difxmessage
  generateIdentifier(argv[1], difxMessageID);
  difxMessageInit(myID, difxMessageID);
  difxMessageSetInputFilename(argv[1]);
  if(myID == 0)
  {
    if(isDifxMessageInUse() && !nocommandthread)
    {
      cout << "NOTE: difxmessage is in use.  If you are not running errormon/errormon2, you are missing all the (potentially important) info messages!" << endl;
    }
  }

  cinfo << startl << "MPI Process " << myID << " is running on host " << processor_name << endl;
 
  for(int i=2;i<argc;i++)
  {
    if(argv[i][0]=='-' && argv[i][1]=='M')
    {
      monitor = true;
      monitoropt = string(argv[i]);
      size_t colindex1 = monitoropt.find_first_of(':');
      size_t colindex2 = monitoropt.find_last_of(':');

      if(colindex2 == colindex1)
      {
        port = atoi(monitoropt.substr(colindex1 + 1).c_str());
        monitor_skip = 1;
      }
      else
      {
        port = atoi(monitoropt.substr(colindex1 + 1, colindex2-colindex1-1).c_str());
        monitor_skip = atoi(monitoropt.substr(colindex2 + 1).c_str());
      }
      strncpy(monhostname, monitoropt.substr(2,colindex1-2).c_str(), sizeof(monhostname)-1);
    }
    else if(argv[i][0]=='-' && argv[i][1]=='r')
    {
      restartseconds = atof(argv[i] + 2);
    }
    else if(strcmp(argv[i], "--nocommandthread") == 0)
    {
      nocommandthread = true;
    }
    else
    {
      cfatal << startl << "Invoke with mpifxcorr <inputfilename> [-M<monhostname>:port[:monitor_skip]] [-rNewStartSec] [--nocommandthread]" << endl;
      MPI_Barrier(world);
      MPI_Finalize();
      return EXIT_FAILURE;
    }
  }

  cinfo << startl << "MPI Process " << myID << " is about to process input file" << endl;

  cverbose << startl << "About to process the input file..." << endl;
  //process the input file to get all the info we need
  config = new Configuration(argv[1], myID, world, restartseconds);
  if(!config->consistencyOK())
  {
    //There was a problem with the input file, so shut down gracefully
    cfatal << startl << "Config encountered inconsistent setup in config file - aborting correlation" << endl;
    MPI_Barrier(world);
    //MPI_Finalize(); //does not always shut down, so just abort
    MPI_Abort(MPI_COMM_WORLD, 1);
    return EXIT_FAILURE;
  }

  //handle difxmessage setup for sending and receiving
  if (isDifxMessageInUse() && !nocommandthread) { 
    // CJP - PTHREAD_CREATE_JOINABLE is the default
    //pthread_attr_init(&attr);
    //pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    //perr = pthread_create(&commandthread, &attr, launchCommandMonitorThread, (void *)(config));
    perr = pthread_create(&commandthread, NULL, launchCommandMonitorThread, (void *)(config));
    //pthread_attr_destroy(&attr);

    if (perr != 0)
      csevere << startl << "Error creating command monitoring thread!" << endl;
    else {
      //wait for commandmonthread to be initialised
      while(!config->commandThreadInitialised() && !config->commandThreadFailed()) {
	usleep(1);
      }
    }
  } else if (myID==0) { // Only warn on fxmanager
    cout << "NOTE: commandThread not launched as DIFXmessages not enabled. Some functionality will not be available" << endl;
  }
  numdatastreams = config->getNumDataStreams();
  numcores = numprocs - (fxcorr::FIRSTTELESCOPEID + numdatastreams);
  if(numcores < 1)
  {
    cfatal << startl << "Must be invoked with at least " << fxcorr::FIRSTTELESCOPEID + numdatastreams + 1 << " processors (was invoked with " << numprocs << " processors) - aborting!" << endl;
    MPI_Barrier(world);
    MPI_Finalize();
    return EXIT_FAILURE;
  }

  //create the ID arrays
  coreids = new int[numcores];
  datastreamids = new int[numdatastreams];
  for(int i=0;i<numcores;i++)
    coreids[i] = fxcorr::FIRSTTELESCOPEID + numdatastreams + i;

  for(int i=0;i<numdatastreams;i++)
    datastreamids[i] = fxcorr::FIRSTTELESCOPEID + i;

  //wait until everyone has caught up
  MPI_Barrier(world);
  /* 2-Nov-2016 CJP: MPI::Exception is not defined in openmpi on my Mac - C++ bindings may have been removed
                     from openmpi. This may affect others on Linux when then upgrade to newer openmpi libraries.

		     http://stackoverflow.com/questions/38680530/use-of-undeclared-identifier-mpi-when-using-c-syntax-for-openmpi-on-macos
  */
  //  try
  {
    //work out what process we are and run accordingly
    if(myID == fxcorr::MANAGERID) //im the manager
    {
      manager = new FxManager(config, numcores, datastreamids, coreids, myID, return_comm, monitor, monhostname, port, monitor_skip);
      MPI_Barrier(world);
      t1 = MPI_Wtime();
      cinfo << startl << "Estimated memory usage by FXManager: " << manager->getEstimatedBytes()/1048576.0 << " MB" << endl;
      manager->execute();
      t2 = MPI_Wtime();
      cinfo << startl << "Total wallclock time was **" << t2 - t1 << "** seconds" << endl;
    }
    else if (myID >= fxcorr::FIRSTTELESCOPEID && myID < fxcorr::FIRSTTELESCOPEID + numdatastreams) //im a datastream
    {
      int datastreamnum = myID - fxcorr::FIRSTTELESCOPEID;
      if(config->isVDIFFile(datastreamnum)) {
        stream = new VDIFDataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening VDIFDataStream" << endl;
#ifdef HAVE_MARK6SG
      } else if(config->isVDIFMark6(datastreamnum)) {
        stream = new VDIFMark6DataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening VDIFMark6DataStream" << endl;
      } else if(config->isMark5BMark6(datastreamnum)) {
        stream = new Mark5BMark6DataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening Mark5BMark6DataStream" << endl;
#endif
      } else if(config->isVDIFNetwork(datastreamnum)) {
        stream = new VDIFNetworkDataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening VDIFNetworkDataStream" << endl;
      } else if(config->isVDIFFake(datastreamnum)) {
        stream = new VDIFFakeDataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening VDIFFakeDataStream" << endl;
      } else if(config->isMark5BFile(datastreamnum)) {
        stream = new Mark5BDataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
          cverbose << startl << "Opening Mark5BDataStream" << endl;
      } else if(config->isMark5BMark5(datastreamnum)) {
        stream = new Mark5BMark5DataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening Mark5BMark5DataStream" << endl;
      } else if(config->isMkV(datastreamnum)) {
        stream = new Mk5DataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
        cverbose << startl << "Opening Mk5DataStream" << endl;
      } else if(config->isNativeMkV(datastreamnum)){
        stream = new NativeMk5DataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
          cverbose << startl << "Opening NativeMk5DataStream" << endl;
      }
      else {
        stream = new DataStream(config, datastreamnum, myID, numcores, coreids, config->getDDataBufferFactor(), config->getDNumDataSegments());
    cverbose << startl << "Opening generic DataStream" << endl;
    }

      stream->initialise();
      MPI_Barrier(world);
      cinfo << startl << "Estimated memory usage by Datastream: " << stream->getEstimatedBytes()/1048576.0 << " MB" << endl;
      stream->execute();
    }
    else //im a processing core
    {
      core = new Core(myID, config, datastreamids, return_comm);
      MPI_Barrier(world);
      cinfo << startl << "Estimated memory usage by Core: " << core->getEstimatedBytes()/1048576.0 << " MB" << endl;
      core->execute();
    }
    MPI_Barrier(world);
  }

  //  /* See comment about MPI bindings above */
  //catch (MPI::Exception e)
  //{
  //  cerror << startl << "Caught an exception!!! " << e.Get_error_string() << endl;
  //  return EXIT_FAILURE;
  //}

  MPI_Finalize();

  if (isDifxMessageInUse() && !nocommandthread) {
    if(myID == 0) difxMessageSendDifxParameter("keepacting", "false", DIFX_MESSAGE_ALLMPIFXCORR);
    //sleep(1); // Give threads a chance to quit
    perc = pthread_cancel(commandthread);
    if(perc != 0 && perc != ESRCH) csevere << startl << "Error(" << perc << ") in cancelling commandthread!!!" << endl;
    perr = pthread_join(commandthread, &prvp);
    if(perr != 0) csevere << startl << "Error(" << perr << ") in closing commandthread!!!" << endl;
    if(prvp == PTHREAD_CANCELED) cverbose << startl << "Command thread cancelled" << endl;
    else if (perc == ESRCH) cverbose << startl << "Command thread died by cancellation" << endl;
    else cverbose << startl << "Command thread return value " << prv << endl;
  }

  delete [] coreids;
  delete [] datastreamids;

  if(manager) delete manager;
  if(stream) delete stream;
  if(core) delete core;

  //delete config;  	// FIXME!!! Revisit this commented out destructor sometime.
  			// It is currently commented out to prevent hang on exit

  cinfo << startl << "MPI ID " << myID << " says BYE!" << endl;
  return EXIT_SUCCESS;
}
// vim: shiftwidth=2:softtabstop=2:expandtab
