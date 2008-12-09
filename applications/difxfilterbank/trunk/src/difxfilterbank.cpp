#include "difxfilterbank.h"

int main(int argc, const char * argv[])
{
  //check for correct invocation
  if(argc != 3 && argc != 4) {
    cout << "Error - invoke with difxfilterbank <config file> <output file> [num channels]" << endl;
    return EXIT_FAILURE;
  }
  configfile = argv[1];
  outputfile = argv[2];
  if (argc == 4)
    numchannelsstring = argv[3];
  else
    numchannelsstring = DEFAULT_CHAN_STRING;
  jobnamestart = configfile.find_last_of('/');
  if (jobnamestart == string::npos) 
    jobnamestart = 0;
  jobnameend = configfile.find_first_of('.', jobnamestart);
  jobname = configfile.substr(jobnamestart,jobnameend-jobnamestart);
  identifier = string(argv[0]) + "." + jobname;
  keepwriting = true;
  numchans = atoi(numchannelsstring.c_str());
  binarymsglength = sizeof(DifxMessageSTARecord) + numchans*sizeof(f32);
  starecord = (DifxMessageSTARecord*)malloc(binarymsglength);

  //parse the config file
  config = new Configuration(configfile.c_str(), 0);

  //initialise message infrastructure and wait for a status message 
  //indicating correlation is ready to receive messages, and open outfile
  difxMessageInit(0, identifier.c_str());
  difxMessageInitBinary();
  output.open(outputfile.c_str(), ios::binary|ios::trunc);
  mainsocket = difxMessageReceiveOpen();
  binarysocket = difxMessageBinaryOpen(BINARY_STA);
  if (mainsocket < 0 || binarysocket < 0) {
    cout << "Could not open either mainsocket (" << mainsocket << ") or binarysocket (" << binarysocket << ") - aborting!!!" << endl;
    return EXIT_FAILURE;
  }
  perr = pthread_create(&commandthread, NULL, launchCommandMonitorThread, (void *)(config));
  if (perr != 0) {
    cout << "STA catching process could not launch command thread! Aborting" << endl;
    return EXIT_FAILURE;
  }
  while(!config->commandThreadInitialised())
    usleep(10);

  //send the message to start the STA dumping
  difxMessageSendDifxParameter("dumpsta", "true", DIFX_MESSAGE_ALLCORES);
  difxMessageSendDifxParameter("stachannels", numchannelsstring.c_str(), 
			       DIFX_MESSAGE_ALLCORES);

  while(keepwriting) {
    //get a binary message
    int nbytes = difxMessageBinaryRecv(binarysocket, (char*)starecord, binarymsglength, 
				       sendername);


    //check it belongs to us (presumably look at some kind of identifier)

    //write it to the file
    if (nbytes <= 0) {
      cout << "Some problem with the binary message - ignoring!" << endl;
    }
    else {
      writeDiFXHeader(&output, starecord->antId, starecord->sec, starecord->ns, 
		      starecord->threadId, starecord->bandId, starecord->nChan);
      output.write((char*)starecord->data, starecord->nChan*sizeof(float));
    }
  }

  //close the sockets and output file
  difxMessageReceiveClose(mainsocket);
  difxMessageBinaryClose(binarysocket);
  output.close();
  free(starecord);
  perr = pthread_join(commandthread, NULL);
  if(perr != 0) cerr << "Error in closing commandthread!!!" << endl;

  return EXIT_SUCCESS;
}

//writes a header before the filterbank data
void writeDiFXHeader(ofstream * output, int antId, int sec, int ns, 
		    int threadId, int bandId, int nchan)
{
  output->write((char*)&SYNC, sizeof(int));
  output->write((char*)&antId, sizeof(int));
  output->write((char*)&sec, sizeof(int));
  output->write((char*)&ns, sizeof(int));
  output->write((char*)&threadId, sizeof(int));
  output->write((char*)&bandId, sizeof(int));
  output->write((char*)&nchan, sizeof(int));
}

//setup message receive thread
void * launchCommandMonitorThread(void * c) {
  Configuration * config = (Configuration*) c;
  int socket, bytesreceived = 1;
  char message[DIFX_MESSAGE_LENGTH];
  char csendername[DIFX_MESSAGE_PARAM_LENGTH];
  bool keepacting = true;
  DifxMessageGeneric * genericmessage = (DifxMessageGeneric *)malloc(sizeof(DifxMessageGeneric));

  socket = difxMessageReceiveOpen();
  if (socket < 0) {
    cerr << "Could not open command monitoring socket! Aborting message receive thread." << endl;
    keepacting = false;
  }
  while (keepacting) {
    bytesreceived = difxMessageReceive(socket, message, DIFX_MESSAGE_LENGTH, csendername);
    if(bytesreceived > 0) {
      difxMessageParse(genericmessage, message);
      //Should also check if the message is from our correlation
      keepacting = actOnCommand(config, genericmessage);
    }
    //else {
    //  cerr << "Problem receiving message! Aborting message receive thread." << endl;
    //  keepacting = false;
    //}
  }
  free(genericmessage);
  if(socket >= 0)
    difxMessageReceiveClose(socket);
  cout << "Command monitor thread shutting down" << endl;
  return 0;
}

//act on an XML command message which was received
bool actOnCommand(Configuration * config, DifxMessageGeneric * difxmessage) {
  //Check parameter setting messages for shutdown
  if (difxmessage->type == DIFX_MESSAGE_PARAMETER) {
    DifxMessageParameter * pmessage = &((difxmessage->body).param);
    cout << "Received a parameter message! Paramname was " << pmessage->paramName << ", paramvalue was " << pmessage->paramValue << endl;
    //And only act on messages that went to everyone (since we are only checking for shutdown)
    if (pmessage->targetMpiId == DIFX_MESSAGE_ALLMPIFXCORR){
      //is it a shutdown message? If not, do nothing
      if (string(pmessage->paramName) == "keepacting" && string(pmessage->paramValue) == "false") {
	keepwriting = false;
        return false;
      }
    }
  }
  else if (difxmessage->type == DIFX_MESSAGE_STATUS) {
    DifxMessageStatus * smessage = &((difxmessage->body).status);
    cout << "Received a status message! State was " << smessage->state << ", we are looking for " << DIFX_STATE_RUNNING << " or " << DIFX_STATE_STARTING << endl;
    if (smessage->state == DIFX_STATE_RUNNING || 
	smessage->state == DIFX_STATE_STARTING)
      config->setCommandThreadInitialised();
  }
  return true;
}
