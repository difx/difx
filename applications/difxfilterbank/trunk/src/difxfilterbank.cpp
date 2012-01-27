#include <pthread.h>
#include <string.h>
#include <string>
#include <fstream>
#include <unistd.h>
#include "architecture.h"
#include "configuration.h"
#include "difxmessage.h"
#include "difxfilterbank.h"

// Variables
int mainsocket, binarysocket, numchans;
int binarymsglength, atsegment;
string configfile, filterbankoutputfile, kurtosisoutputfile, identifier, jobname, numchannelsstring;
bool dofilterbank, dokurtosis, keepwriting, writethreadinitialised;
ofstream fboutput, ktoutput;
pthread_t commandthread;
pthread_t writethread;
pthread_cond_t writecond;
char sendername[DIFX_MESSAGE_PARAM_LENGTH];
char *starecords[BUFFER_LENGTH];
int numrecords[BUFFER_LENGTH];
pthread_mutex_t locks[BUFFER_LENGTH];
Configuration * config;
int debug=0;

int main(int argc, const char * argv[])
{
  DifxMessageSTARecord *record;
  int perr, mtu;
  char * difxmtu = getenv("DIFX_MTU");
  if(difxmtu == 0)
    mtu = 1500;
  else
    mtu = atoi(difxmtu);
  if (mtu > 9000) {
    cerr << "DIFX_MTU was set to " << mtu << " - resetting to 9000 bytes (max)" << endl;
    mtu = 9000;
  }

  //check for correct invocation
  if(argc != 4 && argc != 5) {
    cout << "Error - invoke with difxfilterbank <config file> <filterbank output file> <kurtosis output file> [num channels]" << endl;
    cout << "        Set either <output file> to \"none\" if that output is not desired" << endl;
    return EXIT_FAILURE;
  }

  //fill in some variables
  configfile = argv[1];
  filterbankoutputfile = argv[2];
  kurtosisoutputfile = argv[3];
  dofilterbank = true;
  dokurtosis = true;
  if(filterbankoutputfile.compare("none") == 0)
    dofilterbank = false;
  if(kurtosisoutputfile.compare("none") == 0)
    dokurtosis = false;
  if (argc == 5)
    numchannelsstring = argv[4];
  else
    numchannelsstring = DEFAULT_CHAN_STRING;
  keepwriting = true;
  numchans = atoi(numchannelsstring.c_str());
  binarymsglength = sizeof(DifxMessageSTARecord) + numchans*sizeof(f32);
  cout << "Binary message length is "<< binarymsglength << " bytes for " << numchans << " channels" << endl;
  cout << "About to allocate " << mtu<< " bytes in each of "<<BUFFER_LENGTH<<" receive buffers" << endl;
  for(int i=0;i<BUFFER_LENGTH;i++) {
    perr = pthread_mutex_init(&(locks[i]), NULL);
    if(perr != 0)
      cerr << "Problem initialising lock " << i << endl;
    starecords[i] = (char *)calloc(1,mtu);
    numrecords[i] = 0;
  }

  //lock the first slot, and fire up the write thread
  writethreadinitialised = false;
  pthread_cond_init(&writecond, NULL);
  perr = pthread_mutex_lock(&(locks[0]));
  if(perr != 0)
    cerr << "Problem locking the first lock" << endl;
  perr = pthread_mutex_lock(&(locks[1]));
  if(perr != 0)
    cerr << "Problem locking the second lock" << endl;
  perr = pthread_create(&writethread, NULL, launchWriteThread, NULL);
  if(perr != 0)
    cerr << "Error in launching writethread !!!" << endl;
  while(!writethreadinitialised) {
    perr = pthread_cond_wait(&writecond, &(locks[1]));
    if (perr != 0)
      cerr << "Error waiting on writethreadinitialised condition!!!!" << endl;
  }
  perr = pthread_mutex_unlock(&(locks[1]));
  if(perr != 0)
    cerr << "Problem unlocking the second lock" << endl;

  //parse the config file, get the job name and identifier
  config = new Configuration(configfile.c_str(), 0);
  jobname = config->getJobName().substr(0, DIFX_MESSAGE_PARAM_LENGTH-8);
  identifier = "difxfb." + jobname;

  //initialise message infrastructure and wait for a status message 
  //indicating correlation is ready to receive messages, and open outfile
  difxMessageInit(0, identifier.c_str());
  difxMessageInitBinary();
  if(dofilterbank)
    fboutput.open(filterbankoutputfile.c_str(), ios::binary|ios::trunc);
  if(dokurtosis)
    ktoutput.open(kurtosisoutputfile.c_str(), ios::binary|ios::trunc);
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
  if(dofilterbank)
    difxMessageSendDifxParameter("dumpsta", "true", DIFX_MESSAGE_ALLCORES);
  if(dokurtosis)
    difxMessageSendDifxParameter("dumpkurtosis", "true", DIFX_MESSAGE_ALLCORES);
  difxMessageSendDifxParameter("stachannels", numchannelsstring.c_str(), 
			       DIFX_MESSAGE_ALLCORES);

  atsegment = 0;
  while(keepwriting) {
    //get a binary message
    int nbytes = difxMessageBinaryRecv(binarysocket, starecords[atsegment], mtu, sendername);
    if (nbytes > mtu) {
      cerr<<"Error: read a message of len "<<nbytes<<" bytes, but can only handle "<<mtu<<endl;
    }
    // timeout? just try again...
    if (nbytes <=0) continue;

    if (debug) {
      cerr << "Reader: at segmenmt "<<atsegment<<endl;
    }
    record = (DifxMessageSTARecord *)(starecords[atsegment]);
    fprintDifxMessageSTARecord(stderr,record,0);

    //check it belongs to us
    if(strcmp(record->identifier, jobname.c_str()) == 0) {
      numrecords[atsegment] = nbytes / binarymsglength;
      if (debug) {
        cerr << "Reader: Got a message with id "<< record->identifier <<endl;
        cerr << "Reader: There are "<<numrecords[atsegment]<< " records in this buffer"<<endl;
      }
      if (nbytes % binarymsglength != 0) {
          cerr << "Error: received msg length "<<nbytes<<" is not integer multiple of binarymsglength "<<binarymsglength<<endl;
      }
      perr = pthread_mutex_lock(&(locks[(atsegment+1)%BUFFER_LENGTH]));
      if(perr != 0)
          cerr << "Main thread problem locking " << (atsegment+1)%BUFFER_LENGTH << endl;
      atsegment = (atsegment+1)%BUFFER_LENGTH;
      perr = pthread_mutex_unlock(&(locks[(atsegment+BUFFER_LENGTH-1)%BUFFER_LENGTH]));
      if(perr != 0)
          cerr << "Main thread problem unlocking " << (atsegment+BUFFER_LENGTH-1)%BUFFER_LENGTH << endl;
    }
  }

  //unlock the last lock
  perr = pthread_mutex_unlock(&(locks[atsegment]));
  if(perr != 0)
    cerr << "Main thread problem unlocking " << atsegment << endl;

  //close the sockets and output file
  difxMessageReceiveClose(mainsocket);
  difxMessageBinaryClose(binarysocket);
  perr = pthread_join(writethread, NULL);
  if(perr != 0) cerr << "Error in closing writethread!!!" << endl;
  for(int i=0;i<BUFFER_LENGTH;i++)
    free(starecords[i]);
  perr = pthread_join(commandthread, NULL);
  if(perr != 0) cerr << "Error in closing commandthread!!!" << endl;

  return EXIT_SUCCESS;
}

//writes a header before the filterbank data
void writeDiFXHeader(ofstream * output, int dsindex, int scan, int sec, int ns, int nswidth,  
		    int bandindex, int nchan, int coreindex, int threadindex)
{
  int data[10];

  data[0] = SYNC;
  data[1] = dsindex;
  data[2] = scan;
  data[3] = sec;
  data[4] = ns;
  data[5] = nswidth;
  data[6] = bandindex;
  data[7] = nchan;
  data[8] = coreindex;
  data[9] = threadindex;

  output->write((char *)data, sizeof(data));

}

//setup write thread
void * launchWriteThread(void * nothing) {
  int perr, writesegment, nextsegment;
  ofstream * activeoutput;
  DifxMessageSTARecord * starecord;

  perr = pthread_mutex_lock(&(locks[BUFFER_LENGTH-1]));
  if(perr != 0)
    cerr << "Error in write thread trying to lock last lock!" << endl;
  writethreadinitialised = true;
  perr = pthread_cond_signal(&writecond);
  if(perr != 0)
    cerr << "Error in write thread signalling main thread to wake up!" << endl;

  writesegment = BUFFER_LENGTH-1;
  nextsegment = 0;
  while(keepwriting || nextsegment != atsegment) {
    perr = pthread_mutex_lock(&(locks[nextsegment]));
    if(perr != 0)
      cerr << "Error in writethread trying to lock segment " << nextsegment << endl;
    perr = pthread_mutex_unlock(&(locks[writesegment]));
    if(perr != 0)
      cerr << "Error in writethread trying to unlock segment " << writesegment << endl;
    writesegment = nextsegment;
    nextsegment = (writesegment+1)%BUFFER_LENGTH;
    activeoutput = 0;
    starecord = (DifxMessageSTARecord *)(starecords[writesegment]);
    if(dofilterbank && starecord->messageType == STA_AUTOCORRELATION)
      activeoutput = &fboutput;
    if(dokurtosis && starecord->messageType == STA_KURTOSIS)
      activeoutput = &ktoutput;
    if(activeoutput != 0)
    {
      for(int i=0;i<numrecords[writesegment];i++) {
        starecord = (DifxMessageSTARecord *)(starecords[writesegment] + i*binarymsglength);
        if (debug) {
          cerr<<"Writer: in buf index "<<writesegment<<". Writing record "<< i<< " of "<<numrecords[writesegment]<<" records in the buf"<<endl;
        }

        if (starecord->nChan <=0) {
          cerr<<"Writer error: buf index "<<writesegment<< ". record thinks there are "<<starecord->nChan <<" channels. Printing STA header and exiting"<<endl;
          fprintDifxMessageSTARecord(stdout,starecord,0);
          keepwriting=false;
          return 0;
        }

        //write it out (main thread already filtered out those not belonging to this job)
        writeDiFXHeader(activeoutput, starecord->dsindex, starecord->scan, 
                        starecord->sec, starecord->ns, starecord->nswidth, starecord->bandindex,
                        starecord->nChan, starecord->coreindex, starecord->threadindex);
        activeoutput->write((char*)starecord->data, starecord->nChan*sizeof(float));
      }
    }
  }

  if(dofilterbank)
    fboutput.close();
  if(dokurtosis)
    ktoutput.close();
  cout << "Write thread exiting!" << endl;

  return 0;
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
  //Make sure message is for our correlation
  if (strcmp(difxmessage->identifier, jobname.c_str()) == 0){
    //Check parameter setting messages for shutdown
    if(difxmessage->type == DIFX_MESSAGE_PARAMETER) {
      DifxMessageParameter * pmessage = &((difxmessage->body).param);
      //And only act on messages that went to everyone (since we are only checking for shutdown)
      if (pmessage->targetMpiId == DIFX_MESSAGE_ALLMPIFXCORR){
        //is it a shutdown message? If not, do nothing
        if (string(pmessage->paramName) == "keepacting" && string(pmessage->paramValue) == "false") {
          cout << "Correlation has ended - shutting down difxfilterbank" << endl;
	  keepwriting = false;
          return false;
        }
      }
    }
    else if (difxmessage->type == DIFX_MESSAGE_STATUS) {
      DifxMessageStatus * smessage = &((difxmessage->body).status);
      if (smessage->state == DIFX_STATE_RUNNING || smessage->state == DIFX_STATE_STARTING) {
        if(!config->commandThreadInitialised())
          cout << "Correlation has started - commencing filterbank operation" << endl;
        config->setCommandThreadInitialised();
      } 
    }
  }
  return true;
}
