#include <string>
#include <fstream>
#include <pthread.h>
#include "difxmessage.h"
#include "architecture.h"
#include "configuration.h"

//Constants
#define DEFAULT_CHAN_STRING "32"
static const u32 SYNC = MAX_U32;
static const int BUFFER_LENGTH = 16384;

//Prototypes
void writeDiFXHeader(ofstream * output, int dsindex, int scan, int sec, 
			int ns, int nswidth, int bandindex, int nchan, 
			int coreindex, int threadindex);
void * launchCommandMonitorThread(void * c);
void * launchWriteThread(void * nothing);
bool actOnCommand(Configuration * config, DifxMessageGeneric * difxmessage);

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
DifxMessageSTARecord * starecords[BUFFER_LENGTH];
pthread_mutex_t locks[BUFFER_LENGTH];
Configuration * config;
