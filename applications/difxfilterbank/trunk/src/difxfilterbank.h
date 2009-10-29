#include <string>
#include <fstream>
#include <pthread.h>
#include "difxmessage.h"
#include "architecture.h"
#include "configuration.h"

//Constants
#define DEFAULT_CHAN_STRING "32"
static const u32 SYNC = MAX_U32;

//Prototypes
void writeDiFXHeader(ofstream * output, int dsindex, int scan, int sec, 
			int ns, int nswidth, int bandindex, int nchan, 
			int coreindex, int threadindex);
void * launchCommandMonitorThread(void * c);
bool actOnCommand(Configuration * config, DifxMessageGeneric * difxmessage);

// Variables
int mainsocket, binarysocket, numchans;
int binarymsglength, perr;
string configfile, outputfile, identifier, jobname, numchannelsstring;
bool keepwriting;
ofstream output;
pthread_t commandthread;
char sendername[DIFX_MESSAGE_PARAM_LENGTH];
DifxMessageSTARecord * starecord;
Configuration * config;
