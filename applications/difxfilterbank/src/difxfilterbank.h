//Constants
#define DEFAULT_CHAN_STRING "32"
static const u32 SYNC = MAX_U32;
static const int BUFFER_LENGTH = 1024;

//Prototypes
void writeDiFXHeader(ofstream * output, int dsindex, int scan, int sec, 
			int ns, int nswidth, int bandindex, int nchan, 
			int coreindex, int threadindex);
void * launchCommandMonitorThread(void * c);
void * launchWriteThread(void * nothing);
bool actOnCommand(Configuration * config, DifxMessageGeneric * difxmessage);

