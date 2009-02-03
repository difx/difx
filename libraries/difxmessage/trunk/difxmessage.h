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
#ifndef __DIFX_MESSAGE_H__
#define __DIFX_MESSAGE_H__

#ifdef __cplusplus
extern "C" {
#endif

#define DIFX_MESSAGE_IDENTIFER_LENGTH	128
#define DIFX_MESSAGE_PARAM_LENGTH	32
#define DIFX_MESSAGE_FILENAME_LENGTH	128
#define DIFX_MESSAGE_MAX_TARGETS	128
#define DIFX_MESSAGE_LENGTH		1500
#define DIFX_MESSAGE_MAX_INDEX		8
#define DIFX_MESSAGE_MAX_DATASTREAMS	50
#define DIFX_MESSAGE_MAX_CORES		100
#define DIFX_MESSAGE_MAX_SCANNAME_LEN	64
#define DIFX_MESSAGE_MAX_ENV		8
#define DIFX_MESSAGE_ALLMPIFXCORR	-1
#define DIFX_MESSAGE_ALLCORES		-2
#define DIFX_MESSAGE_ALLDATASTREAMS	-3

/**** LOW LEVEL MULTICAST FUNCTIONS ****/

/* single function for send.  returns message length on success, or -1 */

int MulticastSend(const char *group, int port, const char *message, int length);

/* functions for receive */

int openMultiCastSocket(const char *group, int port);
int closeMultiCastSocket(int sock);
int MultiCastReceive(int sock, char *message, int maxlen, char *from);


/**** DIFX SPECIFIC FUNCTIONS AND DATA TYPES ****/

/* Types of binary data to be sent */
enum BinaryDataChannels
{
	BINARY_STA,
	BINARY_LTA
};

/* Note! Keep this in sync with Mk5StateStrings[][24] in difxmessage.c */
enum Mk5State
{
	MARK5_STATE_OPENING = 0,
	MARK5_STATE_OPEN, 
	MARK5_STATE_CLOSE, 
	MARK5_STATE_GETDIR, 
	MARK5_STATE_GOTDIR, 
	MARK5_STATE_PLAY, 
	MARK5_STATE_IDLE, 
	MARK5_STATE_ERROR,
	MARK5_STATE_BUSY,
	MARK5_STATE_INITIALIZING,
	MARK5_STATE_RESETTING,
	MARK5_STATE_REBOOTING,
	MARK5_STATE_POWEROFF,
	MARK5_STATE_NODATA,
	MARK5_STATE_NOMOREDATA,
	MARK5_STATE_PLAYINVALID,
	MARK5_STATE_START,
	MARK5_STATE_COPY,
	MARK5_STATE_CONDITION,
	MARK5_STATE_COND_ERROR,
	NUM_MARK5_STATES	/* this needs to be the last line of enum */
};

extern const char Mk5StateStrings[][24];

/* Note! Keep this in sync with DifxStateStrings[][24] in difxmessage.c */
enum DifxState
{
	DIFX_STATE_SPAWNING = 0,/* Issued by mpirun wrapper */
	DIFX_STATE_STARTING,	/* fxmanager just started */
	DIFX_STATE_RUNNING,	/* Accompanied by visibility info */
	DIFX_STATE_ENDING,	/* Normal end of job */
	DIFX_STATE_DONE,	/* Normal end to DiFX */
	DIFX_STATE_ABORTING,	/* Unplanned early end due to runtime error */
	DIFX_STATE_TERMINATING,	/* Caught SIGINT, closing down */
	DIFX_STATE_TERMINATED,	/* Finished cleaning up adter SIGINT */
	DIFX_STATE_MPIDONE,	/* mpi process has finished */
	NUM_DIFX_STATES		/* this needs to be the last line of enum */
};

extern const char DifxStateStrings[][24];

/* Note! Keep this in sync with DifxMessageAlertString[][16] in difxmessage.c */
enum DifxAlertLevel
{
	DIFX_ALERT_LEVEL_FATAL = 0,
	DIFX_ALERT_LEVEL_SEVERE,
	DIFX_ALERT_LEVEL_ERROR,
	DIFX_ALERT_LEVEL_WARNING,
	DIFX_ALERT_LEVEL_INFO,
	DIFX_ALERT_LEVEL_VERBOSE,
	DIFX_ALERT_LEVEL_DEBUG
};

extern const char difxMessageAlertString[][16];

/* Note! Keep this in sync with DifxMessageTypeStrings[][24] in difxmessage.c */
enum DifxMessageType
{
	DIFX_MESSAGE_UNKNOWN = 0,
	DIFX_MESSAGE_LOAD,
	DIFX_MESSAGE_ALERT,
	DIFX_MESSAGE_MARK5STATUS,
	DIFX_MESSAGE_STATUS,
	DIFX_MESSAGE_INFO,
	DIFX_MESSAGE_DATASTREAM,
	DIFX_MESSAGE_COMMAND,
	DIFX_MESSAGE_PARAMETER,
	DIFX_MESSAGE_START,
	DIFX_MESSAGE_STOP,
	DIFX_MESSAGE_MARK5VERSION,
	NUM_DIFX_MESSAGE_TYPES	/* this needs to be the last line of enum */
};

extern const char DifxMessageTypeStrings[][24];

typedef struct
{
	enum Mk5State state;
	char vsnA[10];
	char vsnB[10];
	unsigned int status;
	char activeBank;
	int scanNumber;
	char scanName[DIFX_MESSAGE_MAX_SCANNAME_LEN];
	long long position;	/* play pointer */
	float rate;		/* Mbps */
	double dataMJD;
} DifxMessageMk5Status;

typedef struct
{
	char ApiVersion[8];
	char ApiDateCode[12];
	char FirmwareVersion[8];
	char FirmDateCode[12];
	char MonitorVersion[8];
	char XbarVersion[8];
	char AtaVersion[8];
	char UAtaVersion[8];
	char DriverVersion[8];
} DifxMessageMk5Version;

typedef struct
{
	
} DifxMessageDatastream;

typedef struct
{
	float cpuLoad;
	int totalMemory;
	int usedMemory;
	unsigned int netRXRate;		/* Bytes per second */
	unsigned int netTXRate;		/* Bytes per second */
} DifxMessageLoad;

typedef struct
{
	char message[DIFX_MESSAGE_LENGTH];
	int severity;
} DifxMessageAlert;

typedef struct
{
	enum DifxState state;
	char message[DIFX_MESSAGE_LENGTH];
	double mjd;
	float weight[DIFX_MESSAGE_MAX_DATASTREAMS];
	int maxDS;
} DifxMessageStatus;

typedef struct
{
	char message[DIFX_MESSAGE_LENGTH];
} DifxMessageInfo;

typedef struct
{
	char command[DIFX_MESSAGE_LENGTH];
} DifxMessageCommand;

typedef struct
{
	int targetMpiId;
	char paramName[DIFX_MESSAGE_PARAM_LENGTH];
	int nIndex;
	int paramIndex[DIFX_MESSAGE_MAX_INDEX];
	char paramValue[DIFX_MESSAGE_LENGTH];
} DifxMessageParameter;

typedef struct
{
	int nDatastream, nProcess, nEnv;
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char headNode[DIFX_MESSAGE_PARAM_LENGTH];
	char datastreamNode[DIFX_MESSAGE_MAX_DATASTREAMS][DIFX_MESSAGE_PARAM_LENGTH];
	char processNode[DIFX_MESSAGE_MAX_CORES][DIFX_MESSAGE_PARAM_LENGTH];
	char envVar[DIFX_MESSAGE_MAX_ENV][DIFX_MESSAGE_FILENAME_LENGTH];
	int nThread[DIFX_MESSAGE_MAX_CORES];
	char mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH];
	char mpiOptions[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxProgram[DIFX_MESSAGE_FILENAME_LENGTH];
} DifxMessageStart;

typedef struct
{
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
} DifxMessageStop;

typedef struct
{
	enum DifxMessageType type;
	char from[DIFX_MESSAGE_PARAM_LENGTH];
	char to[DIFX_MESSAGE_MAX_TARGETS][DIFX_MESSAGE_PARAM_LENGTH];
	int nTo;
	char identifier[DIFX_MESSAGE_PARAM_LENGTH];
	int mpiId;
	int seqNumber;
	/* FIXME -- add time of receipt ?? */
	union
	{
		DifxMessageMk5Status	mk5status;
		DifxMessageMk5Version	mk5version;
		DifxMessageLoad		load;
		DifxMessageAlert	alert;
		DifxMessageStatus	status;
		DifxMessageInfo		info;
		DifxMessageDatastream	datastream;
		DifxMessageCommand	command;
		DifxMessageParameter	param;
		DifxMessageStart	start;
		DifxMessageStop		stop;
	} body;
	int _xml_level;			/* internal use only here and below */
	char _xml_element[5][32];
	int _xml_error_count;
	char _xml_string[1024];
} DifxMessageGeneric;

/* short term accumulate message type -- antenna-based data (e.g. real) */
typedef struct
{
	int messageType;/* user defined message type */
	int sec;	/* second portion of time since obs start */
	int ns;		/* nanosecond portion of time since obs start */
	int antId;	/* telescope table entry number (from 0) */
	int threadId;	/* core.cpp thread number (starting from 0) */
	int bandId;
	int nChan;
	int nThreads;	/* Number of threads in this core */
	float data[0];	/* Note -- must allocate enough space for your data! */
} DifxMessageSTARecord;

/* long term accumulate message type -- baseline data (e.g. complex) */
typedef struct
{
	int messageType;/* user defined message type */
	int sec;	/* second portion of time since obs start */
	int ns;		/* nanosecond portion of time since obs start */
	int blId;	/* baseline table entry number (from 0) */
	int bandId;
	int nChan;
	int dummy;	/* space reserved for future use */
	float data[0];	/* Note -- must allocate enough space for your data! */
} DifxMessageLTARecord;

const char *difxMessageGetVersion();

int difxMessageInit(int mpiId, const char *identifier);
int difxMessageInitBinary();	/* looks at env vars DIFX_BINARY_GROUP and _PORT */
void difxMessagePrint();
void difxMessageGetMulticastGroupPort(char *group, int *port);

const char *getDifxMessageIdentifier();

int difxMessageSend(const char *message);
int difxMessageSendProcessState(const char *state);
int difxMessageSendMark5Status(const DifxMessageMk5Status *mk5status);
int difxMessageSendMk5Version(const DifxMessageMk5Version *mk5version);
int difxMessageSendDifxStatus(enum DifxState state, const char *stateMessage, 
	double visMJD, int numdatastreams, float *weight);
int difxMessageSendDifxStatus2(const char *jobName, enum DifxState state, 
	const char *stateMessage);
int difxMessageSendLoad(const DifxMessageLoad *load);
int difxMessageSendDifxAlert(const char *errorMessage, int severity);
int difxMessageSendDifxInfo(const char *infoMessage);
int difxMessageSendDifxParameter(const char *name, 
	const char *value, int mpiDestination);
int difxMessageSendDifxParameter1(const char *name, int index1, 
	const char *value, int mpiDestination);
int difxMessageSendDifxParameter2(const char *name, int index1, int index2, 
	const char *value, int mpiDestination);
int difxMessageSendDifxParameterGeneral(const char *name, int nIndex, const int *index,
	const char *value, int mpiDestination);
int difxMessageSendBinary(const char *data, int destination, int size);

int difxMessageReceiveOpen();
int difxMessageReceiveClose(int sock);
int difxMessageReceive(int sock, char *message, int maxlen, char *from);

int difxMessageBinaryOpen(int destination);
int difxMessageBinaryClose(int sock);
int difxMessageBinaryRecv(int sock, char *message, int maxlen, char *from);

int difxMessageParse(DifxMessageGeneric *G, const char *message);
void difxMessageGenericPrint(const DifxMessageGeneric *G);

#ifdef __cplusplus
}
#endif


#endif
