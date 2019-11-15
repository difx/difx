/***************************************************************************
 *   Copyright (C) 2007-2017 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __DIFX_MESSAGE_H__
#define __DIFX_MESSAGE_H__

#include <stdio.h>
#include <limits.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DIFX_MESSAGE_IDENTIFIER_LENGTH	32
#define DIFX_MESSAGE_PARAM_LENGTH	32
#define DIFX_MESSAGE_VERSION_LENGTH	32
#define DIFX_MESSAGE_FILENAME_LENGTH	PATH_MAX
#define DIFX_MESSAGE_COMMENT_LENGTH	512
#define DIFX_MESSAGE_MAX_TARGETS	128
#define DIFX_MESSAGE_LENGTH		8000
#define DIFX_MESSAGE_MAX_INDEX		8
#define DIFX_MESSAGE_MAX_DATASTREAMS	50
#define DIFX_MESSAGE_MAX_CORES		100
#define DIFX_MESSAGE_MAX_SCANNAME_LEN	64
#define DIFX_MESSAGE_MAX_ENV		8
#define DIFX_MESSAGE_ALLMPIFXCORR	-1
#define DIFX_MESSAGE_ALLCORES		-2
#define DIFX_MESSAGE_ALLDATASTREAMS	-3
#define DIFX_MESSAGE_N_DRIVE_STATS_BINS	8
#define DIFX_MESSAGE_N_CONDITION_BINS	DIFX_MESSAGE_N_DRIVE_STATS_BINS	/* deprecated */
#define DIFX_MESSAGE_MARK5_VSN_LENGTH	8
#define DIFX_MESSAGE_MARK6_MSN_LENGTH	8
#define DIFX_MESSAGE_MARK6_GROUP_LENGTH 35
#define DIFX_MESSAGE_DISC_SERIAL_LENGTH	31
#define DIFX_MESSAGE_DISC_MODEL_LENGTH	31
#define DIFX_MESSAGE_MAX_SMART_IDS	32
#define DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH	64
#define DIFX_MESSAGE_HOSTNAME_LENGTH	256

#define _DIFX_MESSAGE_XML_STRING_LENGTH	1024

/**** LOW LEVEL MULTICAST FUNCTIONS ****/

/* single function for send.  returns message length on success, or -1 */

int MulticastSend(const char *group, int port, const char *message, int length);

/* functions for receive */

int openMultiCastSocket(const char *group, int port);
int closeMultiCastSocket(int sock);
int MultiCastReceive(int sock, char *message, int maxlen, char *from);


/**** DIFX SPECIFIC FUNCTIONS AND DATA TYPES ****/

/* check whether using difxMessage or just cout/cerr */
int isDifxMessageInUse();


/* check whether using difxMessage is using Unicast messages */
int isDifxMessageUnicast();

/* Types of binary data to be sent */
enum BinaryDataChannels
{
	BINARY_STA,
	BINARY_LTA
};

/* Note! Keep this in sync with Mk5StateStrings[][24] in difxmessage.c */
/* Note: all new additions should go to the end of the list. */
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
	MARK5_STATE_COPY,	/* Copy from the module */
	MARK5_STATE_CONDITION,
	MARK5_STATE_COND_ERROR,
	MARK5_STATE_TEST,
	MARK5_STATE_TESTWRITE,
	MARK5_STATE_TESTREAD,
	MARK5_STATE_BOOTING,
	MARK5_STATE_RECORD,
	MARK5_STATE_COPYTO,	/* Copy to the module */
	MARK5_STATE_FUSEMK5,	/* module in use by fuseMk5 */
	NUM_MARK5_STATES	/* this needs to be the last line of enum */
};

extern const char Mk5StateStrings[][24];

/* Note: all new additions should go to the end of the list. */
enum Mark6State
{
	MARK6_STATE_OPENING = 0,
	MARK6_STATE_OPEN, 
	MARK6_STATE_CLOSE, 
	MARK6_STATE_GETDIR, 
	MARK6_STATE_GOTDIR, 
	MARK6_STATE_PLAY, 
	MARK6_STATE_IDLE, 
	MARK6_STATE_ERROR,
	MARK6_STATE_BUSY,
	MARK6_STATE_INITIALIZING,
	MARK6_STATE_RESETTING,
	MARK6_STATE_REBOOTING,
	MARK6_STATE_POWEROFF,
	MARK6_STATE_NODATA,
	MARK6_STATE_NOMOREDATA,
	MARK6_STATE_PLAYINVALID,
	MARK6_STATE_START,
	MARK6_STATE_COPY,	/* Copy from the module */
	MARK6_STATE_CONDITION,
	MARK6_STATE_COND_ERROR,
	MARK6_STATE_TEST,
	MARK6_STATE_TESTWRITE,
	MARK6_STATE_TESTREAD,
	MARK6_STATE_BOOTING,
	MARK6_STATE_RECORD,
	MARK6_STATE_COPYTO,	/* Copy to the module */
	NUM_MARK6_STATES	/* this needs to be the last line of enum */
};

extern const char Mark6StateStrings[][24];

/* Note! Keep this in sync with DifxStateStrings[][24] in difxmessage.c */
/* Note: all new additions should go to the end of the list. */
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
	DIFX_STATE_CRASHED,	/* mpi process has crashed */
	NUM_DIFX_STATES		/* this needs to be the last line of enum */
};

extern const char DifxStateStrings[][24];

/* Note! Keep this in sync with DifxDiagnosticStrings[][24] in difxmessage.c */
enum DifxDiagnosticType
{
	DIFX_DIAGNOSTIC_MEMORYUSAGE = 0,
	DIFX_DIAGNOSTIC_BUFFERSTATUS,
	DIFX_DIAGNOSTIC_PROCESSINGTIME,
	DIFX_DIAGNOSTIC_DATACONSUMED,
	DIFX_DIAGNOSTIC_INPUTDATARATE,
	DIFX_DIAGNOSTIC_NUMSUBINTSLOST,
	NUM_DIFX_DIAGNOSTIC_TYPES	/* this needs to be the last line of enum */
};

extern const char DifxDiagnosticStrings[][24];

/* Note! Keep this in sync with DifxMessageAlertString[][16] in difxmessage.c */
enum DifxAlertLevel
{
	DIFX_ALERT_LEVEL_FATAL = 0,
	DIFX_ALERT_LEVEL_SEVERE,
	DIFX_ALERT_LEVEL_ERROR,
	DIFX_ALERT_LEVEL_WARNING,
	DIFX_ALERT_LEVEL_INFO,
	DIFX_ALERT_LEVEL_VERBOSE,
	DIFX_ALERT_LEVEL_DEBUG,
	DIFX_ALERT_LEVEL_DO_NOT_SEND
};

extern const char difxMessageAlertString[][16];

/* Note! Keep this in sync with DifxStartFunctionString[][24] in difxmessage.c */
enum DifxStartFunction
{
	DIFX_START_FUNCTION_UNKNOWN = 0,
	DIFX_START_FUNCTION_DEFAULT,
	DIFX_START_FUNCTION_NRAO,
	DIFX_START_FUNCTION_USNO,
	DIFX_START_FUNCTION_RUN_MONITOR,
	DIFX_START_FUNCTION_CONFIG_ONLY,
	DIFX_START_FUNCTION_BAIL_ON_CONFIG_FAIL,
	NUM_DIFX_START_FUNCTION_TYPES /* this needs to be the last line of the enum */
};

extern const char DifxStartFunctionString[][24];

/* Note! Keep this in sync with DifxMessageTypeStrings[][24] in difxmessage.c */
enum DifxMessageType
{
	DIFX_MESSAGE_UNKNOWN = 0,
	DIFX_MESSAGE_LOAD,
	DIFX_MESSAGE_ALERT,
	DIFX_MESSAGE_MARK6STATUS,
	DIFX_MESSAGE_MARK5STATUS,
	DIFX_MESSAGE_STATUS,
	DIFX_MESSAGE_INFO,
	DIFX_MESSAGE_DATASTREAM,
	DIFX_MESSAGE_COMMAND,
	DIFX_MESSAGE_PARAMETER,
	DIFX_MESSAGE_START,
	DIFX_MESSAGE_STOP,
	DIFX_MESSAGE_MARK5VERSION,
	DIFX_MESSAGE_CONDITION,	/* this is deprecated.  Use DIFX_MESSAGE_DISKSTAT instead */
	DIFX_MESSAGE_TRANSIENT,
	DIFX_MESSAGE_SMART,
	DIFX_MESSAGE_DRIVE_STATS,
	DIFX_MESSAGE_DIAGNOSTIC,
	DIFX_MESSAGE_FILETRANSFER,
	DIFX_MESSAGE_FILEOPERATION,
	DIFX_MESSAGE_VEX2DIFXRUN,
	DIFX_MESSAGE_MACHINESDEFINITION,
	DIFX_MESSAGE_GETDIRECTORY,
	DIFX_MESSAGE_MK5CONTROL,
	DIFX_MESSAGE_MARK5COPY,
	DIFX_MESSAGE_VSIS, /* stop mk5daemon VSIS to allow fuseMk5 use, start when finished with fuseMk5 */
	DIFX_MESSAGE_MARK6ACTIVITY,
	NUM_DIFX_MESSAGE_TYPES	/* this needs to be the last line of enum */
};

extern const char DifxMessageTypeStrings[][24];

/* Note! Keep this in sync with DifxMessageTypeStrings[][24] in difxmessage.c */
enum DriveStatsType
{
	DRIVE_STATS_TYPE_CONDITION = 0,
	DRIVE_STATS_TYPE_CONDITION_R,
	DRIVE_STATS_TYPE_CONDITION_W,
	DRIVE_STATS_TYPE_READ,
	DRIVE_STATS_TYPE_WRITE,
	DRIVE_STATS_TYPE_UNKNOWN,
	DRIVE_STATS_TYPE_TEST,	/* to test the mechanism without entering things into databases... */
	NUM_DRIVE_STATS_TYPES	/* this needs to be the last line of enum */
};

extern const char DriveStatsTypeStrings[][24];

typedef struct
{
	enum Mk5State state;
	char vsnA[DIFX_MESSAGE_MARK5_VSN_LENGTH+2];
	char vsnB[DIFX_MESSAGE_MARK5_VSN_LENGTH+2];
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
        enum Mark6State state;
	char msn1[DIFX_MESSAGE_MARK6_MSN_LENGTH+2];
	char msn2[DIFX_MESSAGE_MARK6_MSN_LENGTH+2];
	char msn3[DIFX_MESSAGE_MARK6_MSN_LENGTH+2];
	char msn4[DIFX_MESSAGE_MARK6_MSN_LENGTH+2];
	char group1[DIFX_MESSAGE_MARK6_GROUP_LENGTH+2];
	char group2[DIFX_MESSAGE_MARK6_GROUP_LENGTH+2];
	char group3[DIFX_MESSAGE_MARK6_GROUP_LENGTH+2];
	char group4[DIFX_MESSAGE_MARK6_GROUP_LENGTH+2];
	int bank1Disks;
	int bank2Disks;
	int bank3Disks;
	int bank4Disks;
	int bank1MissingDisks;
	int bank2MissingDisks;
	int bank3MissingDisks;
	int bank4MissingDisks;
        unsigned int status;
        int scanNumber;
	char scanName[DIFX_MESSAGE_MAX_SCANNAME_LEN];
        long long position;     /* play pointer */
        float rate;             /* Mbps */
        double dataMJD;
} DifxMessageMark6Status;

typedef struct
{
        enum Mark6State state;
	char activeVsn[DIFX_MESSAGE_MARK6_MSN_LENGTH+2];
        unsigned int status;
        int scanNumber;
	char scanName[DIFX_MESSAGE_MAX_SCANNAME_LEN];
        long long position;     /* play pointer */
        float rate;             /* Mbps */
        double dataMJD;
} DifxMessageMark6Activity;

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
	char BoardType[12];
	int SerialNum;
	char DB_PCBType[12];
	char DB_PCBSubType[12];
	char DB_PCBVersion[8];
	char DB_FPGAConfig[12];
	char DB_FPGAConfigVersion[8];
	unsigned int DB_SerialNum;
	unsigned int DB_NumChannels;
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
	int nCore;			/* number of cpu cores in box */
} DifxMessageLoad;

typedef struct
{
	char message[DIFX_MESSAGE_LENGTH];
	int severity;
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
} DifxMessageAlert;

typedef struct
{
	enum DifxState state;
	char message[DIFX_MESSAGE_LENGTH];
	double mjd;	/* current observe time MJD */
	float weight[DIFX_MESSAGE_MAX_DATASTREAMS];
	int maxDS;
	double jobStartMJD, jobStopMJD;	/* MJD range of job; these are optional */
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
} DifxMessageStatus;

typedef struct
{
	char message[DIFX_MESSAGE_LENGTH];
} DifxMessageInfo;

typedef struct
{
	char vsis[DIFX_MESSAGE_LENGTH];
} DifxMessageVsis;

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
	char headNode[DIFX_MESSAGE_HOSTNAME_LENGTH];
	char datastreamNode[DIFX_MESSAGE_MAX_DATASTREAMS][DIFX_MESSAGE_HOSTNAME_LENGTH];
	char processNode[DIFX_MESSAGE_MAX_CORES][DIFX_MESSAGE_HOSTNAME_LENGTH];
	char envVar[DIFX_MESSAGE_MAX_ENV][DIFX_MESSAGE_FILENAME_LENGTH];
	int nThread[DIFX_MESSAGE_MAX_CORES];
	char mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH];
	char mpiOptions[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxProgram[DIFX_MESSAGE_FILENAME_LENGTH];
	int force;
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
	double restartSeconds;		/* Amount of time into job to actually start the correlation */
	int function;   /* specifies which function to use when starting DiFX - presumably a temporary option. */
	char address[DIFX_MESSAGE_PARAM_LENGTH];  /* IP address for reporting diagnostics during run */
	int port;  /* port for reporting diagnostics during run */
} DifxMessageStart;

typedef struct
{
	char origin[DIFX_MESSAGE_FILENAME_LENGTH];
	char destination[DIFX_MESSAGE_FILENAME_LENGTH];
	char dataNode[DIFX_MESSAGE_HOSTNAME_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];
	char direction[DIFX_MESSAGE_PARAM_LENGTH];
	int port;
} DifxMessageFileTransfer;

typedef struct
{
	char path[DIFX_MESSAGE_FILENAME_LENGTH];
	char operation[DIFX_MESSAGE_PARAM_LENGTH];
	char dataNode[DIFX_MESSAGE_HOSTNAME_LENGTH];
	char arg[DIFX_MESSAGE_FILENAME_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];
	int port;
} DifxMessageFileOperation;

typedef struct
{
	char command[DIFX_MESSAGE_PARAM_LENGTH];
	char targetNode[DIFX_MESSAGE_HOSTNAME_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];
	int port;
} DifxMessageMk5Control;

typedef struct
{
	char user[DIFX_MESSAGE_PARAM_LENGTH];
	char headNode[DIFX_MESSAGE_HOSTNAME_LENGTH];
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
	char passPath[DIFX_MESSAGE_FILENAME_LENGTH];
	char v2dFile[DIFX_MESSAGE_FILENAME_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];
	int port;
	char calcCommand[DIFX_MESSAGE_FILENAME_LENGTH];
	int calcOnly;
} DifxMessageVex2DifxRun;

typedef struct
{
	int nDatastream, nProcess;
	char mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH];
	char mpiOptions[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char headNode[DIFX_MESSAGE_HOSTNAME_LENGTH];
	char datastreamNode[DIFX_MESSAGE_MAX_DATASTREAMS][DIFX_MESSAGE_HOSTNAME_LENGTH];
	char processNode[DIFX_MESSAGE_MAX_CORES][DIFX_MESSAGE_HOSTNAME_LENGTH];
	int nThread[DIFX_MESSAGE_MAX_CORES];
	int testProcessors;
	char machinesFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char threadsFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];  /* IP address for reporting diagnostics during run */
	int port;  /* port for reporting diagnostics during run */
} DifxMessageMachinesDefinition;

typedef struct
{
	char mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
	char mark5[DIFX_MESSAGE_PARAM_LENGTH];
	char vsn[DIFX_MESSAGE_PARAM_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];
	int port;
	int generateNew;
} DifxMessageGetDirectory;

typedef struct
{
	char mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
	char mark5[DIFX_MESSAGE_PARAM_LENGTH];
	char vsn[DIFX_MESSAGE_PARAM_LENGTH];
	char scans[DIFX_MESSAGE_PARAM_LENGTH];
	char destination[DIFX_MESSAGE_FILENAME_LENGTH];
	char address[DIFX_MESSAGE_PARAM_LENGTH];
	int port;
} DifxMessageMark5Copy;

typedef struct
{
	char mpiWrapper[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
	char difxProgram[DIFX_MESSAGE_FILENAME_LENGTH];
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
} DifxMessageStop;

typedef struct
{
	char serialNumber[DIFX_MESSAGE_DISC_SERIAL_LENGTH+1];
	char modelNumber[DIFX_MESSAGE_DISC_MODEL_LENGTH+1];
	int diskSize;	/* GB */
	char moduleVSN[DIFX_MESSAGE_MARK5_VSN_LENGTH+2];
	int moduleSlot;
	double startMJD, stopMJD;
	int bin[DIFX_MESSAGE_N_DRIVE_STATS_BINS];
	enum DriveStatsType type;
	long long startByte;
} DifxMessageDriveStats;

/* This message type contains S.M.A.R.T. information for one disk in
 * a Mark5 module. Typically 8 such messages will be needed to 
 * convey results from the conditioning of one module.
 */
typedef struct
{
	char moduleVSN[DIFX_MESSAGE_MARK5_VSN_LENGTH+2];
	int moduleSlot;
	double mjdData;	/* MJD when data was taken */
	int nValue;
	int id[DIFX_MESSAGE_MAX_SMART_IDS];
	long long value[DIFX_MESSAGE_MAX_SMART_IDS];
} DifxMessageSmart;

/* This is used to tell Mk5daemon to copy some data at the end of the
 * correlation to a disk file */
typedef struct
{
	char jobId[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	double startMJD;
	double stopMJD;
	double priority;
	char destDir[DIFX_MESSAGE_FILENAME_LENGTH];
	char comment[DIFX_MESSAGE_COMMENT_LENGTH];
	double dm;	/* dispersion measure */
} DifxMessageTransient;

/* This is used to pass out diagnostic-type info like buffer states */
typedef struct
{
	enum DifxDiagnosticType diagnosticType;
	int threadid;
	long long bytes;
	long long counter;
	double microsec;
	double rateMbps;
	int bufferstatus[3];
} DifxMessageDiagnostic;

typedef struct
{
	enum DifxMessageType type;
	char from[DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH];
	char to[DIFX_MESSAGE_MAX_TARGETS][DIFX_MESSAGE_PARAM_LENGTH];
	int nTo;
	char identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	int mpiId;
	int seqNumber;
	union
	{
		DifxMessageMk5Status	mk5status;
		DifxMessageMark6Status	mark6status;
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
		DifxMessageDriveStats	driveStats;
		DifxMessageTransient	transient;
		DifxMessageSmart	smart;
		DifxMessageDiagnostic	diagnostic;
		DifxMessageFileTransfer    fileTransfer;
		DifxMessageFileOperation    fileOperation;
		DifxMessageVex2DifxRun    vex2DifxRun;
		DifxMessageMachinesDefinition   machinesDefinition;
		DifxMessageGetDirectory  getDirectory;
		DifxMessageMk5Control  mk5Control;
		DifxMessageMark5Copy  mark5Copy;
		DifxMessageVsis		vsis;
		DifxMessageMark6Activity   mark6activity;
	} body;
	int _xml_level;			/* internal use only here and below */
	char _xml_element[5][32];
	int _xml_error_count;
	char _xml_string[_DIFX_MESSAGE_XML_STRING_LENGTH];
	int _inDB;
} DifxMessageGeneric;

/* Standard message "types" for STA and LTA data */
#define STA_AUTOCORRELATION  1
#define STA_CROSSCORRELATION 2
#define STA_KURTOSIS         3
#define LTA_AUTOCORRELATION  4
#define LTA_CROSSCORRELATION 5

/* short term accumulate message type -- antenna-based data (e.g., real) */
/* Header size made the same as for LTA data */
typedef struct
{
	int messageType;/* user defined message type */
	int scan;	/* the scan index (0 based) */
	int sec;	/* second portion of time since scan start */
	int ns;		/* nanosecond portion of time since scan start
			   (centre of integration) */
	int nswidth;	/* width of the integration in nanoseconds */
	int dsindex;	/* index of the datastream for the current config (from 0) */
	int coreindex;	/* index of the core (starting from 0) */
	int threadindex;/* core.cpp thread number (starting from 0) */
	int bandindex;	/* index of the band for this datastream */
	int nChan;	/* number of channels for this band */
	char identifier[DIFX_MESSAGE_PARAM_LENGTH];	/* Jobname (basename) */
	float data[0];	/* Note -- must allocate enough space for your data! */
} DifxMessageSTARecord;

/* long term accumulate message type -- baseline data (e.g. complex) */
/* Header size made the same as for STA data */
typedef struct
{
	int messageType;/* user defined message type */
	int scan;       /* the scan index (0 based) */
	int sec;	/* second portion of time since scan start */
	int ns;		/* nanosecond portion of time since scan start 
			   (centre of integration) */
	int secwidth;	/* width of the integration in whole seconds */
	int nswidth;	/* width of the integration in whole nanoseconds */
	int blId;	/* index of the baseline for the current config (from 0) */
	int bandindex;	/* index of the band for this baseline */
	int nChan;	/* Number of channels for this band */
	int dummy;	/* space reserved for future use */
	char identifier[DIFX_MESSAGE_PARAM_LENGTH];     /* Jobname (basename) */
	float data[0];	/* Note -- must allocate enough space for your data! */
} DifxMessageLTARecord;

const char *difxMessageGetVersion();

int difxMessageInit(int mpiId, const char *identifier);
int difxMessageInitFull(int mpiId, const char *identifier, const char *publishedHostname);
int difxMessageInitBinary();	/* looks @ env vars DIFX_BINARY_GROUP & _PORT */
int difxMessageSetInputFilename(const char *inputFilename);
void difxMessagePrint();
void difxMessageGetMulticastGroupPort(char *group, int *port);

const char *getDifxMessageIdentifier();

int difxMessageSend2(const char *message, int size);
int difxMessageSendProcessState(const char *state);
int difxMessageSendMark6Status(const DifxMessageMark6Status *mark6status);
int difxMessageSendMark6Activity(const DifxMessageMark6Activity *mark6activity);
int difxMessageSendMark5Status(const DifxMessageMk5Status *mk5status);
int difxMessageSendMk5Version(const DifxMessageMk5Version *mk5version);
int difxMessageSendDriveStats(const DifxMessageDriveStats *driveStats);
int difxMessageSendDifxStatus(enum DifxState state, const char *stateMessage, double visMJD, int numdatastreams, float *weight);
int difxMessageSendDifxStatus2(const char *jobName, enum DifxState state, const char *stateMessage);
int difxMessageSendDifxStatus3(enum DifxState state, const char *stateMessage, double visMJD, int numdatastreams, float *weight, double mjdStart, double mjdStop);
int difxMessageSendLoad(const DifxMessageLoad *load);
int difxMessageSendDifxAlert(const char *errorMessage, int severity);
int difxMessageSendDifxInfo(const char *infoMessage);
int difxMessageSendStop(const char *inputFilename, const char *mpiWrapper, const char *difxVersion, const char *difxProgram);
int difxMessageSendDifxDiagnosticBufferStatus(int threadid, int numelements, int startelement, int numactiveelements);
int difxMessageSendDifxDiagnosticProcessingTime(int threadid, double durationMicrosec);
int difxMessageSendDifxDiagnosticMemoryUsage(long long membytes);
int difxMessageSendDifxDiagnosticDataConsumed(long long bytes);
int difxMessageSendDifxDiagnosticInputDatarate(double bytespersec);
int difxMessageSendDifxDiagnosticNumSubintsLost(int numsubintslost);
int difxMessageSendDifxParameter(const char *name, const char *value, int mpiDestination);
int difxMessageSendDifxParameterTo(const char *name, const char *value, const char *to);
int difxMessageSendDifxParameter1(const char *name, int index1, const char *value, int mpiDestination);
int difxMessageSendDifxParameter2(const char *name, int index1, int index2, const char *value, int mpiDestination);
int difxMessageSendDifxParameterGeneral(const char *name, int nIndex, const int *index, const char *value, int mpiDestination);
int difxMessageSendDifxTransient(const DifxMessageTransient *transient);
int difxMessageSendDifxSmart(double mjdData, const char *vsn, int slot, int nValue, const int *ids, const long long *values);
int difxMessageSendBinary(const char *data, int destination, int size);
int difxMessageSendVsis(const char *state, const char *to);

int difxMessageReceiveOpen();
int difxMessageReceiveClose(int sock);
int difxMessageReceive(int sock, char *message, int maxlen, char *from);

int difxMessageBinaryOpen(int destination);
int difxMessageBinaryClose(int sock);
int difxMessageBinaryRecv(int sock, char *message, int maxlen, char *from);

void fprintDifxMessageSTARecord(FILE *out, const DifxMessageSTARecord *record, int printData);
void printDifxMessageSTARecord(const DifxMessageSTARecord *record, int printData);

int difxMessageParse(DifxMessageGeneric *G, const char *message);
void difxMessageGenericPrint(const DifxMessageGeneric *G);

enum DriveStatsType stringToDriveStatsType(const char *str);


/* The following function should not be used anymore.  Instead use difxMessageSend2 */
int difxMessageSend(const char *message);

#ifdef __cplusplus
}
#endif


#endif
