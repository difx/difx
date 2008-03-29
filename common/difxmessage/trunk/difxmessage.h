#ifndef __DIFX_MESSAGE_H__
#define __DIFX_MESSAGE_H__

#ifdef __cplusplus
extern "C" {
#endif


/**** LOW LEVEL MULTICAST FUNCTIONS ****/

/* single function for send.  returns message length on success, or -1 */

int MulticastSend(const char *group, int port, const char *message);


/* functions for receive */

int openMultiCastSocket(const char *group, int port);
int closeMultiCastSocket(int sock);
int MultiCastReceive(int sock, char *message, int maxlen, char *from);


/**** DIFX SPECIFIC FUNCTIONS AND DATA TYPES ****/

/* Note! Keep this in sync with const char Mk5StatusStrings[][24] in
 * difxmessagemark5.c
 */
enum Mk5Status
{
	MARK5_STATE_OPENING,
	MARK5_STATE_OPEN, 
	MARK5_STATE_CLOSE, 
	MARK5_STATE_GETDIR, 
	MARK5_STATE_GOTDIR, 
	MARK5_STATE_PLAY, 
	MARK5_STATE_IDLE, 
	MARK5_STATE_ERROR
};

extern const char Mk5StatusStrings[][24];

typedef struct
{
	enum Mk5Status state;
	char vsnA[10];
	char vsnB[10];
	unsigned long status;
	char activeBank;
	int scanNumber;
	long long position;	/* play pointer */
	float rate;		/* Mbps */
	double dataMJD;
} DifxMessageMk5Status;

typedef struct
{
	float cpuLoad;
	int totalMemory;
	int usedMemory;
} DifxMessageLoad;

int difxMessageInit(int mpiId, const char *identifier);
void difxMessagePrint();

int difxMessageSend(const char *message);
int difxMessageSendProcessState(const char *state);
int difxMessageSendMark5State(const DifxMessageMk5Status *mk5state);
int difxMessageSendLoad(const DifxMessageLoad *load);
int difxMessageSendDifxError(const char *errorString, int severity);

int difxMessageReceiveOpen();
int difxMessageReceiveClose(int sock);
int difxMessageReceive(int sock, char *message, int maxlen, char *from);

#ifdef __cplusplus
}
#endif


#endif
