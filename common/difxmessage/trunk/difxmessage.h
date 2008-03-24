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

enum Mk5StatusState 
{
	Mk5Open, 
	Mk5Close, 
	Mk5GetDir, 
	Mk5Playing, 
	Mk5Idle, 
	Mk5Error
};

typedef struct
{
	char identifier[128];
	int datastreamId;	/* > 0 if part of mpifxcorr, otherwise 0 */
	int seqNum;
	char vsnA[10];
	char vsnB[10];
	unsigned long status;
	char activeBank;
	long long position;	/* play pointer */
	float rate;		/* Mbps */
	double playMJD;
} DifxMessageMk5Status;


int difxMessageInit(const char *identifier);
void difxMessagePrint();

int difxMessageSend(const char *message);
int difxMessageSendProcessState(const char *state);

int difxMessageReceiveOpen();
int difxMessageReceiveClose(int sock);
int difxMessageReceive(int sock, char *message, int maxlen, char *from);

#ifdef __cplusplus
}
#endif


#endif
