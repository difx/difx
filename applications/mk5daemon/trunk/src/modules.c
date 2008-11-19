#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <xlrapi.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <difxmessage.h>
#include "mk5daemon.h"

#define MARK5A_PORT     2620

static int XLR_get_modules(char *vsna, char *vsnb, Mk5Daemon *D)
{
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	XLR_ERROR_CODE xlrError;
	char message[100+(XLR_ERROR_LENGTH)];
	char xlrErrorStr[XLR_ERROR_LENGTH];
	
	xlrRC = XLROpen(1, &xlrDevice);
	D->nXLROpen++;
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_get_modules: "
			"Cannot open streamstor card.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
		return 1;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		xlrError = XLRGetLastError();
		if(xlrError == 148) /* XLR_ERR_DRIVEMODULE_NOTREADY */
		{
			/* this means no modules loaded */
			vsna[0] = vsnb[0] = 0;
			sprintf(message, "XLR VSNs: <%s> <%s> N=%d\n",
				vsna, vsnb, D->nXLROpen);
		}
		else
		{
			XLRGetErrorMessage(xlrErrorStr, xlrError);
			sprintf(message, "ERROR: XLR_get_modules: "
				"Cannot set SkipCheckDir.  N=%d "
				"Error=%u (%s)\n",
				D->nXLROpen,
				xlrError,
				xlrErrorStr);
		}
		Logger_logData(D->log, message);
		XLRClose(xlrDevice);
		return 0;
	}
	
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsnb[0] = 0;

		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_get_modules: "
			"BANK_B XLRGetBankStatus failed.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(vsnb, bank_stat.Label, 16);
		vsnb[8] = 0;
	}
	else
	{
		vsnb[0] = 0;
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsna[0] = 0;

		xlrError = XLRGetLastError();
		XLRGetErrorMessage(xlrErrorStr, xlrError);
		sprintf(message, "ERROR: XLR_get_modules: "
			"BANK_A XLRGetBankStatus failed.  N=%d "
			"Error=%u (%s)\n",
			D->nXLROpen,
			xlrError,
			xlrErrorStr);
		Logger_logData(D->log, message);
	}
	else if(bank_stat.Label[8] == '/')
	{
		strncpy(vsna, bank_stat.Label, 16);
		vsna[8] = 0;
	}
	else
	{
		vsna[0] = 0;
	}

	XLRClose(xlrDevice);

	sprintf(message, "XLR VSNs: <%s> <%s> N=%d\n",
		vsna, vsnb, D->nXLROpen);
	Logger_logData(D->log, message);

	return 0;
}

static int Mark5A_get_modules(char *vsna, char *vsnb, Mk5Daemon *D)
{
	struct sockaddr_in addr;
	int sock;
	struct timeval tv;
	int status;
	char line[512];
	int i, n;
	char *ptr[8];
	int nptr = 0;

	vsna[0] = vsnb[0] = 0;


	sock = socket(AF_INET, SOCK_STREAM, 0);
	if(sock < 0)
	{
		Logger_logData(D->log, "cannot make socket");	
		return 1;
	}

	tv.tv_sec = 9;	
	tv.tv_usec = 0;
	setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
	setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));

	addr.sin_family = AF_INET;
	addr.sin_port = htons(MARK5A_PORT);
	addr.sin_addr.s_addr = inet_addr("127.0.0.1");

	status = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
	if(status != 0)
	{
		Logger_logData(D->log, "cannot connect to Mark5A");	
		close(sock);
		return 1;
	}

	sprintf(line, "bank_set?\n");
	n = strlen(line);

	if(send(sock, line, n, 0) < 1)
	{
		Logger_logData(D->log, "error sending bank_set? to Mark5A");
		close(sock);
		return 1;
	}

	n = recv(sock, line, 511, 0);
	if(n < 1)
	{
		Logger_logData(D->log, "error recving from Mark5A");
		close(sock);
		return 1;
	}
	line[n] = 0;

	close(sock);

	for(i = 0; line[i] && nptr < 8; i++)
	{
		if(line[i] == ':')
		{
			ptr[nptr] = line+i+2;
			line[i] = 0;
			nptr++;
		}
	}
	for(i = nptr; i < 8; i++)
	{
		ptr[i] = line;
	}

	if(ptr[0][0] == 'A' && ptr[1][0] != '-')
	{
		strncpy(vsna, ptr[1], 8);
		vsna[8] = 0;
	}
	if(ptr[0][0] == 'B' && ptr[1][0] != '-')
	{
		strncpy(vsnb, ptr[1], 8);
		vsnb[8] = 0;
	}

	if(ptr[2][0] == 'A' && ptr[3][0] != '-')
	{
		strncpy(vsna, ptr[3], 8);
		vsna[8] = 0;
	}
	if(ptr[2][0] == 'B' && ptr[3][0] != '-')
	{
		strncpy(vsnb, ptr[3], 8);
		vsnb[8] = 0;
	}

	return 0;
}

void Mk5Daemon_getModules(Mk5Daemon *D)
{
	DifxMessageMk5Status dm;
	int n;
	char vsnA[16], vsnB[16];

	memset(&dm, 0, sizeof(DifxMessageMk5Status));

	/* don't let the process type change while getting vsns */
	pthread_mutex_lock(&D->processLock);

	vsnA[0] = vsnB[0] = 0;

	switch(D->process)
	{
	case PROCESS_NONE:
		n = XLR_get_modules(vsnA, vsnB, D);
		if(n == 0)
		{
			dm.state = MARK5_STATE_IDLE;
		}
		else
		{
			dm.state = MARK5_STATE_ERROR;
		}
		break;
	case PROCESS_MARK5A:
		n = Mark5A_get_modules(vsnA, vsnB, D);
		if(n == 0)
		{
			dm.state = MARK5_STATE_BUSY;
		}
		else
		{
			dm.state = MARK5_STATE_ERROR;
		}
		break;
	default:
		dm.state = MARK5_STATE_BUSY;
		break;
	}

	strncpy(D->vsnA, vsnA, 8);
	strncpy(D->vsnB, vsnB, 8);
	strncpy(dm.vsnA, vsnA, 8);
	strncpy(dm.vsnB, vsnB, 8);
	
	pthread_mutex_unlock(&D->processLock);

	difxMessageSendMark5Status(&dm);
}
