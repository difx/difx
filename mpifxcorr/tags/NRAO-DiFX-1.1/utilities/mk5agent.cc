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

const char program[] = "mk5agent";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "1.0";


int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s\n\n",
		program, version, author);
	fprintf(stderr, "A program allow remote queries of Mark5 status\n\n");
	fprintf(stderr, "Usage: mk5agent [<options>]\n\n");

	return 0;
}

int XLR_get_modules(char *vsna, char *vsnb)
{
	SSHANDLE xlrDevice;
	S_BANKSTATUS bank_stat;
	XLR_RETURN_CODE xlrRC;
	
	xlrRC = XLROpen(1, &xlrDevice);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("ERROR Cannot open streamstor card\n");
		return 1;
	}

	xlrRC = XLRSetOption(xlrDevice, SS_OPT_SKIPCHECKDIR);
	if(xlrRC != XLR_SUCCESS)
	{
		printf("ERROR Cannot set SkipCheckDir\n");
		return 1;
	}
	
	xlrRC = XLRGetBankStatus(xlrDevice, BANK_B, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsnb[0] = 0;
	}
	else
	{
		if(bank_stat.Label[8] == '/')
		{
			strncpy(vsnb, bank_stat.Label, 8);
			vsnb[8] = 0;
		}
		else
		{
			vsnb[0] = 0;
		}
	}

	xlrRC = XLRGetBankStatus(xlrDevice, BANK_A, &bank_stat);
	if(xlrRC != XLR_SUCCESS)
	{
		vsna[0] = 0;
	}
	else
	{
		if(bank_stat.Label[8] == '/')
		{
			strncpy(vsna, bank_stat.Label, 8);
			vsna[8] = 0;
		}
		else
		{
			vsna[0] = 0;
		}
	}

	XLRClose(xlrDevice);

	printf("%s %s\n", vsna, vsnb);

	return 0;
}

int Mark5A_get_modules(char *vsna, char *vsnb)
{
	char cmd[] = "echo \"bank_set?\" | tstMark5A";
	char line[512];
	FILE *in;
	int n;
	char *vsn;
	char junk[10];
	char bank;
	int i, ncolon;

	vsna[0] = vsnb[0] = 0;

	in = popen(cmd, "r");
	if(!in)
	{
		printf("ERROR Cannot run tstMark5A\n");
		return 1;
	}

	n = fread(line, 1, 512, in);
	line[511] = 0;
	fclose(in);

	ncolon = 0;
	for(i = 0; line[i]; i++)
	{
		if(line[i] == ':')
		{
			ncolon++;
			if(ncolon == 1 || ncolon == 3)
			{
				bank = line[i+2];
				if(bank == 'A' || bank == 'a')
				{
					vsn = vsna;
				}
				else if(bank == 'B' || bank == 'b')
				{
					vsn = vsnb;
				}
				else
				{
					vsn = junk;
				}
			}
			else if(ncolon == 2 || ncolon == 4)
			{
				if(line[i+2] != '-')
				{
					strncpy(vsn, line+i+2, 8);
					vsn[8] = 0;
				}
			}
		}
	}

	return 0;
}

int running(const char *name)
{
	FILE *in;
	int n;
	char cmd[256];
	char line[512];

	sprintf(cmd, "ps -e | grep %s", name);
	
	in = popen(cmd, "r");
	if(!in)
	{
		printf("ERROR Cannot run ps\n");
		return 1;
	}

	n = fread(line, 1, 512, in);
	line[511] = 0;
	fclose(in);

	if(n > 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

enum Mk5State getvsns(char *vsna, char *vsnb)
{
	char cmd[] = "ps -e | grep Mark5A";
	char line[512];
	FILE *in;
	int n;

	vsna[0] = vsnb[0] = 0;

	if(running("SSErase") ||
	   running("SSReset") ||
	   running("ssopen")  ||
	   running("mpifxcorr"))
	{
		return MARK5_STATE_BUSY;
	}
	
	in = popen(cmd, "r");
	if(!in)
	{
		return MARK5_STATE_ERROR;
	}

	n = fread(line, 1, 512, in);
	line[511] = 0;
	fclose(in);

	if(n > 0)
	{
		n = Mark5A_get_modules(vsna, vsnb);
		if(n == 0)
		{
			return MARK5_STATE_BUSY;
		}
		else
		{
			return MARK5_STATE_ERROR;
		}
	}
	
	n = XLR_get_modules(vsna, vsnb);
	if(n == 0)
	{
		return MARK5_STATE_IDLE;
	}
	else
	{
		return MARK5_STATE_ERROR;
	}
}

int resetStreamstor(DifxMessageMk5Status *dm)
{
	SSHANDLE xlrDevice;
	XLR_RETURN_CODE xlrRC;
	char hn[64];

	gethostname(hn, 63);

	if(running("SSErase")   ||
	   running("SSReset")   ||
	   running("ssopen")    ||
	   running("mpifxcorr") ||
	   running("Mark5A"))
	{
		printf("%s : Won't RESET -- Busy.\n", hn);
		return 1;
	}

	dm->state = MARK5_STATE_RESETTING;
	difxMessageSendMark5Status(dm);

	printf("%s : Resetting\n", hn);
	XLRCardReset(1);
	printf("%s : Reset\n", hn);

	printf("%s : Opening\n", hn);
	xlrRC = XLROpen(1, &xlrDevice);
	XLRClose(xlrDevice);
	printf("%s : Open = %d\n", hn, xlrRC);

	dm->state = MARK5_STATE_IDLE;
	difxMessageSendMark5Status(dm);

	return 0;
}

int main(int argc, char **argv)
{
	int sock;
	char from[32];
	char message[1000];
	char hostname[100];
	DifxMessageMk5Status dm;
	int n;

	if(argc > 1)
	{
		if(strcmp(argv[1], "-h") == 0 ||
		   strcmp(argv[1], "--help") == 0)
		{
			return usage(argv[0]);
		}
	}

	memset(&dm, 0, sizeof(DifxMessageMk5Status));
	dm.activeBank = ' ';

	difxMessageInit(-1, "mk5agent");
	difxMessagePrint();

	sock = openMultiCastSocket("224.2.2.1", 50201);

	for(;;)
	{
		dm.vsnA[0] = dm.vsnB[0] = 0;
		dm.activeBank = ' ';
		n = MultiCastReceive(sock, message, 999, from);
		if(n < 0)
		{
			continue;
		}
		if(strncmp(message, "VSN?", 4) == 0)
		{
			dm.state = getvsns(dm.vsnA, dm.vsnB);
			if(dm.vsnA[0] == 0)
			{
				strcpy(dm.vsnA, "none");
			}
			if(dm.vsnB[0] == 0)
			{
				strcpy(dm.vsnB, "none");
			}
			difxMessageSendMark5Status(&dm);
		}
		else if(strncmp(message, "QUIT", 4) == 0)
		{
			break;
		}
		else if(strncmp(message, "REBOOT", 6) == 0)
		{
			dm.state = MARK5_STATE_REBOOTING;
			system("/sbin/reboot");
		}
		else if(strncmp(message, "POWEROFF", 8) == 0)
		{
			dm.state = MARK5_STATE_POWEROFF;
			system("/sbin/poweroff");
		}
		else if(strncmp(message, "RESET", 5) == 0)
		{
			resetStreamstor(&dm);
		}
	}

	closeMultiCastSocket(sock);

	return 0;
}
