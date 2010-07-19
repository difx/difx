#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <difxmessage.h>

const char program[] = "transientdaemon";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2010 Jul 19";


const char dispatcherProgram[] = "transient_dispatcher";


int die = 0;


int usage(const char *cmd)
{
	printf("\n");
	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);

	return 0;
}

void siginthand(int j)
{
	die = 1;
}

int runCommand(const char *command, int verbose)
{
	int pid;

	if(verbose > 0)
	{
		printf("About to execute : %s \n", command);
	}
	pid = fork();
	if(pid)
	{
		return pid;
	}
	else
	{
		if(verbose > 1)
		{
			printf("<forked and now executing>\n");
		}
		system(command);
		if(verbose > 1)
		{
			printf("<executed and about to exit>\n");
		}
		exit(0);
	}
}

static int handleMessage(const char *message, int verbose)
{
	const int CommandLength = 256;
//	const int TimeLength = 32;
	DifxMessageGeneric G;
	char command[CommandLength];
	int v, pid;
//	char timestr[TimeLength];

	difxMessageParse(&G, message);

	switch(G.type)
	{
	case DIFX_MESSAGE_STATUS:
		if(verbose > 1)
		{
			printf("%s: STATUS %s\n", G.identifier, DifxStateStrings[G.body.status.state]);
		}
		break;
	case DIFX_MESSAGE_START:
		if(verbose > 1)
		{
			printf("%s: START %s %s\n", G.identifier, G.body.start.inputFilename, G.body.start.difxVersion);
		}
		v = snprintf(command, CommandLength, "%s.%s %s\n", dispatcherProgram, G.   body.start.difxVersion, G.body.start.inputFilename);
		if(v >= CommandLength)
		{
			fprintf(stderr, "Error: CommandLength=%d is too small (needs to be > %d)\n",
				CommandLength, v);
		}
		else
		{
			pid = runCommand(command, verbose);
		}

		break;
	case DIFX_MESSAGE_STOP:
		break;
	default:
		/* do nothing */
		break;
	}

	return G.type;
}

int transientdaemon(int verbose)
{
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_PARAM_LENGTH];
	typedef void (*sighandler_t)(int);
	sighandler_t oldsiginthand;
	int l, sock;

	oldsiginthand = signal(SIGINT, siginthand);

	difxMessageInit(-1, program);

	if(verbose > 0)
	{
		difxMessagePrint();
	}

	snprintf(message, DIFX_MESSAGE_LENGTH, "Transient Daemon starting");
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);

	sock = difxMessageReceiveOpen();

	for(;;)
	{
		from[0] = 0;
		l = difxMessageReceive(sock, message, DIFX_MESSAGE_LENGTH-1, from);
		if(l < 0)
		{
			usleep(100000);
			continue;
		}
		if(die)
		{
			if(verbose > 0)
			{
				printf("Caught sigint.  Will die now.\n");
			}
			break;
		}

		handleMessage(message, verbose);
	}

	snprintf(message, DIFX_MESSAGE_LENGTH, "Transient Daemon stopping");
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);

	return 0;
}

int main(int argc, char **argv)
{
	int verbose = 0;
	int a;

	for(a = 1; a < argc; a++)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--verbose") == 0 ||
				strcmp(argv[a], "-v") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[a], "--help") == 0 ||
				strcmp(argv[a], "-h") == 0)
			{
				return usage(argv[0]);
			}
			else
			{
				fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n",
					argv[a]);

				return 0;
			}
		}
	}

	transientdaemon(verbose);

	return 0;
}
