#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <difxmessage.h>
#include <difxio/parsedifx.h>

const char program[] = "transient_daemon";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2010 Jul 22";

const char dispatcherProgram[] = "transient_dispatcher";
/* Command line params are: configfile jobid dmfile */

const char DMGeneratorProgram[] = "dmgen";
/* Command line params are: freq(MHz) [ chanBW(MHz) [ intTime ] ] */

int die = 0;

const int CommandLength = 256;

const char testMessage[] = 
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?><difxMessage><header><from>gui</from><to>boom</to><mpiProcessId>-1</mpiProcessId><identifier>boom_x</identifier><type>DifxStart</type></header><body><difxStart><input>/home/swc/difx/queue/BY129/by129-pos1_06.input</input><manager node=\"boom\"/><datastream nodes=\"boom1 boom2\"/><process threads=\"7\" nodes=\"boom3 boom4 boom4\"/><force>1</force><difxVersion>DIFX-1.5.3</difxVersion></difxStart></body></difxMessage>";

class TransientDaemonState
{
public:
	int verbose;
	int selfTest;
	int startEnable;
	char lastCommand[CommandLength];
	char hostname[DIFX_MESSAGE_PARAM_LENGTH];

	TransientDaemonState();
	void print() const;
};

TransientDaemonState::TransientDaemonState()
{
	verbose = 0;
	selfTest = 0;
	startEnable = 1;
	lastCommand[0] = 0;
	gethostname(hostname, DIFX_MESSAGE_PARAM_LENGTH);
}

void TransientDaemonState::print() const
{
	printf("TransientDaemonState [%s]:\n", hostname);
	printf("  verbose=%d\n", verbose);
	printf("  selfTest=%d\n", selfTest);
	printf("  startEnable=%d\n", startEnable);
	printf("  Last Command=%s\n", lastCommand);
}

int usage(const char *cmd)
{
	printf("\n");
	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("Usage:  %s  [options]\n\n", cmd);
	printf("Options can include:\n\n");
	printf("  --verbose\n");
	printf("  -v           Be more verbose in execution\n\n");
	printf("  --quiet\n");
	printf("  -q           Be quieter\n\n");
	printf("  --test\n");
	printf("  -t           Perform self test and quit (watch for weirdness)\n\n");
	printf("  --help\n");
	printf("  -h           Print this useful help information.\n\n");
	printf("  --disable\n");
	printf("  -d           Don't actually spawn any processes.\n\n");
	printf("To quite: Ctrl-c or send a sigint\n\n");

	return 0;
}

void siginthand(int j)
{
	die = 1;
}

static void generateIdentifier(const char *inputfile, int myID, char *identifier)
{
	int i, l, s=0;

	for(i = 0; inputfile[i]; i++)
	{
		if(inputfile[i] == '/')
		{
			s = i+1;
		}
	}

	if(inputfile[s] == 0)
	{
		s = 0;
	}

	strncpy(identifier, inputfile+s, DIFX_MESSAGE_IDENTIFIER_LENGTH-1);
	identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH-1] = 0;
	l = strlen(identifier);

	// strip off ".input"
	for(i = l-1; i > 0; i--)
	{
		if(identifier[i] == '.')
		{
			identifier[i] = 0;
			break;
		}
	}
}

/* This function needs to look through a .input file and extract some information.  Care
 * needs to be made to allow access to both DiFX 1.5 and 2.0 files.
 */
int getDMGenCommand(const char *inputFile, char *command)
{
	int r, v, nFreq, i;
	DifxParameters *dp;
	double freq, minFreq = 1.0e9;	/* MHz */

	dp = newDifxParametersfromfile(inputFile);
	if(!dp)
	{
		fprintf(stderr, "Cannot open %s for read\n", inputFile);
		return -1;
	}

	r = DifxParametersfind(dp, 0, "FREQ ENTRIES");
	if(r < 0)
	{
		fprintf(stderr, "Cannot find FREQ ENTRIES in %s\n", inputFile);
		return -2;
	}
	nFreq = atoi(DifxParametersvalue(dp, r));
	for(i = 0; i < nFreq; i++)
	{
		r = DifxParametersfind1(dp, r, "FREQ (MHZ) %d", i);
		if(r < 0)
		{
			fprintf(stderr, "Cannot find FREQ (MHZ) %d in %s\n", i, inputFile);
			return -3;
		}
		freq = atof(DifxParametersvalue(dp, r));
		if(freq < minFreq)
		{
			minFreq = freq;
		}
	}

	v = snprintf(command, CommandLength, "`%s %f`", DMGeneratorProgram, minFreq);
	if(v >= CommandLength)
	{
		fprintf(stderr, "Error: CommandLength=%d is too small.  Needs to be > %d\n",
			CommandLength, v);
		return -3;
	}

	return 0;
}

int runCommand(const char *command, int verbose)
{
	int pid;
	int v;

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

		v = system(command);

		if(verbose > 1)
		{
			printf("<Execution compted.  Return value was %d\n", v);
		}
		
		exit(0);
	}
}

static int isTrue(const char *str)
{
	if(strcmp(str, "True") == 0 || 
	   strcmp(str, "true") == 0 || 
	   strcmp(str, "1") == 0)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

static int handleMessage(const char *message, TransientDaemonState *state)
{
	DifxMessageGeneric G;
	char command[CommandLength];
	int v, pid;
	char identifier[DIFX_MESSAGE_IDENTIFIER_LENGTH];
	char dmGenCmd[CommandLength];

	difxMessageParse(&G, message);

	switch(G.type)
	{
	case DIFX_MESSAGE_STATUS:
		if(state->verbose > 1)
		{
			printf("%s: STATUS %s\n", G.identifier, DifxStateStrings[G.body.status.state]);
		}
		break;
	case DIFX_MESSAGE_START:
		if(state->startEnable)
		{
			state->lastCommand[0] = 0;
			if(state->verbose > 1)
			{
				printf("%s: START %s %s\n", G.identifier, G.body.start.inputFilename, G.body.start.difxVersion);
			}

			generateIdentifier(G.body.start.inputFilename, 0, identifier);
			v = getDMGenCommand(G.body.start.inputFilename, dmGenCmd);
			if(v != 0)
			{
				fprintf(stderr, "Error %d generating the DM command\n", v);
				return -1;
			}

			v = snprintf(command, CommandLength, "%s.%s %s %s %s\n", dispatcherProgram, G.body.start.difxVersion, G.body.start.inputFilename, identifier, dmGenCmd);
			if(v >= CommandLength)
			{
				fprintf(stderr, "Error: CommandLength=%d is too small (needs to be > %d)\n",
					CommandLength, v);
				return -2;
			}
			else
			{
				pid = runCommand(command, state->verbose);
				strcpy(state->lastCommand, command);
			}
		}
		else
		{
			if(state->verbose > 0)
			{
				printf("Start message received, but starting of processes is not enabled.  Doing nothing.\n");
			}
		}
		break;
	case DIFX_MESSAGE_PARAMETER:
		if(G.nTo == 1 && strcmp(G.to[0], state->hostname) == 0)
		{
			if(strcmp(G.body.param.paramName, "verbose") == 0)
			{
				state->verbose = atoi(G.body.param.paramValue);
			}
			else if(strcmp(G.body.param.paramName, "enable") == 0)
			{
				state->startEnable = isTrue(G.body.param.paramValue);
			}
			else if(strcmp(G.body.param.paramName, "restart") == 0)
			{
				runCommand(state->lastCommand, state->verbose);
			}
			else if(strcmp(G.body.param.paramName, "test") == 0)
			{
				state->selfTest = atoi(G.body.param.paramValue);
			}
			else if(strcmp(G.body.param.paramName, "die") == 0)
			{
				die = 1;
			}
			else
			{
				printf("Unknown Parameter %s received\n", G.body.param.paramName);
			}

			printf("DiFX Parameter Received: %s = %s\n", G.body.param.paramName, G.body.param.paramValue);
			state->print();
		}
		/* else ignore */
		break;
	case DIFX_MESSAGE_STOP:
		break;
	default:
		/* do nothing */
		break;
	}

	return 0;
}

int transientdaemon(TransientDaemonState *state)
{
	char message[DIFX_MESSAGE_LENGTH];
	char from[DIFX_MESSAGE_PARAM_LENGTH];
	typedef void (*sighandler_t)(int);
	sighandler_t oldsiginthand;
	int l, v, sock;

	oldsiginthand = signal(SIGINT, siginthand);

	difxMessageInit(-1, program);

	if(state->verbose > 0)
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
			if(state->selfTest > 0)
			{
				state->selfTest--;
				strcpy(message, testMessage);
			}
			
			usleep(100000);

			continue;
		}
		if(die)
		{
			if(state->verbose > 0)
			{
				printf("Caught sigint.  Will die now.\n");
			}

			break;
		}

		v = handleMessage(message, state);
		if(v)
		{
			fprintf(stderr, "Error=%d handling command %s\n", v, message);
		}
	}

	snprintf(message, DIFX_MESSAGE_LENGTH, "Transient Daemon stopping");
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);

	return 0;
}

int main(int argc, char **argv)
{
	int a;
	TransientDaemonState *state;

	state = new TransientDaemonState;

	for(a = 1; a < argc; a++)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--verbose") == 0 ||
				strcmp(argv[a], "-v") == 0)
			{
				state->verbose++;
			}
			else if(strcmp(argv[a], "--quiet") == 0 ||
				strcmp(argv[a], "-q") == 0)
			{
				state->verbose--;
			}
			else if(strcmp(argv[a], "--help") == 0 ||
				strcmp(argv[a], "-h") == 0)
			{
				return usage(argv[0]);
			}
			else if(strcmp(argv[a], "--test") == 0 ||
				strcmp(argv[a], "-t") == 0)
			{
				state->selfTest = 1;
			}
			else if(strcmp(argv[a], "--disable") == 0 ||
				strcmp(argv[a], "-d") == 0)
			{
				state->startEnable = 0;
			}
			else
			{
				fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n",
					argv[a]);

				return 0;
			}
		}
	}

	state->print();

	transientdaemon(state);

	delete state;

	return 0;
}
