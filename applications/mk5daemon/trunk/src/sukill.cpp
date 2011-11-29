#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "proc.h"

const char program[] = "sukill";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20111126";


void usage()
{
	printf("\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	printf("A program to kill rampant su processes\n\n");
	printf("\nUsage : %s [<options>] [<dt>]\n\n", program);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             Print this help message\n\n");
	printf("  --verbose\n");
	printf("  -v             Be more verbose\n\n");
	printf("  --quiet\n");
	printf("  -q             Be less verbose\n\n");
	printf("<dt> is an option parameter specifying interval in seconds between attempts.\n");
	printf("     If not provided, the program will try once and terminate.\n\n");
}

int main(int argc, char **argv)
{
	int verbose = 1;
	int n;
	int nKill = 0;
	int dt = -1;

	for(int a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage();

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-q") == 0 ||
			   strcmp(argv[a], "--quiet") == 0)
			{
				--verbose;
			}
			else
			{
				fprintf(stderr, "Unknown option: %s\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
		else
		{
			if(dt >= 0)
			{
				fprintf(stderr, "Unexpected argument: %s\n", argv[a]);
			
				return EXIT_FAILURE;
			}
			if(sscanf(argv[a], "%d", &dt) != 1)
			{
				fprintf(stderr, "Error turning %s into an integer\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
	}

	for(;;)
	{
		n = killSuProcesses(verbose);
	
		nKill += n;

		if(dt < 0)
		{
			break;
		}
		else
		{
			if(verbose > 1)
			{
				if(n > 0)
				{
					printf("+");
				}
				else
				{
					printf(".");
				}
				fflush(stdout);
			}
			sleep(dt);
		}
	}

	printf("nKill = %d\n", nKill);

	return 0;
}
