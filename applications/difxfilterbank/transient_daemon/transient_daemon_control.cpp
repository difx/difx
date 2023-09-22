#include <stdio.h>
#include <string.h>
#include <difxmessage.h>

const char program[] = "transient_daemon_control";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2010 Jul 23";


const char destination[] = "boom";

int main(int argc, char **argv)
{
	difxMessageInit(-1, program);

	if(argc == 3)
	{
		difxMessageSendDifxParameterTo(argv[1], argv[2], destination);
	}

	if(argc == 2)
	{
		difxMessageSendDifxParameterTo(argv[1], "", destination);
	}

	return 0;
}
