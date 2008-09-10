#include <stdio.h>
#include "difxmessage.h"

int main(int argc, char **argv)
{
	char message[256];
	int r;
	
	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	for(;;)
	{
		fgets(message, 255, stdin);
		if(feof(stdin)) break;
		message[255] = 0;
		r = difxMessageSend(message);
		if(r < 0)
		{
			break;
		}
	}

	return 0;
}
