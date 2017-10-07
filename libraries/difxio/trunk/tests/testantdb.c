#include <stdio.h>
#include <difxio/antenna_db.h>

int main()
{
	unsigned int i;

	for(i = 0; ; ++i)
	{
		AntennaDBEntry *ae;

		ae = antennaDBGetByIndex(i);
		if(!ae)
		{
			break;
		}

		fprintAntennaDBEntry(stdout, ae);
	}

	return 0;
}
