#include <stdio.h>
#include <difxio/antenna_db.h>

void findxyz(double x, double y, double z)
{
	AntennaDBEntry *ae;

	ae = antennaDBGetByXYZ(x, y, z);
	if(!ae)
	{
		printf("\nNo antenna found near position (%3.1f, %3.1f, %3.1f)\n", x, y, z);
	}
	else
	{
		printf("\nAntenna found near (%3.1f, %3.1f, %3.1f):  ", x, y, z);
		fprintAntennaDBEntry(stdout, ae);
	}
}

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

	findxyz(-2388896.2, 5043350.1, -3078590.8);
	findxyz(2388896.2, 5043350.1, 3078590.8);

	return 0;
}
