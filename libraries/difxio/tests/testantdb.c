#include <stdio.h>
#include <string.h>
#include <difxio/antenna_db.h>

enum PrintFormat
{
	PrintFormatHuman = 0,
	PrintFormatMachine,
	PrintFormatCSV
};

void findxyz(double x, double y, double z)
{
	const AntennaDBEntry *ae;

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

int main(int argc, char **argv)
{
	unsigned int i;
	int a;
	int format=PrintFormatHuman;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "--human") == 0)
		{
			format = PrintFormatHuman;
		}
		else if(strcmp(argv[a], "-m") == 0 || strcmp(argv[a], "--machine") == 0)
		{
			format = PrintFormatMachine;
		}
		else if(strcmp(argv[a], "-c") == 0 || strcmp(argv[a], "--csv") == 0)
		{
			format = PrintFormatCSV;
		}
	}

	for(i = 0; ; ++i)
	{
		const AntennaDBEntry *ae;

		ae = antennaDBGetByIndex(i);
		if(!ae)
		{
			break;
		}

		if(format == PrintFormatHuman)
		{
			fprintAntennaDBEntry(stdout, ae);
		}
		else if(format == PrintFormatMachine)
		{
			double lat, lon, alt; /* rad, rad, m */

			ecef2lla(&lat, &lon, &alt, ae->x, ae->y, ae->z);

			printf("%5.3f %5.3f %5.3f  %5.6f %5.6f %5.3f  %5.2f # %s %s\n", ae->x, ae->y, ae->z, lat, lon, alt, ae->diameter, ae->name, ae->ivsName);
		}
		else if(format == PrintFormatMachine)
		{
			double lat, lon, alt; /* rad, rad, m */

			ecef2lla(&lat, &lon, &alt, ae->x, ae->y, ae->z);

			printf("%s,%s,%5.3f,%5.3f,%5.3f,%5.6f,%5.6f,%5.3f,%5.2f\n", ae->name, ae->ivsName, ae->x, ae->y, ae->z, lat, lon, alt, ae->diameter);
		}
	}

	if(format == PrintFormatHuman)
	{
		findxyz(-2388896.2, 5043350.1, -3078590.8);
		findxyz(2388896.2, 5043350.1, 3078590.8);
	}

	return 0;
}
