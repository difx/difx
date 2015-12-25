#include <cstdio>
#include <cstring>
#include "old_dirlist.h"
#include "dirlist_datum_mark5.h"

// Imports data from a legacy (Mark5) .dir file
// returns an error code
int loadOldDirList(DirList &D, const char *fileName, std::stringstream &error)
{
	const int MaxLineLength = 255;
	FILE *in;
	char line[MaxLineLength+1];
	int i, j, nscans, n;
	char *v;
	char bankName[2];
	char dirLabel[MaxLineLength];
	char extra[5][20];
	int nNumber = 0;
	unsigned int signature;

	D.clear();
	D.setDefaultIdentifier();

	in = fopen(fileName, "r");
	if(!in)
	{
		error << "Cannot load file: " << fileName << "\n";

		return -1;
	}

	v = fgets(line, MaxLineLength, in);
	if(!v)
	{
		error << "Directory file: " << fileName << " is corrupt.\n";
		fclose(in);

		return -1;
	}

	n = sscanf(line, "%8s %d %1s %u %19s %19s %19s %19s %19s",
		dirLabel, &nscans, bankName, &signature, extra[0], extra[1], extra[2], extra[3], extra[4]);
	if(n < 3)
	{
		error << "Directory file: " << fileName << " is corrupt.\n";
		fclose(in);

		return -1;
	}
	if(n == 3)
	{
		signature = ~0;
	}
	D.setParameter("class", "mark5", "Imported from .dir file");
	D.setParameter("version", 1);
	D.setParameter("vsn", dirLabel);
	D.setParameter("hash", signature);
	D.setParameter("realtime", "false");
	D.setParameter("bankname", bankName);

	for(j = 4; j < n; ++j)
	{
		if(strcmp(extra[j-4], "RT") == 0)
		{
			D.setParameter("realtime", "true");
		}
		else if(strcmp(extra[j-4], "Fast") == 0)
		{
			D.setParameter("fastdir", "true");
		}
		else if(strcmp(extra[j-4], "Synth") == 0)
		{
			D.setParameter("synthetic", "true");
		}
		else if(sscanf(extra[j-4], "%d", &i) == 1)
		{
			switch(nNumber)
			{
			case 0:
				D.setParameter("mark5DirVersion", i);
				break;
			case 1:
				D.setParameter("mark5DirSubversion", i);
				break;
			}
			++nNumber;
		}
	}

	if(nscans < 0)
	{
		error << "Directory file: " << fileName << " is corrupt (nscans < 0).\n";
		fclose(in);

		return -1;
	}

	for(int s = 0; !feof(in); ++s)
	{
		DirListDatumMark5 *DM5;
		char *v;
		bool ok;

		v = fgets(line, MaxLineLength, in);
		if(!v)
		{
			break;
		}

		// strip CR/LF chars from end
		for(unsigned int i = 0; line[i]; ++i)
		{
			if(line[i] < ' ')
			{
				line[i] = 0;

				break;
			}
		}
		
		DM5 = new DirListDatumMark5;
		D.addDatum(DM5);
		ok = DM5->setFromOldString(line);
		if(!ok)
		{
			error << "Directory file: " << fileName << " is corrupt: at least one scan line is invalid.\n";
			fclose(in);

			return -1;
		}
	}
	fclose(in);

	D.sort();
	D.setStationAndExperiments();

	return 0;
}
