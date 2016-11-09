#include <cstdio>
#include <cstring>
#include "old_filelist.h"
#include "utils.h"

int loadOldFileList(DirList &D, const char *fileName, std::stringstream &error)
{
	const int MaxLineLength = 255;
	FILE *in;
	char line[MaxLineLength+1];

	D.clear();
	D.setDefaultIdentifier();

	in = fopen(fileName, "r");
	if(!in)
	{
		error << "Cannot load file: " << fileName << "\n";

		return -1;
	}

	D.setParameter("class", "file", " Imported from filelist file");
	D.setParameter("version", 1);

	for(int s = 0; !feof(in); ++s)
	{
		DirListDatum*DD;
		char *v;
		bool ok;
		char dummy[257];

		v = fgetsNoCR(line, MaxLineLength, in);
		if(!v)
		{
			break;
		}

		for(int i = 0; line[i]; ++i)
		{
			if(line[i] == '#')
			{
				line[i] = 0;
				break;
			}
		}

		// check for line containing at least 3 items.  skip if not true
		if(sscanf(line, "%256s%256s%256s", dummy, dummy, dummy) != 3)
		{
			continue;
		}

		DD = new DirListDatum;
		D.addDatum(DD);
		ok = DD->setFromOldFileListString(line);
		if(!ok)
		{
			error << "Directory file: " << fileName << " is corrupt: at least one scan line is invalid.\n";
			fclose(in);

			return -1;
		}
	}
	fclose(in);

	D.organize();
	D.setPathPrefix();

	return 0;
}
