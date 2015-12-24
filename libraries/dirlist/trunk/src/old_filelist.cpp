#include <cstdio>
#include <cstring>
#include "old_filelist.h"

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

	D.setParameter("class", "file", "Imported from filelist file");
	D.setParameter("version", 1);

	for(int s = 0; !feof(in); ++s)
	{
		DirListDatum*DD;
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

	D.setExperiments();
	D.setStation();
	D.setPathPrefix();

	return 0;
}
