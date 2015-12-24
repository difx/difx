#include <iostream>
#include <dirlist.h>
#include <old_filelist.h>

int main(int argc, char **argv)
{
	DirList D;
	std::stringstream error;
	int v;

	for(int i = 1; i < argc; ++i)
	{
		error.clear();
		v = loadOldFileList(D, argv[i], error);

		std::cout << "Message from loading : " << error.str() << std::endl;

		if(v == 0)
		{
			D.print(std::cout);
		}
	}

	return 0;
}
