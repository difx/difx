#include <iostream>
#include <fstream>
#include "dirlist.h"
#include "old_dirlist.h"
#include "dirlist_exception.h"

int main(int argc, char **argv)
{
	if(argc != 3)
	{
		std::cerr << "Usage: " << argv[0] << " <infile> <outfile>" << std::endl;
	}
	else
	{
		DirList D;

		try
		{
			std::fstream out;

			D.load(argv[1]);

			out.open(argv[2], std::fstream::out);
			if(!out.is_open())
			{
				std::cerr << "Cannot open " << argv[2] << " for write." << std::endl;
			}
			else
			{
				D.print(out);
				out.close();
			}
		}
		catch (DirListException &e)
		{
			std::cerr << "Error: " << e.what() << std::endl;
		}
	}

	return 0;
}
