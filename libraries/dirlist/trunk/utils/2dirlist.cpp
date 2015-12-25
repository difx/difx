#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <dirlist.h>
#include <old_dirlist.h>
#include <old_filelist.h>

const char program[] = "2dirlist";
const char version[] = "0.1";
const char author[] = "Walter Brisken <wbrisken@nrao.edu>";
const char verdate[] = "2015 Dec 25";

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "Usage: %s [options] <inputFile> <outputFile>\n\n", pgm);
	fprintf(stderr, "Options can include:\n\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h      print this useful help information and quit\n\n");
	fprintf(stderr, "Currently input files can be from Mark5 .dir file or a file list\n");
	fprintf(stderr, "output file from vsum or mk5bsum.\n\n");

	return EXIT_SUCCESS;
}

int convert(const char *inputFile, const char *outputFile)
{
	DirList D;
	std::stringstream error;
	std::fstream out;
	int v;

	error.clear();
	v = loadOldDirList(D, inputFile, error);
	if(v != 0)
	{
		error.clear();
		v = loadOldFileList(D, inputFile, error);
	}
	if(v != 0)
	{
		fprintf(stderr, "\nInput file is unrecognizable and wasn't parse.\n\n");
		
		return EXIT_FAILURE;
	}

	out.open(outputFile, std::fstream::out);
	if(!out.is_open())
	{
		fprintf(stderr, "\nCannot open %s for write\n\n", outputFile);
	}
	D.print(out);
	out.close();

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	const char *inputFile = 0;
	const char *outputFile = 0;

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	for(int a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "--help") == 0 || strcmp(argv[a], "-h") == 0)
			{
				return usage(argv[0]);
			}
			else
			{
				fprintf(stderr, "\nUnrecongnized option: %s\n\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
		else if(inputFile == 0)
		{
			inputFile = argv[a];
		}
		else if(outputFile == 0)
		{
			outputFile = argv[a];
		}
		else
		{
			fprintf(stderr, "\nUnexpected parameter: %s\n\n", argv[a]);

			return EXIT_FAILURE;
		}
	}

	if(!outputFile)
	{
		fprintf(stderr, "\nIncomplete command line.\n\n");

		return EXIT_FAILURE;
	}

	return convert(inputFile, outputFile);
}
