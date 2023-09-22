#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>


const char program[] = "e2ecopy";
const char author[]  = "Walter Brisken";
const char version[] = "0.3";
const char verdate[] = "20120523";

const char defaultUser[] = "e2emgr";

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s %s\n\n",
		program, version, author, verdate);
	fprintf(stderr, "A program to copy files using e2emgr's owner/group\n\n");
	fprintf(stderr, "usage: %s [options] <fromdir> <todir> <file1>[:<newname1>] [<file2>[:<newname2>] ... ]\n\n", pgm);
	fprintf(stderr, "options can include:\n\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h         print this help info and quit\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v         be more verbose in operation\n\n");
	fprintf(stderr, "<fromdir> is the source directory of the file(s) to copy\n\n");
	fprintf(stderr, "<todir> is the destination directory\n\n");
	fprintf(stderr, "<fileN> is one of the files to copy\n\n");
	fprintf(stderr, "<newnameN> is an optional new name for the file\n\n\n");
	fprintf(stderr, "Note 1: this program is normally called directly from difxarch and not by hand.\n\n");
	fprintf(stderr, "Note 2: this program needs to be setuid root.\n\n");\

	exit(0);
}

int maketempdir(char *dest, const char *src)
{
	int last = -1;
	int i;

	for(i = 0; src[i]; i++)
	{
		if(src[i] == '/')
		{
			last = i;
		}
	}

	if(last < 0)
	{
		return -1;
	}

	for(i = 0; i < src[i]; ++i)
	{
		*dest = src[i];
		++dest;
		if(i == last)
		{
			*dest = '.';
			++dest;
		}
	}

	return 0;
}

int setperms(int verbose)
{
	const struct passwd *pw;
	const char *archiveUsername;
	int groupId, userId;

	archiveUsername = getenv("DIFX_ARCHIVE_USERNAME");
	if(!archiveUsername)
	{
		archiveUsername = defaultUser;
	}

	pw = getpwnam(archiveUsername);
	if(!pw)
	{
		fprintf(stderr, "e2ecopy: Error: cannot determine uid/gid of user %s.\n", archiveUsername);

		return -1;
	}
	userId = pw->pw_uid;
	groupId = pw->pw_gid;
	if(verbose)
	{
		printf("Archive User = %s  uid = %d  gid = %d\n", archiveUsername, userId, groupId);
	}

	if(setgid(groupId) != 0)
	{
		fprintf(stderr, "e2ecopy: Cannot set group id to %d\n", groupId);
		fprintf(stderr, "Make sure the e2ecopy executable is owned by root and is chmod +s\n");

		return -1;
	}

	if(setuid(userId) != 0)
	{
		fprintf(stderr, "e2ecopy: Cannot set user id to %d\n", userId);
		fprintf(stderr, "Make sure the e2ecopy executable is owned by root and is chmod +s\n");

		return -1;
	}

	return 0;
}

int main(int argc, char **argv)
{
	const int CommandLength = 1024;
	const int PathLength = 256;
	char *fromDir=0, *toDir=0;
	char cmd[CommandLength];
	char tmpDir[PathLength];
	char inFile[PathLength];
	char outPath[PathLength];
	int start=0;
	int verbose = 0;
	int i;
	const char *fromFile;
	const char *toFile;

	for(i = 1; i < argc; ++i)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "-v") == 0||
			   strcmp(argv[i], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[i], "-h") == 0 ||
			   strcmp(argv[i], "--help") == 0)
			{
				usage(argv[0]);
			}
			else
			{
				fprintf(stderr, "Unknown option: %s\n", argv[i]);

				exit(EXIT_FAILURE);
			}
		}
		else if(fromDir == 0)
		{
			fromDir = argv[i];
		}
		else if(toDir == 0)
		{
			toDir = argv[i];
		}
		else if(start == 0)
		{
			start = i;
		}
	}

	if(verbose)
	{
		printf("from = %s\n", fromDir);
		printf("to = %s\n", toDir);
	}

	if(start == 0)
	{
		usage(argv[0]);
	}

	if(setperms(verbose) != 0)
	{
		return -1;
	}

	sprintf(tmpDir, ".%s", toDir);
	if(maketempdir(tmpDir, toDir) < 0)
	{
		fprintf(stderr, "e2ecopy: Error: need fully qualified destination dir\n");

		exit(EXIT_FAILURE);
	}

	sprintf(cmd, "mkdir -p %s", tmpDir);
	if(verbose)
	{
		printf("Executing: %s\n", cmd);
	}
	system(cmd);

	for(i = start; i < argc; ++i)
	{
		int j;

		if(argv[i][0] == '-')
		{
			continue;
		}
		fromFile = argv[i];
		toFile = 0;
		for(j = 0; argv[i][j]; ++j)
		{
			if(argv[i][j] == ':')
			{
				argv[i][j] = 0;
				toFile = fromFile + j + 1;
			}
		}
		if(fromFile[0] == '/')
		{
			snprintf(inFile, PathLength, "%s", fromFile);
		}
		else
		{
			snprintf(inFile, PathLength, "%s/%s", fromDir, fromFile);
		}
		if(toFile)
		{
			snprintf(outPath, PathLength, "%s/%s", tmpDir, toFile);
		}
		else
		{
			snprintf(outPath, PathLength, "%s", tmpDir);
		}

		snprintf(cmd, CommandLength, "cp %s %s", inFile, outPath);

		if(verbose)
		{
			printf("Executing: %s\n", cmd);
		}
		system(cmd);
	}
 
	sprintf(cmd, "mv %s %s", tmpDir, toDir);
	if(verbose)
	{
		printf("Executing: %s\n", cmd);
	}
	system(cmd);

	sprintf(cmd, "chmod -R 600 %s/*", toDir);
	if(verbose)
	{
		printf("Executing: %s\n", cmd);
	}
	system(cmd);

	return EXIT_SUCCESS;
}
