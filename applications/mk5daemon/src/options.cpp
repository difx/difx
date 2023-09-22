#include <cstring>

#include "mk5daemon.h"

using namespace std;
/**
 * Constructor
 **/
Options::Options(void)
{
 	isMk5 = 0;
        isMk6 = 0;
        isHeadNode = 0;
        isEmbedded = 0;
        noSu = 0;
        userID = "difx";
        providedHostname = 0;
        logPath ="/tmp";

}

void Options::setDefaults()
{
	// check for DIFX_LOG_PATH environment
	
	
}

/**
 * Performs sanity checks on the user supplied options
 **/
int Options::validate()
{
	// check logpath length
	if(strlen(logPath) >= MAX_FILENAME_SIZE)
	{
		fprintf(stderr, "Error: logpath is longer than %d chars.\n", MAX_FILENAME_SIZE);
		return -1;
	}

	// check length of userID
	if(strlen(userID) >= MAX_USERID_LENGTH)
	{
		fprintf(stderr, "Error: userID is longer than %d chars.\n", MAX_USERID_LENGTH);
		return -1;
	}
	

	return 0;
}
