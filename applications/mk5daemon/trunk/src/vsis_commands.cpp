#include <cstdio>
#include "config.h"
#include "vsis_commands.h"

int DTS_id_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength)
{
	return snprintf(response, maxResponseLength, "!DTS_id? 0 : mk5daemon : %s : %s : 1", VERSION, D->hostName);
}
