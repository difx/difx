#ifndef __VSIS_COMMANDS_H__
#define __VSIS_COMMANDS_H__

#include "mk5daemon.h"

int DTS_id_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

#endif
