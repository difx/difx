#ifndef __VSIS_COMMANDS_H__
#define __VSIS_COMMANDS_H__

#include "mk5daemon.h"

int DTS_id_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int packet_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int packet_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int bank_set_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);
int bank_set_Command(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int SS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

int OS_rev_Query(Mk5Daemon *D, int nField, char **fields, char *response, int maxResponseLength);

#endif
