#ifndef __LOGGER_H__
#define __LOGGER_H__

#include <stdio.h>
#include <pthread.h>

typedef struct
{
	FILE *out;
	time_t lastTime;
	pthread_mutex_t lock;
	char logPath[256];
	char hostName[32];
} Logger;

Logger *newLogger(const char *logPath);
void deleteLogger(Logger *log);
int Logger_logData(Logger *log, const char *message);

#endif
