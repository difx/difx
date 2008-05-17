#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "logger.h"

static int Logger_newFile(Logger *log, const struct tm *startTime)
{
	char filename[256];
	
	if(log->out)
	{
		fclose(log->out);
	}
	
	sprintf(filename, "/tmp/%04d_%03d.mark5log",
		startTime->tm_year+1900, startTime->tm_yday+1);

	log->out = fopen(filename, "a");

	return 0;
}

Logger *newLogger()
{
	Logger *log;
	time_t t;
	struct tm startTime;

	t = time(0);
	gmtime_r(&t, &startTime);

	log = (Logger *)calloc(1, sizeof(Logger));

	Logger_newFile(log, &startTime);

	pthread_mutex_init(&log->lock, 0);

	return log;
}

void deleteLogger(Logger *log)
{
	if(log)
	{
		pthread_mutex_destroy(&log->lock);
		if(log->out)
		{
			fclose(log->out);
		}
		free(log);
	}
}

int Logger_logData(Logger *log, const char *message)
{
	time_t t;
	struct tm curTime;

	t = time(0);
	gmtime_r(&t, &curTime);
	
	pthread_mutex_lock(&log->lock);
	
	if(t != log->lastTime)
	{
		if(t/86400 != log->lastTime/86400)
		{
			Logger_newFile(log, &curTime);
		}
		log->lastTime = t;
		fprintf(log->out, "\n");
		fprintf(log->out, "%04d/%03d %02d:%02d:%02d\n",
			curTime.tm_year+1900, curTime.tm_yday+1,
			curTime.tm_hour, curTime.tm_min, curTime.tm_sec);
	}
	fprintf(log->out, message);
	fflush(log->out);

	pthread_mutex_unlock(&log->lock);
}
