#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include "logger.h"

static int Logger_newFile(Logger *log, int mjd)
{
	char filename[256];

	if(log->out)
	{
		fclose(log->out);
	}

	sprintf(filename, "%s/%s.%05d.log",
		log->logPath, log->hostName, mjd);

	log->out = fopen(filename, "a");

	return 0;
}

Logger *newLogger(const char *logPath)
{
	Logger *log;
	time_t t;
	int mjd;

	t = time(0);

	log = (Logger *)calloc(1, sizeof(Logger));

	strcpy(log->logPath, logPath);
	gethostname(log->hostName, 32);
	log->hostName[31] = 0;

	mjd = (int)(40587.0 + t/86400.0);

	Logger_newFile(log, mjd);

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
	double mjd;

	t = time(0);
	gmtime_r(&t, &curTime);
	
	pthread_mutex_lock(&log->lock);
	
	if(t != log->lastTime)
	{
		mjd = 40587.0 + t/86400.0;

		if(t/86400 != log->lastTime/86400)
		{
			Logger_newFile(log, (int)(mjd+0.1));
		}
		log->lastTime = t;
		fprintf(log->out, "\n");
		fprintf(log->out, "%04d/%03d %02d:%02d:%02d = %13.7f\n",
			curTime.tm_year+1900, curTime.tm_yday+1,
			curTime.tm_hour, curTime.tm_min, curTime.tm_sec,
			mjd);
	}
	fprintf(log->out, message);
	fflush(log->out);

	pthread_mutex_unlock(&log->lock);

	return 0;
}
