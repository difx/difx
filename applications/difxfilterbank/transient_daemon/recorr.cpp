#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstring>
#include <difxmessage.h>
#include "recorr.h"

const bool RecorrJob::operator <(const RecorrJob& rj) const
{
	return priority < rj.priority;
}

RecorrQueue::RecorrQueue(const std::string &file) : queueFile(file)
{
	pthread_mutex_init(&lock, NULL);
	die = -1;
}

RecorrQueue::~RecorrQueue()
{
	save();
}

int RecorrQueue::add(const char *file, double threshold)
{
	std::ifstream ifs;
	std::string line;
	char A[DIFX_MESSAGE_PARAM_LENGTH];
	char B[DIFX_MESSAGE_FILENAME_LENGTH];
	RecorrJob job;
	int n;

	ifs.open(file);
	if(!ifs.good())
	{
		printf("Cannot open event file %s\n", file);
		fflush(stdout);

		return -1;
	}
	while(ifs.good())
	{
		getline(ifs, line);
		n = sscanf(line.c_str(), "%s%s", A, B);
		if(n == 2)
		{
			if(strcmp(A, "job") == 0)
			{
				snprintf(job.inputFile, DIFX_MESSAGE_FILENAME_LENGTH, "%s", B);
			}
			else if(strcmp(A, "priority") == 0)
			{
				job.priority = atof(B);
			}
		}
	}
	ifs.close();

	if(job.inputFile[0] && job.priority > threshold)
	{
		pthread_mutex_lock(&lock);
		jobs.push_back(job);
		pthread_mutex_unlock(&lock);

		printf("Added %f %s\n", job.priority, job.inputFile);
		fflush(stdout);
	}
	else
	{
		printf("Did not add job in %s.  Threshold was %f\n", file, threshold);
		fflush(stdout);
	}

	save();

	return jobs.size();
}

int RecorrQueue::load()
{
	std::ifstream ifs;
	std::string line;
	char cline[DIFX_MESSAGE_FILENAME_LENGTH];
	RecorrJob job;
	int n;

	ifs.open(queueFile.c_str());
	if(!ifs.good())
	{
		printf("Cannot open queue file %s\n", queueFile.c_str());

		return -1;
	}
	pthread_mutex_lock(&lock);
	while(ifs.good())
	{
		getline(ifs, line);
		snprintf(cline, DIFX_MESSAGE_FILENAME_LENGTH, "%s", line.c_str());
		n = sscanf(cline, "%lf%s", &job.priority, job.inputFile);
		if(n == 2)
		{
			jobs.push_back(job);
			printf("Loaded %f %s\n", job.priority, job.inputFile);
			fflush(stdout);
		}
	}
	pthread_mutex_unlock(&lock);
	ifs.close();

	return jobs.size();
}

int RecorrQueue::save()
{
	std::ofstream ofs;
	std::vector<RecorrJob>::const_iterator j;

	pthread_mutex_lock(&lock);
	ofs.open(queueFile.c_str());
	for(j = jobs.begin(); j != jobs.end(); ++j)
	{
		ofs << j->priority << " " << j->inputFile << std::endl;
	}
	ofs.close();
	pthread_mutex_unlock(&lock);

	printf("Saved recorr queue : %s\n", queueFile.c_str());
	fflush(stdout);

	return jobs.size();
}

static void *runRecorrQueue(void *data)
{
	RecorrQueue *queue;

	queue = reinterpret_cast<RecorrQueue *>(data);

	while(!queue->die)
	{
	}

	return 0;
}

int RecorrQueue::start()
{
	if(die >= 0)
	{
		// already running?

		return -1;
	}

	die = 0;

	pthread_create(&thread, NULL, runRecorrQueue, this);

	return 0;
}

int RecorrQueue::stop()
{
	if(die == 0)
	{
		die = 1;
	}

	pthread_join(thread, 0);

	return 0;
}
