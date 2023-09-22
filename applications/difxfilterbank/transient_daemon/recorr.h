#ifndef __RECORR_H__
#define __RECORR_H__

#include <vector>
#include <algorithm>
#include <string>
#include <pthread.h>
#include <difxmessage.h>

class RecorrJob
{
public:
	double priority;
	char inputFile[DIFX_MESSAGE_FILENAME_LENGTH];

	RecorrJob() { priority = 0; inputFile[0] = 0; }

	const bool operator <(const RecorrJob& rj) const;
};

class RecorrQueue
{
public:
	std::vector<RecorrJob> jobs;
	std::string queueFile;
	int die;	// -1 for idle, 0 for running, 1 for request to die
	pthread_t thread;

	pthread_mutex_t lock;
	RecorrQueue(const std::string &file);
	~RecorrQueue();
	int add(const char *file, double threshold);	// add job to queue
	int load();	// load queue from file
	int save();
	int start();	// start the background procesing
	int stop();	// stop the backgroun processing
};



#endif
