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

	pthread_mutex_t lock;
	RecorrQueue(const std::string file);
	~RecorrQueue();
	int add(std::string file, double threshold);	// add job to queue
	int load();	// load queue from file
	int save();
};



#endif
