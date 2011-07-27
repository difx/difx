#ifndef __RECORR_H__
#define __RECORR_H__

#include <queue>
#include <string>

class RecorrJob
{
public:
	double priority;
	std::string inputFile;

	const bool operator <(const RecorrJob& rj) const;
};

class RecorrQueue
{
public:
	std::priority_queue<RecorrJob> jobs;

	RecorrQueue();
	~RecorrQueue();
	int add(std::string file);	// add job to queue
	int load(std::string file);	// load queue from file
};



#endif
