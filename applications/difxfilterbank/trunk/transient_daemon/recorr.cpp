#include <iostream>
#include <fstream>
#include <cstdio>
#include <difxmessage.h>
#include "recorr.h"

const bool RecorrJob::operator <(const RecorrJob& rj) const
{
	return priority < rj.priority;
}

RecorrQueue::RecorrQueue(const std::string file)
{
	queueFile = file;
}

RecorrQueue::~RecorrQueue()
{
	save();
}

int RecorrQueue::add(std::string file, double threshold)
{
	std::ifstream ifs;
	std::string line;
	char A[DIFX_MESSAGE_PARAM_LENGTH];
	char B[DIFX_MESSAGE_FILENAME_LENGTH];
	RecorrJob job;
	int n;

	ifs.open(file.c_str());
	if(!ifs.good())
	{
		printf("Cannot open event file %s\n", file.c_str());
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
		jobs.push_back(job);
		printf("Added %f %s\n", job.priority, job.inputFile);
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
	}
	while(ifs.good())
	{
		getline(ifs, line);
		snprintf(cline, DIFX_MESSAGE_FILENAME_LENGTH, "%s", line.c_str());
		n = sscanf(cline, "%lf%s", &job.priority, job.inputFile);
		if(n == 2)
		{
			jobs.push_back(job);
			printf("Loaded %f %s\n", job.priority, job.inputFile);
		}
	}
	ifs.close();

	return jobs.size();
}

int RecorrQueue::save()
{
	std::ofstream ofs;
	std::vector<RecorrJob>::const_iterator j;

	ofs.open(queueFile.c_str());
	for(j = jobs.begin(); j != jobs.end(); j++)
	{
		ofs << j->priority << " " << j->inputFile << std::endl;
	}
	ofs.close();

	printf("Saved recorr queue : %s\n", queueFile.c_str());

	return jobs.size();
}
