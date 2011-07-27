#include "recorr.h"

const bool RecorrJob::operator <(const RecorrJob& rj) const
{
	return priority < rj.priority;
}

RecorrQueue::RecorrQueue()
{
}

RecorrQueue::~RecorrQueue()
{
}

int RecorrQueue::add(std::string file)
{
	return jobs.size();
}

int RecorrQueue::load(std::string file)
{
	return jobs.size();
}
