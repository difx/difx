#include <cstdio>
#include "sharedmemorybuffer.h"

SharedMemoryBuffer *attachSharedMemoryBuffer(key_t shmKey)
{
	SharedMemoryBuffer *buffer;
	int shmId;
	int64_t totalSize;

	shmId = shmget(shmKey, sizeof(SharedMemoryBuffer), 0666);
	if(shmId < 0)
	{
		fprintf(stderr, "attachSharedMemoryBuffer: shmget failed (1)\n");

		return 0;
	}
	buffer = (SharedMemoryBuffer *)shmat(shmId, 0, 0);
	if(buffer == 0 || buffer == (void *) -1)
	{
		fprintf(stderr, "attachSharedMemoryBuffer: shmat failed (1)\n");

		return 0;
	}
	totalSize = sizeof(SharedMemoryBuffer) + buffer->size;
	shmdt(buffer);

	shmId = shmget(shmKey, totalSize, 0666);
	if(shmId < 0)
	{
		fprintf(stderr, "attachSharedMemoryBuffer: shmget failed (2)\n");

		return 0;
	}
	buffer = (SharedMemoryBuffer *)shmat(shmId, 0, 0);
	if(buffer == 0 || buffer == (void *) -1)
	{
		fprintf(stderr, "attachSharedMemoryBuffer: shmat failed (2)\n");

		return 0;
	}

	return buffer;

}

void printSharedMemoryBuffer(const SharedMemoryBuffer *buffer)
{
	int i;

	printf("SharedMemoryBuffer [%p]\n", buffer);
	printf("  data = [%p]\n", buffer->data);
	printf("  size = %lld bytes\n", (long long int)(buffer->size));
	printf("  nSecond = %d\n", buffer->nSecond);
	for(i = 0; i < buffer->nSecond; ++i)
	{
		printf("    %d %d %d\n", buffer->second[i], buffer->startFrame[i], buffer->endFrame[i]);
	}
	printf("  packetSize = %lld\n", (long long int)buffer->packetSize);
	printf("  packetsPerSecond = %lld (frames * threads)\n", (long long int)buffer->packetsPerSecond);
	printf("  nThread = %d\n", buffer->nThread);
	printf("  stripSize = %d\n", buffer->stripSize);
	printf("  shmKey = %d\n", buffer->shmKey);
	printf("  shmId = %d\n", buffer->shmId);
}

void detachSharedMemoryBuffer(SharedMemoryBuffer *buffer)
{
	if(buffer)
	{
		shmdt(buffer);
	}
}

