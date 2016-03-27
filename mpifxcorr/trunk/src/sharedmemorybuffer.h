#ifndef __SHARED_MEMORY_BUFFER_H__
#define __SHARED_MEMORY_BUFFER_H__

#include <cstdlib>
#include <stdint.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#define SHM_BUFFER_MAX_SECONDS	60

typedef struct
{
	int64_t size;
	int64_t packetSize;
	int64_t packetsPerSecond;
	int32_t stripSize;	// bytes to strip from start of each packet
	int32_t nSecond;	// length of buffer in seconds; should divide 86400
	int32_t second[SHM_BUFFER_MAX_SECONDS];	// second of day contained in each of the Second slots
	int32_t startFrame[SHM_BUFFER_MAX_SECONDS];	// first valid frame found in this slot
	int32_t endFrame[SHM_BUFFER_MAX_SECONDS];	// last valid frame found in this slot
	int32_t nThread;
	int32_t shmKey;
	int32_t shmId;
	char data[0];
} SharedMemoryBuffer;

SharedMemoryBuffer *attachSharedMemoryBuffer(key_t shmKey);

void detachSharedMemoryBuffer(SharedMemoryBuffer *buffer);

void printSharedMemoryBuffer(const SharedMemoryBuffer *buffer);

#endif
