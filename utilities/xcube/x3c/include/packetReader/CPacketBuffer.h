/*
 * CPacketBuffer.h
 *
 *	Description:	this file defines the class of packet buffer.
 *  Created on:	 	Nov 18, 2011
 *      Author: 	mtaveniku
 *
 */

#ifndef CPACKETBUFFER_H_
#define CPACKETBUFFER_H_

#include <stddef.h>
#include <semaphore.h>
#include <unistd.h>
#include <vector>
using namespace std;

//#include "X3cPacketTypes.h"
#include "X3cPacket.h"
//! Class of packet buffer
/*! The packet buffer class is designed to accommodate one reader and one
 * 	writer process to insert chunks of chunkSz data to the buffer (when
 *  there is space for it) and read X3C packet size chunks of data out of
 *  the buffer when available
 */
class CPacketBuffer {
public:

	//! Constructor
	/*! Creates a new instance of packet buffer, clean all data fields
	 * 	initialize unnamed semaphores                                        */
	CPacketBuffer();

	//! De-constructor
	/*! Detach all shared memory and destroy any unnamed semaphores          */
	virtual ~CPacketBuffer();

	//! Initialize the buffer and set it to a bufSz bytes.
	/*! If buffer is already initialized the old allocation is deleted
	 * 	and all processes using the buffer will have undefined results if
	 * 	using old pointers.
	 *
	 * 	@param bufSz		-	the requested size in bytes
	 * 	@return				-	true if successful                           */
	bool initialize(size_t bufSz);

	//! Check whether packet buffer can be written in nByte data
	/*! Call to the buffer to see if there is enough space in it to write a
	 *  chunk of data. This call can optionally block until such time that
	 *  data is available
	 * 	@param nBytes		-	the number of bytes requested to write
	 * 	@param isBlock		- 	set to true if caller wants to block
	 * 							this call
	 * 	@return 			- 	true if it is ok to write a chunk of
	 * 							data to buffer, or return false              */
	bool isOkToWrite(size_t nByte, bool isBlock);

	//! Get the current write pointer for the buffer
	/*! @return		-	first empty byte in the buffer                       */
	char *getWrPtr();

	//! Add bytes written in the buffer
	/*! when a write to the buffer has been done this function is called to
	 * 	update the valid bytes in the buffer
	 * 	@param nBytes		-	Number of bytes actually written             */
	void addBytesWritten(size_t nBytes);

	//! Set to indicate end of input stream us reached
	/*! The disk writer will call this if the file-reader has reached end
	 * 	of stream or an unrecoverable error condition                        */
	void setEOF();

	//! Query whether the end of input stream is reached
	/*! If end of file is reached in the reader, return true
	 * 	@return		-	true if reader has reached end of stream             */
	bool hasEOF();

	//! Query how many bytes available on this packet buffer
	/*! This is how many free bytes the buffer has, it is always used to
	 * 	judge whether a chunk of data is OK to be written into the buffer
	 *	@return		-	number of empty byte locations in buffer             */
	size_t getFreeBytes();

	//! Query how many valid bytes are there on this packet buffer
	/*! This is how many valid data bytes the buffer currently has
	 * @return		-	number of valid bytes in buffer                      */
	size_t getValidBytes();

	//! Get a pointer to the first packet in the buffer
	/*! @return	-	a pointer to the first X3C packet in the buffer
	 * 				null if not enough data                                  */
	X3cPacket *getPacket();

	//! Return a pointer to a number of bytes in the buffer
	/*! @param nbytes		-	number of bytes requested in array
	 * 							(updates buffer state)
	 * 	@return				-	a pointer to a char[] of nByte size          */
	char *getBytes(size_t nbytes);

	//! Free the number of bytes in the buffer pointed to by ptr.
	/*! Important: The free pointer in the buffer is moved to the last
	 *  position of this packet, this means that all previous packets are
	 *  freed to, it is the responsibility of the calling application to
	 *  free bytes in the buffer up to the last item they want, no checks
	 *  are done here.
	 * 	@param ptr		-	pointer to start of array to free
	 * 	@param nBytes	-	number of bytes in this segment                  */
	void freeBytes(char *ptr, size_t nBytes);

	//! Reset the packet buffer to empty and clean state
	void reset();

	//! Blocking call waiting for a read to happen
	void waitForRead();

	//! Blocking call waiting for a write to happen
	void waitForWrite();


private:

	//! Allocates shared memory for packet buffer
	/*! Allocates a memory buffer with aliased memory space and updates the
	 * 	internal pointers, if successful it returns true.
	 * 	@param bufSz - the buffer size in MByte to allocate
	 * 	@return - true if successful                                         */
	void *allocSharedMemory(size_t bufSz);

	//! internal shared memory segment for buffer
	void *mShmSeg1;

  	//! second segment pointing to same area
	void *mShmSeg2;

    //! identity of the shared memory segment
	int   mShmId;

  	//! Size of the buffer in bytes
	size_t mBufSz;

   	//! actual start of buffer
	char *mPhyStart;

  	//! current first empty space in buffer
	size_t mWrIdx;

	//! current first unread byte in buffer
	size_t mRdIdx;

 	//! index to the last free position in the buffer
	size_t mFreeIdx;

  	//! indicates to internal functions if object is OK to use
	bool mIsOk;

	//! set to indicate end of input stream is reached
	bool mIsEOF;

	 //! total bytes input to buffer
	unsigned long long mBytesWritten;

	//! total bytes read from buffer
	unsigned long long mBytesRead;

	//! indicates a full buffer
	bool mIsFull;

	//! indicates a writer is waiting for free space
	bool mWriterIsWaiting;

	//! used for waiting for permission to write to buffer
	sem_t mWrSem;

	//! indicate that someone is waiting for  write
	bool mReaderIsWaiting;

	//! semaphore for reading thread waiting for write
	sem_t mRdSem;

	//! timeout for wait for RD
	const static unsigned mRdWTo = 2;

	//! timeout for wait for WR
	const static unsigned mWrWTo = 2;

	vector<int> mBadShrBuf;
	vector<void*> mAreas;
};

#endif /* CPACKETBUFFER_H_ */
