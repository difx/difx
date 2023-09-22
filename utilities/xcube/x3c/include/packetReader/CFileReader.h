/*
 * CFileReader.h
 *
 *  Created on: Nov 21, 2011
 *      Author: mtaveniku
 *
 *      This file defines class of file reader. A file reader is responsible
 *      for reading data from file to the packet buffer.
 */

#ifndef CFILEREADER_H_
#define CFILEREADER_H_

// C headers
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <string>

// Project Headers
#include "X3cPacket.h"
#include "X3cFileBuf.h"
#include "exceptions/IoException.h"
#include "CPacketBuffer.h"

//! Class definition of file reader
/*! this CFileReder class provides functions to read data from x3c files
 * 	into buffers which can be used for other purpose, such as replay
 *  data, restore data... Each file reader has its own packet buffer to
 *  write data to and it is also associated with one x3c file which it
 *  reads data from. As long as user start a file reader, it will open
 *  its x3c file and read data into its packet buffer until packet buffer
 *  is full, then it will wait until packet buffer is available for next
 *  chunk of data. This process can be terminated by a stop call or until
 *  it reaches the end of its x3c file.
 */
class CFileReader{

public:
	//! Constructor
	/*! Create a new File Reader that reads from the file "infile" to the
	 * 	packet buffer and have the preferred read set to 'chunk size'
	 *	@param inFileName - the full filename of the stream to read from
	 *	@param pktBuf - the packet buffer to write to
	 *	@param chunkSz - the preferred read size in MByte
	 */
	CFileReader(const char * inFileName, CPacketBuffer *pktBuf, size_t chunkSz);

	//! De-constructor
	/*! Clean a file reader, close the file if it is not closed,
	 *  clean the file name pointer if it is not null
	 */
	virtual ~CFileReader();

	//! Initialize file reader
	/*!	Initialize the file reader and start reading at offset.
	 * 	Can only be called once
	 * 	@param offset		-	the offset to start reading
	 */
	void init(UINT64 offset) throw (IoException);

    void initWithTimeFilter(UINT64 timeVal) throw (IoException);


	//! Start the reader thread and open the files.
	void start();

	//! Stop the reader thread and close the files.
	void stop();

	//! Seek in the current file to position pos in bytes
	/*! Not Implemented
	 *  @param pos - position in file to seek to
	 *	@return - the current file position
	 */
	int seek(UINT64 pos);

	//	/**
	//	 * Get the current file position
	//	 * @return
	//	 */
	//		size_t getFilePosition();

private:
	UINT64 getFPosFromTime(UINT64 timeVal);

	//! File reader thread handler
	/*! handle a file reader. it is the thread does the job of reading data
	 * 	from file to buffer.
	 * 	@param arg			- pointer of a file reader
	 */
	static void * threadHandler(void* arg);

	//! Read data from file to buffer
	/*! read mChunkSz data into packet buffer until packet buffer is full,
	 * 	then will wait until packet buffer has space to be written mChunkSz
	 *  data
	 */
	void bufferWriter();

private:
	//! Packet buffer pointer
	/*! points to this file reader's packet buffer                           */
	CPacketBuffer *mPktBuf;

	//! Name of date file
	/*! Name of file to read data from.                                      */
    std::string mFileName;

	//! Data file pointer
	/*! File pointer for the input file, used for reading                    */
	FILE *mFile;

	//! File number of the opened data file
	/*! File number for low level read().                                    */
	int mFileNo;

	//! X3C file header
	/*! File header  for X3C format files.                                   */
	X3C_HEADER mX3CFileHeader;

	//! Buffer chunk size
	/*! Size in Bytes of one buffer chunk read from file.                    */
	size_t mChunkSz;

	//! Flag indicating whether buffer writer shall be running
	/*! start/stop and bufferWrite can change its status, if it is true,
	 * 	bufferWriter can read packets, or bufferWriter will not read packets */
	bool mDoRun;

	//! The buffer writer thread
	/*! the thread of reading data from file to buffer                       */
	pthread_t mBufWrThread;

	//! File offset
	/*! current file offset for file reader / buffer writer                  */
	size_t mFileOffset;

	//! state variables
	/*! true if file reader is initialized, or it is set as false            */
	bool mIsInitialized;
};

#endif /* CFILEREADER_H_ */
