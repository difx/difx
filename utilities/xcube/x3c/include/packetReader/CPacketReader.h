/*
 * CPacketReader.h
 *
 *  Created on: Nov 18, 2011
 *      Author: mtaveniku
 *  The Packet reader class is a multithreaded class reading X3C packets from
 *  a parallel file system. The Packets in the stream are delivered in
 *  chronological sequence from the files
 *  This class is supposed to work on both Windows and Linux machines
 */

#ifndef CPACKETREADER_H_
#define CPACKETREADER_H_

#include <vector>

#include "X3cPacket.h"
#include "CFileReader.h"
#include "CFrameFilter.h"
#include "CTimeFilter.h"

using namespace std;
//#define INTERNAL_PACKET_HANDLING

//! Class for packet reader
/*! The packet reader is the base class to read packets from an X3C parallel
 * 	or serial input streams (File or other) the system shall be called in
 * 	this sequence:
 * 	1: setinputFiles		- initialize the input file names
 * 	2: setreadChunks		- how big the input buffer from the disk shall be
 * 	3: set buffer_size		- the size of the read buffer
 * 							(how much data we shall keep in memory)
 * 	4: checkArguments	- make sure we are OK  with all inputs
 * 	5: startReader		- starts the input streams
 * 	6: while we need more data ()
 * 	6.1: getPacket()
 * 	6.2: (if packet == NULL check if we have EOF)
 * 	6.3: freePacket(pkt)	- signals the reader that it is OK to free buffer
 * 							  space taken by packet
 * 	6.4: until we do not need more data or EOF
 * 	7: stopReader()		- stops the reader threads
 * 	8: delete the packet reader ..                                           */
class CPacketReader {

public:
	//! Maxium data size a disk can read
	static const size_t MAX_DISK_READ_SIZE_MB = 128;

	//! Structure of packet handle
	typedef struct _packet_handle_ {
		//! the index of the item in list
		int index;
		//! the stream this thing belongs to
		int streamNo;
		//! pointer to the packet
		X3cPacket *pkt;
	} packet_handle_t;

public:

	//! Constructor
	/*! Creates a new packet reader object. The user must initialize the
	 * 	reader before it can be used.                                        */
	CPacketReader();

	//! De-constructor
	/*! Deletes the packet reader and the objects associated with it         */
	virtual ~CPacketReader();

	//! Get/Read a packet
	/*! Read one packet from the combined buffers in time-stamp order
	 * @return		-	a packet ptr, or null if not available               */
	X3cPacket *getPacket();

	//! Free a packet
	/*! Free the packet pointed to by pkt.
	 * @param pkt		-	packet to free                                   */
	void freePacket(X3cPacket *pkt);

	/*!
	 * Sets a packet filter based on start and end packet number.
	 * To get to the start packet, accessed will be sequential.
	 * @param start - start packet number
	 * @param end - end packet number
	 */
    void setPacketFilter(UINT64 start, UINT64 end);

    /*!
     * Sets a time based filter for the reader.  Access to the start
     * packet will use an index file if it exists.
     * 
     * @param tFilter - a reference to the time filter to set
     */
    void setTimeFilter(CTimeFilter const& tFilter);


    UINT64 getPacketCount() const;

	// int seekTime(struct timespec ts);
	// int seek(size_t position);
	// int seekPacket(unsigned long pktNo);


	//! Initialize packet reader
	/*! This method initializes the PacketReader. It is to be called once
	 *  directly after the reader has been created. If the initialization
	 *  is successful the reader is ready to start. On error delete this
	 *  reader object and start over.
	 * @param fileCnt		-	the number of input files to read from
	 * @param fileNames		-	the qualified filenames of the input files
	 * @param bufSz			-	the input buffer size in MByte
	 * @param chunkSz		-	the requested read size from disk
	 * @return 				- 	true if successful, if false
	 * 							(delete this object and start over)          */
	bool initialize(std::vector<std::string> const& fileNames, size_t bufSz,
                    size_t chunkSz);


	//! Start packet reader
	/*! Starts the reader and initializes all data structures needed
	 *	 @return		-	0 on success, negative error code otherwise      */
	int start();

	//! Stop packet reader
	/*! Stop the Packet reader. This should be called before the packet
	 * 	reader is supposed to be destroyed ..
	 * 	@return		-	true if successful                                   */
	bool stop();

#ifdef INTERNAL_PACKET_HANDLING
	//! Get the packet handler
	/*! return a pointer points to this packet reader's packet handler
	 * 	@return		-	pointer points to packet hander                      */
	packet_handle_t *getPacketHandle();
#else
	//! Get the packet handle
	/*! Write packet and stream information into hdl, packet handle has the
	 * 	information about which stream the packet belongs to, so that when
	 *  caller does not need to go through all streams.(this is the
	 *  difference between packet and packethandle)
	 *  @paramhdl		-	packet handle to record packet and stream info   */
	void getPacketHandle(packet_handle_t *hdl);
#endif

	//! Free the packet handle
	/*! Free a packet handle
	 *  @param hdl		-	packet handle to free                            */
	void freePacketHandle(packet_handle_t *hdl);

    //! Reset the packet reader state
	/*! Reset the packet reader state to its initial state                   */
    void reset();

private:
	//! Get a packet
	/*! Internal helper function to really get pkt                           */
	X3cPacket *persistent_get_packet(int streamNo);

	//! Sort packets in mPktPtrs
	/*! sorts the packets so that we got the oldest first */
	void sortPackets();

	//! Free the packet handle
	void freePktHandleIdx(int index);

	//! get a free handle from list
	int  getFreePktHandleIdx();

	//! initialize the list of packet handles
	void initPktHandles();

    //! Reset the CFileReaders to the beginning of the stream
    void resetFileReaders();

    //! Reset the CPacketBuffers to the beginning of the stream
    void resetPacketBuffers();

    //! Resume the CFileReaders after being reset
    void resumeFileReaders();

	//! based on the time filters, should this packet
	/*! be filtered out?
	 * @returns 1 - the packet should be filtered out
	 *          0 - keep the packet
	 *          -1 - past last value
	 */
	CStreamFilter::FilterReturn getShouldBeTimeFiltered(X3cPacket *pkt);

	//! Flag to indicating whether packet reader is stared
	/*! if started, this value is set as true, otherwise, it is set as false */
	bool mIsStarted;

	//! Flag indicating whether packet reader is initialized
	/*! If initialized, this value is set as true, or it's set false         */
	bool mIsInitialized;

	//! Number of input streams
	/*! Number of x3c files to read                                          */
	int mNumInput;

	//! List of filenames
	/*! List of input file names                                             */
	vector<std::string> mFileNames;

	//! Size of data chunk for reading
	/*! Size in MByte of data to read from the disk system (should be set
	 * to disk cache size)                                                   */
	size_t mRdChunkSz;

	//! Size of buffer
	/*! Size in MByte of the packet buffer as requested by user              */
	size_t mBufSz;

	//! List of packet buffers
	vector <CPacketBuffer*> mPacketBuffers;

	//! List of file readers
	/*! List of file readers belonging to this packet reader, each file
	 * reader is associated with input stream                                */
	vector <CFileReader*> mFileReaders;

	//! List of packet pointers
	/*! Array of pointers to the head of packet queue in each stream         */
	vector <X3cPacket *> mPktPtrs;

	//! The oldest stream No
	/*! this is the stream we are reading from now                           */
	int mOldestStream;

	//! The oldest time stamp
	/*! It is used to sort packets                                           */
	UINT64 mOldestTs;

	//! Next time stamp
	/*! It is used to sort packets                                           */
	UINT64 mNextTs;

	//! Size of packet handle
	/*! more than max outstanding packets */
	static const int mFHListSz = 512;

	//A filter on the packet count;
	CFrameFilter mPacketFilter;

    //! A filter on the packet time
    CTimeFilter mTimeFilter;

	UINT64 mPacketCounter;

#ifdef INTERNAL_PACKET_HANDLING
	//! vector of packet handles
	packet_handle_t *mPacketHandles;

	//! List of free packet handles
	int *mFreeHandles;

	//! Free handle head index
	int mFHHd;

	//! Free handle tail index
	int mFHTl;
#endif
};

inline void CPacketReader::setPacketFilter(UINT64 start, UINT64 end)
{
    mPacketFilter.setFilter(start, end);
}

inline void CPacketReader::setTimeFilter(CTimeFilter const& tFilter)
{
    mTimeFilter = tFilter;
}

inline UINT64 CPacketReader::getPacketCount() const
{
    return mPacketCounter;
}

#endif /* CPACKETREADER_H_ */
