/*
 * X3cPacket.h
 *
 *  Created on: Oct 28, 2010
 *      Author: nilham
 */

#ifndef X3CPACKET_H_
#define X3CPACKET_H_

#include "CX3cFile.h"
//#include "xcTypes.h"

class X3cPacket
{
public:
	/*
	 * Wrapper class for X3C packets.
	 * Empty instance is created.
	 */
	X3cPacket();
	/*
	 * Wrapper class for X3C packets.
	 * Populated instance is created.
	 *
	 * Set the flag 'mustClean' to 'true' if the parameters
	 * shall be deleted when the object is destroyed.
	 */
	X3cPacket(X3C_PACKET_HEADER *packet_header, char *pData, bool mustClean);

    /*
     * Wrapper class for X3C packets.
     * Populated instance is created.
     *
     * Same as above but also passes in the packet file position
     */
    X3cPacket(X3C_PACKET_HEADER *packet_header, char *pData, bool mustClean, UINT64 position);

	/*
	 * Wrapper class for X3C packets.
	 * Populated instance is created.
	 *
	 * This constructor shall be replaced with the one above.
	 */
	X3cPacket(X3C_PACKET_HEADER *packet_header, char *pData) __attribute__ ((deprecated));
	/**
	 * For test purposes only.
	 */
	X3cPacket(UINT64 timeStamp);
	virtual ~X3cPacket();
	/*
	 * Get the header part of the packet.
	 */
	X3C_PACKET_HEADER *getHeader() const;
	/*
	 * Get the length of the data in the packet.
	 * Also available through the header data.
	 */
	UINT32 getLen() const;
	/*
	 * Get the timestamp for the packet.
	 */
	UINT64 getTimestamp() const;
	/*
	 * Set timestamp for the packet.
	 */
	void setTimestamp(UINT64 timeStamp);
	/*
	 * Get a pointer to the data in the packet.
	 */
	char *getData() const;

	/*
	 * Get the file position for this packet
	 */
	UINT64 getPosition() const;

private:

    // Assignment and copy assignment are not supported
    X3cPacket(X3cPacket const&);
    X3cPacket& operator=(X3cPacket const&);

	X3C_PACKET_HEADER *xph;
	char *data;
	UINT64 timeStamp;
	bool mustClean;
	UINT64 position;
};

#endif /* X3CPACKET_H_ */
