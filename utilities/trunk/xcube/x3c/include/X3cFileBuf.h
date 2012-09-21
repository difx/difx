/*
 * X3cFileBuf.h
 *
 *  Created on: Jan 23, 2011
 *      Author: nilham
 */

#ifndef X3CFILEBUF_H_
#define X3CFILEBUF_H_

#include <pthread.h>

/*
 * Forward declaration to avoid circular inclusions.
 */
class X3cFileThreadReader;

/*
 * Class for controlling access to buffer area.
 *
 * Notice that the methods all shall be synchronized
 * through mutex access. It may not be necessary
 * for some methods, but is there for consistency.
 */
class X3cFileBuf
{
public:
	/*
	 * Construct and set buffer start and end pointers.
	 */
	X3cFileBuf(X3cFileThreadReader *parent, unsigned int bufNum, char *start, size_t len);
	/*
	 * Destroy, destroy, destroy...
	 */
	virtual ~X3cFileBuf();
	/*
	 * Close the buffer.
	 * Assumption is that close is ONLY called from reader thread,
	 * in which case we know that it's only the writer thread that
	 * has to be released.
	 */
	void close();
	/*
	 * Wait until buffer is empty.
	 * Returns true if wait was OK and false if buffer was closed.
	 */
	bool waitForWrite();
	/*
	 * Wait until buffer isn't empty.
	 * Returns true if wait was OK and false if buffer was closed.
	 */
	bool waitForRead();
	/*
	 * Get length of real data in the buffer.
	 * Notice: May be zero, which indicates either unloaded buffer or EOF.
	 */
	size_t getDataLen();
	/*
	 * Mark buffer as filled and release buffer for read.
	 *
	 * @realLen - Actual amount written into buffer,
	 *            which may be less than the buffer size
	 *            when EOF is encountered.
	 */
	void setFilled(size_t realLen);
	/*
	 * Mark buffer as empty and release buffer for write.
	 */
	void setEmpty();
	/*
	 * Get the pointer to the buffer.
	 */
	char *getBufPtr();
	/*
	 * Get the size of the buffer.
	 */
	size_t getBufSize();
	/*
	 * Set EOF flag.
	 */
	void setEof(bool eof);
	/*
	 * Check EOF flag.
	 */
	bool isEof();
	/*
	 * See if buf is flagged empty.
	 */
	bool isEmpty();

private:
	X3cFileThreadReader *parent;
	bool valid;
	bool empty;
	bool busy;
	bool eof;
	char *start;
	size_t dataLen;
	size_t bufLen;
	pthread_mutex_t mRWMutex;
	pthread_cond_t writeWaiter;
	pthread_cond_t readWaiter;
	unsigned int bufNum;
};

#endif /* X3CFILEBUF_H_ */
