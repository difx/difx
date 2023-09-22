/*
 * ParserException.h
 *
 *  Created on: Oct 26, 2010
 *      Author: nilham
 */

#ifndef IOEXCEPTION_H_
#define IOEXCEPTION_H_

#include <exception>

class IoException : public std::exception
{
public:
	IoException(const char *errMsg);
	virtual ~IoException() throw();
	const char* what() const throw();

private:
	char *errorMessage;
};

#endif /* IOEXCEPTION_H_ */
