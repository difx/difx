#ifndef __DIRLIST_EXCEPTION_H__
#define __DIRLIST_EXCEPTION_H__

#include <string>
#include <sstream>

class DirListException : public std::exception
{
public:
	DirListException(std::string msg) : err_msg(msg) {}
	DirListException(std::string msg, int id) { std::stringstream ss;  ss << msg << id;  msg = ss.str(); }
	~DirListException() throw() {}
	virtual const char *what() const throw() { return err_msg.c_str(); }

private:
	std::string err_msg;
};

#endif
