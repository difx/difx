#ifndef __DIRLIST_EXCEPTION_H__
#define __DIRLIST_EXCEPTION_H__

#include <string>
#include <sstream>

class DirListException : public std::exception
{
public:
	enum Type
	{
		TypeNone,
		TypeCantOpen,
		TypeWrongIdentifier,
		TypeParseError
	};

	DirListException(std::string msg) : err_msg(msg), type(TypeNone) {}
	DirListException(std::string msg, Type t) : err_msg(msg), type(t) {}
	~DirListException() throw() {}
	virtual const char *what() const throw() { return err_msg.c_str(); }
	Type getType() const { return type; }

private:
	std::string err_msg;
	Type type;
};

#endif
