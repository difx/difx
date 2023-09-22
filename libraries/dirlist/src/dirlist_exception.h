/***************************************************************************
 *   Copyright (C) 2016-2017 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

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
