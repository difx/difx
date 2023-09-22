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

#ifndef __DIRLIST_H__
#define __DIRLIST_H__

#include <ostream>
#include <sstream>
#include <utility>
#include <vector>
#include <string>
#include "dirlist_exception.h"
#include "dirlist_parameter.h"
#include "dirlist_datum.h"
#include "dirlist_datum_mark5.h"

// FIXME: move to class constant?
#define DIRLIST_IDENTIFIER_LINE	"VLBI baseband data listing"


// TODO
// * Add functionality to print in comments meaning of each column in the data section
// * Remove comments which are only whitespace


class DirList
{
#if 0
	Not yet sure how to handle this.  May be cleaner to use a DirListParameter?

	enum FileType
	{
		UnknownFile,
		NativeFile,
		Mark5DirFile,
		VSumFile,
		Mark6SListFile
	};
#endif
public:
	DirList() : identifier(DIRLIST_IDENTIFIER_LINE) {};
	~DirList();
	void clear();
	void load(const char *filename);
	int sanityCheck() const;

	bool empty() const { return data.empty(); }
	unsigned int nScan() const { return data.size(); }
	const DirListDatum *getScan(unsigned int index) const { return index < data.size() ? data[index] : 0; }

	const DirListDatumMark5 *getMark5Scan(unsigned int index) const;

	void setDefaultIdentifier() { identifier = DIRLIST_IDENTIFIER_LINE; }
	void setIdentifier(const std::string &str) { identifier = str; }
	bool isParameterTrue(const std::string &key);
	bool isParameterFalse(const std::string &key);
	void addDatum(DirListDatum *datum) { data.push_back(datum); }
	void addParameter(DirListParameter *param);
	void setStationAndExperiments();
	void setTimerange();
	void removeEmptyComments();
	void organize();
	void setPathPrefix();
	void sort();
	void print(std::ostream &os) const;

	// Because this is a template type it must be kept in the .h file
	template <typename Type> void setParameter(const std::string &key, const Type &value, const std::string &comment = "")
	{
		DirListParameter *P;
		std::stringstream ss;

		ss.precision(14);

		P = getParameter(key);
		if(!P)
		{
			P = new DirListParameter(key);
			parameters.push_back(P);
		}

		ss << value;
		P->setValue(ss.str());
		P->setComment(comment);
	}
	const DirListParameter *getConstParameter(const std::string &key) const;
	bool hasParameter(const std::string &key);

private:
//	enum FileType fileType;

	std::string identifier;				// file identifier -- the first line of the file
	std::vector<DirListParameter *> parameters;	// vector so items have an order
	std::vector<DirListDatum *> data;		// pointers to scan data

	DirListParameter *getParameter(const std::string &key);
};

std::ostream& operator << (std::ostream &os, const DirList &x);

#endif
