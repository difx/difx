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

#include <iostream>
#include "dirlist_parameter.h"
#include "dirlist_exception.h"

int main(int argc, char **argv)
{
	DirListParameter *P1;

	P1 = new DirListParameter; P1->print(); delete P1;
	P1 = new DirListParameter("x", "y"); P1->print(); delete P1;
	P1 = new DirListParameter("x2", "y2", "c2"); P1->print(); delete P1;
	P1 = new DirListParameter;
	P1->setKey("setKey");
	P1->setValue("setValue");
	P1->setComment("setComment");
	P1->print();
	try
	{
		std::cout << "Int[100] = " << P1->getInt(100) << std::endl;
	}
	catch (DirListException &e)
	{
		std::cout << "Exception caught: " << e.what() << std::endl;
	}
	delete P1;
	P1 = new DirListParameter("x3", "y3,z3, w3, t3 , u3", "c3"); P1->print(); delete P1;
	P1 = new DirListParameter("x4", "'sterntor'"); P1->print(); delete P1;
	P1 = new DirListParameter("x5", "'1,2,3,4,5'"); P1->print(); delete P1;
	P1 = new DirListParameter("x6", "'1','2','3','4','5'"); P1->print(); delete P1;

	return 0;
}
