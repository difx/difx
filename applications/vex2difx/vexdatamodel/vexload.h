/***************************************************************************
 *   Copyright (C) 2009-2022 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: vexload.h 10846 2022-12-02 21:44:33Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/applications/vex2difx/vexdatamodel/vexload.h $
 * $LastChangedRevision: 10846 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2022-12-03 05:44:33 +0800 (六, 2022-12-03) $
 *
 *==========================================================================*/

#ifndef __VEXLOAD_H__
#define __VEXLOAD_H__

#include <string>
#include <vex_data.h>

void setReportIncompleteModes(int report);

VexData *loadVexFile(const std::string &vexFile, unsigned int *numWarnings);

#endif
