/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
 * $Id: sanitycheck.h 6894 2015-07-28 16:19:06Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision: 6894 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2015-07-29 00:19:06 +0800 (三, 2015-07-29) $
 *
 *==========================================================================*/

#ifndef __SANITYCHECK_H__
#define __SANITYCHECK_H__

#include "vex_data.h"
#include "corrparams.h"

int sanityCheckConsistency(const VexData *V, const CorrParams *P);

#endif
