/***************************************************************************
 *   Copyright (C) 2016 by Walter Brisken                                  *
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
 * $Id: swap.h 7428 2016-08-16 22:13:25Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/src/swap.h $
 * $LastChangedRevision: 7428 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2016-08-17 06:13:25 +0800 (三, 2016-08-17) $
 *
 *==========================================================================*/

#ifndef __SWAP_H__
#define __SWAP_H__

/* Value returned into "usage": positive number means swap in use [kBytes] */
/* Return value: 0 = success, != 0 means error */
int getSwapUsage(int *usage);

#endif
