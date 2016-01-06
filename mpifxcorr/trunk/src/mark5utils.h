/***************************************************************************
 *   Copyright (C) 2008-2014 by Walter Brisken                             *
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
 * $Id: mark5utils.h 5453 2013-06-28 02:13:50Z WalterBrisken $
 * $HeadURL: $
 * $LastChangedRevision: 5453 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2013-06-27 20:13:50 -0600 (Thu, 27 Jun 2013) $
 *
 *==========================================================================*/

#ifndef __MARK5UTILS_H__
#define __MARK5UTILS_H__

#include <xlrapi.h>

#ifndef MARK5_FILL_PATTERN
#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_PATTERN 0x44332211UL
#else
#define MARK5_FILL_PATTERN 0x11223344UL
#endif
#endif

// Test for SDK 9+
#ifdef XLR_MAX_IP_ADDR
#define SDKVERSION 9
typedef unsigned int streamstordatatype;
#else
#define SDKVERSION 8
typedef unsigned long streamstordatatype;
#endif


bool legalVSN(const char *vsn);

XLR_RETURN_CODE openMark5(SSHANDLE *xlrDevice);

XLR_RETURN_CODE difxMark5Read(SSHANDLE xlrDevice, unsigned long long readpointer, unsigned char *dest, unsigned int bytes, unsigned int readDelayMicroseconds);

#endif
