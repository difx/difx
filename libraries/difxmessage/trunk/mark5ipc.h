/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
 * $Id: $
 * $HeadURL:  $
 * $LastChangedRevision: $
 * $Author: $
 * $LastChangedDate: $
 *
 *==========================================================================*/

#ifndef __MARK5_LOCK_H__
#define __MARK5_LOCK_H__

#ifdef __cplusplus
extern "C" {
#endif

#define MARK5_LOCK_WAIT_FOREVER		-1
#define MARK5_LOCK_DONT_WAIT		0

int lockMark5(int wait);	/* max wait time in seconds, or -1 to imply wait forever */
int unlockMark5();
int getMark5LockPID();
int getMark5LockValue();
int deleteMark5Lock();
int getMark5LockSemaphoreID();

#ifdef __cplusplus
}
#endif


#endif
