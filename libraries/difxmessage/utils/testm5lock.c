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

#include <stdio.h>
#include <unistd.h>

#include "mark5ipc.h"


int main()
{
	char cmd[120];
	char *rv;

	for(;;)
	{
		rv = fgets(cmd, 119, stdin);
		if(!rv)
		{
			break;
		}
		if(cmd[0] == '5')
		{
			printf("Locking\n");
			fflush(stdout);
			printf("Locked: %d\n", lockMark5(5));
		}
		else if(cmd[0] == 'n')
		{
			printf("Locking no wait\n");
			fflush(stdout);
			printf("Locked no wait: %d\n", lockMark5(MARK5_LOCK_DONT_WAIT));
		}
		else if(cmd[0] == 'w')
		{
			printf("Locking no wait\n");
			fflush(stdout);
			printf("Locked no wait: %d\n", lockMark5(MARK5_LOCK_WAIT_FOREVER));
		}
		else if(cmd[0] == 'u')
		{
			printf("Unlocking\n");
			fflush(stdout);
			printf("Unlocked: %d\n", unlockMark5());
		}
		else if(cmd[0] == 'p')
		{
			printf("PID = %d\n", getMark5LockPID());
			printf("semid = %p\n", getMark5LockSemaphoreID);
			printf("val = %d\n", getMark5LockValue());
		}
		else if(cmd[0] == 'r')
		{
			printf("rm = %d\n", deleteMark5Lock());
		}
		else
		{
			printf("Options are: 5 n w u p r\n");
		}
	}

	return 0;
}
