/***************************************************************************
 *   Copyright (C) 2010-2012 by Walter Brisken                             *
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
#include <time.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include "../mark5ipc.h"

#define KEY_ID		94539

int semid = 0;

static int initSemaphore()
{
	key_t key;

	//key = ftok(KEY_FILENAME, KEY_ID);
	key = KEY_ID;

	semid = semget(key, 1, IPC_CREAT | 0666);

	if(semid == -1)
	{
		perror("semget failed!");
		semid = 0;
		return -1;
	}

	return 0;
}

int lockMark5(int wait)
{
	struct sembuf sops[2];
	int v;
	int i = 0;

	if(semid == 0)
	{
		initSemaphore();
	}

	sops[0].sem_num = 0;
	sops[0].sem_op = 0;
	sops[0].sem_flg = SEM_UNDO;
	sops[1].sem_num = 0;
	sops[1].sem_op = 1;
	sops[1].sem_flg = SEM_UNDO | IPC_NOWAIT;
	if(wait >= 0)
	{
		sops[0].sem_flg |= IPC_NOWAIT;
	}

	do
	{
		struct timespec ts;

		v = semop(semid, sops, 2);
		if(v == 0)
		{
			break;
		}
		ts.tv_sec = 0;
		ts.tv_nsec = 100000000;
		nanosleep(&ts, 0);
		i++;
	}
	while(i < wait*10);

	return v;
}

int unlockMark5()
{
	struct sembuf sops[1];
	int v;

	if(semid == 0)
	{
		initSemaphore();
	}

	sops[0].sem_num = 0;
	sops[0].sem_op = -1;
	sops[0].sem_flg = SEM_UNDO | IPC_NOWAIT;

	v = semop(semid, sops, 1);

	return v;
}

int getMark5LockPID()
{
	int pid;

	if(semid == 0)
	{
		initSemaphore();
	}

	pid = semctl(semid, 0, GETPID);

	return pid;
}

int getMark5LockValue()
{
	int pid;

	if(semid == 0)
	{
		initSemaphore();
	}

	pid = semctl(semid, 0, GETVAL);

	return pid;
}

int deleteMark5Lock()
{
	int v=0;

	if(semid != 0)
	{
		v = semctl(semid, 0, IPC_RMID);
		semid = 0;
	}

	return v;
}

int getMark5LockSemaphoreID()
{
	return semid;
}
