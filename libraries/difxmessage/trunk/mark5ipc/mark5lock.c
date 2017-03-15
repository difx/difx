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
int nsems = 0;

/***************************************************************************
 * initSemaphore()
 *
 * Description: creates a set of two semaphores. First semaphore is for
 * standard locking and unlocking of mark5 unit. Second semaphore is for
 * locking during fuse usage.
 *
 ***************************************************************************/
static int initSemaphore()
{
	key_t key;

	//key = ftok(KEY_FILENAME, KEY_ID);
	key = KEY_ID;

	semid = semget(key, 2, IPC_CREAT | 0666);

	if(semid == -1)
	{
		semid = semget(key, 1, IPC_CREAT | 0666);

		if(semid == -1)
		{
			perror("semget failed!");
			semid = 0;
			return -1;
		}
		else
		{
			nsems = 1;
		}
	}
	else
	{
		nsems = 2;
	}

	return 0;
}

/***************************************************************************
 * lockMark5()
 * 
 * Argument: int wait - wait time in seconds, 0 no wait, -1 wait forever
 * Return: int - 0 successful lock, -1 semaphore already locked
 *
 * Description: checks both semaphores, will generate error if fuse lock is
 * in use, will lock first semaphore if possible
 *
 ***************************************************************************/
int lockMark5(int wait)
{
	struct sembuf sops[2];
	int v;
	int firstLockVal;
	int fuseLockVal;
	int i = 0;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}

	// check if locked for fuse use
	if(nsems == 2)
	{
		fuseLockVal = semctl(semid, 1, GETVAL);
		if(fuseLockVal > 0)
		{
			firstLockVal = semctl(semid, 0, GETVAL);
			if(firstLockVal > 0)
			{
				fprintf(stderr, "Mark5 unit locked for fuse use by PID %d\n", getFuseLockPID());
			}
			else
			{
				fprintf(stderr, "!!! Fuse lock is locked. Standard lock is not locked.\n");
				fprintf(stderr, "This condition should not exist!\n");
				fprintf(stderr, "Fuse lock held by PID %d\n", getFuseLockPID());
			}
			return -1;
		}
	}

	// do standard locking
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

/***************************************************************************
 * lockFuse()
 * 
 * Return: int - 0 successful lock, -1 already locked or out of sync
 *
 * Description: checks both semaphores, will generate error if semaphores
 * are out of sync, will lock both semaphores if possible, semaphore values
 * will persist after program exits
 *
 ***************************************************************************/
int lockFuse()
{
	struct sembuf sops[2];
	int v;
	int firstLockVal;
	int fuseLockVal;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}
	if(nsems < 2)
	{
		fprintf(stderr, "Semaphore set does not have fuse semaphore. semid %d\n", semid);
		return -1;
	}

	// check lock state
	firstLockVal = semctl(semid, 0, GETVAL);
	if(firstLockVal > 0)
	{
		// locked, let calling method check lock PIDs
		return -1;
	}
	fuseLockVal = semctl(semid, 1, GETVAL);
	if(fuseLockVal > 0)
	{
		// locks out of sync
		fprintf(stderr, "!!! Fuse lock is locked. Standard lock is not locked.\n");
		fprintf(stderr, "This condition should not exist!\n");
		fprintf(stderr, "Fuse lock held by PID %d\n", getFuseLockPID());
		return -1;
	}

	sops[0].sem_num = 0; // standard sempahore
	sops[0].sem_op = 1;  // increment value to locked
	sops[0].sem_flg = 0; // persist after program exit
	sops[1].sem_num = 1; // fuse semaphore
	sops[1].sem_op = 1;  // increment value to locked
	sops[1].sem_flg = 0; // persist after program exit

	// update the semaphores
	v = semop(semid, sops, 2);

	return v;
}

/***************************************************************************
 * unlockMark5()
 * 
 * Return: int - 0 successful unlock, -1 unable to unlock
 *
 * Description: checks both semaphores, will generate error if fuse lock is
 * in use, will unlock first semaphore if possible
 *
 ***************************************************************************/
int unlockMark5()
{
	struct sembuf sops[1];
	int v;
	int firstLockVal;
	int fuseLockVal;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}

	// check if locked for fuse use
	if(nsems == 2)
	{
		fuseLockVal = semctl(semid, 1, GETVAL);
		if(fuseLockVal > 0)
		{
			firstLockVal = semctl(semid, 0, GETVAL);
			if(firstLockVal > 0)
			{
				fprintf(stderr, "Mark5 unit locked for fuse use by PID %d\n", getFuseLockPID());
				fprintf(stderr, "Mark5 unit must be unlocked by stopfuse application.\n");
			}
			else
			{
				fprintf(stderr, "!!! Fuse lock is locked. Standard lock is not locked.\n");
				fprintf(stderr, "This condition should not exist!\n");
				fprintf(stderr, "Fuse lock held by PID %d\n", getFuseLockPID());
			}
			return -1;
		}
	}

	// do standard unlocking
	sops[0].sem_num = 0;
	sops[0].sem_op = -1;
	sops[0].sem_flg = SEM_UNDO | IPC_NOWAIT;

	v = semop(semid, sops, 1);

	return v;
}

/***************************************************************************
 * unlockFuse()
 * 
 * Return: int - 0 successful unlock, -1 unsuccessful or out of sync
 *
 * Description: checks both semaphores, will generate error if semaphores
 * are out of sync, will unlock both semaphores if possible, semaphore values
 * will persist after program exits
 *
 ***************************************************************************/
int unlockFuse()
{
	struct sembuf sops[2];
	int v;
	int firstLockVal;
	int fuseLockVal;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}
	if(nsems < 2)
	{
		fprintf(stderr, "Semaphore set does not have fuse semaphore. semid %d\n", semid);
		return -1;
	}

	// check lock state
	firstLockVal = semctl(semid, 0, GETVAL);
	fuseLockVal = semctl(semid, 1, GETVAL);
	if(firstLockVal == 0 && fuseLockVal > 0)
	{
		// locks out of sync
		fprintf(stderr, "!!! Fuse lock is locked. Standard lock is not locked.\n");
		fprintf(stderr, "This condition should not exist!\n");
		fprintf(stderr, "Fuse lock held by PID %d\n", getFuseLockPID());
		return -1;
	}
	if(firstLockVal > 0 && fuseLockVal == 0)
	{
		// only standard lock set
		fprintf(stderr, "!!! Only standard lock is locked.\n");
		fprintf(stderr, "Check if PID holding lock still has finish!\n");
		fprintf(stderr, "Standard lock held by PID %d\n", getMark5LockPID());
		return -1;
	}

	sops[0].sem_num = 1; // fuse sempahore
	sops[0].sem_op = -1; // decrement value to unlocked
	sops[0].sem_flg = IPC_NOWAIT; // persist after program exit, but don't wait if unlocked
	sops[1].sem_num = 0; // first semaphore
	sops[1].sem_op = -1; // decrement value to unlocked
	sops[1].sem_flg = IPC_NOWAIT; // persist after program exit, but don't wait if unlocked

	// update the semaphores
	v = semop(semid, sops, 2);

	return v;
}

/***************************************************************************
 * getMark5LockPid()
 * 
 * Return: int - value of PID holding lock on the standard lock semaphore
 *
 ***************************************************************************/
int getMark5LockPID()
{
	int pid;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}

	pid = semctl(semid, 0, GETPID);

	return pid;
}

/***************************************************************************
 * getFuseLockPid()
 * 
 * Return: int - value of PID holding lock on the fuse lock semaphore
 *
 ***************************************************************************/
int getFuseLockPID()
{
	int pid;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}
	if(nsems < 2)
	{
		fprintf(stderr, "Semaphore set does not have fuse semaphore. semid %d\n", semid);
		return -1;
	}

	pid = semctl(semid, 1, GETPID);

	return pid;
}

/***************************************************************************
 * getMark5LockValue()
 * 
 * Return: int - value of standard semaphore
 *
 ***************************************************************************/
int getMark5LockValue()
{
	int pid;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}

	pid = semctl(semid, 0, GETVAL);

	return pid;
}

/***************************************************************************
 * getFuseLockValue()
 * 
 * Return: int - value of second semaphore, the fuse lock semaphore
 *
 ***************************************************************************/
int getFuseLockValue()
{
	int val;

	if((semid == 0) && (initSemaphore() == -1))
	{
		return -1;
	}
	if(nsems < 2)
	{
		fprintf(stderr, "Semaphore set does not have fuse semaphore. semid %d\n", semid);
		return -1;
	}

	val = semctl(semid, 1, GETVAL);

	return val;
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
