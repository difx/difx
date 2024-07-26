/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken                             *
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
#ifndef __PROC_H__
#define __PROC_H__

/* routines to get useful information from /proc */

int procGetCoresFromCpuInfo(int *nCore);
int procGetCores(int *nCore);
int procGetMem(int *memused, int *memtot);
int procGetNet(long long *rx, long long *tx);
int procGetCPU(float *l1, float *l5, float *l15);
int procGetStreamstor(int *busy);

/* returns number of processes killed */
int killSuProcesses(int verbose);

#endif
