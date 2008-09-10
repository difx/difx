#ifndef __PROC_H__
#define __PROC_H__

/* routines to get useful information from /proc */

int procGetMem(int *memused, int *memtot);
int procGetNet(long long *rx, long long *tx);
int procGetCPU(float *l1, float *l5, float *l15);
int procGetStreamstor(int *busy);

#endif
