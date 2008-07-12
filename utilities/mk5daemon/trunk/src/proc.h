#ifndef __PROC_H__
#define __PROC_H__

int procGetMem(int *memused, int *memtot);
int procGetNet(long long *rx, long long *tx);
int procGetCPU(float *l1, float *l5, float *l15);

#endif
