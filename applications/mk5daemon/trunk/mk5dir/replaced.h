#ifndef __REPLACED_H__
#define __REPLACED_H__

#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_WORD32 0x44332211UL
#define MARK5_FILL_WORD64 0x4433221144332211ULL
#else
#define MARK5_FILL_WORD32 0x11223344UL
#define MARK5_FILL_WORD64 0x1122334411223344ULL
#endif

#ifdef __cplusplus
extern "C" {
#endif

void countReplaced(const unsigned long *data, int len, long long *wGood, long long *wBad);

#ifdef __cplusplus
}
#endif

#endif
