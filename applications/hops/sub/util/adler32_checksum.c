#include "adler32_checksum.h"

unsigned int adler32_checksum(unsigned char *buf, int len)
    {
    unsigned int s1 = 1;
    unsigned int s2 = 0;
    int n;

    for(n = 0; n < len; n++) 
        {
        s1 = (s1 + buf[n]) % MOD_ADLER;
        s2 = (s2 + s1) % MOD_ADLER;
        }
    return (s2 << 16) + s1;
    }
