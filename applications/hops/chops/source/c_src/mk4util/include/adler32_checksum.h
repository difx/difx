#ifndef ADLER32_CHECKSUM_H__
#define ADLER32_CHECKSUM_H__

#define MOD_ADLER 65521

extern unsigned int adler32_checksum(unsigned char *buf, int len);

#endif /* end of include guard: ADLER32_CHECKSUM_H__ */
