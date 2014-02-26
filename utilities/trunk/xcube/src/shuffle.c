#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <getopt.h>
#include <sys/time.h>
#include <stdint.h>
#include <time.h>

double tim(void);


#define BLOCKSIZE           40
#define TIME               0.5

#define FLIP8(x)  ((((x)>>1)&0x5555555555555555LL) | (((x)<<1)&0xAAAAAAAAAAAAAAAALL))
#define FLIP4(x)  ((((x)>>1)&0x55555555) | (((x)<<1)&0xAAAAAAAA))
#define FLIP2(x)  ((((x)>>1)&0x5555) | (((x)<<1)& 0xAAAA))
#define FLIP1(x)  ((((x)>>1)&0x55) | (((x)<<1)&0xAA))


double tim(void);
void printtime(char *type, double time, int nchan, uint64_t totalbytes);

int main (int argc, char * const argv[]) {
  int i, n, c;
  uint8_t *idata8, *odata8, lookup8[256];
  uint16_t *idata16, *odata16, lookup16[65536];
  uint32_t *idata32, *odata32;  
  uint64_t *idata, *odata;
  double t1, t2;


  // Allocate amd initlalize memory
  n = BLOCKSIZE*1024*1024/sizeof(uint64_t);
  idata = malloc(n*sizeof(uint64_t));
  if (idata==NULL) {
    perror("Trying to allocate memory");
    exit(EXIT_FAILURE);
  }
  odata = malloc(n*sizeof(uint64_t));
  if (idata==NULL) {
    perror("Trying to allocate memory");
    exit(EXIT_FAILURE);
  }
  srandom((unsigned int)time(NULL));
  for (i=0;i<n;i++) {
    idata[i] = random();
  }

  c = 0;
  t1 = t2 = tim();
  while (t2-t1<TIME) {
    for (i=0; i<n; i++) {
      odata[i] = FLIP8(idata[i]);
    }
    c++;
    t2 = tim();
  }
  printf("FLIP8 did %.2f MB/sec\n", n*sizeof(uint64_t)*c/1024/1024/(t2-t1));

  c = 0;
  t1 = t2 = tim();
  idata32 = (int32_t*)idata;
  odata32 = (int32_t*)odata;
  while (t2-t1<TIME) {
    for (i=0; i<n*2; i++) {
      odata32[i] = FLIP4(idata32[i]);
    }
    c++;
    t2 = tim();
  }
  printf("FLIP4 did %.2f MB/sec\n", n*sizeof(uint64_t)*c/1024/1024/(t2-t1));

  c = 0;
  t1 = t2 = tim();
  idata16 = (int16_t*)idata;
  odata16 = (int16_t*)odata;
  while (t2-t1<TIME) {
    for (i=0; i<n*4; i++) {
      odata16[i] = FLIP2(idata16[i]);
    }
    c++;
    t2 = tim();
  }
  printf("FLIP2 did %.2f MB/sec\n", n*sizeof(uint64_t)*c/1024/1024/(t2-t1));

  c = 0;
  t1 = t2 = tim();
  idata8 = (int8_t*)idata;
  odata8 = (int8_t*)odata;
  while (t2-t1<TIME) {
    for (i=0; i<n*8; i++) {
      odata8[i] = FLIP1(idata8[i]);
    }
    c++;
    t2 = tim();
  }
  printf("FLIP1 did %.2f MB/sec\n", n*sizeof(uint64_t)*c/1024/1024/(t2-t1));

  for (i=0; i<256; i++) {
    lookup8[i] = FLIP1(i);
  }
  c = 0;
  t1 = t2 = tim();
  idata8 = (int8_t*)idata;
  odata8 = (int8_t*)odata;
  while (t2-t1<TIME) {
    for (i=0; i<n*8; i++) {
      odata8[i] = lookup8[idata8[i]];
    }
    c++;
    t2 = tim();
  }
  printf("lookup8 did %.2f MB/sec\n", n*sizeof(uint64_t)*c/1024/1024/(t2-t1));


  for (i=0; i<65536; i++) {
    lookup16[i] = FLIP2(i);
  }
  c = 0;
  t1 = t2 = tim();
  idata16 = (int16_t*)idata;
  odata16 = (int16_t*)odata;
  while (t2-t1<TIME) {
    for (i=0; i<n*4; i++) {
      odata16[i] = lookup16[idata16[i]];
    }
    c++;
    t2 = tim();
  }
  printf("lookup16 did %.2f MB/sec\n", n*sizeof(uint64_t)*c/1024/1024/(t2-t1));


}

double tim(void) {
  struct timeval tv;
  double t;

  gettimeofday(&tv, NULL);
  t = (double)tv.tv_sec + (double)tv.tv_usec/1000000.0;

  return t;
}
