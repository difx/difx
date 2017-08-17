#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#ifndef TEST_SYSUTIL
#include "alert.h"
#endif

/**
 * Return "new ifstream(file)", with re-attempts if opening the file fails.
 * Sometimes NFS produces a "Resource temporarily unavailable".
 */
std::ifstream * ifstreamOpen(const char* filename)
{
  int attempt = 1, max_attempts = 10;
  std::ifstream * ifs = new std::ifstream(filename);
  while ((attempt <= max_attempts) && (ifs->fail() || !ifs->is_open())) {
    if (errno == ENOENT)
      break;
#ifndef TEST_SYSUTIL
    cwarn << startl << "Could not open file " << filename << " (" << strerror(errno) << ") - retrying (" << attempt << "/" << max_attempts << ")" << endl;
#else
    std::cout << "Could not open file " << filename << " (" << strerror(errno) << ") - retrying (" << attempt << "/" << max_attempts << ")" << std::endl;
#endif
    ifs->clear();
    usleep(1e6);
    ifs->open(filename);
    attempt++;
  }
  return ifs;
}

/**
 * Open or re-attempt to open a file in the given ifstream.
 */
void ifstreamOpen(std::ifstream& f, const char* filename)
{
  int attempt = 1, max_attempts = 10;
  f.open(filename);
  while ((attempt <= max_attempts) && (f.fail() || !f.is_open())) {
    if (errno == ENOENT)
      break;
#ifndef TEST_SYSUTIL
    cwarn << startl << "Could not open file " << filename << " (" << strerror(errno) << ") - retrying (" << attempt << "/" << max_attempts << ")" << endl;
#else
    std::cout << "Could not open file " << filename << " (" << strerror(errno) << ") - retrying (" << attempt << "/" << max_attempts << ")" << std::endl;
#endif
    f.clear();
    usleep(1e6);
    f.open(filename);
    attempt++;
  }
}

#ifdef TEST_SYSUTIL
// g++ sysutil.cpp -I. -I.. -DTEST_SYSUTIL -o sysutil_test
int main(int argc, char** argv)
{
  std::ifstream * f1;
  std::ifstream f2;
  f1 = ifstreamOpen(argv[1]);
  std::cout << "Result: f1->fail()=" << f1->fail() << " f1->is_open()=" << f1->is_open() << std::endl;
  ifstreamOpen(f2, argv[1]);
  std::cout << "Result: f2.fail()=" << f2.fail() << " f2.is_open()=" << f2.is_open() << std::endl;

}
#endif
