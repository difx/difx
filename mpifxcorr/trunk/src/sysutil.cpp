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

/**
 * Read contents of a stream into a string.
 * \return True on success
 */
bool readFileToString(std::ifstream * in, std::string& out)
{
  out.clear();
  if(in->fail() || !in->is_open())
    return false;
  out = std::string((std::istreambuf_iterator<char>(*in)), (std::istreambuf_iterator<char>()));
  return true;
}

/**
 * Read contents of a file into a string.
 * \return True on success
 */
bool readFileToString(const char* filename, std::string& out)
{
  out.clear();
  std::ifstream * in = ifstreamOpen(filename);
  bool success = readFileToString(in, out);
  delete in;
  return success;
}

#ifdef TEST_SYSUTIL
// Helper functions, with DiFX lacking unit test framework can test these with:
// g++ sysutil.cpp -I. -I.. -DTEST_SYSUTIL -o sysutil_test
// echo -e "Line 1\nLine 2\nLine 3" > x
// valgrind --leak-check=full ./sysutil_test x
int main(int argc, char** argv)
{
  std::ifstream * f1 = ifstreamOpen(argv[1]);
  std::cout << "Result: f1->fail()=" << f1->fail() << " f1->is_open()=" << f1->is_open() << std::endl;

  std::ifstream f2;
  ifstreamOpen(f2, argv[1]);
  std::cout << "Result: f2.fail()=" << f2.fail() << " f2.is_open()=" << f2.is_open() << std::endl;

  std::string contents;
  bool rc = readFileToString(argv[1], contents);
  std::cout << "Result: readFileToString(<filename>)=" << rc << std::endl;
  if (rc) {
    std::cout << "<<< contents >>>\n" << contents << std::endl;
  }

  rc = readFileToString(f1, contents);
  std::cout << "Result: readFileToString(ifstream&)=" << rc << std::endl;
  if (rc) {
    std::cout << "<<< contents >>>\n" << contents << std::endl;
  }

  delete f1;
}
#endif
