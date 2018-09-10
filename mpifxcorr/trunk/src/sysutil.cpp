#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <fstream>
#include <iostream>
#include "alert.h"

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
    cwarn << startl << "Could not open file " << filename << " (" << strerror(errno) << ") - retrying (" << attempt << "/" << max_attempts << ")" << endl;
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
    cwarn << startl << "Could not open file " << filename << " (" << strerror(errno) << ") - retrying (" << attempt << "/" << max_attempts << ")" << endl;
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
