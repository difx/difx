#ifndef __SYS_UTIL_H__
#define __SYS_UTIL_H__

#include <fstream>

/**
 * Return "new ifstream(file)", with re-attempts if opening the file fails,
 * e.g. when NFS produces a "Resource temporarily unavailable".
 */
std::ifstream * ifstreamOpen(const char* filename);

/**
 * Open or re-attempt to open a file in the given ifstream.
 */
void ifstreamOpen(std::ifstream& f, const char* filename);

#endif
