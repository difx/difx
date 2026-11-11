// sched_getaffinity()/CPU_ISSET are GNU extensions; g++ defines this already,
// but say so explicitly so the declarations are visible however we are built.
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include "alert.h"
#include "sysutil.h"

#ifdef __linux__
#include <sched.h>
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

/**
 * Read one line of a sysfs file. Deliberately does NOT use ifstreamOpen(): a
 * missing sysfs attribute is an expected outcome here (older kernels, VMs,
 * non-Linux) and must not trigger its retry-and-warn loop. sysfs is always
 * local, so the NFS retry that opener exists for cannot apply.
 * \return True if a line was read.
 */
static bool readSysfsLine(const std::string& path, std::string& out)
{
  out.clear();
  std::ifstream in(path.c_str());
  if(!in.is_open())
    return false;
  std::getline(in, out);
  return !in.fail();
}

/**
 * Expand a sysfs/cpuset-style id list ("0-7,16,20-23") into the set of ids.
 */
bool parseIdList(const std::string& list, std::set<int>& ids)
{
  ids.clear();
  std::istringstream ss(list);
  std::string item;
  while(std::getline(ss, item, ','))
  {
    if(item.empty())
      continue;
    int lo, hi;
    const size_t dash = item.find('-');
    if(dash == std::string::npos)
    {
      if(sscanf(item.c_str(), "%d", &lo) != 1)
        return false;
      hi = lo;
    }
    else if(sscanf(item.c_str(), "%d-%d", &lo, &hi) != 2)
      return false;
    if(lo < 0 || hi < lo)
      return false;
    for(int i = lo; i <= hi; i++)
      ids.insert(i);
  }
  return true;
}

/**
 * Render a set of ids compactly, collapsing runs into ranges ("0-7,16").
 */
std::string formatIdList(const std::set<int>& ids)
{
  std::ostringstream out;
  std::set<int>::const_iterator it = ids.begin();
  while(it != ids.end())
  {
    const int lo = *it;
    int hi = lo;
    while(++it != ids.end() && *it == hi + 1)
      hi = *it;
    if(out.tellp() > 0)
      out << ",";
    out << lo;
    if(hi != lo)
      out << "-" << hi;
  }
  return out.str();
}

/**
 * The CPUs this thread is currently allowed to run on.
 */
bool getCpuAffinity(std::set<int>& cpus)
{
  cpus.clear();
#ifdef __linux__
  // CPU_SETSIZE (1024) covers every machine DiFX runs on today; a larger one
  // makes sched_getaffinity fail with EINVAL, which is reported as "unknown".
  cpu_set_t mask;
  CPU_ZERO(&mask);
  if(sched_getaffinity(0, sizeof(mask), &mask) != 0)
    return false;
  for(int i = 0; i < CPU_SETSIZE; i++)
    if(CPU_ISSET(i, &mask))
      cpus.insert(i);
  return !cpus.empty();
#else
  return false;
#endif
}

/**
 * The NUMA nodes spanned by the given CPUs.
 */
bool getNumaNodesOfCpus(const std::set<int>& cpus, std::set<int>& nodes)
{
  nodes.clear();
  std::string online;
  if(!readSysfsLine("/sys/devices/system/node/online", online))
    return false;
  std::set<int> onlinenodes;
  if(!parseIdList(online, onlinenodes))
    return false;
  for(std::set<int>::const_iterator n = onlinenodes.begin(); n != onlinenodes.end(); ++n)
  {
    std::ostringstream path;
    path << "/sys/devices/system/node/node" << *n << "/cpulist";
    std::string cpulist;
    std::set<int> nodecpus;
    if(!readSysfsLine(path.str(), cpulist) || !parseIdList(cpulist, nodecpus))
      continue;
    for(std::set<int>::const_iterator c = cpus.begin(); c != cpus.end(); ++c)
      if(nodecpus.count(*c) > 0)
      {
        nodes.insert(*n);
        break;
      }
  }
  return !nodes.empty();
}

/**
 * The NUMA node a PCI device is attached to, or -1 if unknown.
 */
int getPciNumaNode(const std::string& pciBusId)
{
  // Parse the caller's string as a PCI address and rebuild it canonically,
  // rather than filtering characters out of it: only four integers ever reach
  // the path, so nothing the caller passes can steer it elsewhere in the
  // filesystem, and CUDA's uppercase form ("0000:C1:00.0") is normalised to
  // the lowercase sysfs spelling for free. %x stops at the separators, and the
  // trailing %c catches anything after the function digit.
  unsigned int domain, bus, device, function;
  char trailing;
  if(sscanf(pciBusId.c_str(), "%x:%x:%x.%x%c",
            &domain, &bus, &device, &function, &trailing) != 4)
    return -1;
  char path[128];
  snprintf(path, sizeof(path), "/sys/bus/pci/devices/%04x:%02x:%02x.%x/numa_node",
           domain, bus, device, function);
  std::string value;
  if(!readSysfsLine(path, value))
    return -1;
  // Machines with no NUMA topology to report (single socket, VMs) say -1 here.
  const int node = atoi(value.c_str());
  return (node >= 0) ? node : -1;
}
