#ifndef SYSUTIL_H
#define SYSUTIL_H

#include <fstream>
#include <set>
#include <string>

/**
 * Return "new ifstream(file)", with re-attempts if opening the file fails,
 * e.g. when NFS produces a "Resource temporarily unavailable".
 */
std::ifstream * ifstreamOpen(const char* filename);

/**
 * Open or re-attempt to open a file in the given ifstream.
 */
void ifstreamOpen(std::ifstream& f, const char* filename);

/**
 * Read contents of a stream into a string.
 * \return True on success
 */
bool readFileToString(std::ifstream * in, std::string& out);

/**
 * Read contents of a file into a string.
 * \return True on success
 */
bool readFileToString(const char* filename, std::string& out);

/*
 * CPU / NUMA placement enquiry.
 *
 * These exist so a GPU Core can report (and a launcher can be checked against)
 * whether the rank is running on CPUs local to the GPU it drives. Host->device
 * transfers out of page-locked memory that sits on a NUMA node remote from the
 * GPU cross the inter-socket interconnect and run at roughly half bandwidth,
 * which is invisible in the code and only shows up as a slow run. All of them
 * are Linux/sysfs based and return false (or an empty result) elsewhere, so
 * callers must treat "unknown" as a normal outcome and not as an error.
 */

/**
 * Expand a sysfs/cpuset-style id list ("0-7,16,20-23") into the set of ids.
 * \return True if the whole string parsed cleanly.
 */
bool parseIdList(const std::string& list, std::set<int>& ids);

/**
 * Render a set of ids compactly, collapsing runs into ranges ("0-7,16").
 */
std::string formatIdList(const std::set<int>& ids);

/**
 * The CPUs this thread is currently allowed to run on (its affinity mask,
 * which under SLURM/cgroups is the set the launcher bound the rank to).
 * \return True if the mask could be read.
 */
bool getCpuAffinity(std::set<int>& cpus);

/**
 * The NUMA nodes spanned by the given CPUs.
 * \return True if the machine's NUMA topology could be read.
 */
bool getNumaNodesOfCpus(const std::set<int>& cpus, std::set<int>& nodes);

/**
 * The NUMA node a PCI device (e.g. a GPU, "0000:c1:00.0" as reported by
 * cudaDeviceGetPCIBusId) is attached to.
 * \return The node, or -1 if the platform does not expose one.
 */
int getPciNumaNode(const std::string& pciBusId);

#endif
