#include <fstream>
#include <iostream>
#include <set>
#include <string>
#include "alert.h"
#include "sysutil.h"

// echo -e "Line 1\nLine 2\nLine 3" > tmp
// valgrind --leak-check=full ./sysutil_test tmp
//
// The CPU/NUMA placement checks below need no argument and run either way.

static int failures = 0;

static void checkIdList(const char* list, size_t expectcount, const char* expectformat)
{
  std::set<int> ids;
  const bool ok = parseIdList(list, ids);
  const std::string formatted = formatIdList(ids);
  const bool pass = ok && ids.size() == expectcount && formatted == expectformat;
  if (!pass)
    failures++;
  std::cout << (pass ? "  ok   " : "  FAIL ") << "parseIdList(\"" << list << "\") -> "
            << ids.size() << " ids, formatIdList -> \"" << formatted
            << "\" (expected " << expectcount << ", \"" << expectformat << "\")" << std::endl;
}

static void checkIdListRejected(const char* list)
{
  std::set<int> ids;
  const bool pass = !parseIdList(list, ids);
  if (!pass)
    failures++;
  std::cout << (pass ? "  ok   " : "  FAIL ") << "parseIdList(\"" << list
            << "\") rejected as malformed" << std::endl;
}

// Exercise the id-list helpers on the shapes sysfs actually produces, then dump
// this process's real placement (which is informational - its value depends on
// how the test was launched, and is "unknown" wherever the platform does not
// expose a NUMA topology at all).
static void testPlacement()
{
  std::cout << "<<< id list parsing/formatting >>>" << std::endl;
  checkIdList("0-7", 8, "0-7");
  checkIdList("31", 1, "31");
  checkIdList("0-15,32-47", 32, "0-15,32-47");
  checkIdList("3,1,2", 3, "1-3");            // out of order, and made contiguous
  checkIdList("4,4,5", 2, "4-5");            // duplicates collapse
  checkIdList("", 0, "");                    // empty list is valid and empty
  checkIdListRejected("0-");
  checkIdListRejected("abc");
  checkIdListRejected("5-2");                // reversed range

  std::cout << "<<< this process's placement >>>" << std::endl;
  std::set<int> cpus, nodes;
  if (getCpuAffinity(cpus))
    std::cout << "  allowed CPUs: " << formatIdList(cpus) << " (" << cpus.size() << ")" << std::endl;
  else
    std::cout << "  allowed CPUs: unknown (not Linux, or affinity unreadable)" << std::endl;
  if (!cpus.empty() && getNumaNodesOfCpus(cpus, nodes))
    std::cout << "  NUMA nodes:   " << formatIdList(nodes) << std::endl;
  else
    std::cout << "  NUMA nodes:   unknown (no sysfs NUMA topology)" << std::endl;
  // A syntactically valid but certainly absent device must report "unknown"
  // rather than misbehaving, and a path-traversal attempt must be refused.
  if (getPciNumaNode("ffff:ff:1f.7") != -1)
  {
    failures++;
    std::cout << "  FAIL getPciNumaNode() invented a node for an absent device" << std::endl;
  }
  if (getPciNumaNode("../../../etc/hostname") != -1)
  {
    failures++;
    std::cout << "  FAIL getPciNumaNode() accepted a path-traversal id" << std::endl;
  }
}

int main(int argc, const char** argv)
{
  testPlacement();

  if (argc != 2)
  {
    std::cout << "Usage: sysutil_test <anytextfile>  (file tests skipped)" << std::endl;
    return failures == 0 ? 0 : 1;
  }

  std::ifstream * f1 = ifstreamOpen(argv[1]);
  std::cout << "Result: f1->fail()=" << f1->fail() << " f1->is_open()=" << f1->is_open() << std::endl;

  std::ifstream f2;
  ifstreamOpen(f2, argv[1]);
  std::cout << "Result: f2.fail()=" << f2.fail() << " f2.is_open()=" << f2.is_open() << std::endl;

  std::string contents;
  bool rc = readFileToString(argv[1], contents);
  std::cout << "Result: readFileToString(<filename>)=" << rc << std::endl;
  if (rc)
  {
    std::cout << "<<< contents >>>\n" << contents << std::endl;
  }

  rc = readFileToString(f1, contents);
  std::cout << "Result: readFileToString(ifstream&)=" << rc << std::endl;
  if (rc)
  {
    std::cout << "<<< contents >>>\n" << contents << std::endl;
  }

  delete f1;
  return failures == 0 ? 0 : 1;
}
