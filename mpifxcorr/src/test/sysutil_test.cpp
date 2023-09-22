#include <fstream>
#include <string>
#include "alert.h"
#include "sysutil.h"

// echo -e "Line 1\nLine 2\nLine 3" > tmp
// valgrind --leak-check=full ./sysutil_test tmp

int main(int argc, const char** argv)
{
  if (argc != 2)
  {
    std::cout << "Usage: sysutil_test <anytextfile>" << std::endl;
    return 0;
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
}
