#include <difxmessage.h>
#include <mpi.h>
#include "configuration.h"

int main(int argc, char** argv)
{
  Configuration* config;
  const char* difxMessageID = "test";
  const char* inputfilename = "nonexistent.input";
  int mpiid = 0;

  if (argc == 2)
  {
    inputfilename = argv[1];
  }

  MPI_Init(&argc, &argv);

  difxMessageInit(mpiid, difxMessageID);
  difxMessageSetInputFilename(inputfilename);

  config = new Configuration(inputfilename, mpiid, 0.0);

  MPI_Finalize();

  //delete config; // causes segfault during dealloc, need to fix Configuration class

  return 0;
}
