#include <difxmessage.h>
#include <mpi.h>
#include <iostream>

#include "configuration.h"
#include "configurationstorage.h"

int main(int argc, char** argv)
{
  MPI_Comm mpicomm = MPI_COMM_WORLD; // or MPI_COMM_SELF
  const char* difxMessageID = "test";
  int mpiid;

  const char* inputfilename = "nonexistent.input";
  if (argc == 2)
  {
    inputfilename = argv[1];
  }

  // Initialize MPI and difxmessage libary
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(mpicomm, &mpiid);
  difxMessageInit(mpiid, difxMessageID);
  difxMessageSetInputFilename(inputfilename);

  // With file load on all nodes
  Configuration* config = new Configuration(inputfilename, mpiid, 0.0);
  std::cout << "node#" << mpiid << ": creating local Configuration() from file " << inputfilename << "\n";

  // With file pre-load on root node and share over MPI
  std::cout << "node#" << mpiid << ": creating blank ConfigurationStorage()\n";
  ConfigurationStorage* configstorage = new ConfigurationStorage();
  if (mpiid == 0)
  {
    std::cout << "node#" << mpiid << ": loading " << inputfilename << " into master ConfigurationStorage\n";
    configstorage->readInputfileAndAncillaries(inputfilename);
  }
  std::cout << "node#" << mpiid << ": starting exchange of ConfigurationStorage\n";
  int nexchanged = configstorage->exchangeOverMPI(&mpicomm, mpiid);
  std::cout << "node#" << mpiid << ": exchange returned " << nexchanged << " files\n";

  // Done
  MPI_Finalize();

  //delete config; // causes segfault during dealloc, need to fix Configuration class

  return 0;
}
