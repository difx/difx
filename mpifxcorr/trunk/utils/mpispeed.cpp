#include <mpi.h>
#include <cstdlib>
#include <cstdio>
#include <time.h>


void senddata(int rank, int n, int s, char *buffer, MPI_Comm comm)
{
	int i, v;

	for(i = 0; i < n; i++)
	{
		v = MPI_Send(buffer, s, MPI_CHAR, rank+1, i, comm);
		printf("[%d] Sent %d -> %d\n", rank, i, v);
	}
}

void recvdata(int rank, int n, int s, char *buffer, MPI_Comm comm)
{
	int i, v;
	MPI_Status status;

	for(i = 0; i < n; i++)
	{
		v = MPI_Recv(buffer, s, MPI_CHAR, rank-1, i, comm, &status);
		printf("[%d] Recvd %d -> %d\n", rank, i, v);
	}
}

int main(int argc, char **argv)
{
	MPI_Comm world, return_comm;
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
	char *buffer;
	const long long BufferSize = 1<<26;
	const int NumSends = 256;
	time_t t0;
	float dt;

	MPI_Init(&argc, &argv);
	world = MPI_COMM_WORLD;
	MPI_Comm_size(world, &numprocs);
	MPI_Comm_rank(world, &rank);
	MPI_Comm_dup(world, &return_comm);
	MPI_Get_processor_name(processor_name, &namelen);

	printf("Processor = %s\n", processor_name);
	printf("Rank = %d/%d\n", rank, numprocs);

	if(numprocs %2 != 0)
	{
		printf("Sorry, must run with even number of processes\n");
		printf("This program should be invoked in a manner similar to:\n");
		printf("\nmpirun -H host1,host2,...,hostN %s\n", argv[0]);
		MPI_Barrier(world);
		MPI_Finalize();

		return EXIT_FAILURE;
	}

	printf("[%d] Starting\n", rank);

	buffer = (char *)malloc(BufferSize);

	MPI_Barrier(world);

	t0 = time(0);

	if(rank % 2 == 0)
	{
		senddata(rank, NumSends, BufferSize, buffer, world);
	}
	else
	{
		recvdata(rank, NumSends, BufferSize, buffer, world);
	}

	dt = time(0) - t0;

	MPI_Finalize();

	free(buffer);

	printf("[%d] Done\n", rank);

	if(rank == 0)
	{
		printf("Total memory transferred = %lld bytes in %1.0f seconds\n", NumSends*BufferSize, dt);
	
		printf("Transfer rate was %f Mbps\n", NumSends*BufferSize*8.0/1000000.0/dt);
	}

	return EXIT_SUCCESS;
}
