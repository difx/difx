#include <mpi.h>
#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <string.h>

#define UNIT_INT 0
#define UNIT_SEC 1

void senddata(int rank, int n, int nunit, int s, char *buffer, MPI_Comm comm)
{
	int i = 0, v;
	double t0 = MPI_Wtime();

	while (1)
	{
		v = MPI_Ssend(buffer, s, MPI_CHAR, rank+1, i, comm);
		printf("[%d] Sent %d -> %d\n", rank, i, v);
		i++;
		if ( ((nunit == UNIT_INT) && (i >= n)) || ((nunit == UNIT_SEC) && ((MPI_Wtime()-t0) >= n)) || (v != MPI_SUCCESS))
		{
			break;
		}
	}
}

void recvdata(int rank, int n, int nunit, int s, char *buffer, MPI_Comm comm)
{
	int i = 0, v;
	MPI_Status status;
	size_t stotal = 0;
	double t0 = MPI_Wtime();
	double dt_avg, dt, t = t0;

	while (1)
	{
		v = MPI_Recv(buffer, s, MPI_CHAR, rank-1, i, comm, &status);
		dt = MPI_Wtime() - t;
		t  = MPI_Wtime();
		dt_avg = t - t0;
		stotal += s;
		printf("[%d] Recvd %d -> %d : %.2f Mbps curr : %.2f Mbps mean\n", rank, i, v, 8e-6*s/dt, 8e-6*stotal/dt_avg);
		i++;
		if ( ((nunit == UNIT_INT) && (i >= n)) || ((nunit == UNIT_SEC) && ((MPI_Wtime()-t0) >= n)) || (v != MPI_SUCCESS))
		{
			break;
		}
	}
}

int main(int argc, char **argv)
{
	MPI_Comm world, return_comm;
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
	char *buffer;
	long long BufferSize = 1<<26;
	int NumSends = 256;
	int NumSendsUnit = UNIT_INT;
	double t0;
	double dt;

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
		printf("mpirun -H host1,host2,...,hostN %s [<numSends>|<timeSend>s] [<sendSizeMByte>]\n", argv[0]);
		printf("  where\n"
		       "    numSends : number of blocks to send (e.g., %d), or\n"
		       "    timeSend : duration in seconds to send (e.g., 100s)\n", NumSends);
		MPI_Barrier(world);
		MPI_Finalize();

		return EXIT_FAILURE;
	}

	if (argc > 1)
	{
		NumSends = atoi(argv[1]);
		if (argv[1][strlen(argv[1])-1] == 's')
		{
			NumSendsUnit = UNIT_SEC;
		}
	}
	if (argc > 2)
	{
		BufferSize = atoll(argv[2])*1048576;
	}

	printf("[%d] Starting\n", rank);

	buffer = (char *)malloc(BufferSize);

	MPI_Barrier(world);

	t0 = MPI_Wtime();

	if(rank % 2 == 0)
	{
		senddata(rank, NumSends, NumSendsUnit, BufferSize, buffer, world);
	}
	else
	{
		recvdata(rank, NumSends, NumSendsUnit, BufferSize, buffer, world);
	}

	dt = MPI_Wtime() - t0;

	MPI_Finalize();

	free(buffer);

	printf("[%d] Done\n", rank);

	if(rank == 0)
	{
		printf("Total memory transferred = %lld bytes in %.1f seconds\n", NumSends*BufferSize, dt);
	
		printf("Transfer rate was %f Mbps\n", NumSends*BufferSize*8.0/1000000.0/dt);
	}

	return EXIT_SUCCESS;
}
