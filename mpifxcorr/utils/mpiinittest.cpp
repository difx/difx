#include <cstdio>
#include <time.h>
#include <mpi.h>

int main(int argc, char **argv)
{
	MPI_Comm world, return_comm;
	int numprocs, rank, namelen;
	char processor_name[MPI_MAX_PROCESSOR_NAME];
	char *buffer;
	clock_t t1, t2;
	struct timespec r1, r2;
	double t0;
	double dt;

	t1 = clock();
	clock_gettime(CLOCK_REALTIME, &r1);
	
	MPI_Init(&argc, &argv);

	clock_gettime(CLOCK_REALTIME, &r2);
	t2 = clock();
	
	world = MPI_COMM_WORLD;
	MPI_Comm_size(world, &numprocs);
	MPI_Comm_rank(world, &rank);

	fprintf(stderr, "Process %d:  t_cpu=%f s  t_real=%f s  T = %f to %f\n",
		rank, 
		(t2-t1)/(double)CLOCKS_PER_SEC, 
		r2.tv_sec - r1.tv_sec + 1.0e-9*(r2.tv_nsec - r1.tv_nsec), 
		(r1.tv_sec % 60) + 1.0e-9*r1.tv_nsec, 
		(r2.tv_sec % 60) + 1.0e-9*r2.tv_nsec);
	
	MPI_Comm_dup(world, &return_comm);
	MPI_Get_processor_name(processor_name, &namelen);

	MPI_Barrier(world);

	MPI_Finalize();

	return 0;
}
