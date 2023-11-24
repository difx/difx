#!/usr/bin/env bash

jobfile=`realpath $1`
jobpath=`dirname $jobfile`
joblist=`basename $jobfile`
argextension=${1##*\.}

jobs=`tail -n +2 $jobpath/$joblist | cut -d " " -f 1`
jobscsv=`tail -n +2 $jobpath/$joblist | cut -d " " -f 1 | paste -sd","`
if [[ "$jobs" == "" ]]; then
	echo "difxjobslurm_stage2 error: could not find any scans in $jobpath/$joblist"
	exit 1
fi

if [[ "$DIFX_VERSION" == "" ]]; then
    echo "difxjobslurm_stage2 Warning: DIFX_VERSION env var is empty. Trying to correlate anyway..."
fi


echo
echo "Started difxjobslurm_stage2.sh under difx '$DIFX_VERSION' for $jobfile"
echo "Processing  $jobscsv"
echo "under SLURM allocation"
squeue -j $SLURM_JOBID -p correlator

## Ask SLURM for the names of the assigned compute nodes
COMPUTE_HOSTFILE=`mktemp /tmp/$joblist.compute.XXXXXX`
echo "Getting full list of compute node names..."
srun /usr/bin/true : /usr/bin/hostname -s | sort | uniq > $COMPUTE_HOSTFILE
ncompute=`cat $COMPUTE_HOSTFILE | wc -l`

if [[ $ncompute -le 2 ]]; then
	echo "Suspiciosly few compute nodes identified by 'srun', aborting."
	exit 1
fi


## Now dump the SLURM attachment to keep OpenMPI happy
unset SLURM_JOBID
unset SLURM_CLUSTER_NAME

## Handle each scan
for job in $jobs; do

	echo
	echo "At `date` switched to handling $job"
	if compgen -G "$job.difx/DIFX_*" > /dev/null; then
		echo "   $job.difx/DIFX_* file(s) already exists, skipping this job"
		continue
	fi

	## Generate .machines file (FxManager node + datastream nodes only)
	echo "Generating initial datastream-only .machines file..."
	rm -f $job.machines
	genmachines --nocompute $job.input

	## Append the compute nodes
	echo "Appending $ncompute compute nodes from ${COMPUTE_HOSTFILE}..."
	cat $COMPUTE_HOSTFILE >> $job.machines

	## Launch correlation
	startdifx --nomachines $job.input

done

# rm -f $COMPUTE_HOSTFILE
