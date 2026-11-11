#!/usr/bin/env bash
# nsys-wrapper.sh - launched as the executable under mpirun/srun so that ONLY
# the GPU Core rank is profiled under Nsight Systems; every other rank
# (manager, datastreams) runs mpifxcorr bare. All arguments are passed through
# to mpifxcorr.
#
# Env:
#   CORE_RANK  MPI rank to profile (default 11 = 1 manager + 10 datastreams).
#   NSYS_OUT   output report path without extension (default ./benchprof).
#   NSYS_TRACE trace list (default cuda,nvtx,osrt). Do NOT add --sample=cpu on
#              OpenMPI/PMIx clusters - it has crashed MPI teardown (tooarrana).
#
# The rank env var differs by launcher: OpenMPI sets OMPI_COMM_WORLD_RANK,
# SLURM sets SLURM_PROCID, MPICH/PMIx sets PMIX_RANK - take whichever is set.
: "${CORE_RANK:=11}"
: "${NSYS_OUT:=benchprof}"
: "${NSYS_TRACE:=cuda,nvtx,osrt}"

rank="${OMPI_COMM_WORLD_RANK:-${SLURM_PROCID:-${PMIX_RANK:-}}}"

# Remove a stale top-level output directory so mpifxcorr does not abort with
# "Output DIFX file ... already exists". Only the manager (rank 0) does this -
# it is the sole rank that creates and checks the output, so there is no race
# with the datastream/core ranks (which never touch the top-level output dir).
# Derived from the .input argument: <expname>.input -> <expname>.difx.
# Safety: only ever remove a *real directory* - refuse if the target is a
# symlink (so we never follow a link out to some unexpected location), and use
# `--` so a name beginning with '-' cannot be read as an rm option.
if [ "$rank" = "0" ]; then
    for a in "$@"; do
        case "$a" in
            *.input)
                outdir="${a%.input}.difx"
                if [ -L "$outdir" ]; then
                    echo "nsys-wrapper: refusing to remove '$outdir' - it is a symlink" >&2
                elif [ -d "$outdir" ]; then
                    rm -rf -- "$outdir"
                fi
                ;;
        esac
    done
fi

if [ "$rank" = "$CORE_RANK" ]; then
    exec nsys profile --trace="$NSYS_TRACE" --force-overwrite=true \
         -o "$NSYS_OUT" mpifxcorr "$@"
else
    exec mpifxcorr "$@"
fi
