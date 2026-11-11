#!/usr/bin/env bash
# nsys-frac.sh - time gpu_resultsrotatorMultiply on the 2-station benchprof2 job
# (same kernel dimensions as the 10-station benchprof: 2500 windows x 16 bands
# x 128 channels). Used for the Step-1 A/B that partitions the kernel's time
# into "atomic serialisation" vs "everything else": the control build replaces
# the autocorrelation atomicAdds with plain stores to the same addresses, which
# keeps the address arithmetic and the memory traffic but removes the atomic.
#
# Wraps mpirun (not the rank) - the desktop recipe; a per-rank wrapper does not
# survive FakeData teardown. Only the Core rank emits CUDA activity.
#
# Usage: ./nsys-frac.sh <tag>
set -u
TAG="${1:?usage: nsys-frac.sh <tag>}"
cd "$(dirname "$0")"
set +u; source ../../setup.bash >/dev/null 2>&1; set -u

rm -rf benchprof2_1.difx
DIFX_GPU_PIPELINE=0 nsys profile --trace=cuda --force-overwrite=true \
    -o "frac-$TAG" \
    mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
        -machinefile machines -np 4 \
        mpifxcorr benchprof2_1.input --nocommandthread --usegpu \
    > "frac-$TAG.runlog" 2>&1
echo "--- $TAG: kernel totals ---"
python3 - "frac-$TAG.sqlite" "frac-$TAG.nsys-rep" <<'PY'
import sqlite3, subprocess, sys, os
db, rep = sys.argv[1], sys.argv[2]
if not os.path.exists(db) or os.path.getmtime(db) < os.path.getmtime(rep):
    subprocess.run(["nsys","export","--type","sqlite","--force-overwrite","true","-o",db,rep],check=True)
c = sqlite3.connect(db)
sids = {i:v for i,v in c.execute("SELECT id, value FROM StringIds")}
tot = 0
rows = []
for sn,n,ms in c.execute("""select shortName, count(*), sum(end-start)/1e6
                            from CUPTI_ACTIVITY_KIND_KERNEL group by shortName
                            order by sum(end-start) desc"""):
    rows.append((sids.get(sn,str(sn)), n, ms)); tot += ms
for nm,n,ms in rows:
    print(f"  {nm[:38]:<38} {n:>5} {ms:9.1f} ms {100*ms/tot:5.1f}%  {1000*ms/n:7.1f} us/call")
print(f"  {'TOTAL':<38} {'':>5} {tot:9.1f} ms")
PY
