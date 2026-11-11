#!/usr/bin/env python3
"""One-shot GPU-idle analysis of a benchprof nsys capture.

Runs every query used in the 2026-07-21 A100 host-tail-overlap
investigation (docs/gpu-profiling.md) and prints the key summary timings:
GPU busy/idle, the gap histogram, NVTX phase breakdown, kernel mix,
per-thread OS-runtime blocking, and the cuFFT per-exec-sync fingerprint.

Usage:
    ./nsys-analyze.py <capture>.nsys-rep        # exports sqlite next to it
    ./nsys-analyze.py <capture>.sqlite          # uses it directly

The Core rank is the only rank profiled by nsys-wrapper.sh, so a capture
holds exactly one correlator process. `nsys export` must be available
(desktop nsys 2025.6.3; the cluster login node's is too old - scp the
.nsys-rep to the desktop first). Single compute stream => GPU "busy" is
the union of kernel+memcpy intervals and "idle" is the gaps between them.
"""
import os
import sys
import subprocess
import sqlite3
from collections import defaultdict


def load(path):
    if path.endswith(".nsys-rep"):
        db = path[:-len(".nsys-rep")] + ".sqlite"
        if not os.path.exists(db) or os.path.getmtime(db) < os.path.getmtime(path):
            print(f"# exporting {os.path.basename(path)} -> {os.path.basename(db)}")
            subprocess.run(["nsys", "export", "--type", "sqlite",
                            "--force-overwrite", "true", "-o", db, path], check=True)
        path = db
    return sqlite3.connect(path)


def table_exists(c, name):
    return c.execute("SELECT 1 FROM sqlite_master WHERE type='table' AND name=?",
                     (name,)).fetchone() is not None


def h(title):
    print("\n" + "=" * 74 + f"\n== {title}\n" + "=" * 74)


def main():
    if len(sys.argv) != 2:
        print(__doc__)
        sys.exit(1)
    c = load(sys.argv[1])
    sids = {i: v for i, v in c.execute("SELECT id, value FROM StringIds")}
    names = {}
    if table_exists(c, "ThreadNames"):
        names = {t: sids.get(n, str(n))
                 for t, n in c.execute("SELECT globalTid, nameId FROM ThreadNames")}

    # ---- device + spans ---------------------------------------------------
    h("device & spans")
    if table_exists(c, "TARGET_INFO_GPU"):
        for (nm,) in c.execute("SELECT name FROM TARGET_INFO_GPU"):
            print(f"  GPU: {nm}")
    krow = c.execute("SELECT MIN(start), MAX(end), SUM(end-start), COUNT(*) "
                     "FROM CUPTI_ACTIVITY_KIND_KERNEL").fetchone()
    kspan = (krow[1] - krow[0]) / 1e6
    print(f"  kernel span (first->last kernel) : {kspan:9.1f} ms  ({krow[3]} kernels)")
    print(f"  kernel busy (sum of durations)   : {krow[2]/1e6:9.1f} ms  "
          f"({100*krow[2]/(krow[1]-krow[0]):.1f}% of span)")
    if table_exists(c, "CUPTI_ACTIVITY_KIND_RUNTIME"):
        rr = c.execute("SELECT MIN(start), MAX(end) "
                       "FROM CUPTI_ACTIVITY_KIND_RUNTIME").fetchone()
        print(f"  CUDA-API span (proxy for wall)   : {(rr[1]-rr[0])/1e6:9.1f} ms")

    # ---- GPU busy/idle union + gap histogram ------------------------------
    h("GPU busy / idle (union of kernels + memcpy on all streams)")
    iv = list(c.execute("SELECT start, end FROM CUPTI_ACTIVITY_KIND_KERNEL"))
    if table_exists(c, "CUPTI_ACTIVITY_KIND_MEMCPY"):
        iv += list(c.execute("SELECT start, end FROM CUPTI_ACTIVITY_KIND_MEMCPY"))
    iv.sort()
    merged = []
    cs, ce = iv[0]
    for s, e in iv[1:]:
        if s <= ce:
            ce = max(ce, e)
        else:
            merged.append((cs, ce)); cs, ce = s, e
    merged.append((cs, ce))
    span = merged[-1][1] - merged[0][0]
    busy = sum(e - s for s, e in merged)
    idle = span - busy
    print(f"  activity span : {span/1e6:9.1f} ms")
    print(f"  busy (union)  : {busy/1e6:9.1f} ms  ({100*busy/span:.1f}%)")
    print(f"  idle          : {idle/1e6:9.1f} ms  ({100*idle/span:.1f}%)")
    gaps = [(merged[i][0] - merged[i-1][1], merged[i-1][1])
            for i in range(1, len(merged)) if merged[i][0] > merged[i-1][1]]
    tot = sum(g for g, _ in gaps) or 1
    print(f"\n  idle gaps: {len(gaps)}, total {tot/1e6:.1f} ms")
    print(f"  {'bucket':>10}{'count':>8}{'total_ms':>11}{'%idle':>8}")
    for lo, hi, nm in [(0, 5e3, '<5us'), (5e3, 20e3, '5-20us'),
                       (20e3, 100e3, '20-100us'), (100e3, 500e3, '100-500us'),
                       (500e3, 9e18, '>500us')]:
        sel = [g for g, _ in gaps if lo <= g < hi]
        print(f"  {nm:>10}{len(sel):>8}{sum(sel)/1e6:>11.1f}{100*sum(sel)/tot:>7.1f}%")
    print("  (many small gaps => launch-overhead bound; few big gaps => host/data bound)")
    print("\n  top 8 idle gaps (ms @ t_ms from span start):")
    for g, at in sorted(gaps, reverse=True)[:8]:
        print(f"     {g/1e6:8.2f} ms @ t={(at-merged[0][0])/1e6:8.1f}")

    # ---- big-gap NVTX attribution ----------------------------------------
    if table_exists(c, "NVTX_EVENTS"):
        nvtx = list(c.execute("SELECT start, end, text FROM NVTX_EVENTS "
                              "WHERE end IS NOT NULL AND text IS NOT NULL"))
        h("where the big (>500us) idle sits (innermost NVTX range @ gap midpoint)")
        big = [(merged[i-1][1], merged[i][0]) for i in range(1, len(merged))
               if merged[i][0] - merged[i-1][1] > 500e3]
        att = defaultdict(lambda: [0, 0.0]); un = 0.0
        for gs, ge in big:
            mid = (gs + ge) / 2
            cov = sorted((e - s, tx) for s, e, tx in nvtx if s <= mid <= e)
            if cov:
                att[cov[0][1]][0] += 1; att[cov[0][1]][1] += ge - gs
            else:
                un += ge - gs
        print(f"  {len(big)} gaps, {sum(e-s for s,e in big)/1e6:.1f} ms total")
        for tx, (n, ms) in sorted(att.items(), key=lambda x: -x[1][1]):
            print(f"    {tx:<34}{n:>5}{ms/1e6:>9.1f} ms")
        print(f"    {'(no NVTX covering = between ranges)':<34}{'':>5}{un/1e6:>9.1f} ms")

        h("NVTX phase breakdown (host-side ranges)")
        agg = defaultdict(lambda: [0, 0.0])
        for s, e, tx in nvtx:
            agg[tx][0] += 1; agg[tx][1] += e - s
        print(f"  {'phase':<22}{'n':>7}{'total_ms':>11}{'avg_us':>10}")
        for tx, (n, t) in sorted(agg.items(), key=lambda x: -x[1][1]):
            print(f"  {tx:<22}{n:>7}{t/1e6:>11.1f}{t/n/1e3:>10.1f}")

    # ---- kernel mix -------------------------------------------------------
    h("kernel mix (% of kernel busy)")
    print(f"  {'kernel':<32}{'n':>6}{'ms':>9}{'pct':>7}")
    ktot = krow[2] or 1
    for nm, n, t in c.execute(
            "SELECT s.value, COUNT(*), SUM(e.end-e.start) "
            "FROM CUPTI_ACTIVITY_KIND_KERNEL e JOIN StringIds s ON e.shortName=s.id "
            "GROUP BY s.value ORDER BY SUM(e.end-e.start) DESC"):
        print(f"  {nm[:32]:<32}{n:>6}{t/1e6:>9.1f}{100*t/ktot:>6.1f}%")

    # ---- memcpy -----------------------------------------------------------
    if table_exists(c, "CUPTI_ACTIVITY_KIND_MEMCPY"):
        h("memcpy (1=H2D 2=D2H; volume + time on the compute stream)")
        for k, n, gb, ms in c.execute(
                "SELECT copyKind, COUNT(*), SUM(bytes)/1e9, SUM(end-start)/1e6 "
                "FROM CUPTI_ACTIVITY_KIND_MEMCPY GROUP BY copyKind"):
            print(f"  copyKind={k}  n={n:<7} {gb:6.2f} GB  {ms:7.1f} ms")

    # ---- per-thread OS-runtime blocking -----------------------------------
    if table_exists(c, "OSRT_API"):
        h("per-thread OS-runtime blocking (pthread_mutex_lock / MPI recv / poll)")
        by = defaultdict(lambda: defaultdict(lambda: [0, 0.0]))
        for s, e, nid, tid in c.execute(
                "SELECT start, end, nameId, globalTid FROM OSRT_API"):
            by[tid][sids.get(nid, str(nid))][0] += 1
            by[tid][sids.get(nid, str(nid))][1] += e - s
        # Housekeeping APIs (nsys/driver poll loops) are always ~= wall and
        # never a contention signal; a thread is only interesting if it blocks
        # in a real handoff/transfer API.
        HOUSEKEEP = {'poll', 'epoll_wait', 'sem_wait'}
        for tid in sorted(by, key=lambda t: -sum(v[1] for v in by[t].values())):
            tot_t = sum(v[1] for v in by[tid].values())
            real = sum(t for a, (n, t) in by[tid].items() if a not in HOUSEKEEP)
            if real < 5e8:  # skip threads with <0.5s of non-housekeeping blocking
                continue
            print(f"  tid {tid} ({names.get(tid,'?')}) blocked {tot_t/1e9:.2f}s:")
            for api, (n, t) in sorted(by[tid].items(), key=lambda x: -x[1][1])[:4]:
                print(f"      {api:<24}{n:>7}{t/1e9:>8.2f}s")

    # ---- cuFFT per-exec-sync fingerprint ----------------------------------
    if table_exists(c, "CUPTI_ACTIVITY_KIND_RUNTIME"):
        h("cuFFT per-exec-sync fingerprint (process/NVTX thread)")
        proc = None
        if table_exists(c, "NVTX_EVENTS"):
            r = c.execute("SELECT globalTid, COUNT(*) FROM NVTX_EVENTS "
                          "WHERE text IS NOT NULL GROUP BY globalTid "
                          "ORDER BY 2 DESC LIMIT 1").fetchone()
            proc = r[0] if r else None
        where = "WHERE globalTid=?" if proc else ""
        args = (proc,) if proc else ()
        d = defaultdict(lambda: [0, 0.0])
        for s, e, nid in c.execute(
                f"SELECT start, end, nameId FROM CUPTI_ACTIVITY_KIND_RUNTIME {where}", args):
            d[sids.get(nid, str(nid))][0] += 1; d[sids.get(nid, str(nid))][1] += e - s
        watch = ['cudaStreamSynchronize', 'cuStreamIsCapturing', 'cuLaunchKernel',
                 'cudaEventSynchronize', 'cudaLaunchKernel']
        print(f"  {'CUDA API':<28}{'count':>8}{'total_s':>9}{'avg_us':>9}")
        for api, (n, t) in sorted(d.items(), key=lambda x: -x[1][1]):
            if any(w in api for w in watch):
                print(f"  {api[:28]:<28}{n:>8}{t/1e9:>9.2f}{t/n/1e3:>9.1f}")
        ss = next((v for k, v in d.items() if 'cudaStreamSynchronize' in k), None)
        ic = next((v for k, v in d.items() if 'cuStreamIsCapturing' in k), None)
        if ss and ic:
            print(f"\n  cudaStreamSynchronize ({ss[0]}) ~= cuStreamIsCapturing ({ic[0]})"
                  f" => a stream sync per cufftExecC2C." if abs(ss[0]-ic[0]) < 0.05*ss[0]
                  else f"\n  cudaStreamSynchronize={ss[0]} cuStreamIsCapturing={ic[0]}"
                       " (counts differ - sync not 1:1 with FFT)")


if __name__ == "__main__":
    main()
