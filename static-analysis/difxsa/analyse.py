"""Step 2: run the enabled tools over each analysed component.

Output below <results>/<branch>/<date>/:
    <component path>/<tool>.<ext>   raw tool output
    <component path>/findings.json  normalised findings of all tools
    run.json                        checkout, tool versions, per-component counts
and <results>/<branch>/latest -> <date>.
"""

import datetime
import fnmatch
import json
import os
import subprocess
import time
from collections import Counter
from dataclasses import dataclass

from . import compdb, components, findings
from .config import REPO_ROOT
from .tools import TOOLS, ComponentFiles

SOURCE_EXT = compdb.C_CXX_EXT | {".h", ".hh", ".hpp", ".hxx", ".H"}


def log(msg):
    print(f"[analyse] {msg}", flush=True)


@dataclass
class Context:
    """What tools need to know about the run."""
    src: str
    workdir: str
    db_path: str
    jobs: int
    tool_bin: str

    @staticmethod
    def repo_path(path):
        return os.path.join(REPO_ROOT, path)


def component_files(comp, src, compiled, tracked, exclude):
    def keep(rel):
        return rel.startswith(comp.path + "/") and not any(
            fnmatch.fnmatch(rel, pat) for pat in exclude)

    def ext(p):
        return os.path.splitext(p)[1]

    tracked_set = set(tracked)
    # untracked compiled files are generated (flex/bison output etc.)
    return ComponentFiles(
        compiled=[os.path.join(src, p) for p in compiled if keep(p) and p in tracked_set],
        sources=[os.path.join(src, p) for p in tracked if keep(p) and ext(p) in SOURCE_EXT],
        tracked=[os.path.join(src, p) for p in tracked if keep(p)],
    )


def select_tools(cfg, ctx, wanted):
    tools = []
    for name, cls in TOOLS.items():
        settings = cfg.raw.get("tools", {}).get(name, {})
        if wanted is not None:
            if name not in wanted:
                continue
        elif not settings.get("enabled", False):
            continue
        tool = cls(settings, ctx)
        if tool.available:
            tools.append(tool)
        else:
            log(f"WARNING: {name} not found, skipped")
    return tools


def run(cfg, only=None, tools=None, date=None):
    work = cfg.workdir
    src = os.path.join(work, "src")
    a = cfg.raw.get("analysis", {})
    ctx = Context(src=src, workdir=work, db_path=os.path.join(src, "compile_commands.json"),
                  jobs=a.get("jobs", os.cpu_count()), tool_bin=cfg.tool_bin)
    checkout = compdb.read_checkout(work)

    comps = [c for c in components.read_components(cfg.path("install_script"), cfg.excluded)
             if c.analysed]
    only = only or a.get("components")
    if only:
        unknown = set(only) - {c.name for c in comps}
        if unknown:
            raise RuntimeError(f"not analysed components: {', '.join(sorted(unknown))}")
        comps = [c for c in comps if c.name in only]

    active = select_tools(cfg, ctx, tools)
    if not active:
        raise RuntimeError("no tools to run")

    with open(ctx.db_path) as f:
        compiled = [os.path.relpath(e["file"], src) for e in json.load(f)]
    tracked = subprocess.run(["git", "-C", src, "ls-files"], check=True, text=True,
                             stdout=subprocess.PIPE).stdout.splitlines()
    exclude = a.get("exclude", [])

    date = date or datetime.date.today().isoformat()
    branchdir = cfg.results_dir(checkout["branch"])
    rundir = os.path.join(branchdir, date)
    log(f"{checkout['branch']} @ {checkout['sha'][:10]} -> {rundir}")

    # a partial (--only / --tools) rerun of the same checkout keeps the day's other results
    runfile = os.path.join(rundir, "run.json")
    previous = {}
    if os.path.exists(runfile):
        with open(runfile) as f:
            previous = json.load(f)
        if previous.get("checkout", {}).get("sha") != checkout["sha"]:
            previous = {}
    active_names = {t.name for t in active}

    summary = {}
    for comp in comps:
        compdir = os.path.join(rundir, comp.path)
        os.makedirs(compdir, exist_ok=True)
        files = component_files(comp, src, compiled, tracked, exclude)
        result, timing = [], {}
        for tool in active:
            raw = os.path.join(compdir, f"{tool.name}.{tool.raw_ext}")
            t0 = time.time()
            tool.run(comp, files, raw)
            result += tool.parse(comp, raw)
            timing[tool.name] = round(time.time() - t0, 1)
        findings_file = os.path.join(compdir, "findings.json")
        if comp.name in previous.get("components", {}) and os.path.exists(findings_file):
            result += [f for f in findings.load(findings_file) if f.tool not in active_names]
        result = findings.dedupe(result)
        findings.save(result, findings_file)
        counts = Counter(f.tool for f in result)
        summary[comp.name] = {
            "path": comp.path, "category": comp.category,
            "files": {"compiled": len(files.compiled), "sources": len(files.sources),
                      "tracked": len(files.tracked)},
            "counts": {t: counts.get(t, 0) for t in sorted(counts.keys() | active_names)},
            "seconds": timing,
        }
        log(f"{comp.path:34s} " + "  ".join(f"{t}={n}" for t, n in
                                             summary[comp.name]["counts"].items()))

    merged = previous.get("components", {})
    for name, s in summary.items():
        s["seconds"] = {**merged.get(name, {}).get("seconds", {}), **s["seconds"]}
        merged[name] = s
    with open(runfile, "w") as f:
        json.dump({"date": date, "checkout": checkout,
                   "tools": {**previous.get("tools", {}),
                             **{t.name: t.version() for t in active}},
                   "components": merged}, f, indent=1)
    latest = os.path.join(branchdir, "latest")
    if os.path.islink(latest):
        os.remove(latest)
    if not os.path.exists(latest):
        os.symlink(date, latest)
    return 0
