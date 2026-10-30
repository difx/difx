"""Step 2: run the enabled tools over each analysed component.

Besides the install-difx components, every directory below analysis.script_roots
that holds Python or shell scripts outside those components is analysed as a
scripts-only unit (by the tools that look at scripts), e.g. sites/MPIfR.

Output below <results>/<branch>/<date>/:
    <component path>/<tool>.<ext>   raw tool output
    <component path>/findings.json  normalised findings of all tools
    run.json                        checkout, tool versions, per-component counts
and <results>/<branch>/latest -> <date>.

A partial run (--only / --tools) adds to the latest run of the same commit
rather than starting a new date with only its own results.
"""

import datetime
import fnmatch
import json
import os
import re
import subprocess
import time
from collections import Counter
from dataclasses import dataclass

from . import compdb, components, diff, findings
from .config import REPO_ROOT
from .tools import TOOLS, ComponentFiles

SOURCE_EXT = compdb.C_CXX_EXT | {".h", ".hh", ".hpp", ".hxx", ".H"}
PYTHON_EXT = {".py"}
SHELL_EXT = {".sh", ".bash"}


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


def script_kind(src, rel):
    """'python', 'shell' or None, from the extension or else the #! line."""
    ext = os.path.splitext(rel)[1]
    if ext in PYTHON_EXT:
        return "python"
    if ext in SHELL_EXT:
        return "shell"
    if ext:
        return None
    try:
        with open(os.path.join(src, rel), "rb") as f:
            first = f.readline(200)
    except OSError:
        return None
    if not first.startswith(b"#!"):
        return None
    if b"python" in first:
        return "python"
    if re.search(rb"[/ ](ba)?sh\b", first):
        return "shell"
    return None


def script_owner(rel, comps, roots):
    """Path of the unit a script belongs to: its component, else its directory below a root."""
    owners = [c.path for c in comps if rel.startswith(c.path + "/")]
    if owners:
        return max(owners, key=len)
    parts = rel.split("/")
    if parts[0] in roots and len(parts) >= 3:
        return "/".join(parts[:2])
    return None


def assign_scripts(src, tracked, comps, roots, exclude):
    """{unit path: {"python": [...], "shell": [...]}} for all tracked, non-excluded scripts."""
    units = {}
    for rel in tracked:
        if any(fnmatch.fnmatch(rel, pat) for pat in exclude):
            continue
        kind = script_kind(src, rel)
        owner = script_owner(rel, comps, roots) if kind else None
        if owner:
            units.setdefault(owner, {"python": [], "shell": []})[kind].append(rel)
    return units


def component_files(comp, src, compiled, tracked, scripts, exclude):
    def excluded(rel):
        return any(fnmatch.fnmatch(rel, pat) for pat in exclude)

    def keep(rel):
        return (not comp.scripts_only and rel.startswith(comp.path + "/")
                and not excluded(rel))

    def ext(p):
        return os.path.splitext(p)[1]

    def absolute(paths):
        return [os.path.join(src, p) for p in paths]

    tracked_set = set(tracked)
    mine = scripts.get(comp.path, {})
    # untracked compiled files are generated (flex/bison output etc.)
    return ComponentFiles(
        compiled=[os.path.join(src, p) for p in compiled if keep(p) and p in tracked_set],
        sources=[os.path.join(src, p) for p in tracked if keep(p) and ext(p) in SOURCE_EXT],
        tracked=[os.path.join(src, p) for p in tracked if keep(p)],
        python=absolute(mine.get("python", [])),
        shell=absolute(mine.get("shell", [])),
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

    partial = bool(only or tools)
    active = select_tools(cfg, ctx, tools)
    if not active:
        raise RuntimeError("no tools to run")

    with open(ctx.db_path) as f:
        compiled = [os.path.relpath(e["file"], src) for e in json.load(f)]
    tracked = subprocess.run(["git", "-C", src, "ls-files"], check=True, text=True,
                             stdout=subprocess.PIPE).stdout.splitlines()
    exclude = a.get("exclude", [])

    comps = [c for c in components.read_components(cfg.path("install_script"), cfg.excluded)
             if c.analysed]
    roots = a.get("script_roots", components.CATEGORY_ORDER)
    scripts = assign_scripts(src, tracked, comps, roots, exclude)
    for path in sorted(set(scripts) - {c.path for c in comps}):
        comps.append(components.Component(path, True, path.split("/")[0], path,
                                           scripts_only=True))
    only = only or a.get("components")
    if only:
        unknown = set(only) - {c.name for c in comps}
        if unknown:
            raise RuntimeError(f"not analysed components: {', '.join(sorted(unknown))}")
        comps = [c for c in comps if c.name in only]

    branchdir = cfg.results_dir(checkout["branch"])
    if not date:
        date = datetime.date.today().isoformat()
        dates = diff.run_dates(branchdir)
        if partial and dates and date not in dates:
            date = dates[-1]
    rundir = os.path.join(branchdir, date)
    log(f"{checkout['branch']} @ {checkout['sha'][:10]} -> {rundir}")

    # a partial (--only / --tools) rerun of the same checkout keeps the run's other results
    runfile = os.path.join(rundir, "run.json")
    previous = {}
    if os.path.exists(runfile):
        with open(runfile) as f:
            previous = json.load(f)
    if partial and previous.get("checkout", {}).get("sha") != checkout["sha"]:
        # the run would hold only these results, and render would report the rest as fixed
        raise RuntimeError(f"no full run of {checkout['sha'][:10]} in {rundir}; "
                           "run `sa.py analyse` without --only/--tools first")
    if previous and previous.get("checkout", {}).get("sha") != checkout["sha"]:
        previous = {}
        # a Claude summary of the replaced results no longer applies
        for stale in ("claude-summary.md", "claude-digest.md"):
            if os.path.exists(os.path.join(rundir, stale)):
                os.remove(os.path.join(rundir, stale))
    active_names = {t.name for t in active}

    summary = {}
    for comp in comps:
        compdir = os.path.join(rundir, comp.path)
        os.makedirs(compdir, exist_ok=True)
        files = component_files(comp, src, compiled, tracked, scripts, exclude)
        result, timing = [], {}
        for tool in active:
            if not tool.inputs(files):
                continue
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
            "path": comp.path, "category": comp.category, "scripts_only": comp.scripts_only,
            "files": {"compiled": len(files.compiled), "sources": len(files.sources),
                      "tracked": len(files.tracked), "python": len(files.python),
                      "shell": len(files.shell)},
            "counts": {t: counts.get(t, 0) for t in sorted(counts.keys() | set(timing))},
            "seconds": timing,
        }
        log(f"{comp.path:34s} " + "  ".join(f"{t}={n}" for t, n in
                                             summary[comp.name]["counts"].items()))

    merged = previous.get("components", {})
    if not only:
        # every unit was analysed: drop ones that no longer exist
        merged = {k: v for k, v in merged.items() if k in summary}
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
