"""On-demand Claude summary of a run (not part of `sa.py run`).

Builds a compact digest of the run's findings, then runs the `claude` CLI
headless with read-only tools (Read, Grep, Glob) in the analysed source tree,
so it can check findings against the code.  The answer is saved as
<rundir>/claude-summary.md, which render publishes as its own wiki page.
"""

import datetime
import glob
import json
import os
import shutil
import subprocess
from collections import Counter, defaultdict

from . import compdb, diff
from .config import REPO_ROOT
from .findings import SEVERITIES

MAX_ERRORS = 150
MAX_NEW = 100
EXAMPLES = 3


def log(msg):
    print(f"[summary] {msg}", flush=True)


def find_claude(configured=None):
    """The claude CLI: configured path, PATH, the native installer, or the VS Code extension."""
    if configured:
        return configured
    candidates = [shutil.which("claude"),
                  os.path.expanduser("~/.local/bin/claude"),
                  os.path.expanduser("~/.claude/local/claude")]
    candidates += sorted(glob.glob(os.path.expanduser(
        "~/.vscode*/extensions/anthropic.claude-code-*/resources/native-binary/claude")),
        key=os.path.getmtime, reverse=True)
    return next((c for c in candidates if c and os.access(c, os.X_OK)), None)


def location(f):
    return f"{f.file}:{f.line}"


def digest(run, rundir, prevdir, only=None):
    """Markdown digest of a run, small enough to read in one go."""
    co = run["checkout"]
    out = [f"# Static-analysis digest: {co['branch']} @ {co['sha'][:10]}, {run['date']}", "",
           "Tools: " + ", ".join(f"{n} ({v})" for n, v in run["tools"].items()), "",
           "## Counts per unit", "",
           "| Unit | " + " | ".join(SEVERITIES) + " | total |",
           "|---|" + "---:|" * (len(SEVERITIES) + 1)]
    units = {name: c for name, c in run["components"].items()
             if not only or name in only or c["path"] in only}
    all_items, new_items = {}, []
    for name, comp in units.items():
        items = diff.load_component(rundir, comp["path"]) or []
        all_items[name] = items
        sev = Counter(f.severity for f in items)
        out.append(f"| {comp['path']} | " + " | ".join(str(sev.get(s, 0)) for s in SEVERITIES)
                   + f" | {len(items)} |")
        before = diff.load_component(prevdir, comp["path"]) if prevdir else None
        if before is not None:
            new_items += diff.compare(items, before)[0]

    errors = [f for items in all_items.values() for f in items if f.severity == "error"]
    out += ["", f"## All error-level findings ({len(errors)})", ""]
    out += [f"- `{location(f)}` {f.tool}/{f.check}: {f.message}" for f in errors[:MAX_ERRORS]]
    if len(errors) > MAX_ERRORS:
        out.append(f"- … {len(errors) - MAX_ERRORS} more")

    out += ["", "## Other findings by check (count, examples)", ""]
    for name, items in all_items.items():
        groups = defaultdict(list)
        for f in items:
            if f.severity != "error":
                groups[(f.tool, f.check, f.severity)].append(f)
        if not groups:
            continue
        out += [f"### {units[name]['path']}", ""]
        for (tool, check, sev), group in sorted(groups.items(), key=lambda kv: -len(kv[1])):
            ex = "; ".join(f"`{location(f)}` {f.message[:120]}" for f in group[:EXAMPLES])
            out.append(f"- {tool}/{check} [{sev}] ×{len(group)}: {ex}")
        out.append("")

    if prevdir:
        out += [f"## New findings since {os.path.basename(prevdir)} ({len(new_items)})", ""]
        out += [f"- `{location(f)}` [{f.severity}] {f.tool}/{f.check}: {f.message}"
                for f in new_items[:MAX_NEW]]
    return "\n".join(out) + "\n"


def run(cfg, only=None, model=None, max_turns=None):
    s = cfg.raw.get("summary", {})
    claude = find_claude(s.get("claude"))
    if not claude:
        raise RuntimeError("claude CLI not found; set summary.claude in config.toml")

    branch = compdb.read_checkout(cfg.workdir)["branch"]
    branchdir = cfg.results_dir(branch)
    dates = diff.run_dates(branchdir)
    if not dates:
        raise RuntimeError(f"no results for branch {branch}")
    rundir = os.path.join(branchdir, dates[-1])
    prev = diff.previous_date(branchdir, dates[-1])
    with open(os.path.join(rundir, "run.json")) as f:
        run_info = json.load(f)
    src = os.path.join(cfg.workdir, "src")
    if git_head(src) != run_info["checkout"]["sha"]:
        raise RuntimeError(f"{src} is not at the analysed commit; rerun `sa.py compdb`")

    digest_file = os.path.join(rundir, "claude-digest.md")
    with open(digest_file, "w") as f:
        f.write(digest(run_info, rundir, os.path.join(branchdir, prev) if prev else None, only))
    with open(os.path.join(REPO_ROOT, s.get("prompt", "static-analysis/claude-summary-prompt.md"))) as f:
        template = f.read()
    focus = (f"\nConcentrate on: {', '.join(only)}.\n" if only else "")
    prompt = template.replace("{digest}", digest_file).replace("{focus}", focus)

    cmd = [claude, "-p", prompt, "--output-format", "text",
           "--allowedTools", "Read,Grep,Glob", "--add-dir", rundir,
           "--max-turns", str(max_turns or s.get("max_turns", 40))]
    if model or s.get("model"):
        cmd += ["--model", model or s["model"]]
    log(f"asking {os.path.basename(claude)} about {branch} {dates[-1]} "
        f"(digest {os.path.getsize(digest_file) // 1024} KB); this takes a few minutes")
    p = subprocess.run(cmd, cwd=src, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if p.returncode or not p.stdout.strip():
        raise RuntimeError(f"claude failed (exit {p.returncode}): "
                           f"{(p.stderr + p.stdout).strip()[-2000:]}\n"
                           "(if it ran out of turns, raise --max-turns / summary.max_turns)")

    stamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    scope = f" (focus: {', '.join(only)})" if only else ""
    with open(os.path.join(rundir, "claude-summary.md"), "w") as f:
        f.write(f"_Written by Claude on {stamp}{scope} from the {run_info['date']} results; "
                "an AI review, so check before acting on it._\n\n" + p.stdout.strip() + "\n")
    log(f"saved {os.path.join(rundir, 'claude-summary.md')}")
    return 0


def git_head(src):
    return subprocess.run(["git", "-C", src, "rev-parse", "HEAD"], text=True,
                          stdout=subprocess.PIPE).stdout.strip()
