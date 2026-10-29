"""Step 4: commit the generated wiki pages and push them.

Only the generated directory (paths.wiki_subdir) is committed, so other edits
in the wiki clone are never swept in.  Before pushing, the clone is rebased on
the remote; the push is refused if it would also publish local wiki commits
that touch anything outside the generated directory.
"""

import json
import os
import subprocess

from . import compdb, diff


def log(msg):
    print(f"[publish] {msg}", flush=True)


def git(repo, *args, check=True):
    p = subprocess.run(["git", "-C", repo, *args], text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if check and p.returncode:
        raise RuntimeError(f"git {' '.join(args)} failed:\n{p.stdout}")
    return p


def commit_message(cfg, branch):
    """One-line summary of the latest run, e.g. for `git log --oneline`."""
    branchdir = cfg.results_dir(branch)
    dates = diff.run_dates(branchdir)
    if not dates:
        return "Static-Analysis: update"
    rundir = os.path.join(branchdir, dates[-1])
    with open(os.path.join(rundir, "run.json")) as f:
        run = json.load(f)
    total = sum(sum(c["counts"].values()) for c in run["components"].values())
    msg = (f"Static-Analysis: {branch} @ {run['checkout']['sha'][:10]}, "
           f"analysed {run['date']}: {total} findings")
    dfile = os.path.join(rundir, "diff.json")
    if os.path.exists(dfile):
        with open(dfile) as f:
            d = json.load(f)
        if d.get("previous"):
            new = sum(c["new"] for c in d["components"].values())
            fixed = sum(c["fixed"] for c in d["components"].values())
            msg += f" ({new} new, {fixed} fixed since {d['previous']})"
    return msg


def foreign_commits(wiki, remote_ref, subdir):
    """Local commits not on the remote that touch files outside subdir."""
    result = []
    for sha in git(wiki, "rev-list", f"{remote_ref}..HEAD").stdout.split():
        files = git(wiki, "diff-tree", "--no-commit-id", "--name-only", "-r", sha).stdout.split()
        if any(not f.startswith(subdir + "/") for f in files):
            result.append(git(wiki, "log", "-1", "--format=%h %s", sha).stdout.strip())
    return result


def run(cfg, push=True, branch=None):
    wiki = cfg.path("wiki")
    subdir = os.path.relpath(cfg.wiki_dir, wiki)
    remote = cfg.raw.get("report", {}).get("wiki_remote", "origin")
    wiki_branch = cfg.raw.get("report", {}).get("wiki_branch", "master")
    branch = branch or compdb.read_checkout(cfg.workdir)["branch"]

    current = git(wiki, "rev-parse", "--abbrev-ref", "HEAD").stdout.strip()
    if current != wiki_branch:
        raise RuntimeError(f"{wiki} is on branch {current}, expected {wiki_branch}")

    git(wiki, "add", "-A", "--", subdir)
    if git(wiki, "diff", "--cached", "--quiet", "--", subdir, check=False).returncode == 0:
        log(f"no changes in {subdir}")
    else:
        msg = commit_message(cfg, branch)
        # a pathspec commits only these paths, whatever else is staged
        git(wiki, "commit", "-q", "-m", msg, "--", subdir)
        log(f"committed: {msg}")

    if not push:
        log("not pushing (--no-push)")
        return 0
    git(wiki, "pull", "-q", "--rebase", "--autostash", remote, wiki_branch)
    remote_ref = f"{remote}/{wiki_branch}"
    foreign = foreign_commits(wiki, remote_ref, subdir)
    if foreign:
        log("NOT pushing: these local wiki commits change pages outside "
            f"{subdir}; push them yourself if intended:\n  " + "\n  ".join(foreign))
        return 1
    ahead = git(wiki, "rev-list", "--count", f"{remote_ref}..HEAD").stdout.strip()
    if ahead == "0":
        log("wiki already up to date")
        return 0
    git(wiki, "push", "-q", remote, wiki_branch)
    log(f"pushed {ahead} commit(s) to {remote} {wiki_branch}")
    return 0
