"""Compare a run's findings with the previous run of the same branch.

Findings are matched on Finding.key() (file, tool, check, message), ignoring the
line number so that unrelated edits shifting code do not show up as new/fixed.
Identical keys are matched by count.
"""

import os
import re
from collections import defaultdict

from . import findings

DATE_DIR = re.compile(r"^\d{4}-\d{2}-\d{2}$")


def run_dates(branchdir):
    """Dates with results for a branch, oldest first."""
    if not os.path.isdir(branchdir):
        return []
    return sorted(d for d in os.listdir(branchdir)
                  if DATE_DIR.match(d) and os.path.isdir(os.path.join(branchdir, d)))


def previous_date(branchdir, date):
    earlier = [d for d in run_dates(branchdir) if d < date]
    return earlier[-1] if earlier else None


def load_component(rundir, comp_path):
    path = os.path.join(rundir, comp_path, "findings.json")
    return findings.load(path) if os.path.exists(path) else None


def compare(current, previous):
    """Return (new, fixed) lists of Finding."""
    def by_key(items):
        groups = defaultdict(list)
        for f in items:
            groups[f.key()].append(f)
        return groups

    cur, prev = by_key(current), by_key(previous)
    new, fixed = [], []
    for k, items in cur.items():
        extra = len(items) - len(prev.get(k, []))
        if extra > 0:
            new += items[-extra:]
    for k, items in prev.items():
        gone = len(items) - len(cur.get(k, []))
        if gone > 0:
            fixed += items[-gone:]
    order = lambda f: (f.file, f.line, f.tool, f.check)
    return sorted(new, key=order), sorted(fixed, key=order)
