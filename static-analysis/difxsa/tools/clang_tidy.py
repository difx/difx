"""clang-tidy, run per translation unit in parallel; text output is parsed.

clang-tidy prints file names as the compiler saw them, i.e. relative to the
translation unit's build directory, so each TU's output in the raw file is
preceded by a header line carrying that directory.
"""

import json
import os
import re
import subprocess
from concurrent.futures import ThreadPoolExecutor

from . import Tool
from ..findings import Finding

HEADER = re.compile(r"^### (?P<file>\S+) \(exit (?P<exit>-?\d+)\) in (?P<dir>.+)$")
DIAG = re.compile(r"^(?P<file>[^:\s][^:]*):(?P<line>\d+):(?P<col>\d+): "
                  r"(?P<sev>warning|error): (?P<msg>.*?)(?: \[(?P<check>[^\]]+)\])?$")
NOTE = re.compile(r"^(?P<file>[^:\s][^:]*):(?P<line>\d+):(?P<col>\d+): note: (?P<msg>.*)$")
# Analyzer path notes that point at the origin of a problem reported elsewhere
# (a leak is reported where the memory is lost, not where it was allocated).
ORIGIN_NOTE = re.compile(r"Memory is allocated|Returned allocated memory|Stream opened here|"
                         r"initialized to a null pointer value|Null pointer value stored|"
                         r"declared without an initial value|Storing uninitialized value")
MAX_RELATED = 2

# Check-name prefix -> normalised severity (default: warning).
SEVERITY_BY_PREFIX = {
    "performance-": "performance",
    "portability-": "portability",
    "readability-": "style",
    "modernize-": "style",
}


class ClangTidy(Tool):
    name = "clang-tidy"
    default_exe = "clang-tidy"

    def __init__(self, settings, ctx):
        super().__init__(settings, ctx)
        self.extra_args = ["-Wno-unknown-warning-option", *settings.get("extra_args", [])]

    def _command(self, path):
        # all non-system headers; findings outside the component are dropped in parse()
        cmd = [self.exe, "-p", self.ctx.src, "--quiet", "--header-filter=.*"]
        cmd += ["--extra-arg=" + a for a in self.extra_args]
        if self.settings.get("config"):
            cmd.append("--config-file=" + self.ctx.repo_path(self.settings["config"]))
        return cmd + [path]

    def inputs(self, files):
        return files.compiled

    def run(self, comp, files, raw_path):
        with open(self.ctx.db_path) as f:
            directory = {e["file"]: e["directory"] for e in json.load(f)}

        def one(path):
            p = subprocess.run(self._command(path), text=True, errors="replace",
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            # stderr is mostly "N warnings generated"; keep it only on failure
            tail = p.stderr if p.returncode else ""
            return (f"### {self.rel(path)} (exit {p.returncode}) in {directory[path]}\n"
                    f"{p.stdout}{tail}")

        with ThreadPoolExecutor(self.ctx.jobs) as pool:
            outputs = list(pool.map(one, files.compiled))
        with open(raw_path, "w") as f:
            f.writelines(outputs)

    def parse(self, comp, raw_path):
        result, cwd, current = [], self.ctx.src, None
        with open(raw_path) as f:
            for line in f:
                line = line.rstrip("\n")
                h = HEADER.match(line)
                if h:
                    cwd, current = h["dir"], None
                    continue
                n = NOTE.match(line)
                if n:
                    if current and ORIGIN_NOTE.search(n["msg"]) \
                            and len(current.related) < MAX_RELATED:
                        rel = self.rel(os.path.normpath(os.path.join(cwd, n["file"])))
                        entry = [rel, int(n["line"]), n["msg"]]
                        if (rel, int(n["line"])) != (current.file, current.line) \
                                and entry not in current.related:
                            current.related.append(entry)
                    continue
                m = DIAG.match(line)
                if not m:
                    continue  # source snippet lines between a warning and its notes
                current = None
                path = os.path.normpath(os.path.join(cwd, m["file"]))
                if not self.in_component(comp, path):
                    continue
                check = m["check"] or "clang-diagnostic-error"
                severity = "error" if m["sev"] == "error" else next(
                    (s for p, s in SEVERITY_BY_PREFIX.items() if check.startswith(p)),
                    "warning")
                current = Finding(comp.path, self.rel(path), int(m["line"]),
                                  int(m["col"]), self.name, check, severity, m["msg"])
                result.append(current)
        return result
