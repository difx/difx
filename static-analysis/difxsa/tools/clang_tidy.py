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
        result, cwd = [], self.ctx.src
        with open(raw_path) as f:
            for line in f:
                line = line.rstrip("\n")
                h = HEADER.match(line)
                if h:
                    cwd = h["dir"]
                    continue
                m = DIAG.match(line)
                if not m:
                    continue
                path = os.path.normpath(os.path.join(cwd, m["file"]))
                if not self.in_component(comp, path):
                    continue
                check = m["check"] or "clang-diagnostic-error"
                severity = "error" if m["sev"] == "error" else next(
                    (s for p, s in SEVERITY_BY_PREFIX.items() if check.startswith(p)),
                    "warning")
                result.append(Finding(comp.path, self.rel(path), int(m["line"]),
                                      int(m["col"]), self.name, check, severity, m["msg"]))
        return result
