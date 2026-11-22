"""ruff on Python scripts; JSON output is parsed.

Python 2 files produce a syntax error on nearly every print statement, so all
syntax errors of a file are reported as one finding.
"""

import json
import subprocess

from . import Tool
from ..findings import Finding

ERROR_CODES = ("invalid-syntax", "E9", "F7", "F82")  # cannot run, or fails when reached


class Ruff(Tool):
    name = "ruff"
    raw_ext = "json"
    default_exe = "ruff"

    def inputs(self, files):
        return files.python

    def run(self, comp, files, raw_path):
        cmd = [self.exe, "check", "--no-cache", "--output-format", "json", "--force-exclude",
               "--exit-zero"]
        if self.settings.get("config"):
            cmd += ["--config", self.ctx.repo_path(self.settings["config"])]
        p = subprocess.run(cmd + files.python, cwd=self.ctx.src, text=True,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if p.returncode:
            raise RuntimeError(f"ruff failed (exit {p.returncode}): {p.stderr[-2000:]}")
        with open(raw_path, "w") as f:
            f.write(p.stdout)

    def parse(self, comp, raw_path):
        with open(raw_path) as f:
            records = json.load(f)
        result, syntax = [], {}
        for r in records:
            code = r.get("code") or "invalid-syntax"
            loc = r["location"]
            if code == "invalid-syntax":
                first, n = syntax.get(r["filename"], (r, 0))
                syntax[r["filename"]] = (first, n + 1)
                continue
            severity = "error" if code.startswith(ERROR_CODES) else "warning"
            result.append(Finding(comp.path, self.rel(r["filename"]), loc["row"], loc["column"],
                                  self.name, code, severity, r["message"]))
        for path, (r, n) in syntax.items():
            more = f" ({n} syntax errors in file; Python 2?)" if n > 1 else ""
            result.append(Finding(comp.path, self.rel(path), r["location"]["row"],
                                  r["location"]["column"], self.name, "invalid-syntax", "error",
                                  r["message"] + more))
        return result
