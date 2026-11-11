"""Compiler warnings: each translation unit is re-run through the compiler that
built it (from compile_commands.json, so gcc, g++ or mpicxx with the real flags)
with -fsyntax-only plus extra warning flags.  Nothing is built.
"""

import json
import os
import re
import shlex
import subprocess
from concurrent.futures import ThreadPoolExecutor

from . import Tool
from .clang_tidy import HEADER
from ..findings import Finding

DIAG = re.compile(r"^(?P<file>[^:\s][^:]*):(?P<line>\d+):(?P<col>\d+): "
                  r"(?P<sev>warning|error): (?P<msg>.*?)(?: \[(?P<flag>-W[^\]]+)\])?$")

# Arguments dropped from the recorded command: output and dependency generation.
DROP_WITH_VALUE = {"-o", "-MF", "-MT", "-MQ"}
DROP = {"-c", "-MD", "-MMD", "-MP", "-M", "-MM"}
# Warning flags that are hygiene rather than likely bugs.
STYLE_PREFIXES = ("Wunused",)


def syntax_only(args, flags):
    out, skip = [], False
    for a in args:
        if skip:
            skip = False
        elif a in DROP_WITH_VALUE:
            skip = True
        elif a not in DROP:
            out.append(a)
    return [out[0], "-fsyntax-only", "-fdiagnostics-plain-output", *flags, *out[1:]]


class Compiler(Tool):
    name = "compiler"
    default_exe = "gcc"

    def inputs(self, files):
        return files.compiled

    def run(self, comp, files, raw_path):
        with open(self.ctx.db_path) as f:
            entries = {e["file"]: e for e in json.load(f)}
        flags = self.settings.get("flags", ["-Wall", "-Wextra"])

        def one(path):
            e = entries[path]
            args = e.get("arguments") or shlex.split(e["command"])
            p = subprocess.run(syntax_only(args, flags), cwd=e["directory"], text=True,
                               errors="replace", stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT)
            return f"### {self.rel(path)} (exit {p.returncode}) in {e['directory']}\n{p.stdout}"

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
                check = m["flag"].lstrip("-") if m["flag"] else m["sev"]
                severity = "style" if (m["sev"] == "warning"
                                       and check.startswith(STYLE_PREFIXES)) else m["sev"]
                result.append(Finding(comp.path, self.rel(path), int(m["line"]), int(m["col"]),
                                      self.name, check, severity, m["msg"]))
        return result
