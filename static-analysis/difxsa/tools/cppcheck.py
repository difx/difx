"""cppcheck driven by compile_commands.json; XML (v2) output is parsed."""

import os
import subprocess
import xml.etree.ElementTree as ET

from . import Tool
from ..findings import Finding

# cppcheck's own bookkeeping messages, not code findings.
IGNORED_IDS = {"missingInclude", "missingIncludeSystem", "checkersReport",
               "normalCheckLevelMaxBranches", "toomanyconfigs", "unmatchedSuppression",
               "noValidConfiguration"}


class Cppcheck(Tool):
    name = "cppcheck"
    raw_ext = "xml"
    default_exe = "cppcheck"

    def inputs(self, files):
        return files.compiled

    def run(self, comp, files, raw_path):
        builddir = os.path.join(self.ctx.workdir, "cppcheck-build", comp.path)
        os.makedirs(builddir, exist_ok=True)
        cmd = [self.exe, "--project=" + self.ctx.db_path,
               "--file-filter=" + os.path.join(self.ctx.src, comp.path) + "/*",
               "--cppcheck-build-dir=" + builddir,
               "-j", str(self.ctx.jobs), "--quiet", "--xml",
               "--output-file=" + raw_path, *self.settings.get("args", [])]
        p = subprocess.run(cmd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        if p.returncode:
            raise RuntimeError(f"cppcheck failed (exit {p.returncode}): {p.stdout[-2000:]}")

    def parse(self, comp, raw_path):
        result = []
        for err in ET.parse(raw_path).getroot().iter("error"):
            if err.get("id") in IGNORED_IDS or err.get("severity") == "information":
                continue
            # anchor at the first location inside the component (the first one listed
            # may be in a header of another library); the others become related
            locs = [(os.path.join(self.ctx.src, l.get("file")), l) for l in err.iter("location")]
            mine = [(p, l) for p, l in locs if self.in_component(comp, p)]
            if not mine:
                continue
            path, loc = mine[0]
            related, seen = [], {int(loc.get("line", 0))}
            for p, l in locs:
                line = int(l.get("line", 0))
                if self.rel(p) == self.rel(path) and line not in seen:
                    seen.add(line)
                    related.append([self.rel(p), line, l.get("info", "")])
            severity = err.get("severity")
            if severity not in ("error", "warning", "performance", "portability"):
                severity = "style"
            result.append(Finding(comp.path, self.rel(path), int(loc.get("line", 0)),
                                  int(loc.get("column", 0)), self.name, err.get("id"),
                                  severity, err.get("msg"), related[:2]))
        return result
