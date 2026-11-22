"""clang-format in check mode; one finding per file that is not formatted to style."""

import re
import subprocess
from collections import Counter

from . import Tool
from ..findings import Finding

VIOLATION = re.compile(r"^(?P<file>/[^:]+):(?P<line>\d+):(?P<col>\d+): "
                       r"(?:warning|error): code should be clang-formatted")


class ClangFormat(Tool):
    name = "clang-format"
    default_exe = "clang-format"

    def inputs(self, files):
        return files.sources

    def run(self, comp, files, raw_path):
        style = self.settings.get("style", "file")
        with open(raw_path, "w") as f:
            for path in files.sources:
                p = subprocess.run([self.exe, "--dry-run", "--style=" + style, path],
                                   text=True, errors="replace",
                                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
                f.write(p.stderr)

    def parse(self, comp, raw_path):
        first, count = {}, Counter()
        with open(raw_path) as f:
            for line in f:
                m = VIOLATION.match(line)
                if m:
                    count[m["file"]] += 1
                    first.setdefault(m["file"], (int(m["line"]), int(m["col"])))
        return [Finding(comp.path, self.rel(path), *first[path], self.name,
                        "clang-format-violations", "style",
                        f"{n} place(s) differ from the configured style")
                for path, n in count.items()]
