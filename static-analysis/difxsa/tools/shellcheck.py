"""shellcheck on sh/bash scripts; json1 output is parsed."""

import json
import subprocess

from . import Tool
from ..findings import Finding

SEVERITY = {"error": "error", "warning": "warning", "info": "style", "style": "style"}
BATCH = 200


class Shellcheck(Tool):
    name = "shellcheck"
    raw_ext = "jsonl"
    default_exe = "shellcheck"

    def inputs(self, files):
        return files.shell

    def run(self, comp, files, raw_path):
        cmd = [self.exe, "--format=json1", *self.settings.get("args", [])]
        with open(raw_path, "w") as f:
            for i in range(0, len(files.shell), BATCH):
                # exit status 1 just means issues were found
                p = subprocess.run(cmd + files.shell[i:i + BATCH], cwd=self.ctx.src,
                                   text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                if p.returncode not in (0, 1):
                    raise RuntimeError(f"shellcheck failed (exit {p.returncode}): {p.stderr}")
                f.write(p.stdout.strip() + "\n")

    def parse(self, comp, raw_path):
        result = []
        with open(raw_path) as f:
            for line in f:
                if not line.strip():
                    continue
                for c in json.loads(line)["comments"]:
                    result.append(Finding(comp.path, self.rel(c["file"]), c["line"], c["column"],
                                          self.name, f"SC{c['code']}",
                                          SEVERITY.get(c["level"], "style"), c["message"]))
        return result
