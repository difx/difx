"""typos spell checker on every tracked file; JSON-lines output is parsed.

typos splits identifiers into words (ippsFIRSRGetSize -> ipps, FIRSR, Get,
Size), so it flags pieces of external API names and of our own names that
can't be renamed cheaply.  Those are dropped: a typo is kept only when it is
a whole word, as in comments, strings and messages.
"""

import json
import re
import subprocess
from functools import lru_cache

from . import Tool
from ..findings import Finding

BATCH = 500  # files per typos invocation, to keep command lines short
WORD_CHAR = re.compile(rb"\w")


@lru_cache(maxsize=64)
def read_lines(path):
    with open(path, "rb") as f:
        return f.read().splitlines()


def in_identifier(path, line_num, offset, word):
    """True when the word is only part of a longer name, e.g. FIRSR in ippsFIRSRGetSize."""
    try:
        line = read_lines(path)[line_num - 1]
    except (OSError, IndexError):
        return False
    end = offset + len(word.encode())
    before = line[offset - 1:offset] if offset > 0 else b""
    return bool(WORD_CHAR.match(before) or WORD_CHAR.match(line[end:end + 1]))


class Typos(Tool):
    name = "typos"
    raw_ext = "jsonl"
    default_exe = "typos"

    def inputs(self, files):
        return files.tracked

    def run(self, comp, files, raw_path):
        # --force-exclude: apply the config's excludes to files named on the command line
        cmd = [self.exe, "--format", "json", "--force-exclude"]
        if self.settings.get("config"):
            cmd += ["--config", self.ctx.repo_path(self.settings["config"])]
        with open(raw_path, "w") as f:
            for i in range(0, len(files.tracked), BATCH):
                # exit status 2 just means typos were found
                p = subprocess.run(cmd + files.tracked[i:i + BATCH], cwd=self.ctx.src,
                                   text=True, stdout=f, stderr=subprocess.PIPE)
                if p.returncode not in (0, 2):
                    raise RuntimeError(f"typos failed (exit {p.returncode}): {p.stderr}")

    def parse(self, comp, raw_path):
        result = []
        with open(raw_path) as f:
            for line in f:
                rec = json.loads(line)
                if rec.get("type") != "typo" or in_identifier(
                        rec["path"], rec["line_num"], rec.get("byte_offset", 0), rec["typo"]):
                    continue
                fixes = ", ".join(rec.get("corrections", [])) or "?"
                result.append(Finding(comp.path, self.rel(rec["path"]), rec["line_num"],
                                      rec.get("byte_offset", 0) + 1, self.name, "typo",
                                      "style", f"`{rec['typo']}` should be `{fixes}`"))
        return result
