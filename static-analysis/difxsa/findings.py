"""The common record every tool's output is normalised into."""

import json
from dataclasses import asdict, dataclass, field

# Normalised severities, most severe first.
SEVERITIES = ["error", "warning", "performance", "portability", "style"]


@dataclass
class Finding:
    component: str  # component path, e.g. libraries/difxio
    file: str       # relative to the repository root
    line: int
    column: int
    tool: str
    check: str      # tool-specific rule id
    severity: str   # one of SEVERITIES
    message: str
    # other places that explain the finding, e.g. where leaked memory was
    # allocated: [[file, line, note], ...]
    related: list = field(default_factory=list)

    def key(self):
        """Identity that survives unrelated edits moving the line (for run-to-run diffs)."""
        return (self.file, self.tool, self.check, self.message)


def dedupe(findings):
    """Drop repeats, e.g. a header warning reported once per including source file."""
    seen, result = set(), []
    for f in findings:
        k = (f.file, f.line, f.column, f.tool, f.check, f.message)
        if k not in seen:
            seen.add(k)
            result.append(f)
    return sorted(result, key=lambda f: (f.file, f.line, f.column, f.tool, f.check))


def save(findings, path):
    with open(path, "w") as f:
        json.dump([asdict(x) for x in findings], f, indent=1)


def load(path):
    with open(path) as f:
        return [Finding(**x) for x in json.load(f)]
