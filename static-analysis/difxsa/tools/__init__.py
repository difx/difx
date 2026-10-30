"""Pluggable analysis tools.

Adding a tool means adding a module here with a Tool subclass (runner + parser to
the common Finding format) and listing it in TOOLS.  Each tool gets its config
section [tools.<name>] from config.toml.
"""

import os
import shutil
import subprocess
from dataclasses import dataclass, field


def find_exe(name, tool_bin=None):
    path = os.environ.get("PATH", "")
    if tool_bin:
        path = tool_bin + os.pathsep + path
    return shutil.which(name, path=path)


@dataclass
class ComponentFiles:
    """Files of one component handed to the tools.  Paths are absolute."""
    compiled: list = field(default_factory=list)  # in compile_commands.json
    sources: list = field(default_factory=list)   # tracked C/C++ sources and headers
    tracked: list = field(default_factory=list)   # every tracked file
    python: list = field(default_factory=list)    # *.py and python #! scripts
    shell: list = field(default_factory=list)     # *.sh, *.bash and sh/bash #! scripts


class Tool:
    name = ""
    raw_ext = "txt"
    default_exe = ""

    def __init__(self, settings, ctx):
        self.settings = settings  # this tool's [tools.<name>] table
        self.ctx = ctx            # analyse.Context
        self.exe = find_exe(settings.get("exe", self.default_exe), ctx.tool_bin)

    @property
    def available(self):
        return self.exe is not None

    def version(self):
        out = subprocess.run([self.exe, "--version"], text=True,
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT).stdout
        return next((l.strip() for l in out.splitlines() if "version" in l.lower()),
                    out.strip().splitlines()[0] if out.strip() else "unknown")

    def inputs(self, files):
        """The files this tool looks at; the tool is skipped when there are none."""
        raise NotImplementedError

    def start(self, units):
        """Optionally start work on all (component, files) units ahead of run()."""

    def stop(self):
        """Release whatever start() set up, cancelling work not yet collected."""

    def run(self, comp, files, raw_path):
        """Run on one component, writing raw output to raw_path."""
        raise NotImplementedError

    def parse(self, comp, raw_path):
        """Return the list of Finding parsed from raw_path."""
        raise NotImplementedError

    def rel(self, path):
        return os.path.relpath(path, self.ctx.src)

    def in_component(self, comp, path):
        return self.rel(path).startswith(comp.path + "/")


from . import clang_format, clang_tidy, compiler, cppcheck, ruff, shellcheck, typos  # noqa: E402

TOOLS = {t.name: t for t in (compiler.Compiler, clang_tidy.ClangTidy, cppcheck.Cppcheck,
                             clang_format.ClangFormat, ruff.Ruff, shellcheck.Shellcheck,
                             typos.Typos)}
