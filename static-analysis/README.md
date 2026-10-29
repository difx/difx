# DiFX static-analysis suite

Runs existing static-analysis tools over the DiFX components that `install-difx`
builds, and normalises their output for reporting.  Plain Python 3.11+ (standard
library only) plus the tools; intended to run unattended, e.g. daily from cron.

## Quick start

```bash
cd difx/static-analysis
./sa.py components          # what will be built / analysed
./sa.py run                 # fresh clone + build + compile db + all tools
```

Individual steps:

```bash
./sa.py compdb                          # clone, build under bear, compile_commands.json
./sa.py compdb --report-only            # re-filter the database and show coverage
./sa.py analyse                         # run all enabled tools
./sa.py analyse --only difxio,mpifxcorr --tools clang-tidy
```

## What it does

1. **Components** come from the `components` dict in `install-difx` (read, not
   executed), so True/False there decides what is built and analysed.
   `[components.exclude]` in `config.toml` lists components that are built but
   not analysed (e.g. difxcalc11, which is Fortran).
2. **compdb**: clones `source.repo` at `source.ref` into `<workdir>/src`, makes a
   copy of `setup.bash` with `DIFXROOT=<workdir>/install`, and runs
   `install-difx --doonly=<enabled components>` under `bear`.  The normal DiFX
   checkout and installation are not touched.  The filtered database is
   `<workdir>/src/compile_commands.json`; per-component coverage is printed.
3. **analyse**: for each analysed component runs every enabled tool, writing to
   `<results>/<branch>/<date>/<component path>/`:
   - raw tool output (`clang-tidy.txt`, `cppcheck.xml`, `typos.jsonl`, ...)
   - `findings.json`: all tools' findings in one format
     (`component, file, line, column, tool, check, severity, message`)

   plus `run.json` (commit, tool versions, counts) and a `latest` symlink.

Untracked files (flex/bison output and other generated sources) and the globs in
`analysis.exclude` are never analysed.

## Tools

| Tool | Config | Notes |
|---|---|---|
| clang-tidy | `clang-tidy.yaml` | curated bug-finding checks, incl. clang static analyzer core/unix/cplusplus |
| cppcheck | `[tools.cppcheck] args` | uses the compile database |
| typos | `typos.toml` | spelling in all tracked text files; domain words / variable names whitelisted there |
| clang-format | `[tools.clang-format]` | disabled until DiFX has a `.clang-format` |

Adding a tool: a module in `difxsa/tools/` with a `Tool` subclass (`run` writes raw
output, `parse` returns `Finding`s), listed in `TOOLS`, plus a `[tools.<name>]`
section in `config.toml`.

## Requirements

Debian packages: `bear clang-tidy clang-format cppcheck` (LLVM 19), and the
normal DiFX build dependencies.  `libomp-19-dev` is recommended so clang can parse
mpifxcorr's OpenMP code.  Tools not packaged by Debian go in a venv that
`build.tool_bin` points at (searched before `PATH`):

```bash
python3 -m venv ../difx-sa-work/venv
../difx-sa-work/venv/bin/pip install typos lizard
```

## Running from cron

```cron
# m h dom mon dow
30 2 * * *  cd $HOME/difx/static-analysis && ./sa.py run > ../../difx-sa-work/logs/cron.log 2>&1
```

A full run (build ~6 min, analysis ~3 min on 32 cores) needs no interaction.
