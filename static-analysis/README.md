# DiFX static-analysis suite

Runs existing static-analysis tools over the DiFX components that `install-difx`
builds, and normalises their output for reporting.  Plain Python 3.11+ (standard
library only) plus the tools; intended to run unattended, e.g. daily from cron.

## Quick start

```bash
cd difx/static-analysis
./sa.py components          # what will be built / analysed
./sa.py run                 # fresh clone + build + compile db + all tools + wiki pages
./sa.py publish             # commit the regenerated pages in the wiki clone and push
```

or all in one: `./sa.py run --publish`.

Occasionally, for an AI review of the latest results (never part of `run`):

```bash
./sa.py summary                     # Claude summary of everything, then re-render
./sa.py summary --only difxio,mpifxcorr
./sa.py publish                     # the summary appears as Static-Analysis-Claude-Summary
```

Individual steps:

```bash
./sa.py compdb                          # clone, build under bear, compile_commands.json
./sa.py compdb --report-only            # re-filter the database and show coverage
./sa.py analyse                         # run all enabled tools
./sa.py analyse --only difxio,mpifxcorr --tools clang-tidy   # adds to the latest run of this commit
./sa.py render                          # markdown for the latest run into the wiki clone
./sa.py render --date 2026-10-29        # ... or for a given run
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
`analysis.exclude` (autotools boilerplate, third-party `applications/hops`) are
never analysed.  Python and shell scripts are also checked outside the built
components: every `<root>/<dir>` below `analysis.script_roots` that holds scripts
becomes a scripts-only unit (ruff, shellcheck) with its own page, e.g.
`sites/MPIfR`, `applications/polconvert`.
4. **render**: compares each component's findings with the previous run of the
   same branch (matched on file, tool, check and message, so code moving up or
   down does not count as a change) and writes markdown into the wiki clone
   (`paths.wiki` / `paths.wiki_subdir`):
   - `Static-Analysis.md`: counts per component by severity, grouped as
     libraries / mpifxcorr / applications / utilities, new/fixed, trend
   - `<category>/SA-<component path>.md`: one page per component with new and
     fixed findings, counts by check, and findings by file (collapsible), each
     linked to the source line on GitHub at the analysed commit

   GitHub wiki page names are global, hence the `SA-` prefix
   (`report.page_prefix`).  Generated pages carry a marker comment; pages for
   components no longer analysed are removed, nothing else is touched.
5. **publish**: commits only the generated directory in the wiki clone
   (message: branch, commit, date, total / new / fixed), rebases on the remote
   and pushes to `report.wiki_branch`.  It refuses to push if that would also
   publish local wiki commits touching other pages.  `--no-push` commits only.

## Tools

| Tool | Config | Notes |
|---|---|---|
| compiler | `[tools.compiler] flags` | the gcc/g++/mpicxx that built each file, `-fsyntax-only -Wall -Wextra` (minus deliberate patterns) |
| clang-tidy | `clang-tidy.yaml` | curated bug-finding checks, incl. clang static analyzer core/unix/cplusplus |
| cppcheck | `[tools.cppcheck] args` | uses the compile database |
| ruff | `ruff.toml` | Python: syntax errors (one finding per Python 2 file), undefined names, invalid comparisons |
| shellcheck | `[tools.shellcheck] args` | sh/bash scripts (by extension or `#!`), warning and above |
| typos | `typos.toml` | spelling in all tracked text files of built components; words inside longer identifiers (e.g. `FIRSR` in `ippsFIRSRGetSize`) are ignored; domain words / variable names whitelisted there |
| clang-format | `[tools.clang-format]` | disabled until DiFX has a `.clang-format` |

Adding a tool: a module in `difxsa/tools/` with a `Tool` subclass (`run` writes raw
output, `parse` returns `Finding`s), listed in `TOOLS`, plus a `[tools.<name>]`
section in `config.toml`.

## Claude summary (AI review)

An optional review of the latest results by Claude, run by hand when wanted; it is
never part of `sa.py run`.

### Getting a review

1. Make sure there is a full run of the current checkout (`./sa.py run`, or
   `compdb` + `analyse`).  `summary` stops if `<workdir>/src` has moved on from
   the analysed commit.
2. Run the summary from a terminal (VS Code need not be open):
   ```bash
   cd difx/static-analysis
   ./sa.py summary                          # everything; several minutes
   ./sa.py summary --only difxio,mpifxcorr  # or concentrate on some components (cheaper)
   ```
3. Read `<results>/dev/latest/claude-summary.md`, or the re-rendered page
   `Static-Analysis/Static-Analysis-Claude-Summary.md` in the wiki clone.
4. If it is worth sharing, `./sa.py publish` puts it on the wiki, linked from the
   Static-Analysis page.  Otherwise leave it: it stays local.

The summary belongs to its run: a later full `analyse` of a new commit removes it.

### Requirements

- The `claude` command-line tool, installed and logged in.  It is looked for on
  `PATH`, in `~/.local/bin` and `~/.claude/local`, then in the VS Code extension
  (`~/.vscode*/extensions/anthropic.claude-code-*`), whose copy shares the
  extension's login in `~/.claude`.  `[summary] claude` in `config.toml` sets the
  path explicitly.
- Claude quota: it runs on the logged-in account.  `--only` keeps a session small.
- `[summary] model` / `--model` choose the model (default: the CLI's default);
  `max_turns` / `--max-turns` (default 40) limit how long it may explore.  If it
  fails having run out of turns, raise that.

### What it does

1. Writes `claude-digest.md` into the run directory: counts per unit, every
   error-level finding, the other findings grouped by check with examples, and
   findings new since the previous run.
2. Runs `claude -p` headless in the analysed checkout (`<workdir>/src`) with the
   prompt in `claude-summary-prompt.md` and read-only tools only (`Read`, `Grep`,
   `Glob`): it can read the code to check findings, but cannot change files or run
   commands.
3. Saves the answer as `claude-summary.md` (headed as an AI review to check before
   acting on) and re-renders the wiki pages.

The summary has sections Overview, Most likely real bugs (with confidence and a
suggested fix), Likely false positives and noise, Python and shell scripts, and
Suggested next steps.  Edit `claude-summary-prompt.md` to change what it asks for.

## Requirements

Debian packages: `bear clang-tidy clang-format cppcheck shellcheck libomp-19-dev`
(LLVM 19; libomp so clang can parse mpifxcorr's OpenMP code), and the normal DiFX
build dependencies.  Tools not packaged by Debian go in a venv that
`build.tool_bin` points at (searched before `PATH`):

```bash
python3 -m venv ../difx-sa-work/venv
../difx-sa-work/venv/bin/pip install typos ruff lizard
```

## Running unattended

A full run (build ~6 min, analysis ~3 min on 32 cores) needs no interaction, so
it can also go in cron, e.g. weekly:

```cron
# m h dom mon dow
30 2 * * 1  cd /path/to/difx/static-analysis && ./sa.py run --publish > ../../difx-sa-work/logs/cron.log 2>&1
```

Pushing needs non-interactive write access to the wiki (e.g. an SSH key).
