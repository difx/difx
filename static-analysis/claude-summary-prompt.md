You are reviewing the output of the DiFX static-analysis suite (compiler warnings,
clang-tidy, cppcheck, ruff, shellcheck, typos) for the DiFX VLBI correlator.

The digest of the run is in `{digest}`. Read it first.
The analysed source tree (the exact commit that was analysed) is the current
directory; read source files to check findings before you judge them.
{focus}
Write a summary for the DiFX developers, in GitHub markdown, with these sections
(level-2 headings, no preamble, no closing remarks):

## Overview
Three to six sentences: overall state, where findings concentrate, and what changed
since the previous run if the digest has a "New findings" section.

## Most likely real bugs
Up to 10 findings you checked in the source and believe are genuine defects,
most serious first. For each: `file:line`, tool/check, one or two sentences on what
goes wrong and when, confidence (high / medium / low), and a one-line suggested fix.
Prefer the libraries and mpifxcorr. Do not list a finding you have not read the code for.

## Likely false positives and noise
Recurring checks or patterns that are mostly not defects in this codebase, with a
concrete suggestion for each (disable or suppress the check, add a word to
typos.toml, etc.).

## Python and shell scripts
Brief: scripts that cannot run (e.g. Python 2 syntax), undefined names, and the
directories most affected.

## Suggested next steps
Three to five concrete actions, in priority order.

Keep it under about 900 words. Use plain `file:line` references (they are
relative to the repository root). Do not modify any files.
