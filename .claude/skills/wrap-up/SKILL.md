---
name: wrap-up
description: End a working session on gpudifx so the next person (or session) can pick it up - nothing left uncommitted, the diff reviewed and pushed, the wiki to-do list and the benchmark ledger updated, and what is pending stated plainly. Use when the user says they are finishing, stopping, rebooting or handing over.
---

# Session wrap-up (gpudifx)

Several people work on `gpudifx`, and much of its state lives in the repository rather
than in anyone's head. A session is not finished until someone else could pick up from
the repository alone.

Do these in order, then stop. Do not start new work during a wrap-up.

## 1. Leave the tree clean

- `git status`: no uncommitted source changes. A later session cannot tell work in
  progress from an abandoned experiment. Commit it (step 2), or, if it is truly
  throwaway, say so and ask before discarding it.
- Allowed to stay uncommitted: local scale-downs of `tests/Synthetic/test-*.v2d`/`.vex`,
  and git-ignored files (`setup.local.bash`, test data, `local-runs/`).
- No stray build products in the source tree (`git status --ignored` if in doubt).
- Delete scratch branches and worktrees that are finished (`git worktree list`,
  `git branch`).

## 2. Commit and push

- Show the user the complete diff (`git --no-pager diff --color=always`, and the full
  content of new files) and **wait for their agreement in a separate message** before
  committing. Being told earlier to "commit at the end" is not that agreement.
- The commit message says what changed and why, and what was tested (scenarios,
  `gpu0`/`gpu1`, phase cal or not).
- Push `gpudifx` once the user agrees. Report the branch and the HEAD hash.
- If tests were not run, or something failed, say so in the commit message and in your
  report. Don't push a known regression to the shared branch without the user deciding to.

## 3. Update the shared records

- **The to-do list on the wiki** (linked from `TODO.md`): delete items that landed (the
  commit records them), add what came up, and keep the "Agreed order" section current.
  It is outside the repository, so show the user the change before saving it.
- **`tests/FakeData/BENCHMARKS.md`**: any timing or profile worth keeping, with the
  commit, machine, GPU and configuration it was measured on. Headline results also go on
  the wiki benchmarks page; the `benchmark` skill says which goes where.
- **`mpifxcorr/GPU_MERGE_NOTES.md`**: tick off a merge follow-up from section 7 if one
  was closed.
- Fix any reference you noticed to a file or branch that no longer exists.
- These records go in the same commit as the work, or a follow-up commit, through the
  same review step.

## 4. Report

In a few lines:
- branch, HEAD, pushed or not;
- what was done, and what was tested;
- what is pending or half-done, and what to check first next time;
- anything running elsewhere (cluster jobs, with their IDs and where output will land).

Then stop.
