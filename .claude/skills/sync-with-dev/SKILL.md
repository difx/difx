---
name: sync-with-dev
description: Move changes between dev and gpudifx in the agreed way - merge origin/dev into gpudifx (no rebase, hand-port dev changes into code the GPU branch moved, review with remerge-diff, test), and send fixes to shared code back to dev through a short-lived bugfix branch. Use when dev has moved on, or when a gpudifx change fixes a bug that dev also has.
---

# Syncing gpudifx with dev

`gpudifx` is a long-lived branch of difx/difx that other people pull. Its history is
kept as merges: dev comes **in** by merge, and fixes to shared code go **out** to dev on
their own branch. Nothing is rebased, force-pushed or cherry-picked.

One-time setup in each clone, so that `git pull` keeps merge commits even if
`branch.autosetuprebase` is set globally:

```bash
git config branch.gpudifx.rebase merges
```

## A. Merging dev into gpudifx

1. **Start clean.** `git status` shows nothing to commit; `git fetch origin`; gpudifx is
   up to date with `origin/gpudifx`.
2. **See what is coming.**
   ```bash
   git log --oneline HEAD..origin/dev
   git diff --stat $(git merge-base HEAD origin/dev) origin/dev -- mpifxcorr libraries
   ```
   For every dev change under `mpifxcorr/src`, decide before merging whether it lands in
   code the GPU branch moved or duplicated:
   - dev's `Mode::process` (`mode.cpp`) is `CPUMode::process` (`cpumode.cpp`) here, and
     `Mode::process` is an `abort()` stub. Dev changes there must be **hand-ported** into
     `CPUMode::process`.
   - dev's `Mk5Mode` is `Mk5_CPUMode` here.
   - `pcal.cpp` is dev's file plus the GPU buffer injection (`setPcalReal`,
     `setPcalComplex`, the `*_owned` flags). Take dev's version and re-apply the injection.
   - Any behaviour change to CPU processing also needs a decision about its GPU twin (see
     the CPU/GPU map in the `difx-bug-review` skill). Port it, or add it to the wiki to-do
     list (linked from `TODO.md`).
3. **Merge without committing:**
   ```bash
   git merge --no-ff --no-commit origin/dev
   ```
   Resolve conflicts **one at a time**, and check each resolution against both parents.
   Never resolve a whole file or tree with `-X ours`, `-X theirs` or `checkout --ours`
   without reading it. A clean auto-merge can still be wrong where code moved: check
   every item from step 2 even if git reported no conflict there.
4. **Build and test** before committing: `rebuild-and-verify`, then `gpu-parity-test`
   (all scenarios, plus phase cal if `pcal.cpp` or `mode.cpp` changed).
5. **Commit** (only after the user has seen the diff and agreed). The message lists what
   came from dev, every hand port, and anything deferred to the wiki to-do list. Then
   review the result as a reviewer will see it:
   ```bash
   git show --remerge-diff HEAD      # only what differs from git's own automatic merge
   ```
   Correct anything you find with `git commit --amend` **before** pushing.
6. **Record it.** Update the wiki to-do list, linked from `TODO.md`: new follow-ups, items
   closed. Add a section to `mpifxcorr/GPU_MERGE_NOTES.md` only if the merge needed real
   decisions (hand ports, behaviour changes), so reviewers can find them.
7. **Push** `gpudifx`, with the user's agreement.

## B. Sending a fix for shared code to dev

When work on gpudifx finds a bug that dev also has (in code that is not GPU-specific):

1. Make a short-lived branch off dev in a separate worktree, so gpudifx is not disturbed:
   ```bash
   git fetch origin
   git worktree add ../difx-bugfix-<topic> -b bugfix-<topic> origin/dev
   ```
2. **Hand-port** the fix to dev's code; don't cherry-pick, because the code is structured
   differently (see A.2). Keep the commit to just the fix, with a message that says how
   it is triggered.
3. Build and test it **against dev's own code**, installing into a separate prefix (never
   over your gpudifx install). Reproduce the bug on unfixed dev first if you can.
4. Show the diff, and get the user's agreement. Then, depending on your rights on difx/difx:
   - maintainers: `git checkout dev && git merge --no-ff bugfix-<topic>`, push dev, then
     delete the branch locally and on GitHub (`git push origin --delete bugfix-<topic>`);
   - everyone else: push `bugfix-<topic>` and open a pull request against `dev`
     (see `CONTRIBUTION.md`).
5. Remove the worktree: `git worktree remove ../difx-bugfix-<topic>`.
6. Bring the fix into gpudifx with section A (`git merge --no-ff origin/dev`), and record
   in that merge whether the GPU twin needed the same fix.

## Never

- `git rebase` of gpudifx onto dev, or `git push --force` to gpudifx or dev.
- Committing a fix to shared code only on gpudifx when dev has the same bug.
- Merging gpudifx into dev. That needs the refactor in the wiki to-do list and a separate
  review first.
