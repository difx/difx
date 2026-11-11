"""Claude code review of DiFX, one unit at a time (`sa.py review`).

A unit is a group of files of one component, listed in config.toml under
[[review.units]] in priority order.  Each unit is reviewed in its own headless
`claude -p` session with read-only tools (Read, Grep, Glob), in a checkout
pinned at one commit (<review dir>/src), following the difx-bug-review skill.
Claude answers with JSON; the code snippets are quoted by this module from the
pinned checkout, so they are exact.

Issues go to one wiki page per component.  People edit those pages (status,
notes, issues of their own), so a page is never regenerated: new issues are
appended (skipping ones already there), and only the block between the
review:units markers is rewritten.  The index page is fully generated.

The queue, <review dir>/state.json, records each unit's status and what each
claude call cost, so runs can be stopped and resumed, e.g. from cron.  A run
stops at Claude's usage limit, at the weekly cost cap, or after --max-units.
"""

import datetime
import fcntl
import fnmatch
import hashlib
import json
import os
import re
import subprocess
import time

from . import components, diff, findings, publish
from .config import REPO_ROOT, resolve
from .summary import find_claude

SOURCE_EXT = (".c", ".cpp", ".cc", ".cxx", ".h", ".hpp", ".hh", ".y", ".l")
SEVERITY_ORDER = ["critical", "major", "minor"]
STATUSES = ["open", "confirmed", "not a bug", "fixed", "won't fix"]
MAX_SNIPPET = 30
MAX_LEADS = 200
LIMIT_RE = re.compile(r"usage limit|limit reached|hit your limit|rate.?limit|"
                      r"out of extra usage|session limit|weekly limit", re.I)
UNITS_BEGIN, UNITS_END = "<!-- review:units:begin -->", "<!-- review:units:end -->"
ISSUE_MARK = re.compile(r"<!-- review:issue (\S+) key=(\w+)")


def log(msg):
    print(f"[review] {msg}", flush=True)


# ---------------------------------------------------------------- settings / state

def settings(cfg):
    return cfg.raw.get("review", {})


def review_dir(cfg):
    return resolve(settings(cfg).get("dir", "../difx-sa-work/reviews"))


def units_config(cfg):
    units = settings(cfg).get("units", [])
    names = [u["name"] for u in units]
    if len(set(names)) != len(names):
        raise RuntimeError("duplicate unit names in [[review.units]]")
    return {u["name"]: u for u in units}


def load_state(cfg):
    path = os.path.join(review_dir(cfg), "state.json")
    if not os.path.exists(path):
        return None
    with open(path) as f:
        return json.load(f)


def save_state(cfg, state):
    path = os.path.join(review_dir(cfg), "state.json")
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        json.dump(state, f, indent=1)
    os.replace(tmp, path)


def git(repo, *args, check=True):
    p = subprocess.run(["git", "-C", repo, *args], text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if check and p.returncode:
        raise RuntimeError(f"git {' '.join(args)} failed:\n{p.stdout}")
    return p.stdout.strip()


def src_dir(cfg):
    return os.path.join(review_dir(cfg), "src")


def init(cfg, ref=None):
    """Pin the review checkout at ref; (re)queue every unit.  Spending history is kept."""
    ref = ref or settings(cfg).get("ref", "HEAD")
    repo = cfg.source_repo
    sha = git(repo, "rev-parse", f"{ref}^{{commit}}") if os.path.isdir(repo) else None
    src = src_dir(cfg)
    os.makedirs(review_dir(cfg), exist_ok=True)
    if not os.path.isdir(os.path.join(src, ".git")):
        log(f"cloning {repo} into {src}")
        subprocess.run(["git", "clone", "-q", "--no-checkout", repo, src], check=True)
    else:
        git(src, "fetch", "-q", "origin")
    sha = sha or git(src, "rev-parse", f"origin/{ref}^{{commit}}")
    git(src, "checkout", "-q", "--detach", sha)
    old = load_state(cfg) or {}
    state = {"sha": sha, "ref": ref, "pinned": now_iso(),
             "subject": git(src, "log", "-1", "--format=%s", sha),
             "units": {}, "ledger": old.get("ledger", [])}
    for name in units_config(cfg):
        prev = old.get("units", {}).get(name)
        if prev and old.get("sha") == sha:
            state["units"][name] = prev
        else:
            state["units"][name] = {"status": "todo", "attempts": 0}
    save_state(cfg, state)
    log(f"pinned {ref} at {sha[:10]} ({state['subject']}); {len(state['units'])} units queued")
    return state


def now_iso():
    return datetime.datetime.now().astimezone().isoformat(timespec="seconds")


def week_start(spec, now=None):
    """Start of the current quota week, e.g. spec "Fri 08:00"."""
    now = now or datetime.datetime.now().astimezone()
    day, hm = spec.split()
    wd = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"].index(day[:3].lower())
    h, m = (int(x) for x in hm.split(":"))
    start = now.replace(hour=h, minute=m, second=0, microsecond=0)
    start -= datetime.timedelta(days=(now.weekday() - wd) % 7)
    if start > now:
        start -= datetime.timedelta(days=7)
    return start


def week_cost(cfg, state):
    start = week_start(settings(cfg).get("week_start", "Fri 08:00"))
    return sum(e.get("cost", 0) for e in state.get("ledger", [])
               if datetime.datetime.fromisoformat(e["time"]) >= start)


# ---------------------------------------------------------------- unit inputs

def unit_files(cfg, unit, src):
    """Tracked source files of a unit with their line counts."""
    excl = (unit.get("exclude", []) + settings(cfg).get("exclude", [])
            + cfg.raw.get("analysis", {}).get("exclude", []))
    result = []
    for f in git(src, "ls-files").splitlines():
        if (f.endswith(SOURCE_EXT) and any(fnmatch.fnmatch(f, g) for g in unit["files"])
                and not any(fnmatch.fnmatch(f, g) for g in excl)):
            with open(os.path.join(src, f), errors="replace") as fh:
                result.append((f, sum(1 for _ in fh)))
    return result


def component_paths(cfg):
    comps = components.read_components(cfg.path("install_script"), cfg.excluded)
    return {c.name: c.path for c in comps}


def write_leads(cfg, state, unit, files, outdir):
    """Static-analysis findings for the unit's files, if results exist for this commit."""
    s = settings(cfg)
    branchdir = cfg.results_dir(state["ref"])
    rundir = None
    for date in reversed(diff.run_dates(branchdir)):
        try:
            with open(os.path.join(branchdir, date, "run.json")) as f:
                if json.load(f)["checkout"]["sha"] == state["sha"]:
                    rundir = os.path.join(branchdir, date)
                    break
        except (OSError, KeyError, ValueError):
            continue
    comp_path = component_paths(cfg).get(unit["component"])
    if not rundir or not comp_path:
        return None
    items = diff.load_component(rundir, comp_path) or []
    names = {f for f, _ in files}
    tools = s.get("lead_tools", ["compiler", "clang-tidy", "cppcheck"])
    sevs = s.get("lead_severities", ["error", "warning"])
    items = [f for f in items if f.file in names and f.tool in tools and f.severity in sevs]
    items.sort(key=lambda f: (findings.SEVERITIES.index(f.severity), f.file, f.line))
    path = os.path.join(outdir, f"{unit['name']}.leads.md")
    with open(path, "w") as fh:
        fh.write(f"# Static-analysis leads for {unit['name']} ({len(items)} findings, "
                 f"{os.path.basename(rundir)} run of the same commit)\n\n")
        for f in items[:MAX_LEADS]:
            fh.write(f"- {f.file}:{f.line} [{f.severity}] {f.tool}/{f.check}: {f.message}\n")
        if len(items) > MAX_LEADS:
            fh.write(f"- ... {len(items) - MAX_LEADS} more (lower priority) not listed\n")
    return path if items else None


def known_issues(cfg, unit, files):
    """Issues already on the component page for this unit's files."""
    path = os.path.join(wiki_dir(cfg), page_name(cfg, unit["component"]) + ".md")
    if not os.path.exists(path):
        return []
    names = {f for f, _ in files}
    with open(path) as fh:
        return [f"- {i[0]}: {i[1]} ({i[3]}:{i[4]})" for i in page_issues(fh.read())
                if i[3] in names]


def make_prompt(unit, files, sha, leads, max_turns, known=()):
    total = sum(n for _, n in files)
    lines = [f"Review unit `{unit['name']}` of the DiFX component `{unit['component']}` "
             f"at commit {sha[:10]} (the current directory is that checkout), following the "
             "DiFX bug review method in your instructions.", "",
             f"Files in this unit ({len(files)} files, {total} lines):"]
    lines += [f"- {f} ({n})" for f, n in files]
    lines.append("")
    if leads:
        lines.append(f"Static-analysis leads for these files: {leads}")
    else:
        lines.append("There are no static-analysis leads for this unit.")
    if known:
        lines += ["", "Already reported on the wiki (line numbers may be from an older commit); "
                  "do not report these again, look for other problems:"] + list(known) + [""]
    lines += ["You may read other files of the repository for context, but only report "
              "issues in the files of this unit.",
              f"You have at most {max_turns} turns; keep about five in hand to write the answer.",
              "Cover every file of the unit (skimming repetitive code is fine); stop early only "
              "if the turn budget runs out, and then list what you skipped under not_reviewed.",
              "Finish with only the JSON object described under \"Headless output\"."]
    return "\n".join(lines)


def skill_text(cfg):
    path = os.path.join(REPO_ROOT, settings(cfg).get(
        "skill", ".claude/skills/difx-bug-review/SKILL.md"))
    with open(path) as f:
        text = f.read()
    if text.startswith("---"):
        text = text.split("---", 2)[2]
    return text.strip()


# ---------------------------------------------------------------- calling claude

def claude_cmd(cfg, claude, prompt, model, max_turns, extra_dirs, resume=None):
    s = settings(cfg)
    cmd = [claude, "-p", prompt, "--output-format", "json",
           "--tools", "Read,Grep,Glob", "--allowedTools", "Read,Grep,Glob",
           "--strict-mcp-config", "--max-turns", str(max_turns),
           "--append-system-prompt", skill_text(cfg)]
    for d in extra_dirs:
        cmd += ["--add-dir", d]
    if model:
        cmd += ["--model", model]
    if s.get("effort"):
        cmd += ["--effort", s["effort"]]
    if resume:
        cmd += ["--resume", resume]
    return cmd


def call(cmd, cwd, timeout):
    """Run claude; return its JSON result (or a synthetic error result)."""
    env = dict(os.environ, ENABLE_CLAUDEAI_MCP_SERVERS="false")
    t0 = time.time()
    try:
        p = subprocess.run(cmd, cwd=cwd, text=True, env=env, timeout=timeout,
                           stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    except subprocess.TimeoutExpired:
        return {"is_error": True, "subtype": "timeout", "result": f"timed out after {timeout} s",
                "duration_ms": int((time.time() - t0) * 1000)}
    try:
        out = json.loads(p.stdout)
    except ValueError:
        out = {"is_error": True, "subtype": f"exit {p.returncode}",
               "result": (p.stdout + p.stderr).strip()[-3000:]}
    if p.returncode and not out.get("is_error"):
        out["is_error"] = True
    out.setdefault("duration_ms", int((time.time() - t0) * 1000))
    return out


def is_limit(out):
    return out.get("is_error") and LIMIT_RE.search(str(out.get("result", "")))


def main_model(out):
    """The model that did the work (the CLI also uses a small model for housekeeping)."""
    usage = out.get("modelUsage") or {}
    return max(usage, key=lambda m: usage[m].get("costUSD", 0)) if usage else ""


def ledger_entry(unit_name, out):
    u = out.get("usage") or {}
    return {"time": now_iso(), "unit": unit_name,
            "model": main_model(out),
            "cost": round(out.get("total_cost_usd", 0) or 0, 4),
            "input": u.get("input_tokens", 0), "output": u.get("output_tokens", 0),
            "cache_read": u.get("cache_read_input_tokens", 0),
            "cache_write": u.get("cache_creation_input_tokens", 0),
            "turns": out.get("num_turns", 0),
            "seconds": round(out.get("duration_ms", 0) / 1000),
            "subtype": out.get("subtype", "")}


def parse_answer(text):
    text = (text or "").strip()
    m = re.search(r"```(?:json)?\s*(\{.*\})\s*```", text, re.S)
    if m:
        text = m.group(1)
    i, j = text.find("{"), text.rfind("}")
    if i < 0 or j < i:
        raise ValueError("no JSON object in the answer")
    answer = json.loads(text[i:j + 1])
    if not isinstance(answer.get("issues"), list):
        raise ValueError("answer has no issues list")
    return answer


def review_unit(cfg, state, unit, claude, model_override=None, dry_run=False):
    """Review one unit.  Returns "done", "failed" or "limit"."""
    s = settings(cfg)
    src = src_dir(cfg)
    outdir = os.path.join(review_dir(cfg), state["sha"][:12])
    os.makedirs(outdir, exist_ok=True)
    files = unit_files(cfg, unit, src)
    if not files:
        raise RuntimeError(f"unit {unit['name']} matches no files")
    leads = write_leads(cfg, state, unit, files, outdir)
    max_turns = int(s.get("max_turns", 60))
    model = model_override or unit.get("model")
    prompt = make_prompt(unit, files, state["sha"], leads, max_turns,
                         known_issues(cfg, unit, files))
    cmd = claude_cmd(cfg, claude, prompt, model, max_turns, [outdir])
    total = sum(n for _, n in files)
    if dry_run:
        print(prompt)
        print("\ncommand:", " ".join(repr(c) if " " in c else c for c in cmd[:3]), "...",
              " ".join(c for c in cmd[3:] if "\n" not in c and len(c) < 200))
        return "dry-run"

    st = state["units"][unit["name"]]
    st.update(status="running", attempts=st.get("attempts", 0) + 1, started=now_iso())
    save_state(cfg, state)
    log(f"{unit['name']}: {len(files)} files, {total} lines, model {model or 'default'}"
        f"{', with leads' if leads else ''}")
    timeout = int(s.get("timeout_minutes", 90)) * 60
    out = call(cmd, src, timeout)
    state["ledger"].append(ledger_entry(unit["name"], out))
    raw = os.path.join(outdir, f"{unit['name']}.out.json")
    with open(raw, "w") as f:
        json.dump(out, f, indent=1)

    if out.get("subtype") == "error_max_turns" and out.get("session_id"):
        log(f"{unit['name']}: out of turns; asking for the answer so far")
        cmd = claude_cmd(cfg, claude, "You have run out of turns. Reply now with the JSON "
                         "object for what you have reviewed so far, listing the rest under "
                         "not_reviewed.", model, 2, [outdir], resume=out["session_id"])
        out = call(cmd, src, timeout)
        state["ledger"].append(ledger_entry(unit["name"], out))
        with open(raw, "a") as f:
            f.write("\n")
            json.dump(out, f, indent=1)

    if is_limit(out):
        st.update(status="todo")
        save_state(cfg, state)
        log(f"{unit['name']}: usage limit reached ({str(out.get('result'))[:200]}); "
            "left in the queue")
        return "limit"
    try:
        if out.get("is_error"):
            raise ValueError(f"{out.get('subtype')}: {str(out.get('result'))[:500]}")
        answer = parse_answer(out.get("result"))
    except ValueError as e:
        st.update(status="failed", error=str(e)[:500], finished=now_iso())
        save_state(cfg, state)
        log(f"{unit['name']}: FAILED: {e}")
        return "failed"

    answer["unit"] = unit["name"]
    answer["files"] = files
    answer["model"] = main_model(out) or model or ""
    answer["date"] = datetime.date.today().isoformat()
    with open(os.path.join(outdir, f"{unit['name']}.json"), "w") as f:
        json.dump(answer, f, indent=1)
    costs = [e for e in state["ledger"] if e["unit"] == unit["name"]
             and e["time"] >= st["started"]]
    st.update(status="done", finished=now_iso(), issues=len(answer["issues"]),
              lines=total, model=answer["model"], merged=False,
              cost=round(sum(e["cost"] for e in costs), 4),
              minutes=round(sum(e["seconds"] for e in costs) / 60, 1))
    st.pop("error", None)
    save_state(cfg, state)
    log(f"{unit['name']}: {len(answer['issues'])} issues, ${st['cost']:.2f} equivalent, "
        f"{st['minutes']} min")
    return "done"


# ---------------------------------------------------------------- wiki pages

def wiki_dir(cfg):
    return os.path.join(cfg.path("wiki"), settings(cfg).get("wiki_subdir", "Code-Review"))


def page_name(cfg, component):
    return f"{settings(cfg).get('page_prefix', 'Review')}-{component}"


def id_prefix(component):
    return re.sub(r"[^A-Z0-9]", "", component.upper())


def source_link(cfg, sha, file, start, end=None):
    url = cfg.raw.get("report", {}).get(
        "source_url", "https://github.com/difx/difx/blob/{sha}/{file}#L{line}")
    line = f"{start}-L{end}" if end and end != start else str(start)
    return url.format(sha=sha, file=file, line=line)


def snippet(src, file, start, end):
    path = os.path.join(src, file)
    if not os.path.isfile(path) or start < 1:
        return None
    with open(path, errors="replace") as f:
        lines = f.read().splitlines()
    end = max(start, min(end, start + MAX_SNIPPET - 1, len(lines)))
    if start > len(lines):
        return None
    return "\n".join(lines[start - 1:end])


def issue_key(issue, code):
    basis = code if code else issue.get("title", "")
    basis = re.sub(r"\s+", " ", basis).strip()
    return hashlib.sha1(f"{issue.get('file')}|{issue.get('function')}|{basis}".encode()
                        ).hexdigest()[:12]


def as_int(v, default=0):
    try:
        return int(v)
    except (TypeError, ValueError):
        return default


def render_issue(cfg, sha, issue_id, issue, code, key, unit_name, model, date):
    file = issue.get("file", "?")
    start = as_int(issue.get("start_line")) or as_int(issue.get("line"))
    end = as_int(issue.get("end_line"), start)
    line = as_int(issue.get("line"), start)
    func = issue.get("function")
    where = (f"[`{file}:{line}`]({source_link(cfg, sha, file, start, end)})"
             + (f" in `{func}`" if func else "") + f" (commit `{sha[:9]}`)")
    out = [f"### {issue_id}: {issue.get('title', '(untitled)').strip()}",
           f"<!-- review:issue {issue_id} key={key} unit={unit_name} -->", "",
           "- **Status:** open",
           f"- **Severity:** {issue.get('severity', '?')} · **Confidence:** "
           f"{issue.get('confidence', '?')} · **Category:** {issue.get('category', '?')}",
           f"- **Where:** {where}",
           f"- **Found:** {date} by Claude ({model})", ""]
    if code:
        lang = "cpp" if file.endswith((".cpp", ".cc", ".cxx", ".hpp", ".hh")) else (
            "c" if file.endswith((".c", ".h")) else "")
        fence = "````" if "```" in code else "```"
        out += [f"{fence}{lang}", code, fence, ""]
    out += [issue.get("description", "").strip(), ""]
    if issue.get("trigger"):
        out += [f"**When it goes wrong:** {issue['trigger'].strip()}", ""]
    if issue.get("fix"):
        out += [f"**Suggested fix:** {issue['fix'].strip()}", ""]
    also = []
    for loc in issue.get("also_at") or []:
        m = re.match(r"\s*([^:\s]+):(\d+)", str(loc))
        also.append(f"[`{loc}`]({source_link(cfg, sha, m.group(1), int(m.group(2)))})"
                    if m else f"`{loc}`")
    if also:
        out += ["**Same pattern at:** " + ", ".join(also), ""]
    out += ["**Notes:** _(anyone: add what you find, with your name)_", "", "---", ""]
    return "\n".join(out)


def page_header(component):
    return "\n".join([
        "<!-- Created by `sa.py review` (difx/static-analysis).  Edit freely: change an "
        "issue's Status, add notes, or add issues of your own in the same layout with the "
        "next free number.  The tool only rewrites the block between the review:units "
        "markers and appends new issues at the end. -->",
        f"# Code review: {component}", "",
        "> AI-generated review (Claude) of DiFX code for serious bugs, not style.  Every "
        "issue is **unverified** until its Status says *confirmed*.  Code snippets are "
        "quoted from the reviewed commit and the links point at that commit, so they stay "
        "valid as the code changes.",
        ">",
        "> **Status:** open (not checked yet) · confirmed · not a bug · fixed · won't fix.  "
        "If you look at an issue, please update its Status and add to its Notes.", "",
        UNITS_BEGIN, UNITS_END, "", "## Issues", "", ""])


def units_block(cfg, state, component, answers):
    uc = units_config(cfg)
    rows, notes = [], []
    for name, u in uc.items():
        if u["component"] != component:
            continue
        st = state["units"].get(name, {})
        a = answers.get(name)
        if st.get("status") != "done" or not a:
            rows.append(f"| {name} | | | | not yet reviewed | | |")
            continue
        sha = state["sha"]
        commit = f"[`{sha[:9]}`](https://github.com/difx/difx/tree/{sha})"
        rows.append(f"| {name} | {len(a['files'])} | {sum(n for _, n in a['files']):,} | "
                    f"{commit} | {a['date']} | {a['model']} | {len(a['issues'])} |")
        note = [f"**{name}**: {a.get('summary', '').strip()}"]
        if a.get("not_reviewed"):
            note.append("Not reviewed: " + "; ".join(map(str, a["not_reviewed"])))
        note.append("Files: " + ", ".join(f"`{f}`" for f, _ in a["files"]))
        notes.append("\n\n".join(note))
    out = [UNITS_BEGIN, "## Reviewed", "",
           "| Unit | Files | Lines | Commit | Reviewed | Model | Issues found |",
           "|---|---:|---:|---|---|---|---:|"] + rows
    if notes:
        out += ["", "<details><summary>What each unit covers, and Claude's overall "
                "impression</summary>", "", "\n\n".join(notes), "", "</details>"]
    return "\n".join(out + [UNITS_END])


def load_answers(cfg, state):
    outdir = os.path.join(review_dir(cfg), state["sha"][:12])
    answers = {}
    for name, st in state["units"].items():
        path = os.path.join(outdir, f"{name}.json")
        if st.get("status") == "done" and os.path.exists(path):
            with open(path) as f:
                answers[name] = json.load(f)
    return answers


def merge_component(cfg, state, component, answers):
    """Append new issues of done units to the component page; refresh its units block."""
    path = os.path.join(wiki_dir(cfg), page_name(cfg, component) + ".md")
    text = open(path).read() if os.path.exists(path) else page_header(component)
    if UNITS_BEGIN in text and UNITS_END in text:
        a, rest = text.split(UNITS_BEGIN, 1)
        text = a + units_block(cfg, state, component, answers) + rest.split(UNITS_END, 1)[1]
    else:
        text = text.rstrip() + "\n\n" + units_block(cfg, state, component, answers) + "\n"
    keys = {m.group(2) for m in ISSUE_MARK.finditer(text)}
    existing = page_issues(text)
    prefix = id_prefix(component)
    numbers = [int(n) for n in re.findall(rf"^### {prefix}-(\d+)\b", text, re.M)]
    next_no = max(numbers, default=0) + 1
    src, added = src_dir(cfg), 0
    for name, a in answers.items():
        if units_config(cfg).get(name, {}).get("component") != component:
            continue
        issues = sorted(a["issues"], key=lambda i: SEVERITY_ORDER.index(i.get("severity"))
                        if i.get("severity") in SEVERITY_ORDER else 9)
        for issue in issues:
            start = as_int(issue.get("start_line")) or as_int(issue.get("line"))
            end = as_int(issue.get("end_line"), start)
            code = snippet(src, issue.get("file", ""), start, end) if start else None
            key = issue_key(issue, code)
            file = issue.get("file", "")
            if key in keys or (start and overlaps(existing, state["sha"], file, start, end)):
                continue
            keys.add(key)
            existing.append(("", "", state["sha"], file, start, end))
            text = text.rstrip() + "\n\n" + render_issue(
                cfg, state["sha"], f"{prefix}-{next_no}", issue, code, key, name,
                a["model"], a["date"])
            next_no += 1
            added += 1
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(text.rstrip() + "\n")
    return added


WHERE_RE = re.compile(r"\*\*Where:\*\*\s*\[`[^`]*`\]\(https?://\S+?/blob/(\w+)/([^#\s)]+)#L(\d+)(?:-L(\d+))?")


def page_issues(text):
    """(id, title, sha, file, start, end) of each issue on a page, from its Where link."""
    result = []
    for section in re.split(r"^### ", text, flags=re.M)[1:]:
        head = section.split("\n", 1)[0]
        m = re.match(r"(\S+?):\s*(.*)", head)
        w = WHERE_RE.search(section)
        if m and w:
            start = int(w.group(3))
            result.append((m.group(1), m.group(2).strip(), w.group(1), w.group(2), start,
                           int(w.group(4) or start)))
    return result


def overlaps(existing, sha, file, start, end, slack=3):
    return any(e[2] == sha and e[3] == file and start <= e[5] + slack and end >= e[4] - slack
               for e in existing)


def page_statuses(text):
    counts = {}
    for section in re.split(r"^### ", text, flags=re.M)[1:]:
        m = re.search(r"\*\*Status:\*\*\s*([^\n]*)", section)
        status = re.sub(r"[*_`.]", "", m.group(1)).strip().lower() if m else "?"
        status = next((s for s in STATUSES if status.startswith(s)), "other")
        counts[status] = counts.get(status, 0) + 1
    return counts


def write_index(cfg, state):
    uc = units_config(cfg)
    comps = list(dict.fromkeys(u["component"] for u in uc.values()))
    cols = STATUSES + ["other"]
    rows = []
    for c in comps:
        path = os.path.join(wiki_dir(cfg), page_name(cfg, c) + ".md")
        names = [n for n, u in uc.items() if u["component"] == c]
        done = sum(1 for n in names if state["units"].get(n, {}).get("status") == "done")
        if os.path.exists(path):
            counts = page_statuses(open(path).read())
            link = f"[{c}]({page_name(cfg, c)})"
        else:
            counts, link = {}, c
        rows.append(f"| {link} | {done} of {len(names)} | {sum(counts.values())} | "
                    + " | ".join(str(counts.get(s, 0) or "") for s in cols) + " |")
    sha = state["sha"]
    text = "\n".join([
        "<!-- Generated by `sa.py review`; edits here are overwritten.  Edit the "
        "component pages instead. -->",
        "# Code review (AI-assisted)", "",
        "Claude reviewed the DiFX libraries, mpifxcorr and the main applications for serious "
        "bugs: memory errors, logic errors, wrong units or maths, races.  Style was out of "
        "scope.  It ran unit by unit with read-only access to the code, using the method in "
        "`.claude/skills/difx-bug-review/SKILL.md`; the code snippets and links come from "
        f"commit [`{sha[:9]}`](https://github.com/difx/difx/tree/{sha}) ({state['ref']}).", "",
        "**These are unverified AI findings.**  If you know the code, please open a component "
        "page and set the Status of issues you check (confirmed / not a bug / fixed / won't "
        "fix), add notes, or add issues of your own.  The counts below are read from the "
        "pages.", "",
        "| Component | Units reviewed | Issues | " + " | ".join(cols) + " |",
        "|---|---|---:|" + "---:|" * len(cols)] + rows + [
        "", "See also the [static-analysis results](Static-Analysis).", ""])
    os.makedirs(wiki_dir(cfg), exist_ok=True)
    with open(os.path.join(wiki_dir(cfg), settings(cfg).get("index_page", "Code-Review")
                           + ".md"), "w") as f:
        f.write(text)


def update_pages(cfg, state, pull):
    """Merge every done unit not yet on its page; rewrite the index."""
    wiki = cfg.path("wiki")
    if pull:
        branch = cfg.raw.get("report", {}).get("wiki_branch", "master")
        remote = cfg.raw.get("report", {}).get("wiki_remote", "origin")
        git(wiki, "pull", "-q", "--rebase", "--autostash", remote, branch)
    answers = load_answers(cfg, state)
    comps = {units_config(cfg)[n]["component"] for n in answers if n in units_config(cfg)}
    for c in sorted(comps):
        added = merge_component(cfg, state, c, answers)
        log(f"page {page_name(cfg, c)}: {added} new issue(s)")
    for n in answers:
        state["units"][n]["merged"] = True
    save_state(cfg, state)
    write_index(cfg, state)


def publish_pages(cfg, state, units_done, push=True):
    wiki = cfg.path("wiki")
    subdir = os.path.relpath(wiki_dir(cfg), wiki)
    msg = (f"Code-Review: {', '.join(units_done) if units_done else 'update'} "
           f"({state['ref']} @ {state['sha'][:10]})")
    return publish.run(cfg, push=push, subdir=subdir, message=msg)


# ---------------------------------------------------------------- driver

def print_status(cfg, state):
    uc = units_config(cfg)
    print(f"commit {state['sha'][:10]} ({state['ref']}): {state.get('subject', '')}")
    print(f"{'unit':26s} {'component':12s} {'model':8s} {'status':8s} {'issues':>6s} "
          f"{'lines':>6s} {'cost$':>7s} {'min':>5s}")
    for name, u in uc.items():
        st = state["units"].get(name, {"status": "new"})
        print(f"{name:26s} {u['component']:12s} {u.get('model', ''):8s} {st['status']:8s} "
              f"{st.get('issues', ''):>6} {st.get('lines', ''):>6} "
              f"{st.get('cost', ''):>7} {st.get('minutes', ''):>5}"
              + (f"  {st['error'][:60]}" if st.get("error") else ""))
    s = settings(cfg)
    cap = s.get("weekly_cost_cap", 0)
    print(f"cost this quota week (since {week_start(s.get('week_start', 'Fri 08:00')):%a %d %b %H:%M}):"
          f" ${week_cost(cfg, state):.2f}" + (f" of cap ${cap}" if cap else " (no cap)"))
    print(f"all-time: ${sum(e.get('cost', 0) for e in state['ledger']):.2f} "
          f"in {len(state['ledger'])} claude calls")


def run(cfg, init_ref=None, only=None, max_units=None, publish_pages_=False, retry=False,
        status=False, pages=False, model=None, dry_run=False, reset=None):
    os.makedirs(review_dir(cfg), exist_ok=True)
    lock = open(os.path.join(review_dir(cfg), ".lock"), "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        log("another `sa.py review` is running; exiting")
        return 0

    state = load_state(cfg)
    if init_ref is not None:            # --init only pins and queues; it never reviews
        print_status(cfg, init(cfg, init_ref or None))
        return 0
    if state is None:
        raise RuntimeError("no review queue yet; run `sa.py review --init` first")
    for name in units_config(cfg):
        state["units"].setdefault(name, {"status": "todo", "attempts": 0})
    for name in ([] if dry_run else reset or []):
        if name not in state["units"]:
            raise RuntimeError(f"unknown unit {name}")
        state["units"][name] = {"status": "todo", "attempts": 0}
    for st in state["units"].values():
        if st["status"] == "running":   # a previous run died
            st["status"] = "todo"
    save_state(cfg, state)
    if git(src_dir(cfg), "rev-parse", "HEAD") != state["sha"]:
        raise RuntimeError(f"{src_dir(cfg)} is not at the pinned commit; use --init")
    if status:
        print_status(cfg, state)
        return 0
    if pages:
        update_pages(cfg, state, pull=publish_pages_)
        return publish_pages(cfg, state, []) if publish_pages_ else 0

    claude = find_claude(settings(cfg).get("claude") or cfg.raw.get("summary", {}).get("claude"))
    if not claude:
        raise RuntimeError("claude CLI not found; set review.claude in config.toml")
    uc = units_config(cfg)
    wanted = [n for n in uc if not only or n in only or uc[n]["component"] in only]
    if only and not wanted:
        raise RuntimeError(f"no units match {','.join(only)}")
    queue = [n for n in wanted
             if state["units"][n]["status"] == "todo"
             or (retry and state["units"][n]["status"] == "failed")]
    if not queue:
        log("nothing to do (all selected units are done or failed; --retry re-runs failed)")
    cap = settings(cfg).get("weekly_cost_cap", 0)
    done_now, result = [], 0
    for name in queue:
        if max_units is not None and len(done_now) >= max_units:
            break
        if cap and week_cost(cfg, state) >= cap and not dry_run:
            log(f"weekly cost cap reached (${week_cost(cfg, state):.2f} of ${cap}); stopping")
            break
        outcome = review_unit(cfg, state, uc[name], claude, model, dry_run)
        if outcome == "dry-run":
            return 0
        if outcome == "limit":
            break
        if outcome == "failed":
            result = 1
            continue
        done_now.append(name)
        if publish_pages_:
            update_pages(cfg, state, pull=True)
            result = publish_pages(cfg, state, [name]) or result
    if done_now and not publish_pages_:
        update_pages(cfg, state, pull=False)
        log(f"pages written to {wiki_dir(cfg)} (not published; use --publish or --pages --publish)")
    return result
