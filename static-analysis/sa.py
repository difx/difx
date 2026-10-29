#!/usr/bin/env python3
"""DiFX static-analysis suite driver.

    sa.py components    list components from install-difx and whether they are analysed
    sa.py compdb        fresh checkout + build under an interceptor -> compile_commands.json
    sa.py analyse       run the enabled tools on the analysed components
    sa.py run           compdb followed by analyse (the daily job)

Needs only Python 3.11+ and the tools themselves; see README.md.
"""

import argparse
import sys

from difxsa import analyse, compdb, components, config


def cmd_components(cfg, args):
    comps = components.read_components(cfg.path("install_script"), cfg.excluded)
    for c in comps:
        state = "analyse" if c.analysed else ("built, excluded: " + c.excluded
                                             if c.enabled else "disabled")
        print(f"{c.name:16s} {c.path:36s} {state}")


def csv(value):
    return [v.strip() for v in value.split(",") if v.strip()]


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("-c", "--config", help="config file (default: static-analysis/config.toml)")
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("components", help="list components")

    p_db = sub.add_parser("compdb", help="build and generate compile_commands.json")
    p_an = sub.add_parser("analyse", help="run the analysis tools")
    p_run = sub.add_parser("run", help="compdb + analyse")
    for p in (p_db, p_run):
        p.add_argument("--no-checkout", action="store_true",
                       help="reuse the existing work checkout instead of re-cloning")
    p_db.add_argument("--report-only", action="store_true",
                      help="skip the build; only re-filter the database and report coverage")
    for p in (p_an, p_run):
        p.add_argument("--only", type=csv, help="comma-separated component names")
        p.add_argument("--tools", type=csv, help="comma-separated tool names "
                       "(default: those enabled in the config)")
        p.add_argument("--date", help="results date label (default: today)")
    args = parser.parse_args()

    cfg = config.load(args.config)
    if args.command == "components":
        cmd_components(cfg, args)
        return 0
    status = 0
    if args.command in ("compdb", "run"):
        status = compdb.run(cfg, checkout=not args.no_checkout,
                            build=not getattr(args, "report_only", False))
    if args.command in ("analyse", "run"):
        status = analyse.run(cfg, only=args.only, tools=args.tools, date=args.date) or status
    return status


if __name__ == "__main__":
    sys.exit(main())
