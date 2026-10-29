"""Read the component list from install-difx without executing it.

install-difx defines a module-level dict `components` (name -> True/False) and
then, for each enabled component, appends [subdir, version, ...] to one of
libtargets / utiltargets / apptargets.  Both are recovered here with `ast`, so
the suite always follows whatever install-difx would build.
"""

import ast
from dataclasses import dataclass

TARGET_LISTS = {
    "libtargets": "libraries",
    "utiltargets": "utilities",
    "apptargets": "applications",
}

# Components built by hand-written code in install-difx rather than a *targets list.
SPECIAL = {
    "mpifxcorr": ("mpifxcorr", "mpifxcorr"),
    "perl": ("libraries", "libraries/vex"),
}

CATEGORY_ORDER = ["libraries", "mpifxcorr", "applications", "utilities"]


@dataclass
class Component:
    name: str
    enabled: bool
    category: str      # libraries, utilities, applications or mpifxcorr
    path: str          # relative to the repository root
    excluded: str = "" # reason, if enabled but not analysed

    @property
    def analysed(self):
        return self.enabled and not self.excluded


def _component_test(test):
    """Return X if `test` is components["X"], else None."""
    if (isinstance(test, ast.Subscript) and isinstance(test.value, ast.Name)
            and test.value.id == "components"
            and isinstance(test.slice, ast.Constant)):
        return test.slice.value
    return None


def _appended_target(stmt):
    """Return (list name, subdir) for `<list>.append([subdir, ...])`, else None."""
    if not (isinstance(stmt, ast.Expr) and isinstance(stmt.value, ast.Call)):
        return None
    func = stmt.value.func
    if not (isinstance(func, ast.Attribute) and func.attr == "append"
            and isinstance(func.value, ast.Name)
            and func.value.id in TARGET_LISTS):
        return None
    args = stmt.value.args
    if args and isinstance(args[0], ast.List) and args[0].elts \
            and isinstance(args[0].elts[0], ast.Constant):
        return func.value.id, args[0].elts[0].value
    return None


def read_components(install_script, excluded=None):
    excluded = excluded or {}
    with open(install_script) as f:
        tree = ast.parse(f.read(), install_script)

    enabled = None
    locations = dict(SPECIAL)
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
                isinstance(t, ast.Name) and t.id == "components" for t in node.targets):
            enabled = ast.literal_eval(node.value)
        elif isinstance(node, ast.If):
            name = _component_test(node.test)
            for stmt in node.body if name else []:
                target = _appended_target(stmt)
                if target:
                    category = TARGET_LISTS[target[0]]
                    locations[name] = (category, f"{category}/{target[1]}")

    if enabled is None:
        raise RuntimeError(f"no `components` dict found in {install_script}")

    result = []
    for name, on in enabled.items():
        if name not in locations:
            raise RuntimeError(f"cannot locate source directory of component '{name}'")
        category, path = locations[name]
        result.append(Component(name, bool(on), category, path, excluded.get(name, "")))
    result.sort(key=lambda c: (CATEGORY_ORDER.index(c.category), c.path))
    return result
