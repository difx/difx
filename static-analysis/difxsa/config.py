"""Load config.toml and resolve paths against the repository root."""

import os
import tomllib
from dataclasses import dataclass

SUITE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
REPO_ROOT = os.path.dirname(SUITE_DIR)
DEFAULT_CONFIG = os.path.join(SUITE_DIR, "config.toml")


def resolve(path):
    return os.path.normpath(os.path.join(REPO_ROOT, os.path.expanduser(path)))


@dataclass
class Config:
    raw: dict

    def path(self, key):
        return resolve(self.raw["paths"][key])

    @property
    def workdir(self):
        return self.path("workdir")

    @property
    def source_repo(self):
        repo = self.raw["source"]["repo"]
        return repo if "://" in repo or repo.startswith("git@") else resolve(repo)

    @property
    def source_ref(self):
        return self.raw["source"].get("ref", "HEAD")

    @property
    def build(self):
        return self.raw.get("build", {})

    @property
    def tool_bin(self):
        tb = self.build.get("tool_bin")
        return resolve(tb) if tb else None

    @property
    def excluded(self):
        return self.raw.get("components", {}).get("exclude", {})


def load(path=None):
    with open(path or DEFAULT_CONFIG, "rb") as f:
        return Config(tomllib.load(f))
