from __future__ import annotations
import typing as t


class Config:
    GitHub: GitHubConfig


class GitHubConfig:
    Username: str
    APIKey: str


from metashape.runtime import get_walker

w = get_walker([Config])
for cls in w.walk(nocheck=True):
    for field_name, info, metadata in w.for_type(cls).walk():
        if info.user_defined_type is not None:
            print("@", "@@", info.user_defined_type)
            w.append(info.user_defined_type)
    print("@", cls)
