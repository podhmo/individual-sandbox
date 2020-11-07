from __future__ import annotations
import typing as t
import dataclasses


@dataclasses.dataclass
class User:
    name: str
    age: int
