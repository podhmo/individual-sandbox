from __future__ import annotations
import typing as t
from metashape import runtime


class A:
    b: B
    cs: t.List[C]
    i: I


class B:
    d: D
    es: t.Tuple[E]
    i: I


class C:
    f: F
    gs: t.Dict[G, H]
    i: I


class D:
    name: str


class E:
    name: str


class F:
    name: str


class G:
    name: str


class H:
    name: str


class I:
    name: str


print(list(runtime.get_walker(A).walk()))
print(list(runtime.get_walker(A, aggressive=True).walk()))
print(list(runtime.get_walker(A, aggressive=True, recursive=True).walk()))
