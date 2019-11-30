from __future__ import annotations
import typing as t
import dataclasses
from handofcats.injector import Injector


@dataclasses.dataclass
class _Act:
    name: str
    args: t.List[t.Any]
    kwargs: t.Dict[str, t.Any]
    i: int
    used: bool


class ActGenerator:
    def __init__(self):
        self.i = 0

    def __call__(
        self,
        name: str,
        *,
        args: t.List[t.Any],
        kwargs: t.Dict[str, t.Any],
        used: bool = True,
    ):
        self.i += 1
        return _Act(name=name, args=args, kwargs=kwargs, i=self.i, used=used)


class CapturedMock:
    history: t.List[Act]
    children: t.Dict[str, CapturedMock]
    Act = ActGenerator()

    def __init__(self, *args, **kwargs):
        self.history = [self.Act("__init__", args=args, kwargs=kwargs)]
        self.children = {}
        self.parent = None
        self.i = None

    def __getattr__(self, name):
        child = self.children.get(name)
        if child is None:
            child = self.children[name] = self.__class__()
        self.history.append(self.Act(name=name, used=False, args=[], kwargs={}))

        child.i = self.history[-1].i  # xxx:
        child.parent = self
        child.history = self.history  # xxx

        return child

    def __call__(self, *args, **kwargs):
        self.history.append(self.Act("__call__", args=args, kwargs=kwargs))
        return self


def add(*, x: int, y: int) -> int:
    return x + y


def hello(*, name: str = "world") -> None:
    print(f"hello {name}")


parser = CapturedMock()
sparsers = parser.add_subparsers(required=True, title="subcommands")
assert sparsers is not None

Injector(add).inject(sparsers.add_parser("add"))
Injector(hello).inject(sparsers.add_parser("hello"))


for act in parser.history:
    print(act)
