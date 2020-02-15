from __future__ import annotations
import typing as t
import dataclasses
from handofcats import as_command


@dataclasses.dataclass
class MessageEvent:
    id: str
    content: str

    @classmethod
    def from_text(cls, text: str) -> MessageEvent:
        return cls(id="-1", content=text)


@dataclasses.dataclass
class UnknownEvent:
    content: str
    _type: str = "?"

    @classmethod
    def from_text(cls, text: str) -> UnknownEvent:
        return cls(content=text)


Event = t.Union[MessageEvent, UnknownEvent]


def consume(ev: Event) -> None:
    print("!", ev.content)


@as_command
def feed():
    import sys

    g = globals()

    for line in sys.stdin:
        typ, line = [x.strip() for x in line.rstrip().split(":", 1)]
        cls = g.get(f"{typ}Event") or UnknownEvent
        ev = cls.from_text(line)
        if cls is UnknownEvent:
            ev._type = typ  # xxx:
        consume(ev)
