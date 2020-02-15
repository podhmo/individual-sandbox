from __future__ import annotations
import typing as t
import dataclasses
from collections import defaultdict
from handofcats import as_command
from handofcats.langhelpers import reify


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


class Subscription:
    @reify
    def handler_map(self) -> t.Dict[t.Type[Event], t.Callable[[Event], t.Any]]:
        return defaultdict(list)

    def subscribe(self, event_type: t.Type[Event], *, normalize=None):
        def _wrapped(fn: t.Callable[[Event], None]):
            _caller = normalize(fn) if normalize is not None else fn
            self.handler_map[event_type].append(_caller)
            return fn

        return _wrapped

    def __call__(self, ev: Event):
        handlers = self.handler_map[ev.__class__]

        for h in handlers:
            h(ev)


s = Subscription()


@s.subscribe(MessageEvent)
def consume(ev: Event) -> None:
    print("!", ev.content)


@as_command
def feed():
    import sys

    g = globals()

    for line in sys.stdin:
        typ, line = [x.strip() for x in line.rstrip().split(":", 1)]

        cls = g.get(f"{typ}Event") or UnknownEvent  # XXX: unsafe
        ev = cls.from_text(line)
        if cls is UnknownEvent:
            ev._type = typ  # XXX:

        s(ev)
