import typing as t
import dataclasses

T = t.TypeVar("T")


@dataclasses.dataclass
class AEvent(t.Generic[T]):
    name: str
    raw: T


@dataclasses.dataclass
class BEvent(t.Generic[T]):
    raw: T


Event = t.Union[AEvent[T], BEvent[T]]


def f(ev: Event[str]) -> None:
    print(ev.raw)
