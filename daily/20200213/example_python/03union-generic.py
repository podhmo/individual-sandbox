import typing as t

T = t.TypeVar("T")


class AEvent(t.Generic[T]):
    name: str
    raw: T


class BEvent(t.Generic[T]):
    raw: T


Event = t.Union[AEvent[T], BEvent[T]]


def f(ev: Event[str]) -> None:
    reveal_type(ev)
    print(ev.raw)
