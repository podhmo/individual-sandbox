from __future__ import annotations
import typing as t
from egoist.generate.go.types import GoError


class Message:
    gopackage = "m/internal2"


class Greeter:
    gopackage = "m/internal2"


class Event:
    gopackage = "m/internal2"


def NewMessage() -> Message:
    pass


def NewGreeter(message: Message, grumby: bool) -> Greeter:
    pass


def NewEvent(g: Greeter) -> t.Tuple[Event, GoError]:
    pass
