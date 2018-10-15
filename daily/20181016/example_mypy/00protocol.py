from typing import Protocol


class Caller(Protocol):
    def __call__(self) -> None:
        ...


def call() -> None:
    pass


def func(caller: Caller) -> None:
    pass


func(call)
