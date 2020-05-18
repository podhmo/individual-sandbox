import typing as t
import contextlib


@contextlib.contextmanager
def verbose(message: str) -> t.Iterator[str]:
    print("hello")
    yield message
    print("bye")


@verbose
def use(message: str) -> None:
    pass


with verbose("message") as m:
    print("hoi", m)
