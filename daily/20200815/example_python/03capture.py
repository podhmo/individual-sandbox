from __future__ import annotations
import sys
import typing as t


class Capture:
    def __init__(self, fn: t.Callable[..., t.Any]) -> None:
        self.fn = fn
        self.called = False

    def __call__(self, *args, **kwargs):
        self.called = True
        return self.fn(*args, **kwargs)


def hello(name: str) -> None:
    print("hello", name)


# 手抜き
class fake:
    hello = Capture(hello)


sys.modules["fake"] = fake


def callback_something():
    from fake import hello

    hello("world")


def run():
    callback_something()
    from fake import hello

    print(hello.called)


run()
