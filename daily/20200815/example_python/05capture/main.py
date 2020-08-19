from __future__ import annotations


def callback_something() -> None:
    from fake import hello

    hello("world")


def run() -> None:
    callback_something()


run()
