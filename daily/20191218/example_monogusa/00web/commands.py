from __future__ import annotations
from monogusa import component


def hello(writer: Writer, *, name: str = "world") -> None:
    """hello world"""
    writer.write(f"hello {name}")


class Writer:
    def write(self, msg: str) -> None:
        print(msg)


@component
def writer() -> Writer:
    return Writer()


if __name__ == "__main__":
    from monogusa.cli import run

    run()
