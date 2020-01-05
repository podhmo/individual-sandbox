import typing as t


def hello(
    *, name: str, nickname: t.Optional[t.Any] = None, debug: bool = False
) -> None:
    print(name, nickname)
