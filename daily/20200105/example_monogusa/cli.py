import typing as t


def hello(*, name: str, nickname: t.Optional[t.Any] = None) -> None:
    print(name, nickname)
