from typing_extensions import Protocol


class ASetting(Protocol):
    x: str
    y: str


def use_a(setting: ASetting) -> None:
    print(setting.x, setting.y)
