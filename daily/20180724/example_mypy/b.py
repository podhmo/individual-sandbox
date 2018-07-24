from typing_extensions import Protocol


class BSetting(Protocol):
    x: str
    z: str


def use_b(setting: BSetting) -> None:
    print(setting.x, setting.z)
