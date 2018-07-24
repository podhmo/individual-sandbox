from typing_extensions import Protocol
from a import ASetting, use_a
from b import BSetting, use_b


class Setting(ASetting, BSetting, Protocol):  # please, intersection type
    pass


def use(s: Setting) -> None:
    use_a(s)
    use_b(s)


class setting:
    def __init__(self) -> None:
        self.x = "foo"  # type: str
        self.y = "bar"  # type: str
        self.z = "boo"  # type: str


def main() -> None:
    s = setting()
    use(s)
