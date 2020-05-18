import typing as tx
from egoist.typing import resolve_name, NewNamedType

Direction = NewNamedType("Direction", tx.Literal["N", "S", "W", "E"])


def use(d: Direction) -> None:
    print("@", d)


print(resolve_name(Direction))
use("N")
use("X")
