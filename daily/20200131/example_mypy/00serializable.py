import typing_extensions as tx
import dataclasses


class Serializable(tx.Protocol):
    def serialize(self) -> str:
        ...


class F:
    def __init__(self, name: str) -> None:
        self.name = name

    def serialize(self) -> str:
        return f"F[{self.name}]"


@dataclasses.dataclass
class G:
    name: str


def serialize(ob: Serializable) -> str:
    return ob.serialize()


print(serialize(F(name="foo")))
print(serialize(G(name="bar")))
