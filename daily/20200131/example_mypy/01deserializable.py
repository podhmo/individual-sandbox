import typing as t
import typing_extensions as tx
from dataclasses import dataclass

T = t.TypeVar("T")
T_co = t.TypeVar("T_co", covariant=True)


class Deserializable(tx.Protocol[T_co]):
    def deserialize(self, b: str) -> T_co:
        ...


@dataclass
class Foo:
    name: str


class FooDeserializer:
    def deserialize(self, b: str) -> Foo:
        import json

        d = json.loads(b)
        return Foo(**d)


class JSONDeserializer(t.Generic[T]):
    Type: t.ClassVar[t.Callable[..., T]]

    def deserialize(self, b: str) -> T:
        import json

        d = json.loads(b)
        return self.Type(**d)


class FooDeserializer2(JSONDeserializer[Foo]):
    Type = Foo


def deserialize(b: str, deserializer: Deserializable[T_co]) -> T_co:
    return deserializer.deserialize(b)


print(deserialize('{"name": "foo"}', FooDeserializer()))
print(deserialize('{"name": "foo"}', FooDeserializer2()))
