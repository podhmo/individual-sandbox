import typing as t
import typing_extensions as tx


class Person:
    name: str
    info: tx.Annotated[str, "info"]


print(t.get_type_hints(Person))
p = Person()
p.name = "foo"
p.info = "foo"
if t.TYPE_CHECKING:
    reveal_type(p)
    reveal_type(p.name)
    reveal_type(p.info)

foo: tx.Annotated[str, "info"] = "hai"
if t.TYPE_CHECKING:
    reveal_type(foo)
