import typing as t
from prestring.python import Module


# とりあえず、値として持ち運べるようなクラスを定義したい
class CodeObject:
    def __init__(self, name: str, *, emit: t.Callable[..., Module]) -> None:
        self.name = name
        self._emit = emit

    def __str__(self) -> str:
        return self.name

    def emit(self, *, m: t.Optional[Module] = None) -> Module:
        m = m or Module()
        return self._emit(m, name=self.name)


def codeobject(emit: t.Callable[..., Module]) -> CodeObject:
    name = emit.__name__
    return CodeObject(name, emit=emit)


@codeobject
def Person(m: Module, name: str) -> Module:
    # todo: import
    with m.class_(name, "BaseModel"):
        m.stmt("name: str")
        m.stmt("age : int = 0")
    return m


print(Person)
print(Person.emit())
