from __future__ import annotations
import typing as t
from prestring.python import Module
from prestring.utils import LazyArgumentsAndKeywords


# メソッドを扱うためには代入が欲しくなる


def is_class(co) -> bool:
    return co._args is None and co._kwargs is None


def let(name: str, co: CodeObject) -> Assign:
    return Assign(name, co=co)


class CodeObject:
    def __init__(
        self,
        name: str,
        *,
        emit: t.Callable[..., Module],
        args: t.Optional[t.List[t.Any]] = None,
        kwargs: t.Optional[t.Dict[str, t.Any]] = None,
    ) -> None:
        self.name = name
        self._emit = emit

        self._args = args
        self._kwargs = kwargs

    def __str__(self) -> str:
        if is_class(self):
            return self.name
        else:
            return f"{self.name}({LazyArgumentsAndKeywords(self._args, self._kwargs)})"

    def __call__(self, *args, **kwargs):
        return self.__class__(name=self.name, emit=self._emit, args=args, kwargs=kwargs)

    def emit(self, *, m: Module) -> Module:
        return self._emit(m, name=self.name)


class Assign:
    def __init__(self, name: str, co: CodeObject) -> None:
        self.name = name
        self._co = co

    def emit(self, *, m: Module) -> Module:
        m.stmt("{} = {}", self.name, self._co)
        return m

    def __str__(self) -> str:
        return self.name


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


m = Module()
Person.emit(m=m)
m.stmt("{} # is class? {}", Person, is_class(Person))
m.stmt("p = {} # is class? {}", Person("foo", age=20), is_class(Person("foo", age=20)))

print("---")
p = let("p", Person("foo", age=20))
p.emit(m=m)
m.stmt("{}()", p)
print(m)
