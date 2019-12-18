import typing as t
from prestring.python import Module
from prestring.utils import LazyArgumentsAndKeywords


# やっぱりオブジェクトとしても扱いたいかも
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
        if self.is_class:
            return self.name
        else:
            return f"{self.name}({LazyArgumentsAndKeywords(self._args, self._kwargs)})"

    @property
    def is_class(self):
        return self._args is None and self._kwargs is None

    def __call__(self, *args, **kwargs):
        return self.__class__(name=self.name, emit=self._emit, args=args, kwargs=kwargs)

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


m = Module()
Person.emit(m=m)
m.stmt("{} # is class? {}", Person, Person.is_class)
m.stmt("p = {} # is class? {}", Person("foo", age=20), Person("foo", age=20).is_class)
print(m)
