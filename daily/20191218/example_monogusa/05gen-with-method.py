from __future__ import annotations
import typing as t
import typing_extensions as tx
from prestring.python import Module as _Module
from prestring.utils import LazyArgumentsAndKeywords, UnRepr


# メソッドを扱いたい
class Module(_Module):
    def stmt(self, fmt: t.Any, *args: t.Any, **kwargs: t.Any) -> t.Iterable[t.Any]:
        if getattr(fmt, "emit", None) is not None:
            assert not args
            assert not kwargs
            return super().stmt("{}", fmt)
        return super().stmt(str(fmt), *args, **kwargs)


class Emittable(tx.Protocol):
    def emit(self, *, m: Module) -> Module:
        ...


def is_class(co) -> bool:
    return co._args is None and co._kwargs is None


def let(name: str, co: Emittable) -> Assign:
    return Assign(name, co=co)


def as_string(val: t.Any) -> t.Union[t.Dict[str, t.Any], t.List[t.Any], str]:
    if isinstance(val, dict):
        return {k: as_string(v) for k, v in val.items()}
    elif isinstance(val, (tuple, list)):
        return [as_string(v) for v in val]
    elif hasattr(val, "emit"):
        return UnRepr(val)
    else:
        return repr(val)


class Object(Emittable):
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
        self._use_count = 0

    def __str__(self) -> str:
        if is_class(self):
            return self.name
        else:
            return f"{self.name}({LazyArgumentsAndKeywords(self._args, self._kwargs)})"

    def __call__(self, *args, **kwargs):
        return self.__class__(name=self.name, emit=self._emit, args=args, kwargs=kwargs)

    def emit(self, *, m: Module) -> Module:
        return self._emit(m, name=self.name)

    def __getattr__(self, name: str) -> t.Any:
        if self._use_count > 1:
            raise RuntimeError("assign to a variable")
        self._use_count += 1
        return Attr(name, co=self)


class Assign(Emittable):
    def __init__(self, name: str, co: Emittable) -> None:
        self.name = name
        self._co = co

    def emit(self, *, m: Module) -> Module:
        m.stmt("{} = {}", self.name, self._co)
        return m

    def __str__(self) -> str:
        return self.name

    def __getattr__(self, name: str) -> Attr:
        return Attr(name, co=self)


class Attr:
    emit = None  # for Module.stmt

    def __init__(self, name: str, co: Emittable) -> None:
        self.name = name
        self._co = co

    def __str__(self) -> str:
        return f"{self._co}.{self.name}"

    def __call__(self, *args, **kwargs) -> Call:
        return Call(self.name, co=self, args=args, kwargs=kwargs)

    def __getattr__(self, name: str) -> Attr:
        return Attr(name, co=self)


class Call:
    emit = None  # for Module.stmt

    def __init__(
        self,
        name: str,
        *,
        co: Emittable,
        args: t.Tuple[t.Any, ...],
        kwargs: t.Dict[str, t.Any],
    ) -> None:
        self._name = name

        self._co = co
        self._args = args
        self._kwargs = kwargs

    def __str__(self) -> str:
        args = as_string(self._args)
        kwargs = as_string(self._kwargs)
        lparams = LazyArgumentsAndKeywords(args, kwargs)
        return f"{self._co}({lparams})"

    def __getattr__(self, name: str) -> Attr:
        return Attr(name, co=self)

    @property
    def name(self):
        return str(self._co)


def codeobject(emit: t.Callable[..., Module]) -> Object:
    name = emit.__name__
    return Object(name, emit=emit)


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

m.sep()
p = let("p", Person("foo", age=20))
p.emit(m=m)
let("foo", p.foo).emit(m=m)
m.stmt(p.foo(p, name="foo", kwargs={"foo": p}))
m.stmt(p.foo("foo", "boo"))
print("----------------------------------------")
print(m)
