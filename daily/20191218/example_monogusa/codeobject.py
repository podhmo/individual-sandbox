from __future__ import annotations
from functools import update_wrapper
import typing as t
import typing_extensions as tx
from prestring.python import Module as _Module
from prestring.utils import LazyArgumentsAndKeywords, UnRepr


class Module(_Module):
    def stmt(
        self, fmt_or_emittable: t.Any, *args: t.Any, **kwargs: t.Any
    ) -> t.Iterable[t.Any]:
        if getattr(fmt_or_emittable, "emit", None) is not None:  # Emittable
            assert not args
            assert not kwargs
            return fmt_or_emittable.emit(m=self)
        return super().stmt(str(fmt_or_emittable), *args, **kwargs)

    def let(self, name: str, val: Emittable) -> Emittable:
        assigned = let(name, val)
        assigned.emit(m=self)
        return assigned

    assign = let  # alias


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
    emit = None  # for stmt

    def __init__(self, name: str, co: Emittable) -> None:
        self.name = name
        self._co = co

    def __str__(self) -> str:
        return f"{self._co}.{self.name}"

    if t.TYPE_CHECKING:
        # INTERNAL ERROR -- Please try using mypy master on Github:
        def __call__(self, *args, **kwargs) -> Call:
            ...

    else:

        def __call__(self, *args, **kwargs) -> Call:
            return Call(self.name, co=self, args=args, kwargs=kwargs)

    if t.TYPE_CHECKING:

        def __getattr__(self, name: str) -> Attr:
            ...
            # INTERNAL ERROR -- Please try using mypy master on Github:

    else:

        def __getattr__(self, name: str) -> Attr:
            # if name == "emit":
            #     raise AttributeError(name)
            return Attr(name, co=self)


class Call:
    emit = None  # for stmt

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

    if t.TYPE_CHECKING:

        def __getattr__(self, name: str) -> Attr:
            ...
            # INTERNAL ERROR -- Please try using mypy master on Github:

    else:

        def __getattr__(self, name: str) -> Attr:
            # if name == "emit":
            #     raise AttributeError(name)
            return Attr(name, co=self)

    @property
    def name(self):
        return str(self._co)


if t.TYPE_CHECKING:
    # INTERNAL ERROR -- Please try using mypy master on Github:
    def codeobject(
        emit: t.Callable[[Module, str], Module], *, name: t.Optional[str] = None
    ) -> Object:
        ...


else:

    def codeobject(
        emit: t.Callable[[Module, str], Module], *, name: t.Optional[str] = None
    ) -> Object:
        name = name or emit.__name__
        ob = Object(name, emit=emit)
        update_wrapper(ob, emit)
        return ob
