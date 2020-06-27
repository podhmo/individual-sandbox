from __future__ import annotations
import typing as t
import typing_extensions as tx
from functools import partial
import dataclasses
from collections import defaultdict

if t.TYPE_CHECKING:
    from contextstack import ContextStack, ArgsAttr
T = t.TypeVar("T")
N = t.TypeVar("N", bound=int)


class Marker(t.Generic[T]):
    pool: t.Dict[str, t.Callable[..., t.Any]]
    default_pool: t.Dict[t.Type[t.Any], t.Callable[..., t.Any]]

    def __init__(self, name: str, *, default: T) -> None:
        self.name = name
        self.pool: t.Dict[str, t.List[t.Callable[..., t.Any]]] = defaultdict(list)
        self._default = default

    def mark(
        self,
        fn: t.Callable[..., t.Any],
        *,
        name: t.Optional[str] = None,
        val: t.Optional[T] = None,
    ) -> t.Callable[..., t.Any]:
        name = name or fn.__name__
        self.pool[name].append(fn)
        val = val or self._default
        setattr(fn, f"_marked_as_{self.name}", val)
        return fn

    def __call__(self, fn: t.Callable[..., t.Any]) -> T:
        v = getattr(fn, f"_marked_as_{self.name}", self._default)  # type: T
        return v

    def is_marked(self, fn: t.Callable[..., t.Any]) -> bool:
        v = getattr(fn, f"_marked_as_{self.name}", self._default)
        return v != self._default

    def __contains__(self, name: str) -> bool:
        return name in self.pool


class Metadata(tx.TypedDict):
    path: str
    method: str
    tags: t.List[str] = None
    summary: str = None
    description: str = None

    # callbacks: List[routing.APIRoute] = None


class API:
    def __init__(self) -> None:
        self._zero_metadata: Metadata = {}
        self._marker = Marker("api_route", default=self._zero_metadata)
        self._stack = _create_default_runtime_context()

    def get_current_context(self) -> ArgsAttr[Parameter]:
        return self._stack.current

    def _register_with(
        self, fn: t.Callable[..., t.Any], *, method: str, path: str, metadata: Metadata
    ) -> t.Callable[..., t.Any]:
        metadata["method"] = method.lower()
        metadata["path"] = path
        k = f'{metadata["method"]}::{metadata["path"]}'
        return self._marker.mark(fn, name=k, val=metadata)

    def post(
        self, path: str, metadata: t.Optional[Metadata] = None,
    ) -> t.Callable[[t.Callable[..., t.Any]], t.Callable[..., t.Any]]:
        metadata: Metadata = metadata or {}
        return partial(self._register_with, method="post", path=path, metadata=metadata)

    def get(
        self, path: str, metadata: t.Optional[Metadata] = None,
    ) -> t.Callable[[t.Callable[..., t.Any]], t.Callable[..., t.Any]]:
        metadata: Metadata = metadata or {}
        return partial(self._register_with, method="get", path=path, metadata=metadata)

    def put(
        self, path: str, metadata: t.Optional[Metadata] = None,
    ) -> t.Callable[[t.Callable[..., t.Any]], t.Callable[..., t.Any]]:
        metadata: Metadata = metadata or {}
        return partial(self._register_with, method="put", path=path, metadata=metadata)

    @property
    def routes(self) -> t.Iterator[t.Tuple[t.Callable[..., t.Any], Metadata]]:
        for fns in self._marker.pool.values():
            for fn in fns:
                metadata = self._marker(fn)
                yield fn, metadata


class Emittable(tx.Protocol):
    @classmethod
    def __emit__(cls, name: str, base: t.Type[T], d: t.Dict[str, t.Any]) -> Parameter:
        ...


class Dictable(tx.Protocol):
    def asdict(self) -> t.Dict[str, t.Any]:
        ...


def asdict_default(ob: object, *, extra_key="extra_data") -> t.Dict[str, t.Any]:
    d = dataclasses.asdict(ob)
    for k, v in list(d.items()):
        if v is None:
            d.pop(k)
        if k.startswith("_"):
            d.pop(k)  # e.g. for "_asdict"
        if k.endswith("_"):
            d[k.rstrip("_")] = d.pop(k)  # e.g. for "in"

    extra_data = d.pop(extra_key, None)
    if extra_data is not None:
        d.update(extra_data)
    return d


# response
class DefaultStatus:
    def __init__(self, code: int = 200) -> None:
        self.code = code


@dataclasses.dataclass(eq=False)
class Response(t.Generic[T]):
    result: T
    code: int = 200
    description: str = "Successful Response"
    _asdict: t.Callable[[Parameter], t.Dict[str, t.Any]] = asdict_default

    def asdict(self) -> t.Dict[str, t.Any]:
        return self._asdict(self)


ErrorResponse = partial(Response, code=500, description="unexpected error")


# parameters
class Query(t.Generic[T]):
    @classmethod
    def __emit__(cls, name: str, base: t.Type[T], d: t.Dict[str, t.Any]) -> Parameter:
        return Parameter(name=name, in_="query", required=False, _type=base)


class Path(t.Generic[T]):
    @classmethod
    def __emit__(cls, name: str, base: t.Type[T], d: t.Dict[str, t.Any]) -> Parameter:
        return Parameter(name=name, in_="path", required=True, _type=base)


class Formdata(t.Generic[T]):
    @classmethod
    def __emit__(cls, name: str, base: t.Type[T], d: t.Dict[str, t.Any]) -> Parameter:
        return Parameter(name=name, in_="formdata", required=True, _type=base)


class Body(t.Generic[T]):
    @classmethod
    def __emit__(cls, name: str, base: t.Type[T], d: t.Dict[str, t.Any]) -> Parameter:
        return Parameter(name=name, in_="formdata", required=True, _type=base)


@dataclasses.dataclass(eq=False)
class Parameter:
    name: str
    in_: tx.Literal["header", "query", "path", "body", "formData"] = "path"
    description: t.Optional[str] = None
    required: t.Optional[bool] = True
    deprecated: t.Optional[bool] = None
    allowEmptyValue: t.Optional[bool] = None
    schema: t.Optional[t.Dict[str, t.Any]] = None

    _asdict: t.Callable[[Parameter], t.Dict[str, t.Any]] = asdict_default
    _type: t.Optional[t.Type[t.Any]] = None
    extra_data: t.Optional[t.Dict[str, t.Any]] = None

    def asdict(self) -> t.Dict[str, t.Any]:
        return self._asdict(self)


def _create_default_runtime_context(
    *, reset: bool = False
) -> ContextStack[ArgsAttr[Parameter]]:
    from contextstack import ContextStack, ArgsAttr

    def arg_factory(name: str) -> Parameter:
        return Parameter(name=name, _type=str)

    def args_attr_factory(names: t.List[t.Any]) -> ArgsAttr[Parameter]:
        return ArgsAttr(names, factory=arg_factory)

    s = ContextStack(factory=args_attr_factory)
    s.push([])  # root
    return s
