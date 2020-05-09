from __future__ import annotations
import typing as t
import typing_extensions as tx
import inspect
import dataclasses
import enum
import os.path
from types import ModuleType
from inflection import pluralize
from prestring import NEWLINE
from prestring import Module as _Module
from prestring.utils import reify
from prestring.codeobject import Symbol, CodeObjectModuleMixin
from metashape.analyze.walker import Walker

Command = t.Callable[..., t.Any]


def scan_module(
    module: ModuleType,
    *,
    is_ignored: t.Optional[t.Callable[[Command], bool]] = None,  # inspect.isclass
    targets: t.Optional[t.List[str]] = None,
) -> t.Dict[str, Command]:
    targets = targets or list(module.__dict__.keys())
    defs = {}

    for name in targets:
        v = module.__dict__.get(name)
        if v is None:
            continue
        if name.startswith("_"):
            continue
        if not hasattr(v, "__module__"):
            continue

        if is_ignored and is_ignored(v):
            continue

        if v.__module__ == __name__:
            pass
        elif not hasattr(v, "__origin__") and name[0] != name[0].upper():
            continue

        if not callable(v):
            continue
        defs[name] = v
    return defs


Kind = tx.Literal["object", "service", "list", "enum", "unknown"]


@dataclasses.dataclass
class Item:
    name: str
    type_: t.Type[t.Any]
    kind: Kind


# special type
class Service:
    _service_attrs_cache = None

    @classmethod
    def _get_methods(cls):
        if "_service_attrs_cache" not in cls.__dict__:
            parent_cache = Service._service_attrs_cache
            if parent_cache is None:
                parent_cache = Service._service_attrs_cache = set(
                    Service.__dict__.keys()
                )
            cls._service_attrs_cache = list(
                [k for k in cls.__dict__.keys() if k not in parent_cache]
            )
        return [getattr(cls, name) for name in cls._service_attrs_cache]


def walk(defs: t.Dict[str, t.Type[t.Any]]) -> t.Iterator[Item]:
    for name, v in defs.items():
        kind: Kind = "unknown"
        if hasattr(v, "__origin__"):
            kind = v.__origin__.__name__  # list
            assert kind == "list"
        elif v == Service:
            continue
        elif inspect.isclass(v):
            if issubclass(v, enum.Enum):
                kind = "enum"
            elif issubclass(v, Service):
                kind = "service"
            else:
                kind = "object"

        yield Item(name=name, kind=kind, type_=v)


class Module(CodeObjectModuleMixin, _Module):
    def sep(self) -> None:
        self.body.append(NEWLINE)

    @reify
    def _import_area(self) -> Module:
        sm = self.submodule("", newline=True)
        self.stmt("")
        return sm

    def import_(self, path: str) -> Symbol:
        im = self._import_area
        im.stmt(f'import "{path}";')
        prefix = os.path.dirname(path).replace("/", ".")
        return self.symbol(prefix)


class TypeResolver:
    def __init__(
        self, m: Module, *, aliases: t.Optional[t.Dict[t.Any, Symbol]] = None
    ) -> None:
        self.m = m
        self.aliases = aliases or {}
        self.aliases.update({str: "string"})

    def resolve_type(self, typ: t.Type[t.Any]) -> Symbol:
        if typ in self.aliases:
            return self.aliases[typ]

        if hasattr(typ, "PROTO_PACKAGE"):
            return getattr(self.m.import_(typ.PROTO_PACKAGE), typ.__name__)
        return typ.__name__


def emit(items: t.Iterator[Item], *, name: str) -> Module:
    from metashape.runtime import get_walker

    m = Module()
    m.stmt('syntax = "proto3";')
    m.sep()
    m.stmt(f"package {name};")
    m.sep()
    m._import_area

    items = list(items)
    w = get_walker([])

    classes: t.Dict[t.Type[t.Any], Item] = {}
    aliases: t.Dict[t.Any, Symbol] = {}
    for item in items:
        if item.kind in ("object", "enum", "service"):
            classes[item.type_] = item
            w.append(item.type_)

        if hasattr(item.type_, "PROTO_PACKAGE"):
            prefix = m.import_(item.type_.PROTO_PACKAGE)
            aliases[item.type_] = getattr(prefix, item.name)
        else:
            aliases[item.type_] = m.symbol(item.name)

    resolver = TypeResolver(m, aliases=aliases)

    for cls in w.walk(kinds=["object", None]):
        if hasattr(cls, "PROTO_PACKAGE"):
            continue

        item = classes[cls]
        if item.kind == "object":
            emit_class(m, item, w=w, resolver=resolver)
        elif item.kind == "enum":
            emit_enum(m, item, w=w, resolver=resolver)
        elif item.kind == "service":
            emit_service(m, item, w=w, resolver=resolver)
        m.sep()

    for item in items:
        if item.kind == "list":
            emit_list(m, item, w=w, resolver=resolver)
    return m


def emit_class(m: Module, item: Item, *, w: Walker, resolver: TypeResolver) -> Symbol:
    name = item.name
    cls = item.type_

    i = 1
    m.stmt(f"message {name} {{")
    with m.scope():
        for name, info, _metadata in w.for_type(cls).walk(ignore_private=False):
            typ = resolver.resolve_type(info.normalized)
            m.stmt(f"{typ} {name} = {i};")  # todo: deprecated
            i += 1
    m.stmt("}")
    return m.symbol(name)


def emit_enum(m: Module, item: Item, *, w: Walker, resolver: TypeResolver) -> Symbol:
    name = item.name
    cls = t.cast(t.Type[enum.Enum], item.type_)  # enum.Enum

    m.stmt(f"enum {name} {{")
    with m.scope():
        for attr in cls:
            m.stmt(f"{attr.name} = {attr.value};")
    m.stmt("}")
    return m.symbol(name)


def emit_service(m: Module, item: Item, *, w: Walker, resolver: TypeResolver) -> Symbol:
    name = item.name
    cls = item.type_

    i = 1
    m.stmt(f"service {name} {{")
    with m.scope():
        for method in cls._get_methods():
            argspec = inspect.getfullargspec(method)
            argspec.annotations.update(t.get_type_hints(method))

            args = [
                str(resolver.resolve_type(argspec.annotations[name]))
                for name in argspec.args
                if name in argspec.annotations
            ]
            ret_type = argspec.annotations.get("return")

            # todo: suppot tuple
            returns = [str(resolver.resolve_type(ret_type))]
            m.stmt(
                f"rpc {method.__name__}({', '.join(args)}) returns ({', '.join(returns)}) {{"
            )
            with m.scope():
                pass
            m.stmt("}")
    m.stmt("}")
    return m.symbol(name)


def emit_list(m: Module, item: Item, *, w: Walker, resolver: TypeResolver) -> Symbol:
    name = item.name
    cls = t.get_args(item.type_)[0]
    i = 1
    m.stmt(f"message {name} {{")
    with m.scope():
        m.stmt(f"repeated {cls.__name__} {pluralize(cls.__name__.lower())} = {i};")
    m.stmt("}")

    return m.symbol(name)
