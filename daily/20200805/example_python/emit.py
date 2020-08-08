from __future__ import annotations
from functools import lru_cache
import typing as t
import typing_extensions as tx
import os
import sys
import inspect
import dataclasses
from types import ModuleType
from prestring.text import Module
from metashape import runtime
from metashape.types import Kind


def _get_type_name(typ: t.Type[t.Any]) -> None:
    if hasattr(typ, "__origin__"):
        return str(typ)
    return runtime.get_name(typ)


def _get_html_id(name: str) -> str:
    return name


def _get_method_signature(name: str, attr: t.Callable[..., t.Any]) -> str:
    return name + str(inspect.signature(attr)).replace("->", ":").replace(
        "NoneType", "void"
    ).replace("(self, ", "(").replace("(self)", "()")


@lru_cache
def _get_mro(cls: t.Type[t.Any]) -> t.List[t.Type[t.Any]]:
    r = []
    for x in cls.mro()[1:-1]:
        if x == tx.Protocol or hasattr(x, "__origin__") and x.__origin__ == tx.Protocol:
            break
        r.append(x)
    return r


@lru_cache
def _is_interface(cls: t.Type[t.Any]) -> t.List[t.Type[t.Any]]:
    for x in cls.mro()[1:-1]:
        if x == tx.Protocol or hasattr(x, "__origin__") and x.__origin__ == tx.Protocol:
            return True
    return False


@dataclasses.dataclass(frozen=True, eq=False)
class Context:
    walker: runtime.Walker
    is_minimum: bool = False


def emit_node(ctx: Context, cls: t.Type[t.Any], *, m: Module) -> Module:
    w = ctx.walker

    name = _get_type_name(cls)
    labelname = name
    if cls.mro()[1] == tx.Protocol:
        labelname = f"&lt;&lt;Interface&gt;&gt;&nbsp;{name}"

    methods = []
    attributes = []

    # todo: cache?
    seen = set()
    for parent_cls in _get_mro(cls):
        for attrname, _, _ in w.walk_fields(parent_cls, ignore_private=True):
            seen.add(attrname)
    for attrname, info, metadata in w.walk_fields(cls, ignore_private=True):
        if attrname in seen:
            continue
        if info.user_defined_type is not None:
            continue
        attributes.append(f"+ {attrname}: {_get_type_name(info.type_)}")

    # todo: walk_methods(cls, ignore_private=True)
    for attrname, attr in cls.__dict__.items():
        if attrname.startswith("_"):
            continue
        if not callable(attr):
            continue

        # for inspect.signature() with PEP563
        attr.__annotations__ = t.get_type_hints(attr)
        methods.append(f"+ {_get_method_signature(attrname, attr)}")

    m.stmt(f"{name} [")
    with m.scope():
        m.stmt('shape = "none"')
        m.stmt(f'URL = "#{_get_html_id(name)}"')
        if ctx.is_minimum:
            m.stmt(
                f'label = <<TABLE BGCOLOR="gray95" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="6" ><TR><TD>{labelname}</TD></TR></TABLE>>'
            )
        else:
            m.stmt(
                f'label = <<TABLE BGCOLOR="gray95" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="6" ><TR><TD>{labelname}</TD></TR><TR><TD ALIGN="LEFT" BALIGN="LEFT">{"<BR/>".join(attributes)}</TD></TR><TR><TD ALIGN="LEFT" BALIGN="LEFT">{"<BR/>".join(methods)}</TD></TR></TABLE>>'
            )
    m.stmt("]")
    m.sep()
    return m


def emit(ctx: Context, *, m: t.Optional[Module] = None) -> Module:
    w = ctx.walker
    m = m or Module()

    m.stmt("digraph G {")
    with m.scope():
        # setup
        m.stmt("graph [")
        with m.scope():
            m.stmt("compound = true")
        m.stmt("]")
        m.sep()
        m.stmt("node [")
        with m.scope():
            m.stmt('shape = "record"')
        m.stmt("]")
        m.sep()
        m.stmt("edge [")
        with m.scope():
            m.stmt('dir = "back"')
            m.stmt('arrowtail = "empty"')
            m.stmt("arrowsize = 0.65")
        m.stmt("]")
        m.sep()

        has_inheritance_types: t.List[t.Type[t.Any]] = []
        relations: t.List[
            t.Tuple[str, t.Type[t.Any], t.Type[t.Any]]
        ] = []  # name, from, to

        for cls in w.walk():
            emit_node(ctx, cls, m=m)
            if len(_get_mro(cls)) > 0:
                has_inheritance_types.append(cls)
                for parent_cls in _get_mro(cls):
                    w.append(parent_cls)

            # todo: cache?
            seen = set()
            for parent_cls in _get_mro(cls):
                for attrname, _, _ in w.walk_fields(parent_cls, ignore_private=True):
                    seen.add(attrname)
            for attrname, info, metadata in w.walk_fields(cls, ignore_private=True):
                if attrname in seen:
                    continue
                if info.user_defined_type is None:
                    continue
                from_ = cls
                to = info.user_defined_type

                # todo: 1,0,*,?
                from_n = ""
                to_n = ""
                if info.is_container and info.container_type in ("list", "tuple"):
                    to_n = "*"
                label = metadata.get("label")
                relations.append((attrname, from_, to, (from_n, to_n, label)))

        # emit link (inheritance)
        for cls in has_inheritance_types:
            direct_parent_cls = cls.mro()[1]
            if _is_interface(cls):
                m.stmt(
                    f'{_get_type_name(direct_parent_cls)} -> {_get_type_name(cls)} [style="dashed"]'
                )
            else:
                m.stmt(f"{_get_type_name(direct_parent_cls)} -> {_get_type_name(cls)}")

        # emit link (relation)
        if len(relations) > 0:
            m.sep()
            m.stmt("edge [")
            with m.scope():
                m.stmt("constraint = false")
                m.stmt("minlen = 3")
                # todo: more styles
                m.stmt('arrowtail = "none"')
                # m.stmt('arrowtail = "normal"')
                # m.stmt('headlabel = "*"')
                # m.stmt('taillabel = "1"')
            m.stmt("]")
        for relname, from_, to, (from_n, to_n, label) in relations:
            attrs = []
            if label is not None:
                attrs.append(f'label = "{label}"')
            if to_n:
                attrs.append(f'headlabel = "{to_n}"')

            if attrs:
                attrs_str = f" [{', '.join(attrs)}]"
            else:
                attrs_str = ""
            m.stmt(f"{_get_type_name(from_)} -> {_get_type_name(to)}{attrs_str}")

    m.stmt("}")
    return m


def main(mod: ModuleType) -> None:
    def _guess_kind(
        cls: t.Any,
        *,
        _builtins=set(id(v) for v in sys.modules["builtins"].__dict__.values()),
    ) -> t.Optional[Kind]:
        # is module?
        if hasattr(cls, "__loader__"):
            return None

        # is typed user_defined_type class or callable?
        if not hasattr(cls, "__name__"):
            return None

        if id(cls) in _builtins:
            return None
        if not callable(cls):
            return None

        if inspect.isclass(cls):
            return "object"
        return None

    w = runtime.get_walker(
        mod, recursive=True, aggressive=True, _guess_kind=_guess_kind
    )
    m = Module()
    o = sys.stderr if bool(os.environ.get("DEBUG", "")) else sys.stdout
    ctx = Context(w, is_minimum=bool(os.environ.get("MINIMUM", "")))
    print(emit(ctx, m=m), file=o)


if __name__ == "__main__":
    from magicalimport import import_module

    m = import_module(sys.argv[1])
    main(m)
