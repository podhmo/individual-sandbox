from __future__ import annotations
import typing as t
import inspect
from prestring.go.codeobject import Module, gofile
from prestring.go import goname
from prestring.naming import untitleize
from egoist.go.resolver import get_resolver, Resolver
from egoist.go.types import get_gopackage
from metashape.declarative import field  # noqa: F401
from walker import walk, metadata, Item  # noqa: F401


def build_gotags(tags: t.Dict[str, t.List[str]]) -> str:
    return " ".join(f'''{k}:"{', '.join(vs)}"''' for k, vs in tags.items())


def emit_struct(m: Module, item: Item, *, resolver: Resolver) -> None:
    gopackage = get_gopackage(item.type_)
    if gopackage is not None:
        return

    typename = str(resolver.resolve_gotype(item.type_))

    doc = inspect.getdoc(item.type_)
    if doc:
        lines = doc.split("\n")
        m.stmt(f"// {typename} {lines[0]}")
        for line in lines[1:]:
            m.stmt(f"// {line}")

    m.stmt(f"type {typename} struct {{")
    with m.scope():
        for name, typeinfo, metadata in item.fields:
            gotype: str = resolver.resolve_gotype(typeinfo.raw)

            # handling field (private field?, embedded?)
            if metadata.get("inline", False):
                m.append(gotype)
            elif name.startswith("_"):
                m.append(f"{untitleize(goname(name))} {gotype}")
            else:
                m.append(f"{goname(name)} {gotype}")

            # todo: handling tags
            if "tags" not in metadata:
                metadata["tags"] = {}
            metadata["tags"] = {"json": [name.rstrip("_")]}
            m.append(f" `{build_gotags(metadata['tags'])}`")

            # handling comments
            if metadata.get("inline", False):
                m.stmt(f"  // {metadata}")
            else:
                comment = metadata.get("comment", "")
                if comment:
                    m.stmt(f"  // {comment.split(_NEWLINE, 1)[0]}")
                else:
                    m.stmt("")

    m.stmt("}")


def emit_union(m: Module, item: Item, *, resolver: Resolver) -> None:
    typename = goname(item.type_.__name__)

    # todo: type_
    m.stmt(f"type {typename} struct {{")
    with m.scope():
        m.stmt('Kind string `json:"kind"`')
        for subtype in item.args:
            gotype: str = resolver.resolve_gotype(subtype)
            m.append(f"{gotype} *{gotype}")

            tags = {"json": [untitleize(str(gotype)).rstrip("_")]}
            m.stmt(f" `{build_gotags(tags)}`")

    m.stmt("}")
    m.sep()


def emit(classes: t.List[t.Type[t.Any]], *, name: str = "main") -> Module:
    m = gofile(name)
    r = get_resolver(m)

    for item in walk(classes):
        if item.is_union:
            emit_union(m, item, resolver=r)
        else:
            emit_struct(m, item, resolver=r)
        m.sep()

    return m


_NEWLINE = "\n"
