from __future__ import annotations
import typing as t
import typing_extensions as tx
import inspect  # doc
from prestring.go.codeobject import Module, gofile
from prestring.go import goname
from prestring.naming import untitleize
from egoist.go.resolver import get_resolver
from egoist.go.types import get_gopackage
from metashape.declarative import MISSING, field  # noqa: F401
from walker import walk


def metadata(*, inline: bool = False, required: bool = True) -> t.Dict[str, t.Any]:
    # todo: required false?
    d: Metadata = {"inline": inline, "required": required}
    return d  # type: ignore


class Metadata(tx.TypedDict, total=False):
    inline: bool
    required: bool
    default: t.Any


def emit(classes: t.List[t.Type[t.Any]], *, name: str = "main") -> Module:
    m = gofile(name)
    r = get_resolver(m)

    for item in walk(classes):
        gopackage = get_gopackage(item.cls)
        if gopackage is not None:
            continue

        typename = goname(item.cls.__name__)
        doc = inspect.getdoc(item.cls)
        if doc:
            lines = doc.split("\n")
            m.stmt(f"// {typename} {lines[0]}")
            for line in lines[1:]:
                m.stmt(f"// {line}")

        m.stmt(f"type {typename} struct {{")
        with m.scope():
            for name, typeinfo, _metadata in item.fields:
                metadata = t.cast(Metadata, _metadata)
                if metadata.get("default") == MISSING:
                    metadata.pop("default")

                typ = typeinfo.raw
                if metadata.get("pointer", False):
                    typ = t.Optional[typ]
                gotype: str = r.resolve_gotype(typ)

                if metadata.get("inline", False):
                    m.append(gotype)
                elif name.startswith("_"):
                    m.append(f"{untitleize(goname(name))} {gotype}")
                else:
                    m.append(f"{goname(name)} {gotype}")

                if metadata:
                    m.stmt(f"  // {metadata}")
                else:
                    m.stmt("")

        m.stmt("}")
        m.sep()

    return m
