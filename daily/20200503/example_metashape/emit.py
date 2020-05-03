from __future__ import annotations
import typing as t
import inspect  # doc
from prestring.go.codeobject import Module, gofile
from prestring.go import goname
from prestring.naming import untitleize
from egoist.go.resolver import get_resolver
from egoist.go.types import get_gopackage
from metashape.declarative import field  # noqa: F401
from walker import walk, metadata, Item  # noqa: F401


def emit(classes: t.List[t.Type[t.Any]], *, name: str = "main") -> Module:
    m = gofile(name)
    r = get_resolver(m)

    for item in walk(classes):
        # Union:
        if item.is_union:
            print("@", item)
            continue

        # Object:
        gopackage = get_gopackage(item.type_)
        if gopackage is not None:
            continue

        typename = str(r.resolve_gotype(item.type_))

        doc = inspect.getdoc(item.type_)
        if doc:
            lines = doc.split("\n")
            m.stmt(f"// {typename} {lines[0]}")
            for line in lines[1:]:
                m.stmt(f"// {line}")

        m.stmt(f"type {typename} struct {{")
        with m.scope():
            for name, typeinfo, metadata in item.fields:
                gotype: str = r.resolve_gotype(typeinfo.raw)

                # handling field (private field?, embedded?)
                if metadata.get("inline", False):
                    m.append(gotype)
                elif name.startswith("_"):
                    m.append(f"{untitleize(goname(name))} {gotype}")
                else:
                    m.append(f"{goname(name)} {gotype}")

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
        m.sep()

    return m


_NEWLINE = "\n"
