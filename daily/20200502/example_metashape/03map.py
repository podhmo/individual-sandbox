from __future__ import annotations
import typing as t
import typing_extensions as tx
from prestring.go.codeobject import Module, gofile
from prestring.go import goname
from egoist.go.resolver import get_resolver
from egoist.go.types import get_gopackage
from walker import walk
from metashape.declarative import field, MISSING


class Person:
    name: str
    age: int
    info: Info
    father: t.Optional[Person]
    mother: t.Optional[Person]
    children: t.List[Person]


class Person2:
    name: str
    age: int
    info: Info = field(metadata={"inline": True})


class Info:
    memo: str
    data: t.Dict[str, t.Any]
    nested: t.Dict[str, t.Dict[str, str]]


def metadata(
    *, inline: bool = False, pointer: bool = False, required: bool = True
) -> t.Dict[str, t.Any]:
    # todo: required false?
    d: Metadata = {"inline": inline, "pointer": pointer, "required": required}
    return d  # type: ignore


class Metadata(tx.TypedDict, total=False):
    inline: bool
    pointer: bool
    required: bool
    default: t.Any


def run() -> Module:
    r = get_resolver()
    m = gofile("main")

    classes = [Person, Person2]

    for item in walk(classes):
        gopackage = get_gopackage(item.cls)
        if gopackage is not None:
            continue

        m.stmt(f"type {goname(item.cls.__name__)} struct {{")
        with m.scope():
            for name, typeinfo, _metadata in item.fields:
                metadata = t.cast(Metadata, _metadata)
                if metadata.get("default") == MISSING:
                    metadata.pop("default")

                typ = typeinfo.raw
                if metadata.get("pointer", False):
                    typ = t.Optional[typ]
                gotype: str = r.resolve_gotype(typ)
                gopackage = get_gopackage(
                    typeinfo.normalized
                )  # todo: support composite

                if gopackage is not None:
                    gotype = f"{m.import_(gopackage)}.{gotype}"

                if metadata.get("inline", False):
                    m.append(gotype)
                else:
                    m.append(f"{goname(name)} {gotype}")

                if metadata:
                    m.stmt(f"  // {metadata}")
                else:
                    m.stmt("")

        m.stmt("}")
        m.sep()
    return m


def gofmt(code: str) -> str:
    import os

    if not bool(os.environ.get("GOFMT")):
        return code

    import subprocess
    import tempfile

    with tempfile.TemporaryFile("w+") as wf:
        wf.write(code)
        wf.seek(0)
        p = subprocess.run(
            ["gofmt"], stdin=wf, stdout=subprocess.PIPE, text=True, check=True
        )
        return p.stdout


print(gofmt(str(run())))
