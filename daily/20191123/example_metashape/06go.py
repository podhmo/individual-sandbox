from __future__ import annotations
import typing as t
import logging
from metashape.runtime import get_walker
from prestring.go import Module, goname
from prestring.output import output
from metashape.analyze import typeinfo

logger = logging.getLogger(__name__)


class Info:
    memo: str


class Person:
    name: str
    age: int
    nickname: t.Optional[str]
    info: t.Dict[str, Info]
    second: t.Optional[Person]


class Team:
    name: str
    members: t.List[Person]


def _underlying_schema_type(info: typeinfo.Atom) -> str:
    typ = info.underlying
    if info.custom is not None:  # t.Type?
        return typ.__name__

    if issubclass(typ, str):
        return "string"
    elif issubclass(typ, bool):
        return "bool"
    elif issubclass(typ, float):
        return "float"
    elif issubclass(typ, int):
        return "int"  # int8, int16, int32, int64
    logger.warning("unexpected type: %r", info)
    raise ValueError("unsupported %r", info)


def detect_type(info: typeinfo.TypeInfo) -> str:
    if isinstance(info, typeinfo.Container):
        if info.container == "dict" and len(info.args) == 2:
            typ = f"map[{detect_type(info.args[0])}]{detect_type(info.args[1])}"
            if info.is_optional:
                typ = f"*{typ}"
            return typ
        elif info.container in ("list", "tuple") and len(info.args) == 1:
            typ = detect_type(info.args[0])
            typ = f"[]{typ}"
            if info.is_optional:
                typ = f"*{typ}"
            return typ
    else:  # Atom
        typ = _underlying_schema_type(info)
        if info.is_optional:
            typ = f"*{typ}"
        return typ
    logger.warning("unexpected type: %r", info)
    raise ValueError("unsupported %r", info)


w = get_walker(aggressive=True)
walked = w.walked()
m = Module()
package = "main"

for cls in walked.objects:
    with output(package, use_console=True, verbose=True, opener=Module) as fs:
        with fs.open(f"{cls.__name__}.go", "w") as m:
            m.stmt(f"package {package}")

            typename = goname(cls.__name__)
            m.comment(f"{typename} ...")
            with m.type_(typename, "struct"):
                for name, info, metadata in w.for_type(cls).walk():
                    m.stmt(f"""{goname(name)} {detect_type(info)} `json:"{name}"`""")
