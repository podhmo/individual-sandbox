from __future__ import annotations
import typing as t
import sys
import json
from dataclasses import dataclass, field, MISSING
from prestring.python.codeobject import Module


@dataclass
class Node:
    name: str
    fields: t.List[Field] = field(default_factory=list)


@dataclass
class Field:
    name: str
    value: t.Any = field(default=MISSING)
    type_: str  # t.Type[t.Any]


def decode_value(x: str) -> t.Any:
    x = x.strip()
    if x.startswith("{") and x.endswith("}"):
        return json.loads(x)
    elif x.startswith("[") and x.endswith("]"):
        return json.loads(x)
    return x


def parse(args: t.List[str]) -> t.List[Node]:
    nodes = []
    current = None
    for x in iter(args):
        if x == "-":
            nodes.append(current)
            current = None
            continue
        if current is None:
            current = Node(name=x, fields=[])
            continue

        if x.startswith("--"):
            current.fields.append(Field(name=x[2:], type_="str"))
        elif x.startswith("@"):
            current.fields[-1].type_ = x[1:]
        else:
            current.fields[-1].value = decode_value(x)

    if current is not None:
        nodes.append(current)
    return nodes


def emit(
    nodes: t.List[Node],
    *,
    m: t.Optional[Module] = None,
    aliases: t.Optional[t.Dict[str, str]] = None,
) -> Module:
    if aliases is None:
        aliases = {}
    m = Module()
    m.toplevel = m.submodule()

    for node in nodes:
        with m.class_(node.name):
            if len(node.fields) == 0:
                m.stmt("pass")
            for f in node.fields:
                if "." in f.type_:
                    module_path = f.type_.rsplit(".", 1)[0]
                    as_ = None
                    if module_path in aliases:
                        as_, module_path, = module_path, aliases[module_path]
                    m.toplevel.import_(module_path, as_=as_)
                m.stmt(f"{f.name}: {f.type_}")

    if str(m.toplevel) != "":
        m.toplevel.sep()
    return m


args = sys.argv[1:]
nodes = parse(args)
print(emit(nodes, aliases={"t": "typing"}))
