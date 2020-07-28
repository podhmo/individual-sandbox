from __future__ import annotations
import typing as t
import typing_extensions as tx
import sys
import dataclasses
from handofcats import as_command
from dictknife import loading
from prestring.python.codeobject import Module


@dataclasses.dataclass
class Node:
    path: str
    method: str
    tags: t.List[str]
    args: t.List[Param]
    summary: str
    description: str
    operationId: str


@dataclasses.dataclass
class Param:
    name: str
    type_: str
    required: bool
    description: str
    default: t.Any = None


Method = tx.Literal["GET", "POST", "PUT", "DELETE"]


# tags[0] -> Node
def parse(d: t.Dict[str, t.Any]) -> t.Dict[str, t.List[Node]]:
    r = {}
    for path, pathinfo in d["paths"].items():
        for method, endpoint in pathinfo.items():
            if method not in ("get", "post", "put", "delete"):
                print("hmm", method, file=sys.stderr)
                continue
            operationId = endpoint.get("operationId")
            if operationId is None:
                operationId = f"unknown__{path.replace('/', '_')}__{method}"

            # parameters in (body, path, query, header)
            args = []
            for x in endpoint.get("parameters", []):
                if x["in"] == "body":
                    args.append(
                        Param(
                            name=x["name"],
                            type_="objects." + x["schema"]["$ref"].rsplit("/", 1)[-1],
                            description=x.get("description"),
                            required=x.get("required", False),
                        )
                    )
                if x["in"] == "query":
                    args.append(
                        Param(
                            name=x["name"],
                            type_=f"Query[{x.get('type')}]",
                            description=x.get("description"),
                            required=x.get("required", False),
                        )
                    )
            node = Node(
                path=path,
                method=method.upper(),
                args=args,
                tags=endpoint.get("tags", []),
                description=endpoint.get("description", ""),
                summary=endpoint.get("summary", ""),
                operationId=operationId,
            )
            first_tag = node.tags[0] if len(node.tags) > 0 else "Unknown"
            if first_tag not in r:
                r[first_tag] = []
            r[first_tag].append(node)
            # todo: parameters, responses
    return r


def emit(r: t.Dict[str, t.List[Node]]) -> Module:
    m = Module()
    m.toplevel = m.submodule()
    m.import_("objects")
    m.sep()

    for name, nodelist in r.items():
        m.stmt(f"@first_tag({name!r})")
        with m.class_(name.replace(" ", "_") + "Service"):
            for node in nodelist:
                m.stmt(
                    f"@endpoint(path={node.path!r}, method={node.method.upper()!r}, summary={node.summary!r})"
                )  # , tags={node.tags!r})

                args = []
                for x in node.args:
                    typ = f"{x.type_}"
                    if not x.required:
                        prefix = m.toplevel.import_("typing", as_="t")
                        typ = f"{prefix}.Optional[{typ}]"
                    if x.description:
                        prefix = m.toplevel.import_("typing_extensions", as_="tx")
                        typ = f"{prefix}.Annotated[{typ}, Description({x.description!r})]"
                    args.append(f"{x.name}: {typ}")
                with m.def_(
                    node.operationId, "self", *args,
                ):
                    m.docstring(node.description)
                    m.stmt("pass")
                    pass
    return m


@as_command
def run(filename: str) -> None:
    d = loading.loadfile(filename)
    nodes = parse(d)
    print(emit(nodes))
