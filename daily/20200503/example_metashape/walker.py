import typing as t
import dataclasses
from metashape import runtime

Row = t.Tuple[str, t.Any, t.MutableMapping[str, t.Any]]


@dataclasses.dataclass
class Item:
    cls: t.Type[t.Any]
    fields: t.List[Row]


def walk(classes: t.List[t.Type[t.Any]]) -> t.Iterator[Item]:
    w = runtime.get_walker(classes)
    for cls in w.walk(kinds=["object", None]):
        fields: t.List[Row] = []
        for name, typeinfo, metadata in w.for_type(cls).walk(ignore_private=False):
            if name.startswith("_") and name.endswith("_"):
                continue

            if metadata is None:
                metadata = {}
            if typeinfo.normalized.__module__ != "builtins":
                w.append(typeinfo.normalized)
            fields.append((name, typeinfo, metadata))
        yield Item(cls=cls, fields=fields)
