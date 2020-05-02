import typing as t
import typing_extensions as tx
import dataclasses
from metashape import runtime
from metashape.declarative import MISSING


def metadata(
    *, inline: bool = False, required: bool = True, comment: str = ""
) -> t.Dict[str, t.Any]:
    # todo: required false?
    d: Metadata = {"inline": inline, "required": required, "comment": comment}
    return d  # type: ignore


class Metadata(tx.TypedDict, total=False):
    inline: bool
    required: bool
    comment: str
    default: t.Any


Row = t.Tuple[str, t.Any, Metadata]


@dataclasses.dataclass
class Item:
    type_: t.Type[t.Any]
    fields: t.List[Row]


def walk(classes: t.List[t.Type[t.Any]]) -> t.Iterator[Item]:
    w = runtime.get_walker(classes)
    for cls in w.walk(kinds=["object", None]):
        fields: t.List[Row] = []
        for name, typeinfo, _metadata in w.for_type(cls).walk(ignore_private=False):
            if name.startswith("_") and name.endswith("_"):
                continue

            metadata = t.cast(Metadata, dict(_metadata).copy() if _metadata else {})
            if metadata.get("default") == MISSING:
                metadata.pop("default")

            if typeinfo.normalized.__module__ != "builtins":
                w.append(typeinfo.normalized)
            if hasattr(typeinfo.normalized, "__origin__"):  # list, dict, etc..
                for subtyp in t.get_args(typeinfo.normalized):
                    if subtyp.__module__ != "builtins":
                        w.append(subtyp)

            fields.append((name, typeinfo, metadata))
        yield Item(type_=cls, fields=fields)
