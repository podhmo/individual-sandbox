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
    tags: t.Dict[str, t.List[str]]


Row = t.Tuple[str, t.Any, Metadata]


@dataclasses.dataclass
class Item:
    type_: t.Type[t.Any]
    fields: t.List[Row]
    args: t.List[t.Type[t.Any]]

    @property
    def is_object(self):
        return not self.args

    @property
    def is_union(self):
        return not self.fields


def walk(
    classes: t.List[t.Type[t.Any]], *, _nonetype: t.Type[t.Any] = type(None)
) -> t.Iterator[Item]:
    w = runtime.get_walker(classes)
    for cls in w.walk(kinds=["object", None]):
        if (
            getattr(cls, "__origin__", None) == t.Union
            and _nonetype not in cls.__args__
        ):
            yield Item(type_=cls, fields=[], args=cls.__args__)
            for subtyp in cls.__args__:
                if subtyp.__module__ != "builtins":
                    w.append(subtyp)
            continue

        fields: t.List[Row] = []
        for name, typeinfo, _metadata in w.for_type(cls).walk(ignore_private=False):
            if name.startswith("_") and name.endswith("_"):
                continue

            filled_metadata: Metadata = metadata()
            filled_metadata.update(_metadata)
            if filled_metadata.get("default") == MISSING:
                filled_metadata.pop("default")
            if getattr(typeinfo, "is_optional", False):  # xxx
                filled_metadata["required"] = False

            if typeinfo.normalized.__module__ != "builtins":
                w.append(typeinfo.normalized)
            if hasattr(typeinfo.normalized, "__origin__"):  # list, dict, etc..
                for subtyp in t.get_args(typeinfo.normalized):
                    if subtyp.__module__ != "builtins":
                        w.append(subtyp)

            fields.append((name, typeinfo, filled_metadata))
        yield Item(type_=cls, fields=fields, args=[])
