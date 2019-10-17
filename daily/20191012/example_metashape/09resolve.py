import typing as t
import typing_extensions as tx
import typing_inspect


TypeInfo = t.Union["TypeInfoAtom", "TypeInfoContainer"]


class TypeInfoContainer(tx.TypedDict, total=True):
    raw: t.Type[t.Any]  # t.Optional[t.List[int]] -> t.Optional[t.List[int]]
    container: t.Type[t.Any]
    items: t.Tuple[TypeInfo]

    is_optional: bool  # t.Optional[int] -> True, int -> False
    is_composite: bool  # t.Union -> True, dict -> False
    is_primitive: bool  # int -> True, Person -> False, t.List[int] -> True   # xxx?


class TypeInfoAtom(tx.TypedDict, total=True):
    raw: t.Type[t.Any]  # t.Optional[t.List[int]] -> t.Optional[t.List[int]]
    args: t.Sequence[t.Type[t.Any]]
    base: t.Type[t.Any]  # t.Optionall[int] -> int, t.Dict[str, t.Any] -> dict

    is_optional: bool  # t.Optional[int] -> True, int -> False
    is_composite: bool  # t.Union -> True, dict -> False
    is_primitive: bool  # int -> True, Person -> False, t.List[int] -> True   # xxx?


def resolve_type_info(
    typ: t.Type[t.Any], *, is_optional=False, container=None, _nonetype=type(None)
) -> TypeInfo:
    raw = typ
    args = typing_inspect.get_args(typ)
    base = getattr(typ, "__origin__", None)
    if base is None:
        if not hasattr(typ, "__iter__"):
            base = typ  # xxx
        elif issubclass(typ, str):
            base = typ
        elif issubclass(typ, t.Sequence):
            return {
                "raw": raw,
                "items": (resolve_type_info(t.Any),),
                "container": list,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            }
        elif issubclass(typ, t.Mapping):
            return {
                "raw": raw,
                "items": (resolve_type_info(t.Any), resolve_type_info(t.Any)),
                "container": dict,
                "is_optional": False,
                "is_composite": False,
                "is_primitive": True,
            }
        else:
            base = typ  # xxx
    else:
        if len(args) == 2 and base == t.Union:
            is_optional = True
            if args[0] == _nonetype:
                base = args[1]
                args = (args[1],)
            elif args[1] == _nonetype:
                base = args[0]
                args = (args[0],)
            typ = args[0]
        if hasattr(typ, "__origin__"):
            base = typ.__origin__
            if issubclass(base, t.Sequence):
                # TODO: tuple
                args = typing_inspect.get_args(typ)
                return {
                    "raw": raw,
                    "items": tuple([resolve_type_info(t) for t in args]),
                    "container": list,
                    "is_optional": is_optional,
                    "is_composite": False,
                    "is_primitive": True,
                }
            elif issubclass(base, t.Mapping):
                args = typing_inspect.get_args(typ)
                return {
                    "raw": raw,
                    "items": tuple([resolve_type_info(t) for t in args]),
                    "container": dict,
                    "is_optional": is_optional,
                    "is_composite": False,
                    "is_primitive": True,
                }
            else:
                raise ValueError(f"unsuported type %{typ}")
    if not args:
        args = (base,)
    return {
        "raw": raw,
        "args": args,  # xxx
        "base": base,
        "is_optional": is_optional,
        "is_composite": False,
        "is_primitive": True,
    }


def resolve_type(origin: t.Type[t.Any], *, unknown="object") -> str:
    if issubclass(origin, str):
        return "string"
    elif hasattr(origin, "keys"):
        return "object"
    elif issubclass(origin, bool):
        return "boolean"
    elif issubclass(origin, int):
        return "integer"
    elif issubclass(origin, float):
        return "number"
    elif issubclass(origin, (list, tuple)):  # sequence?
        return "array"
    else:
        return unknown
